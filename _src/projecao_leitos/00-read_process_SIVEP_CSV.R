#PRJROOT = rprojroot::find_root(".here")
#PRJROOT = "~/code"
#P = function(...) file.path(PRJROOT, ...)
#DATAROOT = "~/data/Municipio_SP/SRAG_hospitalizados/dados"

srag_files = sort(grep("SRAGH_2020", dir(file.path(DATAROOT), full.names = TRUE), value = TRUE))

### Set if looking for specific date
#data_date = as.Date("2020-04-02")
#data_date = NULL\

if (is.null(data_date)) {
  data_date <- as.Date(get.last.date(DATAROOT), format = "%Y_%m_%d")
}

say(paste("Data date is set to:", format(data_date, "%d %B %Y")), "cow")

srag.20.raw <- read.sivep(dir = DATAROOT,
                          escala = escala,
                          geocode = geocode,
                          data = format(data_date, "%Y_%m_%d"),
                          residentes = FALSE)
dt.cols <- grepl("dt_", names(srag.20.raw))
names(srag.20.raw)[dt.cols] <- substr(names(srag.20.raw[,dt.cols]), start=1, stop=6)

dt_cols_class = sapply(srag.20.raw[,dt.cols], is.Date)
if(!all(dt_cols_class))
  srag.20.raw[,dt.cols][,!dt_cols_class] = lapply(srag.20.raw[,dt.cols][,!dt_cols_class], as.Date, format = "%Y-%m-%d")

## Datas de primeiro sintoma e notificação
set_week_start("Sunday") ## para incluir semana epidemiologica

srag.dt = srag.20.raw %>%
  dplyr::rename(UTI = uti) %>%
  dplyr::filter(hospital == 1) %>%
  dplyr::select(dt_int, dt_not, dt_dig, dt_pcr, dt_sin, dt_evo, dt_enc, dt_ent, dt_sai, 
                UTI, evolucao, nu_idade_n, pcr_sars2, classi_fin) %>%
  dplyr::rename(dt_entuti=dt_ent, dt_saiuti=dt_sai) %>%
  dplyr::mutate(dt_rec = pmax(dt_not, dt_dig, dt_pcr, na.rm = T)) %>%
  dplyr::mutate(dt_mnd = pmax(dt_not, dt_dig, na.rm = T)) %>% #max not dig
  dplyr::filter(!is.na(dt_sin) & !is.na(dt_rec) & !dt_int >= today() & !dt_sin >= today()) %>%
  dplyr::mutate(ID = 1:nrow(.)) %>%
  dplyr::mutate(nu_idade_n = as.numeric(nu_idade_n)) %>%
  mutate(age_class = classifyAgeFast(nu_idade_n)) %>%
  mutate(dt_evo = coalesce(dt_evo, dt_enc)) %>%
  mutate(dt_entuti = if_else(UTI==1, coalesce(dt_entuti, dt_int), as.Date(NA))) %>%
  mutate(dt_saiuti = if_else(UTI==1 & !is.na(evolucao) & !is.na(dt_evo), 
                             coalesce(dt_saiuti, dt_evo), as.Date(NA))) 

covid.dt = srag.dt %>%
  dplyr::filter(pcr_sars2 == 1 | classi_fin == 5) 

if(!exists("fix_missing_dates") || fix_missing_dates){
  if(!exists("fix_missing_dates"))
    warning("'fix_missing_dates' variable is missing, assuming TRUE.")
  excluded = srag.dt %>%
    dplyr::filter((!is.na(evolucao) & is.na(dt_evo))) %>%
    nrow
  if(excluded > 0){
    warning(paste("Fixing", excluded, "records with inconsistent dates."))
    load(C("hospitalStatsFits.Rdata"))
    if(!require(brms)){install.packages("brms"); library(brms)}
    srag.dt %<>%
      ddply(.(ID), fixUTIDates, time_fits0$srag$UTI) %>%
      ddply(.(ID), fixEVODates, time_fits0$srag$afterUTI, time_fits0$srag$notUTI) %>%
      as.data.frame()
    covid.dt %<>%
      ddply(.(ID), fixUTIDates, time_fits0$covid$UTI) %>%
      ddply(.(ID), fixEVODates, time_fits0$covid$afterUTI, time_fits0$covid$notUTI) %>%
      as.data.frame()
  }
} else{
  excluded = srag.dt %>%
    dplyr::filter((!is.na(evolucao) & is.na(dt_evo))) %>%
    nrow
  warning(paste("Excluded", excluded, "records for inconsistent dates."))
  srag.dt = srag.dt %>%
    dplyr::filter(!(!is.na(evolucao) & is.na(dt_evo)))  %>%
    as.data.frame()
  covid.dt = srag.dt %>%
    dplyr::filter(pcr_sars2 == 1)  %>%
    as.data.frame()
}

say(paste("READ FILES FOR DATE:", format(data_date, "%d %B %Y")), "cat")
