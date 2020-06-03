if(!require(optparse)){install.packages("optparse"); library(optparse)}
if(!require(Hmisc)){install.packages("Hmisc"); library(Hmisc)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}


if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../dados/estado_SP/SRAG_hospitalizados/dados/",
                metavar = "dir"),
    make_option("--tipo", default = "covid",
                help = ("tipo da internação. 'srag' ou 'covid."),
                metavar = "tipo"),
    make_option("--escala", default = "municipio",
                 help = ("Nível administrativo, um de: municipio, micro, meso, estado, país"),
                 metavar = "escala"),
    make_option("--sigla", default = "SP", # ainda nao estamos usando
                 help = ("Sigla do estado a ser atualizado"),
                 metavar = "sigla"),
    make_option("--geocode", default = 3550308,
                help = ("Geocode de município, micro-mesorregião ou estado"),
                metavar = "geocode"),
    make_option("--window", type = "integer", default = 40,
                help = ("Largura da running window do nowcasting (dias)"),
                metavar = "window"),
    make_option("--nAdapt", type = "integer", default = 3000,
                help = ("Iterações de adaptação"),
                metavar = "nAdapt"),
    make_option("--nBurnin", type = "integer", default = 3000,
                help = ("Iterações de burnin"),
                metavar = "nBurnin"),
    make_option("--nThin", type = "integer", default = 10,
                help = ("Trim das iterações"),
                metavar = "nThin"),
    make_option("--nSamp", type = "integer", default = 1000,
                help = ("Número de amostras da posterior"),
                metavar = "nSamp"),
    make_option("--dataBase", default = NULL,
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--dataInicial", default = as.Date("2020-03-08"),
                help = ("Data do início dos casos, formato 'yyyy_mm_dd'"),
                metavar = "dataInicial"),
    make_option("--formatoData", default = "%Y_%m_%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),#ast antes de tirar checar outras fontes de dados
                metavar = "formatoData"),
    make_option("--fix_dates", default = FALSE,
                help = ("Booleana. Define se as os casos com datas inconsistentes devem ser concertadas (TRUE) ou excluídas (FALSE)"),
                metavar = "fix_dates"),
    make_option("--out_dir", default = NULL,
                help = ("Pasta de saída dos resultados"),
                metavar = "out_dir")
        # make_option("--updateGit", default = "FALSE",
    #             help = ("Fazer git add, commit e push?"),
    #             metavar = "updateGit")
  )


  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para importar csv da sivep gripe,
                                executar nowcasting e salvar os resultados")
  
  ## aliases
  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)
  DATAROOT <- opt$options$dir
  out.root <- if(is.null(opt$options$out_dir)) {"../dados_processados"} else opt$options$out_dir
  disease <- opt$options$tipo
  escala <- opt$options$escala
  sigla <- opt$options$sigla
  geocode <- opt$options$geocode
  window <- opt$options$window
  data_date <- if(is.null(opt$options$dataBase)) {NULL} else as.Date(opt$options$dataBase, format("%Y_%m_%d"))
  initial_date <- as.Date(opt$options$dataInicial)
  formato.data <- opt$options$formatoData
  fix_missing_dates <- opt$options$fix_dates
  nAdapt = opt$options$nAdapt
  nBurnin = opt$options$nBurnin
  nThin = opt$options$nThin
  nSamp = opt$options$nSamp


}

####################################################
### to run INTERACTIVELY:
#You only have to set up the variables that are not already set up above or the ones that you would like to change #
#geocode <- "3550308" # municipio SP
# geocode <- "3509502" # municipio Campinas
# DATAROOT <- "../dados/municipio_campinas/SRAG_hospitalizados/dados/"
# intial_date <- as.Date("2020-03-16")
#data <- "2020_05_16"
# DATAROOT = "../dados/estado_SP/SRAG_hospitalizados/dados/"
# initial_date = as.Date("2020-03-08")
# data_date = as.Date("2020_05_27", format = "%Y_%m_%d")
# escala  = "drs"
# sigla = "SP"
# geocode = 1  
# out.root =  "../dados_processados/"
# nowcasting = FALSE 
# fit_models = FALSE 
# make_report = TRUE
# report.dir  = "~/Desktop"
#######################################################


if(exists("PRJROOT")){
  if(!is.null(PRJROOT)) 
    PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  
} else 
  PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  

P = function(...) file.path(PRJROOT, ...)

source(P("fct/load_packages.R"))
source(P("_src/funcoes.R"))

name_path <- check.geocode(escala = escala, geocode = geocode, sigla = sigla)#ast falta checar outras escalas e fontes de dados e destinos para push
label_escala = check.geocode(geocode = geocode, escala = escala, sigla = sigla, nonascii = FALSE)
output.dir <- file.path(out.root, "nowcast_posterior", name_path)

if (!file.exists(output.dir))
  dir.create(output.dir, showWarnings = TRUE, recursive = TRUE)

if (is.null(data_date)) {
  data_date <- as.Date(get.last.date(DATAROOT), format = "%Y_%m_%d")
}

say(paste("Data date is set to:", format(data_date, "%d %B %Y"), "\nScale:", escala, "\nLocal:", label_escala), "cow")

O = function(...) file.path(output.dir, ...)
P = function(...) file.path(PRJROOT, ...)
CODEROOT = paste0(PRJROOT, "/_src/projecao_leitos")
C = function(...) file.path(CODEROOT, ...)	

srag_files = sort(grep("SRAGH_2020", dir(file.path(DATAROOT), full.names = TRUE), value = TRUE))

### Set if looking for specific date
#data_date = as.Date("2020-04-02")
#data_date = NULL\

srag.20.raw <- read.sivep(dir = DATAROOT,
                          escala = escala,
                          geocode = geocode,
                          data = format(data_date, "%Y_%m_%d"),
                          sigla = sigla,
                          residentes = TRUE)

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

if(disease=="srag") dados = srag.dt
if(disease=="covid") dados = covid.dt
now.Date  <-  max(dados$dt_sin)
nowcast_posterior <- NobBS.posterior(
  data = dados,
  now = now.Date,
  onset_date = "dt_sin",
  report_date = "dt_rec",
  units = "1 day",
  moving_window =  window,
  specs = list(nAdapt = nAdapt, nBurnin = nBurnin, nThin = nThin, nSamp = nSamp)
)

write_csv(nowcast_posterior, path = O(paste0(data_date, "_posterior_trajetoria_nowcast.csv")))