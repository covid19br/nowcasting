# carrega funcoes----
source("_src/funcoes.R")
# Libraries
library(optparse)
library(rmarkdown)

################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../dados/SIVEP-Gripe",
                metavar = "dir"),
    make_option("--outputDir",
                default = "../dados_processados/integridade_SIVEP",
                help = ("Diretório de destino"),
                metavar = "outputDir"),
    make_option("--updateGit", default = "FALSE",
                help = ("Fazer git add, commit e push?"),
                metavar = "updateGit")
  )
  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para checar base")

  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                    positional_arguments = TRUE)
  dir <- opt$options$dir
  out.dir <- opt$options$outputDir
  update.git <- opt$options$updateGit
}

file.names <- list.files(dir, pattern = paste0(".*", ".(csv|zip)"), full.names = TRUE)
datas <- stringr::str_extract(file.names,
                              "(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
          as.Date(format = "%Y_%m_%d")

db.info <- data.frame(data = datas,
                      file = basename(file.names),
                      size.read = NA,
                      size.file = NA,
                      casos.covid = NA,
                      casos.srag = NA,
                      obitos.covid = NA,
                      obitos.srag = NA)
N <- dim(db.info)[1]
dados_covid_br <- list()
dados_srag_br <- list()
dados_obcovid_br <- list()
dados_obsrag_br <- list()
dados_covid_est <- list()
dados_srag_est <- list()
dados_obcovid_est <- list()
dados_obsrag_est <- list()

old_dados_covid_br <- read.csv(paste0(out.dir, "/dados_covid_br.csv"))
old_dados_covid_br$dt_sin_pri <- as.Date(old_dados_covid_br$dt_sin_pri)

old_dados_srag_br <- read.csv(paste0(out.dir, "/dados_srag_br.csv"))
old_dados_srag_br$dt_sin_pri <- as.Date(old_dados_srag_br$dt_sin_pri)

old_dados_obcovid_br <- read.csv(paste0(out.dir, "/dados_obcovid_br.csv"))
old_dados_obcovid_br$dt_evoluca <- as.Date(old_dados_obcovid_br$dt_evoluca)

old_dados_obsrag_br <- read.csv(paste0(out.dir, "/dados_obsrag_br.csv"))
old_dados_obsrag_br$dt_evoluca <- as.Date(old_dados_obsrag_br$dt_evoluca)

old_dados_covid_est <- read.csv(paste0(out.dir, "/dados_covid_est.csv"))
old_dados_covid_est$dt_sin_pri <- as.Date(old_dados_covid_est$dt_sin_pri)

old_dados_srag_est <- read.csv(paste0(out.dir, "/dados_srag_est.csv"))
old_dados_srag_est$dt_sin_pri <- as.Date(old_dados_srag_est$dt_sin_pri)

old_dados_obcovid_est <- read.csv(paste0(out.dir, "/dados_obcovid_est.csv"))
old_dados_obcovid_est$dt_evoluca <- as.Date(old_dados_obcovid_est$dt_evoluca)

old_dados_obsrag_est <- read.csv(paste0(out.dir, "/dados_obsrag_est.csv"))
old_dados_obsrag_est$dt_evoluca <- as.Date(old_dados_obsrag_est$dt_evoluca)

for (i in seq(N)) {
  # pula bases que já têm dados
  datadash <- format(db.info[i,"data"], "%Y-%m-%d")
  if (exists(old_dados_covid_br) & datadash %in% old_dados_covid_br$data)
      next

  data <- format(db.info[i,"data"], "%Y_%m_%d")
  dados <- read.sivep(dir = dir, escala = "pais",
                      data = data)
  db.info[i, "size.read"] <- dim(dados)[1]
  db.info[i, "size.file"] <- count.lines(paste0(dir, "/", db.info[i, "file"]))

  ## 1.1 casos covid ####
  dados2 <- dados %>%
     filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
     select(dt_notific, dt_sin_pri, dt_pcr, dt_digita, sg_uf) %>%
     mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE)) %>%
     filter(!is.na(dt_pcr_dig))

  dados_covid_br[[i]] <- dados2 %>%
     group_by(dt_sin_pri) %>%
     summarise(n = n())

  db.info[i, "casos.covid"] <- sum(dados_covid_br[[i]]$n)

  dados_covid_est[[i]] <- dados2 %>%
    group_by(dt_sin_pri, sg_uf) %>%
    summarise(n = n())

  ## 1.2. casos srag ####
  dados2 <- dados %>%
    select(dt_notific, dt_sin_pri, dt_digita, sg_uf) %>%
    mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) %>% # nome aqui é pcr mas não tem pcr
    filter(!is.na(dt_pcr_dig))

  dados_srag_br[[i]] <- dados2 %>%
    group_by(dt_sin_pri) %>%
    summarise(n = n())

  db.info[i, "casos.srag"] <- sum(dados_srag_br[[i]]$n)

  dados_srag_est[[i]] <- dados2 %>%
    group_by(dt_sin_pri, sg_uf) %>%
    summarise(n = n())

  ## 2.1. obitos covid ####
  dados2 <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, sg_uf) %>%
    filter(! is.na(dt_encerra))

  dados_obcovid_br[[i]] <- dados2 %>%
    group_by(dt_evoluca) %>%
    summarise(n = n())

  db.info[i, "obitos.covid"] <- sum(dados_obcovid_br[[i]]$n)

  dados_obcovid_est[[i]] <- dados2 %>%
    group_by(dt_evoluca, sg_uf) %>%
    summarise(n = n())

  ## 2.2. obitos srag ####
  dados2 <- dados %>%
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, sg_uf) %>%
    filter(! is.na(dt_encerra))

  dados_obsrag_br[[i]] <- dados2 %>%
    group_by(dt_evoluca) %>%
    summarise(n = n())

  db.info[i, "obitos.srag"] <- sum(dados_obsrag_br[[i]]$n)

  dados_obsrag_est[[i]] <- dados2 %>%
    group_by(dt_evoluca, sg_uf) %>%
    summarise(n = n())
}

data_v <- as.character(datas)

if (length(dados_covid_br) > 0) {
  names(dados_covid_br) <- data_v
  names(dados_srag_br) <- data_v
  names(dados_obcovid_br) <- data_v
  names(dados_obsrag_br) <- data_v
  names(dados_covid_est) <- data_v
  names(dados_srag_est) <- data_v
  names(dados_obcovid_est) <- data_v
  names(dados_obsrag_est) <- data_v
  dados_covid_br <- bind_rows(dados_covid_br, .id="data")
  dados_srag_br <- bind_rows(dados_srag_br, .id="data")
  dados_obcovid_br <- bind_rows(dados_obcovid_br, .id="data")
  dados_obsrag_br <- bind_rows(dados_obsrag_br, .id="data")
  dados_covid_est <- bind_rows(dados_covid_est, .id="data")
  dados_srag_est <- bind_rows(dados_srag_est, .id="data")
  dados_obcovid_est <- bind_rows(dados_obcovid_est, .id="data")
  dados_obsrag_est <- bind_rows(dados_obsrag_est, .id="data")

  dados_covid_br <- rbind(dados_covid_br, old_dados_covid_br)
  dados_srag_br <- rbind(dados_srag_br, old_dados_srag_br)
  dados_obcovid_br <- rbind(dados_obcovid_br, old_dados_obcovid_br)
  dados_obsrag_br <- rbind(dados_obsrag_br, old_dados_obsrag_br)
  dados_covid_est <- rbind(dados_covid_est, old_dados_covid_est)
  dados_srag_est <- rbind(dados_srag_est, old_dados_srag_est)
  dados_obcovid_est <- rbind(dados_obcovid_est, old_dados_obcovid_est)
  dados_obsrag_est <- rbind(dados_obsrag_est, old_dados_obsrag_est)

  write.csv(dados_covid_br, paste0(out.dir, "/dados_covid_br.csv"), row.names=FALSE)
  write.csv(dados_srag_br, paste0(out.dir, "/dados_srag_br.csv"), row.names=FALSE)
  write.csv(dados_obcovid_br, paste0(out.dir, "/dados_obcovid_br.csv"), row.names=FALSE)
  write.csv(dados_obsrag_br, paste0(out.dir, "/dados_obsrag_br.csv"), row.names=FALSE)
  write.csv(dados_covid_est, paste0(out.dir, "/dados_covid_est.csv"), row.names=FALSE)
  write.csv(dados_srag_est, paste0(out.dir, "/dados_srag_est.csv"), row.names=FALSE)
  write.csv(dados_obcovid_est, paste0(out.dir, "/dados_obcovid_est.csv"), row.names=FALSE)
  write.csv(dados_obsrag_est, paste0(out.dir, "/dados_obsrag_est.csv"), row.names=FALSE)
} else {
  dados_covid_br <- old_dados_covid_br
  dados_srag_br <- old_dados_srag_br
  dados_obcovid_br <- old_dados_obcovid_br
  dados_obsrag_br <- old_dados_obsrag_br
  dados_covid_est <- old_dados_covid_est
  dados_srag_est <- old_dados_srag_est
  dados_obcovid_est <- old_dados_obcovid_est
  dados_obsrag_est <- old_dados_obsrag_est
}

### plots

plot.covid <- ggplot(dados_covid_br,
                     aes(x = dt_sin_pri, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
    scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL

plot.srag <- ggplot(dados_srag_br,
                     aes(x = dt_sin_pri, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
  xlab("Dia do primeiro sintoma") +
  ylab("Número de novos casos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL

plot.obitos.covid <- ggplot(dados_obcovid_br,
                    aes(x = dt_evoluca, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL

plot.obitos.srag <- ggplot(dados_obsrag_br,
                    aes(x = dt_evoluca, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL


## plots com facets por estado

plot.covid.est <- ggplot(dados_covid_est,
                     aes(x = dt_sin_pri, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
    scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  facet_wrap(~ sg_uf, scales="free", ncol=4) +
  NULL

plot.srag.est <- ggplot(dados_srag_est,
                     aes(x = dt_sin_pri, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
  xlab("Dia do primeiro sintoma") +
  ylab("Número de novos casos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  facet_wrap(~ sg_uf, scales="free", ncol=4) +
  NULL

plot.obitos.covid.est <- ggplot(dados_obcovid_est,
                    aes(x = dt_evoluca, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  facet_wrap(~ sg_uf, scales="free", ncol=4) +
  NULL

plot.obitos.srag.est <- ggplot(dados_obsrag_est,
                    aes(x = dt_evoluca, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  facet_wrap(~ sg_uf, scales="free", ncol=4) +
  NULL


if (!file.exists(out.dir))
  dir.create(out.dir, showWarnings = TRUE, recursive = TRUE)

data <- max(db.info$data)

fname <- paste0('integridade_SIVEP_', data, '.html')
render(input = 'integridade_sivep.Rmd',
       output_file = fname,
       output_dir = out.dir)

if (update.git) {
  tabelas <- paste("dados_covid_br.csv",
                   "dados_srag_br.csv",
                   "dados_obcovid_br.csv",
                   "dados_obsrag_br.csv",
                   "dados_covid_est.csv",
                   "dados_srag_est.csv",
                   "dados_obcovid_est.csv",
                   "dados_obsrag_est.csv")

  system(paste("cd", out.dir, "&& git pull"))
  system(paste("cd", out.dir, "&& git add", fname, tabelas,
               "&& git commit -m ':robot: relatório integridade SIVEP de",
               max(db.info$data), "'",
               "&& git push")
               )
}

