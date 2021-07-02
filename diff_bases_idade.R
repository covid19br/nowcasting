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
    make_option("--facetEstados",
                default = "TRUE",
                help = ("Fazer facets de estados?"),
                metavar = "facetEstados"),
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
  facet.estados <- opt$options$facetEstados
  update.git <- opt$options$updateGit
}

file.names <- list.files(dir, pattern = paste0(".*202.*", ".(csv|zip|csv.xz)"), full.names = TRUE)
datas <- stringr::str_extract(file.names,
                              "(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
          as.Date(format = "%Y_%m_%d")

if (file.exists(paste0(out.dir, "/age_db.info.csv"))) {
  old.db.info <- read.csv(paste0(out.dir, "/age_db.info.csv"))
  # só datas novas serão analisadas
  old.db.info$data <- as.Date(old.db.info$data)
  datasn <- datas[which(! datas %in% old.db.info$data)]
} else {
    datasn <- datas
}

if (length(datasn) == 0) {
  db.info <- old.db.info
} else {
  db.info <- data.frame(data = datasn,
                        file = basename(file.names[which(datas %in% datasn)]),
                        size.read = NA,
                        size.file = NA,
                        casos.covid = NA,
                        casos.srag = NA,
                        obitos.covid = NA,
                        obitos.srag = NA)
}

dados_covid_br <- list()
dados_srag_br <- list()
dados_obcovid_br <- list()
dados_obsrag_br <- list()
if (facet.estados) {
  dados_covid_est <- list()
  dados_srag_est <- list()
  dados_obcovid_est <- list()
  dados_obsrag_est <- list()
}

N <- length(datasn)
for (i in seq(length.out = N)) {
  data <- format(db.info[i,"data"], "%Y_%m_%d")
  print(paste("Lendo base de dados de", data))
  dados <- read.sivep(dir = dir, escala = "pais",
                      data = data) %>%
    dplyr::mutate(nu_idade_n = as.numeric(nu_idade_n)) %>%
    plyr::mutate(age_class = classifyAgeFast(nu_idade_n)) %>%
    select(dt_notific, dt_sin_pri, dt_pcr, dt_digita, sg_uf, age_class,
           dt_evoluca, dt_encerra, pcr_sars2, classi_fin, evolucao)
  db.info[i, "size.read"] <- dim(dados)[1]
  db.info[i, "size.file"] <- count.lines(paste0(dir, "/", db.info[i, "file"]))


  ## 1.1 casos covid ####
  dados2 <- dados %>%
     filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
     select(dt_notific, dt_sin_pri, dt_pcr, dt_digita, age_class, sg_uf) %>%
     mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE)) %>%
     filter(!is.na(dt_pcr_dig))

  dados_covid_br[[i]] <- dados2 %>%
     group_by(dt_sin_pri, age_class) %>%
     summarise(n = n())

  db.info[i, "casos.covid"] <- sum(dados_covid_br[[i]]$n)

  if (facet.estados) {
    dados_covid_est[[i]] <- dados2 %>%
      group_by(dt_sin_pri, age_class, sg_uf) %>%
      summarise(n = n())
  }

  ## 1.2. casos srag ####
  dados2 <- dados %>%
    select(dt_notific, dt_sin_pri, dt_digita, age_class, sg_uf) %>%
    mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) %>% # nome aqui é pcr mas não tem pcr
    filter(!is.na(dt_pcr_dig))

  dados_srag_br[[i]] <- dados2 %>%
    group_by(dt_sin_pri, age_class) %>%
    summarise(n = n())

  db.info[i, "casos.srag"] <- sum(dados_srag_br[[i]]$n)

  if (facet.estados) {
    dados_srag_est[[i]] <- dados2 %>%
      group_by(dt_sin_pri, age_class, sg_uf) %>%
      summarise(n = n())
  }

  ## 2.1. obitos covid ####
  dados2 <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, age_class, sg_uf) %>%
    filter(! is.na(dt_encerra))

  dados_obcovid_br[[i]] <- dados2 %>%
    group_by(dt_evoluca, age_class) %>%
    summarise(n = n())

  db.info[i, "obitos.covid"] <- sum(dados_obcovid_br[[i]]$n)

  if (facet.estados) {
    dados_obcovid_est[[i]] <- dados2 %>%
      group_by(dt_evoluca, age_class, sg_uf) %>%
      summarise(n = n())
  }

  ## 2.2. obitos srag ####
  dados2 <- dados %>%
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, age_class, sg_uf) %>%
    filter(! is.na(dt_encerra))

  dados_obsrag_br[[i]] <- dados2 %>%
    group_by(dt_evoluca, age_class) %>%
    summarise(n = n())

  db.info[i, "obitos.srag"] <- sum(dados_obsrag_br[[i]]$n)

  if (facet.estados) {
    dados_obsrag_est[[i]] <- dados2 %>%
      group_by(dt_evoluca, age_class, sg_uf) %>%
      summarise(n = n())
  }

  # clean memory
  rm(dados, dados2)
  mem = gc()
  mem = sum(mem[,2])
  cat(paste("\n\n**** MEM USED:", mem, " MB ****\n\n"))
  if (mem > 4000) {
    print("Uso de RAM maior que 4G, parando")
    break
  }
}

if (N > 0) {
  datasn <- datasn[1:i]
  data_v <- as.character(datasn)
  names(dados_covid_br) <- data_v
  names(dados_srag_br) <- data_v
  names(dados_obcovid_br) <- data_v
  names(dados_obsrag_br) <- data_v
  dados_covid_br <- bind_rows(dados_covid_br, .id="data")
  dados_srag_br <- bind_rows(dados_srag_br, .id="data")
  dados_obcovid_br <- bind_rows(dados_obcovid_br, .id="data")
  dados_obsrag_br <- bind_rows(dados_obsrag_br, .id="data")
  if (facet.estados) {
    names(dados_covid_est) <- data_v
    names(dados_srag_est) <- data_v
    names(dados_obcovid_est) <- data_v
    names(dados_obsrag_est) <- data_v
    dados_covid_est <- bind_rows(dados_covid_est, .id="data")
    dados_srag_est <- bind_rows(dados_srag_est, .id="data")
    dados_obcovid_est <- bind_rows(dados_obcovid_est, .id="data")
    dados_obsrag_est <- bind_rows(dados_obsrag_est, .id="data")
  }

  if (exists("old.db.info")) {
    if (i < N)
      db.info <- db.info[1:i,]
    db.info <- rbind(db.info, old.db.info)

    # carregando dados anteriores
    old_dados_covid_br <- read.csv(paste0(out.dir, "/age_dados_covid_br.csv"))
    old_dados_covid_br$dt_sin_pri <- as.Date(old_dados_covid_br$dt_sin_pri)
    
    old_dados_srag_br <- read.csv(paste0(out.dir, "/age_dados_srag_br.csv"))
    old_dados_srag_br$dt_sin_pri <- as.Date(old_dados_srag_br$dt_sin_pri)
    
    old_dados_obcovid_br <- read.csv(paste0(out.dir, "/age_dados_obcovid_br.csv"))
    old_dados_obcovid_br$dt_evoluca <- as.Date(old_dados_obcovid_br$dt_evoluca)
    
    old_dados_obsrag_br <- read.csv(paste0(out.dir, "/age_dados_obsrag_br.csv"))
    old_dados_obsrag_br$dt_evoluca <- as.Date(old_dados_obsrag_br$dt_evoluca)
  
    if (facet.estados) {
      old_dados_covid_est <- read.csv(paste0(out.dir, "/age_dados_covid_est.csv"))
      old_dados_covid_est$dt_sin_pri <- as.Date(old_dados_covid_est$dt_sin_pri)
      
      old_dados_srag_est <- read.csv(paste0(out.dir, "/age_dados_srag_est.csv"))
      old_dados_srag_est$dt_sin_pri <- as.Date(old_dados_srag_est$dt_sin_pri)
      
      old_dados_obcovid_est <- read.csv(paste0(out.dir, "/age_dados_obcovid_est.csv"))
      old_dados_obcovid_est$dt_evoluca <- as.Date(old_dados_obcovid_est$dt_evoluca)
      
      old_dados_obsrag_est <- read.csv(paste0(out.dir, "/age_dados_obsrag_est.csv"))
      old_dados_obsrag_est$dt_evoluca <- as.Date(old_dados_obsrag_est$dt_evoluca)
    }

    dados_covid_br <- rbind(dados_covid_br, old_dados_covid_br)
    dados_srag_br <- rbind(dados_srag_br, old_dados_srag_br)
    dados_obcovid_br <- rbind(dados_obcovid_br, old_dados_obcovid_br)
    dados_obsrag_br <- rbind(dados_obsrag_br, old_dados_obsrag_br)
    if (facet.estados) {
      dados_covid_est <- rbind(dados_covid_est, old_dados_covid_est)
      dados_srag_est <- rbind(dados_srag_est, old_dados_srag_est)
      dados_obcovid_est <- rbind(dados_obcovid_est, old_dados_obcovid_est)
      dados_obsrag_est <- rbind(dados_obsrag_est, old_dados_obsrag_est)
    }
  }

  write.csv(db.info, paste0(out.dir, "/age_db.info.csv"), row.names = FALSE)
  write.csv(dados_covid_br, paste0(out.dir, "/age_dados_covid_br.csv"), row.names=FALSE)
  write.csv(dados_srag_br, paste0(out.dir, "/age_dados_srag_br.csv"), row.names=FALSE)
  write.csv(dados_obcovid_br, paste0(out.dir, "/age_dados_obcovid_br.csv"), row.names=FALSE)
  write.csv(dados_obsrag_br, paste0(out.dir, "/age_dados_obsrag_br.csv"), row.names=FALSE)
  if (facet.estados) {
    write.csv(dados_covid_est, paste0(out.dir, "/age_dados_covid_est.csv"), row.names=FALSE)
    write.csv(dados_srag_est, paste0(out.dir, "/age_dados_srag_est.csv"), row.names=FALSE)
    write.csv(dados_obcovid_est, paste0(out.dir, "/age_dados_obcovid_est.csv"), row.names=FALSE)
    write.csv(dados_obsrag_est, paste0(out.dir, "/age_dados_obsrag_est.csv"), row.names=FALSE)
  }
} else {
  db.info <- old.db.info
  dados_covid_br <- old_dados_covid_br
  dados_srag_br <- old_dados_srag_br
  dados_obcovid_br <- old_dados_obcovid_br
  dados_obsrag_br <- old_dados_obsrag_br
  if (facet.estados) {
    dados_covid_est <- old_dados_covid_est
    dados_srag_est <- old_dados_srag_est
    dados_obcovid_est <- old_dados_obcovid_est
    dados_obsrag_est <- old_dados_obsrag_est
  }
}

if (update.git) {
  tabelas <- paste("age_db.info.csv",
                   "age_dados_covid_br.csv",
                   "age_dados_srag_br.csv",
                   "age_dados_obcovid_br.csv",
                   "age_dados_obsrag_br.csv")
  if (facet.estados) {
    tabelas <- paste(tabelas,
                   "age_dados_covid_est.csv",
                   "age_dados_srag_est.csv",
                   "age_dados_obcovid_est.csv",
                   "age_dados_obsrag_est.csv")
  }
  system(paste("cd", out.dir, "&& git pull"))
  system(paste("cd", out.dir, "&& git add", tabelas,
               "&& git commit -m ':robot: diff de bases por idade da SIVEP de",
               max(db.info$data), "'",
               "&& git push")
               )
}

