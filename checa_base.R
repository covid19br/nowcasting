# Libraries
library(optparse)
library(rmarkdown)
library(purrr)
library(dplyr)
library(ggplot2)

# carrega funcoes----
source("_src/funcoes.R")
source("fct/count.lines.R")

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
dados_covid <- list()
dados_srag <- list()
dados_obcovid <- list()
dados_obsrag <- list()
dados_br <- list()
dados_br2 <- list()
dados_br3 <- list()
dados_br4 <- list()
dados_estados <- list()
dados_estados2 <- list()
dados_estados3 <- list()
dados_estados4 <- list()

#le tudo de vez
all_sivep <- purrr::map(data_v,
                  ~read.sivep(dir = dir,
                              escala = "pais",
                             data = .))
#save(all_sivep, file = "all.rda")
#load("all_sivep.rda")
#ast deveria ter um jeito de guardar as tabelas anteriores e só ler a mais recente.
for (i in seq(N)) {
  data <- format(db.info[i,"data"], "%Y_%m_%d")
  dados <- all_sivep[[i]]
  db.info[i, "size.read"] <- dim(dados)[1]
  db.info[i, "size.file"] <- count.lines(paste0(dir, "/", db.info[i, "file"]))

filename <- paste0(dir, "/", db.info[i, "file"])
  ## 1.1 casos covid ####
   dados_covid[[i]] <- dados %>%
     filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
     select(dt_notific, dt_sin_pri, dt_pcr, dt_digita, sg_uf) %>%
     mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE)) %>%
     filter(!is.na(dt_pcr_dig))

  dados_br[[i]] <- dados_covid[[i]] %>%
     group_by(dt_sin_pri) %>%
     summarise(n = n())

  db.info[i, "casos.covid"] <- sum(dados_br[[i]]$n)

  dados_estados[[i]] <- dados_covid[[i]] %>%
    group_by(dt_sin_pri, sg_uf) %>%
    summarise(n = n())


   ## 1.2. casos srag ####
   dados_srag[[i]] <- dados %>%
     select(dt_notific, dt_sin_pri, dt_digita, sg_uf) %>%
     mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) %>% # nome aqui é pcr mas não tem pcr
     filter(!is.na(dt_pcr_dig))

  dados_br2[[i]] <- dados_srag[[i]] %>%
    group_by(dt_sin_pri) %>%
    summarise(n = n())

  db.info[i, "casos.srag"] <- sum(dados_br2[[i]]$n)

  dados_estados2[[i]] <- dados_srag[[i]] %>%
    group_by(dt_sin_pri, sg_uf) %>%
    summarise(n = n())

  ## 2.1. obitos covid ####
  dados_obcovid[[i]] <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, sg_uf) %>%
    filter(! is.na(dt_encerra))

  dados_br3[[i]] <- dados_obcovid[[i]] %>%
    group_by(dt_evoluca) %>%
    summarise(n = n())

  db.info[i, "obitos.covid"] <- sum(dados_br3[[i]]$n)

  dados_estados3[[i]] <- dados_obcovid[[i]] %>%
    group_by(dt_evoluca, sg_uf) %>%
    summarise(n = n())

  ## 2.2. obitos srag ####
  dados_obsrag[[i]] <- dados %>%
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, sg_uf) %>%
    filter(! is.na(dt_encerra))

  dados_br4[[i]] <- dados_obsrag[[i]] %>%
    group_by(dt_evoluca) %>%
    summarise(n = n())

  db.info[i, "obitos.srag"] <- sum(dados_br4[[i]]$n)

  dados_estados4[[i]] <- dados_obsrag[[i]] %>%
    group_by(dt_evoluca, sg_uf) %>%
    summarise(n = n())

}

##AST: os plots por fora do loop, usando listas, bind_rows
#junta tudo
names(dados_br) <- data_v
#acrescenta coluna de data
dados.br <- purrr::map2(.x = dados_br,
            .y = data_v,
           ~mutate(.x, data = .y))
#junta tudo
dados.br <- bind_rows(dados.br)
plot.covid <- ggplot(dados.br,
                     aes(x = dt_sin_pri, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
    scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL
plot.covid
#ggsave("covid.png")

#plot.srag
names(dados_br2) <- data_v
dados_br2 <- purrr::map2(.x = dados_br2,
                        .y = data_v,
                        ~mutate(.x, data = .y))
dados.br2 <- bind_rows(dados_br2)

plot.srag <- ggplot(dados.br2,
                     aes(x = dt_sin_pri, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
  xlab("Dia do primeiro sintoma") +
  ylab("Número de novos casos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL
plot.srag
#ggsave("srag.png")

##ob covid
names(dados_br3) <- data_v
dados_br3 <- purrr::map2(.x = dados_br3,
                         .y = data_v,
                         ~mutate(.x, data = .y))
dados.br3 <- bind_rows(dados_br3)

plot.obitos.covid <- ggplot(dados.br3,
                    aes(x = dt_evoluca, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL
plot.obitos.covid
#ggsave("obscov.png")

##ob srag
names(dados_br4) <- data_v
dados_br4 <- purrr::map2(.x = dados_br4,
                         .y = data_v,
                         ~mutate(.x, data = .y))
dados.br4 <- bind_rows(dados_br4)
head(dados.br4)
plot.obitos.srag <- ggplot(dados.br4,
                    aes(x = dt_evoluca, y = n, group = factor(data))) +
  geom_line(aes(col = factor(data))) +
  scale_x_date(date_labels = "%d/%b", limits = c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
  plot.formatos +
  scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
  NULL
plot.obitos.srag
#ggsave("obsrag.png")



if (!file.exists(out.dir))
  dir.create(out.dir, showWarnings = TRUE, recursive = TRUE)

data <- max(db.info$data)

fname <- paste0('integridade_SIVEP_', data, '.html')
render(input = 'integridade_sivep.Rmd',
       output_file = fname,
       output_dir = out.dir)

if (update.git) {
  system(paste("cd", out.dir, "&& git pull"))
  system(paste("cd", out.dir, "&& git add", fname,
               "&& git commit -m ':robot: relatório integridade SIVEP de",
               max(db.info$data), "'",
               "&& git push")
               )
}

