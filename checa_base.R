# Libraries
library(optparse)
library(rmarkdown)

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

db.info <- data.frame(data=datas, file=basename(file.names), size.read=NA,
                      size.file=NA, casos.covid=NA, casos.srag=NA,
                      obitos.covid=NA, obitos.srag=NA)
N <- dim(db.info)[1]
plot.covid <- ggplot()
plot.srag <- ggplot()
plot.obitos.covid <- ggplot()
plot.obitos.srag <- ggplot()

for (i in seq(N)) {
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
    filter(! is.na(dt_pcr_dig))
  dados.br <- dados2 %>%
    group_by(dt_sin_pri) %>%
    summarise(n = n())
  db.info[i, "casos.covid"] <- sum(dados.br$n)
  dados.estados <- dados2 %>%
    group_by(dt_sin_pri, sg_uf) %>%
    summarise(n = n())
  plot.covid <- plot.covid +
      geom_line(data = dados.br,
                aes(x = dt_sin_pri, y = n, colour = factor(data)))

  ## 1.2. casos srag ####
  dados2 <- dados %>%
    select(dt_notific, dt_sin_pri, dt_digita, sg_uf) %>%
    mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) %>% # nome aqui é pcr mas não tem pcr
    filter(! is.na(dt_pcr_dig))
  dados.br <- dados2 %>%
    group_by(dt_sin_pri) %>%
    summarise(n = n())
  db.info[i, "casos.srag"] <- sum(dados.br$n)
  dados.estados <- dados2 %>%
    group_by(dt_sin_pri, sg_uf) %>%
    summarise(n = n())
  plot.srag <- plot.srag +
      geom_line(data = dados.br,
                aes(x = dt_sin_pri, y = n, colour = factor(data)))

  ## 2.1. obitos covid ####
  dados2 <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, sg_uf) %>%
    filter(! is.na(dt_encerra))
  dados.br <- dados2 %>%
    group_by(dt_evoluca) %>%
    summarise(n = n())
  db.info[i, "obitos.covid"] <- sum(dados.br$n)
  dados.estados <- dados2 %>%
    group_by(dt_evoluca, sg_uf) %>%
    summarise(n = n())
  plot.obitos.covid <- plot.obitos.covid +
      geom_line(data = dados.br,
                aes(x = dt_evoluca, y = n, colour = factor(data)))

  ## 2.2. obitos srag ####
  dados2 <- dados %>%
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, sg_uf) %>%
    filter(! is.na(dt_encerra))
  dados.br <- dados2 %>%
    group_by(dt_evoluca) %>%
    summarise(n = n())
  db.info[i, "obitos.srag"] <- sum(dados.br$n)
  dados.estados <- dados2 %>%
    group_by(dt_evoluca, sg_uf) %>%
    summarise(n = n())
  plot.obitos.srag <- plot.obitos.srag +
      geom_line(data = dados.br,
                aes(x = dt_evoluca, y = n, colour = factor(data)))

}

plot.covid <- plot.covid +
    scale_x_date(date_labels = "%d/%b", limits=c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
    plot.formatos
plot.srag <- plot.srag +
    scale_x_date(date_labels = "%d/%b", limits=c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
    plot.formatos
plot.obitos.covid <- plot.obitos.covid +
    scale_x_date(date_labels = "%d/%b", limits=c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
    scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
    plot.formatos
plot.obitos.srag <- plot.obitos.srag +
    scale_x_date(date_labels = "%d/%b", limits=c(as.Date("2020-03-15"), NA)) +
    xlab("Dia do óbito") +
    ylab("Número de novos óbitos") +
    scale_colour_manual(name = "data base", values = viridis::viridis(N), labels = db.info$data) +
    plot.formatos

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

