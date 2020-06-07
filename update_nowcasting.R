# Libraries
library(widgetframe)
library(tidyverse)
library(plotly)
library(lubridate)
library(optparse)
library(Hmisc)
library(stringr)
library(foreign)
library(dplyr)
library(NobBS)
library(zoo)
library(tidyr)


# carrega funcoes----
source("_src/funcoes.R")

################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../dados/municipio_SP/SRAG_hospitalizados/dados/",
                metavar = "dir"),
    make_option("--escala", default = "municipio",
                help = ("Nível administrativo, um de: municipio, micro, meso, estado, drs, país"),
                metavar = "escala"),
    make_option("--sigla", default = "SP", # ainda nao estamos usando
                help = ("Sigla do estado a ser atualizado"),
                metavar = "sigla"),
    make_option("--geocode",
                help = ("Geocode de município, micro-mesorregião ou estado"),
                metavar = "geocode"),
    make_option("--window", type = "integer", default = 40,
                help = ("Largura da running window do nowcasting (dias)"),
                metavar = "window"),
    make_option("--trim", type = "integer", default = 2,
                help = ("Últimos dias da serie temporal a tirar do nowcasting"),
                metavar = "trim"),
    make_option("--dataBase",
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%Y_%m_%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),#ast antes de tirar checar outras fontes de dados
                metavar = "formatoData"),
    make_option("--updateGit", default = "FALSE",
                help = ("Fazer git add, commit e push?"),
                metavar = "updateGit"),
    make_option("--outputDir",
                default = "../dados_processados/nowcasting",
                help = ("Diretório de destino"),
                metavar = "outputDir"),
    make_option("--plot", default = FALSE,
                help = ("Fazer plots?"),
                metavar = "plot"),
    make_option("--residentes", default = TRUE,
                help = ("Filtrar só por residentes? Default: TRUE"),
                metavar = "residentes")
  )
  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para importar csv da sivep gripe,
                                executar nowcasting e salvar os resultados")

  ## TO TEST INTERACTIVELY the command-line arguments
  #input <- "--dir ../dados/estado_SP/SRAG_hospitalizados/dados/ --escala municipio --geocode 350750 --dataBase 2020_05_20"
  #command.args <- strsplit(input, " ")[[1]]
  #opt <- parse_args(parser_object, args = command.args, positional_arguments = TRUE)
  ## SKIP opt line below
  ## aliases
  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                    positional_arguments = TRUE)
  dir <- opt$options$dir
  escala <- opt$options$escala
  sigla <- opt$options$sigla
  geocode <- opt$options$geocode
  window <- opt$options$window
  trim.now <- opt$options$trim
  data <- opt$options$dataBase
  formato.data <- opt$options$formatoData
  update.git <- opt$options$updateGit
  out.dir <- opt$options$outputDir
  plots <- opt$options$plot
  residentes <- opt$options$residentes
}
####################################################
### to run INTERACTIVELY:
#You only have to set up the variables that are not already set up above or the ones that you would like to change #
#geocode <- "3550308" # municipio SP
#data <- "2020_05_20"
#######################################################

if (!exists('geocode')) {
  print("Geocode não definido")
  quit(status = 1)
}
# sets paths
name_path <- check.geocode(escala = escala,
                           geocode = geocode, sigla = sigla)

output.dir <- paste0(out.dir, "/", name_path, "/")
# só para o output
out.path <- paste0(output.dir, "output_nowcasting/")
# só para as tabelas
df.path <- paste0(output.dir, "tabelas_nowcasting_para_grafico/")

if (!file.exists(df.path))
  dir.create(df.path, showWarnings = TRUE, recursive = TRUE)
if (!file.exists(out.path))
  dir.create(out.path, showWarnings = TRUE, recursive = TRUE)

################################################################################
## comandos git: PULL ANTES de adicionar arquivos
################################################################################
if (update.git)
    system(paste("cd",  output.dir, "; git pull --ff-only"))

# pegando a data mais recente
if (is.null(data)) {
  data <- get.last.date(dir)
}

print(paste("Atualizando", gsub(x = name_path, pattern = "/", replacement = " "), data))

if (!plots) {
  source("_src/01_gera_nowcastings_SIVEP.R")
  source("_src/02_prepara_dados_nowcasting.R")
  source("_src/03_analises_nowcasting.R")
}

###############################################################################
## Comando git: commits e pushs
################################################################################
if (update.git) {
  files_para_push <- list.files(out.path, pattern = paste0("*.", data, ".csv"))
  files_para_push <- files_para_push[-grep(files_para_push, pattern = "post")]
  files_para_push <- paste0("output_nowcasting/", files_para_push)
  #aqui também poderia rolar um push das tabelas pro site mesmo
  tabelas_para_push <- list.files(df.path, pattern = paste0("*.", data, ".csv"))
  tabelas_para_push <- paste0("tabelas_nowcasting_para_grafico/",  tabelas_para_push)

  ## todos os arquivos da data
  system(paste("cd", paste0(out.dir,"/", name_path),
               "&& git add", paste0(files_para_push , collapse = " "),
               "&& git add", paste0(tabelas_para_push, collapse = " "),
               "&& git commit -m ':robot: outputs nowcasting",
               gsub(x = name_path, pattern = "/", replacement = " "),
               "dados:", data, "'",
               "&& git push")
               )
}

#Plots / git has to be here
if (plots) {
  source("_src/04_plots_nowcasting.R")
  plots_para_push <- list.files(plot.dir)
  plots_para_push <- paste0("plots/", plots_para_push)
  if (update.git) {
    ## todos os arquivos da data
    system(paste("cd", paste0(out.dir,"/", name_path),
                 "&& git add", paste0(plots_para_push, collapse = " "),
                 "&& git commit -m ':robot: plots nowcasting",
                 gsub(x = name_path, pattern = "/", replacement = " "),
                 "dados:", data, "'",
                 "&& git push")
    )
  }
}
