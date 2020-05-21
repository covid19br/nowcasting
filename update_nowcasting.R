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
    make_option("--outputDir", default = "./dados_processados/nowcasting",
                help = ("Diretório de destino"),
                metavar = "outputDir")
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
  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)
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
}
####################################################
### to run INTERACTIVELY:
#You only have to set up the variables that are not already set up above or the ones that you would like to change #
#geocode <- "3550308" # municipio SP
#data <- "2020_05_20"
#######################################################

################################################################################
## comandos git: PULL ANTES de adicionar arquivos
################################################################################
if (update.git)
  system("git pull")

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

# pegando a data mais recente
if (is.null(data)) {
  data <- get.last.date(dir)
}

print(paste("Atualizando", gsub(x = name_path, pattern = "/", replacement = " ")))

source("_src/01_gera_nowcastings_SIVEP.R")
source("_src/02_prepara_dados_nowcasting.R")
source("_src/03_analises_nowcasting.R")
source("_src/04_plots_nowcasting.R")

files_para_push <- list.files(output.dir, pattern = paste0("*.", data, ".csv"),
                              full.names = TRUE)
files_para_push <- files_para_push[-grep(files_para_push, pattern = "post")]
#aqui também poderia rolar um push das tabelas pro site mesmo
tabelas_para_push <- list.files(df.path, pattern = paste0("*.", data, ".csv"),
                                full.names = TRUE)

######plots----

# Graficos a serem atualizados
plots.para.atualizar <- makeNamedList(
  # covid
  plot.nowcast.covid,
  plot.nowcast.cum.covid,
  plot.estimate.R0.covid,
  plot.tempo.dupl.covid,
  # srag
  plot.nowcast.srag,
  plot.nowcast.cum.srag,
  plot.estimate.R0.srag,
  plot.tempo.dupl.srag,
  # obitos covid
  plot.nowcast.ob.covid,
  plot.nowcast.cum.ob.covid,
  plot.tempo.dupl.ob.covid,
  # obitos srag
  plot.nowcast.ob.srag,
  plot.nowcast.cum.ob.srag,
  plot.tempo.dupl.ob.srag
  #obitos srag.proaim
  #plot.nowcast.ob.srag.proaim,
  #plot.nowcast.cum.ob.srag.proaim,
  #plot.tempo.dupl.ob.srag.proaim
)
plots.true <- sapply(plots.para.atualizar, function(x) !is.null(x))

filenames <- gsub(".", "_", names(plots.para.atualizar), fixed = TRUE)
filenames <- paste0(plot.dir, filenames)

n <- 1:length(plots.para.atualizar)

for (i in n[plots.true]) {
  fig.name <- filenames[i]

  # SVG ####
  # fazendo todos os graficos svg para o site
  graph.svg <- plots.para.atualizar[[i]] +
    theme(axis.text = element_text(size = 6.65)
          #plot.margin = margin(10, 0, 0, 7, "pt")
    )
  ggsave(paste(fig.name, ".svg", sep = ""),
         plot = graph.svg,
         device = svg,
         scale = 1,
         width = 215,
         height = 146,
         units = "mm")
  #ast nao chequei as dimensoes, só tirei o que parece redundante
}

#

###############################################################################
## Comando git: commits e pushs
################################################################################
if (update.git) {
  system("git pull")
  ## todos os arquivos da data
  system(paste("git add", paste(files_para_push, collapse = " ")))
  system(paste("git add", paste(tabelas_para_push, collapse = " ")))
  system(paste("git commit -m ':robot: nowcasting",
               gsub(x = name_path, pattern = "/", replacement = " "),
               "dados:", data,
               "'"))
  system("git push")
}


#falta git plot
