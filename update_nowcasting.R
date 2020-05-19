# Libraries
library(widgetframe)
library(tidyverse)
library(plotly)
library(lubridate)
library(optparse)
library(Hmisc)
library(stringr)


################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../../dados/municipio_SP/SRAG_hospitalizados/dados/",
                metavar = "dir"),
    make_option("--escala", default = "municipio",
                help = ("Nível administrativo, um de: municipio, micro, meso, estado, pais"),
                metavar = "escala"),
    make_option("--sigla", default = "SP",
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
    make_option("--dataBase", default = "NULL",
                help = ("Data da base de dados, formato 'yyyy-mm-dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%Y/%m/%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),
                metavar = "formatoData"),
    make_option("--updateGit", default = "FALSE",
                help = ("Fazer git add, commit e push?"),
                metavar = "updateGit"),
    make_option("--pushFolder", default = "../../site", #ö seria isso?
                help = ("Aonde fazer o push (pasta que leva ao repositório do site"),
                metavar = "pushFolder")
  )

  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para importar csv da sivep gripe, filtrar por estado, executar nowcasting e salvar os resultados no diretorio do estado")

  ## aliases
  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)
  dir <- opt$options$dir
  escala <- opt$options$escala
  sigla <- opt$options$sigla
  geocode <- opt$options$geocode
  data <- opt$options$dataBase
  window <- opt$options$window
  trim.now <- opt$options$trim
  formato.data <- opt$options$formatoData
  push.folder <- opt$options$pushFolder
  update.git <- opt$options$updateGit
}

# pegando a data mais recente
if (data == "NULL") {
  data <- get.last.date(dir)
}

#push.folder e df.path são usadas em analises. push.folder permite mudar a pasta de destino caso seja necessário
#df.path <- paste0(push.folder, "/dados/", escala, "_", sigla, "/tabelas_nowcasting_para_grafico/")
df.path <- paste0(output.dir, "/tabelas_nowcasting_para_grafico/")  #this was wrong but i needed to see this. push folder needs to be solved but we need to compare 11_05 output before and after too
print(paste("Atualizando", escala , sigla))

#ast todos estes checks precisamos rever e ver se ficam no 00. a parametrização dupla pode dar erro. tirei por enquanto mas entendo que o ideal é ser passos aparte.

sigla.municipios <- c(SP = "São Paulo",
                      RJ = "Rio de Janeiro")

source("_src/01_gera_nowcastings_SIVEP.R")

source('_src/02_prepara_dados_nowcasting.R')

# códigos de análise e plot genéricos (mas pode usar as variáveis `mun` e `municipio` pra títulos de plot etc.
source('_src/03_analises_nowcasting.R')
# source('plots_nowcasting.R')

