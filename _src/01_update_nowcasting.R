# Libraries
library(widgetframe)
library(tidyverse)
library(plotly)
library(lubridate)
library(optparse)
library(Hmisc)
library(stringr)
#library(withr)

##############################################################################
# Helper Functions ###########################################################
##############################################################################
## lista de nomes
# makeNamedList <- function(...) {
#   structure(list(...), names = as.list(substitute(list(...)))[-1L])
# }
##############################################################################

# if executed from command line, look for arguments
# else variable `mun` is assumed to be defined
if (sys.nframe() == 0L) {
  # Parsing command line arguments
  option_list <- list(
    make_option("--escala", default = "municipio",
                help = ("Selecione uma escala administrativa: estado, municipio"),
                metavar = "escala"),
    make_option("--sigla", default = "SP",
                help = ("Estado a ser atualizado"),
                metavar = "sigla"),
    make_option("--dataBase", default = "NULL",
                help = ("Data da base de dados, formato 'yyyy-mm-dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%Y-%m-%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),
                metavar = "formatoData")
  )
  #ö checar os detalles do parse usage aqui
  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [sigla UF]\n",
                                option_list = option_list,
                                description = "Script para atualizar análise e plots do site do OBSERVATORIO COVID-19 BR com resultados por município ou estado")

  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                    positional_arguments = TRUE)

  ## aliases
adm <- opt$options$escala
sigla.adm <- opt$options$sigla
data.base <- opt$options$dataBase
formato.data <- opt$options$formatoData
#ö: isto ficou assim porque municipio SP ainda está independente o terminal vai perguntar estado e sigla. mas isso não muda a parametrizaçãõ de adm e sigla.adm
}

#if you are going to run this interactively uncomment:
#adm <- "municipio"
#sigla.adm <- "SP"
#data.base <- "NULL" #ast o problema com is.null e =="NULL" é que o loop acima faz com que data.base seja "NULL" e não NULL, por isso estava desse jeito. o robot vai ler em caracteres.
#formato.data <- "%Y-%m-%d"
if (!exists('sigla.adm')) {
  print("Sigla do estado não definida")
  quit(status = 1)
}
print(paste("Atualizando", adm , sigla.adm))

sigla.municipios <- c(SP = "São Paulo",
                      RJ = "Rio de Janeiro")

estados <- read.csv("../dados/estados_code.csv", row.names = 1,
                    stringsAsFactors = F)
sigla.estados <- estados$nome
names(sigla.estados) <- estados$sigla
#só mantendo o formato de sigla.municipios

if (adm == "estado" & !sigla.adm %in% names(sigla.estados) |
    adm == "municipio" & !sigla.adm %in% names(sigla.municipios)) {
  print(paste(Hmisc::capitalize(adm), sigla.adm, "não consta na lista de suportados."))
  #quit(status = 1)
}
if (adm == "estado")
  nome.adm <- sigla.estados[sigla.adm]
if (adm == "municipio")
  nome.adm <- sigla.municipios[sigla.adm]


# este arquivo deve se encarregar de procurar na pasta certa pelo arquivo com a
# data mais recente
source('01-a_prepara_dados_nowcasting.R')

# códigos de análise e plot genéricos (mas pode usar as variáveis `mun` e
# `municipio` pra títulos de plot etc.%isso agora adm.sigla too
source('01-b_analises_nowcasting.R')
# source('plots_nowcasting.R')
