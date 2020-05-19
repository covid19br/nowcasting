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


# carrega funcoes
source("_src/funcoes.R")

################################################################################
## comandos git: PULL ANTES de adicionar arquivos
################################################################################
system("git pull")


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
                help = ("Nível administrativo, um de: municipio, micro, meso, estado, país"),
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
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%Y_%m_%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),
                metavar = "formatoData"),
    make_option("--updateGit", default = "FALSE",
                help = ("Fazer git add, commit e push?"),
                metavar = "updateGit"),
    make_option("--pushRepo", default = "site", #opcoes site e NULL?
                help = ("Aonde fazer o push (pasta que leva ao repositório do site"),
                metavar = "pushRepo")
  )
  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para importar csv da sivep gripe,
                                executar nowcasting e salvar os resultados")

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
  push.repo <- opt$options$pushRepo
}
####################################################
### to run INTERACTIVELY:
#You only have to set up the variables that are not already set up above or the ones that you would like to change #
geocode <- "3550308" # municipio SP
#ast aqui teria que ter um jeito de pegar o nome a partir do geocode se a gente nao criar nome (is.null(nome)), aí a gente nomeia com o geocode e a escala.
data <- "2020_05_18"
# o output dir deveria ser parametro, tirei do meio
if (push.repo <- "NULL")
  output.dir <- paste0("/dados_processados/nowcasting/", escala,"_", sigla,"/")
if (push.repo <- "site")
  output.dir <- paste0("../", push.repo, "/dados_", escala,"_", sigla, output.dir)#ou nome

if (!file.exists(output.dir)) dir.create(output.dir, showWarnings = FALSE) #ast tirei da funcao só para que ficasse junto
# só para as tabelas
if (push.repo <- "site")
  df.path <- paste0(push.repo, "/dados/", escala, "_", sigla, "/tabelas_nowcasting_para_grafico/")
if (push.repo <- "NULL")
  df.path <- paste0("/dados_processados/nowcasting/", escala, "_", sigla, "/tabelas_nowcasting_para_grafico/")

# pegando a data mais recente
if (data == "NULL") {
  data <- get.last.date(dir)
}



print(paste("Atualizando", escala , sigla))

#ast todos estes checks precisamos rever ainda

sigla.municipios <- c(SP = "São Paulo",
                      RJ = "Rio de Janeiro")

source("_src/01_gera_nowcastings_SIVEP.R")
source('_src/02_prepara_dados_nowcasting.R')
source('_src/03_analises_nowcasting.R')


files.para.push <- list.files(output.dir, pattern = paste0("*.", data, ".csv"))

################################################################################
## Comando git: commits e pushs
################################################################################
if (update.git) {
  system("git pull")
  ## todos os arquivos da data
  system(paste("git add", paste(files.para.push, collapse = " ")))
  system(paste0("git commit -m '[auto] atualizacao automatica nowcasting estado ", sigla, "' &&
       git push origin master"))
}

##%PI: ainda a implementar para cada estado
## ################################################################################
## ## Executa analises de comparacao dos resultados por versoes da base SIVEP
## ## compila e relatorio destas comparacoes e dá push dos aquivos resultantes
## ################################################################################
## ## Executa as analises, ver codigo compara_nowcasting_versoes_sivep.R
## system("Rscript compara_nowcasting_versoes_sivep.R --diretorio 'Municipio_SP' --trim 2 --covid FALSE --obitos FALSE")
## system("Rscript compara_nowcasting_versoes_sivep.R --diretorio 'Municipio_SP' --trim 2 --covid TRUE --obitos FALSE")
## system("Rscript compara_nowcasting_versoes_sivep.R --diretorio 'Municipio_SP' --trim 2 --covid FALSE --obitos FALSE --hospital TRUE")
## system("Rscript compara_nowcasting_versoes_sivep.R --diretorio 'Municipio_SP' --trim 2 --covid TRUE --obitos FALSE --hospital TRUE")
## ## Executa o script que compila relatorio nowcasting e dá commit e push
## system(paste0("cd Municipio_SP/SRAG_hospitalizados/; Rscript compara_nowcasting_versoes_sivep_compila_rmd.R --trim ",
##               trim.now, " --dataBase ", data.base))

