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
    make_option("--dataBase", #ast ö cara, era NULL só porque aqui estava "NULL" assim.
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%Y_%m_%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),#ast antes de tirar checar outras fontes de dados
                metavar = "formatoData"),
    make_option("--updateGit", default = "FALSE",
                help = ("Fazer git add, commit e push?"),
                metavar = "updateGit"),
    make_option("--pushRepo", #opcoes site e NULL?
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
name_path <- check.geocode(escala = escala,
              geocode = geocode)#ast falta checar outras escalas e fontes de dados e destinos para push
data <- "2020_05_16"
#push.repo <- "site"#NOT WORKING DEIXA NULL
if (is.null(push.repo))
  output.dir <- paste0("./dados_processados/nowcasting/", name_path, "/")
if (!is.null(push.repo))#ö ast falta checar que seja necessariamente "site" ou decidir as opções
  output.dir <- paste0("../", push.repo, "/dados/", name_path, "/")#sorry not sorry
#ö ast contar para a galera do site que o esquema vai mudar
#ast: para fazer bem o push tinha um esquema de ter os nomes separados em dois - copiar do PI ou pensar como fazer. eu faço isto, não precisa.

# só para as tabelas
df.path <- paste0(output.dir, "tabelas_nowcasting_para_grafico/")

#ast cria mais abrangente melhor, até a pasta das tabelas
if (!file.exists(df.path))
  dir.create(df.path, showWarnings = TRUE, recursive = TRUE)

# pegando a data mais recente
if (is.null(data)) {
  data <- get.last.date(dir)
}

print(paste("Atualizando", escala , sigla, data))

source("_src/01_gera_nowcastings_SIVEP.R")
source('_src/02_prepara_dados_nowcasting.R')
source('_src/03_analises_nowcasting.R')


#ast: acho que para o add e commit tem que ser o caminho inteiro - R/tem
#ast nao é, tem que separar o path em dois. com push.repo null tá servindo,
#para site ainda nao. ver o esquema do PI ¬¬
files.para.push <- list.files(output.dir, pattern = paste0("*.", data, ".csv"),
                              full.names = TRUE)
files.para.push <- files.para.push[-grep(files.para.push, pattern = "post")]
#aqui também poderia rolar um push das tabelas pro site mesmo
tabelas.para.push <- list.files(df.path, pattern = paste0("*.", data, ".csv"))

################################################################################
## Comando git: commits e pushs
################################################################################
if (update.git) {
  if (is.null(push.repo)) {#funciona
    system("git pull")
    ## todos os arquivos da data
    system(paste("git add", paste(files.para.push, collapse = " ")))
    system(paste("git add", paste(tabelas.para.push, collapse = " ")))
    system(paste("git commit -m ':robot: atualizacao desde o script nowcasting",
                  gsub(x = name_path, pattern = "/", replacement = " "),
                 "dados:", data,
                  "'"))
    system("git push")
  }
  if (push.repo == "site") {#NAO FUNCIONA. tem que escolher entre dar o caminho inteiro (nao pode) e fazer cd,
    #(dai perde o caminho) por isso pi separa o path em dois
    #tambem nao pode fazer cd num comando e viver achando que mudou ¬¬
    system(paste0("cd ../", push.repo, " &&
                  git pull &&
                  git add", paste(files.para.push, collapse = " "), "&&
                  git add", paste(tabelas.para.push, collapse = " "), "&&
                  git commit -m ':robot: atualizacao desde o script nowcasting ",
                   gsub(x = name_path, pattern = "/", replacement = " "),
                   " dados:", data,
                   "'"))
    system("git push origin master")
  }
  }

