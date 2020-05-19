library(optparse)
library(foreign)
library(dplyr)
library(NobBS)

# carrega funcoes
source("funcoes.R")

################################################################################
## 1. Leitura de arquivo  do sivep gripe no diretorio SIVEP-Gripe
## 2. Execucao do nowcasting, e criação de tabelas de n de casos por data de sintoma e notificacao
## 3. Dá push destes arquivos no repo do site OBSERVATÓRIO COVID-19 BR
################################################################################


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
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%Y_%m_%d",
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
####################################################
### to run INTERACTIVELY:
#you only have to set up the variables that are not already set up above
#exemplo municipio SP
#escala <- "municipio"
geocode <- "3550308" # municipio SP - aqui pode ser qualquer escala
#sigla <- "SP"
data <- "2020_05_18" # dois objetivos: fazer rodar e comparar com o nowcasting da semana passada efeito de cortar a data inicial de srag para 15/03
#window <- 40
#trim.now <- 2
## update.git <- FALSE
# o output dir deveria ser parametro, tirei do meio
output.dir <- paste0("../dados_processados/nowcasting/", escala, "_", sigla, "/")

if (!file.exists(output.dir)) dir.create(output.dir, showWarnings = FALSE) #ast tirei da funcao só para que ficasse junto

################################################################################
## Importacao de preparacao dos dados
################################################################################
if (data == "NULL") {
  data <- get.last.date(dir)
}

# Se for para baixar de data especifica, usar o argumento data
dados <- read.sivep(dir = dir,
                    escala = escala,
                    geocode = geocode,
                    data = data)

# exemplo com estado RJ
# dados <- read.sivep(dir = "../../dados/SIVEP-Gripe/",
#                     escala = escala,
#                     sigla = "RJ",#deveria funcionar com qualquer um mas ainda nao sei se sigla deveria ser parametro mesmo
#                     geocode = "33",
#                     data = NULL)

################################################################################
## comandos git: PULL ANTES de adicionar arquivos
################################################################################
system("git pull")

################################################################################
## Nowcastings
################################################################################
## dados necessarios: data de 1o sintoma, maior data entre resultado do teste, digitacao ou notificação
## Guarda data de notificação também para gerar a tabela de n de notificaoes por data
## pois o nowcasting retorna tabela de n de casos por data do 1o sintoma

now.covid <- gera.nowcasting(dados = dados,
                             caso = TRUE,
                             tipo = "covid",
                             trim.now = trim.now,
                             window = window)

now.srag <- gera.nowcasting(dados = dados,
                            caso = TRUE,
                            tipo = "srag",
                            trim.now = trim.now,
                            window = window)

now.ob.covid <- gera.nowcasting(dados = dados,
                                caso = FALSE,
                                tipo = "covid",
                                trim.now = trim.now,
                                window = window)

now.ob.srag <- gera.nowcasting(dados = dados,
                               caso = FALSE,
                               tipo = "srag",
                               trim.now = trim.now,
                               window = window)

################################################################################
## Exporta data frames com totais observados de casos ou obitos
## Exporta data frames com outputs de nowcasting
################################################################################


# COVID ####
write.notificacoes.data(dados = now.covid$dados,
                        tipo = "covid",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.covid$now)) {
  write.nowcasting(now = now.covid$now,
                   output.dir = output.dir,
                   tipo = "covid",
                   data = data)
}

# SRAG ####
write.notificacoes.data(dados = now.srag$dados,
                        tipo = "srag",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.srag$now)) {
  write.nowcasting(now = now.srag$now,
                   output.dir = output.dir,
                   tipo = "srag",
                   data = data)
}

# OBITOS COVID ####

write.notificacoes.data(dados = now.ob.covid$dados,
                        tipo = "obitos_covid",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.ob.covid$now)) {
  write.nowcasting(now = now.ob.covid$now,
                  output.dir = output.dir,
                  tipo = "obitos_covid",
                  data = data)
}


# OBITOS SRAG ####

write.notificacoes.data(dados = now.ob.srag$dados,
                        tipo = "obitos_srag",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.ob.srag$now)) {
  write.nowcasting(now = now.ob.srag$now,
                  output.dir = output.dir,
                  tipo = "obitos_srag",
                  data = data)
}


files.para.push <- list.files(output.dir, pattern = paste0("*.", data, ".csv"))

# ast a gente precisa decidir aqui um padrão.
# se o push_repo é o mesmo repo e as coisas vão para dados_processados (eu preferiria que fosse output_nowcasting para saber daonde vem), push_repo pode ser NULL e a vida segue.
# se o push_repo é site o output_folder é
#lembrando que vai ser assim:
#output.dir <- "/dados_processados/nowcasting/municipio_SP/"
#if (push_repo <- "site")
#output.dir <- paste0("../../", push_repo, "/dados_", escala,"_", sigla, output.dir)
#(mas o ponto duplo vai sair)
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
