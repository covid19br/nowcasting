library(optparse)
library(foreign)
library(dplyr)
library(NobBS)

source("funcoes.R")

################################################################################
## 1. Leitura de arquivo  do sivep gripe no diretorio SIVEP-Gripe
## 2. Execucao do nowcasting, e criação de tabelas de n de casos por data de sintoma e notificacao
## 3. Dá push destes arquivos no repo do site OBSERVATÓRIO COVID-19 BR
################################################################################


### to run INTERACTIVELY: SKIP THIS and JUMP
################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--file",
                help = ("Arquivo csv com base sivep gripe"),
                metavar = "file"),
    make_option("--adm", default = "estado",
                help = ("nível administrativo, um de: municipio, micro, meso, estado, pais"),
                metavar = "adm"),
    make_option("--estado",
                help = ("Sigla do estado"),
                metavar = "estado"),
    make_option("--window", type = "integer", default = 40,
                help = ("Largura da running window do nowcasting (dias)"),
                metavar = "window"),
    make_option("--trim", type = "integer", default = 2,
                help = ("Últimos dias da serie temporal a tirar do nowcasting"),
                metavar = "trim"),
    make_option("--dataBase", default = "NULL",
                help = ("Data da base de dados, formato 'yyyy-mm-dd'"),
                metavar = "dataBase"),
    make_option("--formatoData", default = "%d/%m/%Y",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),
                metavar = "formatoData"),
    make_option("--updateGit", default = "FALSE",
                help = ("Fazer git add, commit e push?"),
                metavar = "updateGit")
  )

  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para importar csv da sivep gripe, filtrar por estado, executar nowcasting e salvar os resultados no diretorio do estado")

  ## aliases
  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)
  nome <- opt$options$file
  adm <- opt$options$adm
  sigla <- opt$options$estado
  data <- opt$options$dataBase
  window <- opt$options$window
  trim.now <- opt$options$trim
  formato.data <- opt$options$formatoData
  update.git <- opt$options$updateGit
}
################################################################################
### to run INTERACTIVELY: START HERE
################################################################################
# you have to set the variables below before proceeding
## dir <- "../../dados/municipio_SP/SRAG_hospitalizados/dados"
## geocode <- "3550308" #municipio SP para teste. aqui pode ser estado ou municipio por enquanto
## data <- NULL
## filtro <- TRUE
## window <- 40
## trim.now <- 2
## formato.data <- "%d/%m/%Y"
## update.git <- FALSE

################################################################################
## Importacao de preparacao dos dados
################################################################################

dados <- read.sivep(dir = dir,
                    filtro = filtro,
                    geocode = geocode,
                    data = data)


################################################################################
## comandos git: PULL ANTES de adicionar arquivos
################################################################################
if (update.git)
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

# ö srm parei aqui!

################################################################################
## Cria data frames com totais obseravdos de casos ou obitos
################################################################################
## Data frame com n de casos notificados por dia
##COVID##
n.notificacoes <- dados2 %>%
  group_by(dt_notific) %>%
  summarise(n.notific = n()) %>%
  as.data.frame()
##SRAG##
n.notificacoes.srag <- dados2.srag %>%
  group_by(dt_notific) %>%
  summarise(n.notific = n()) %>%
  as.data.frame()
##obitos covid##
n.notificacoes.obitos.covid <- dados2.obitos_covid %>%
  group_by(dt_notific) %>%
  summarise(n.notific = n()) %>%
  as.data.frame()
##obitos srag
n.notificacoes.obitos.srag <- dados2.obitos_srag %>%
  group_by(dt_notific) %>%
  summarise(n.notific = n()) %>%
  as.data.frame()

## Data frame com n de casos por data de 1o sintoma
## Necessário porque nowcasting so guarda os dias da janela de nowcasting
##COVID##
n.data.sintoma  <-
  dados2 %>%
  group_by(dt_sin_pri) %>%
  summarise(n.casos = n()) %>%
  as.data.frame()
##SRAG##
n.data.sintoma.srag  <-
  dados2.srag %>%
  group_by(dt_sin_pri) %>%
  summarise(n.casos = n()) %>%
  as.data.frame()
##obitos##
n.data.obitos.covid  <-
  dados2.obitos_covid %>%
  group_by(dt_evoluca) %>%
  summarise(n.casos = n()) %>%
  as.data.frame()
##obitos##
n.data.obitos.srag  <-
  dados2.obitos_srag %>%
  group_by(dt_evoluca) %>%
  summarise(n.casos = n()) %>%
  as.data.frame()

################################################################################
## Grava os objetos
################################################################################
## nomes dos objetos e seus paths

output_folder <- paste0("../dados/", adm, "_", sigla, "/")
dir.create(output_folder, showWarnings = FALSE)

##COVID##
##nome.now <- paste0(output_folder, "nowcasting_covid_",format(data.base,"%Y_%m_%d"),".rds")
nome.now.df <- paste0(output_folder, "nowcasting_covid_previstos_",format(data.base,"%Y_%m_%d"),".csv")
nome.now.post <- paste0(output_folder, "nowcasting_covid_post_",format(data.base,"%Y_%m_%d"),".csv")
nome.not <- paste0(output_folder, "notificacoes_covid_",format(data.base,"%Y_%m_%d"),".csv")
nome.sint <- paste0(output_folder, "n_casos_data_sintoma_covid_",format(data.base,"%Y_%m_%d"),".csv")
##SRAG##
##nome.now.srag <- paste0(output_folder, "nowcasting_srag_",format(data.base,"%Y_%m_%d"),".rds")
nome.now.df.srag <- paste0(output_folder, "nowcasting_srag_previstos_",format(data.base,"%Y_%m_%d"),".csv")
nome.now.post.srag <- paste0(output_folder, "nowcasting_srag_post_",format(data.base,"%Y_%m_%d"),".csv")
nome.not.srag <- paste0(output_folder, "notificacoes_srag_",format(data.base,"%Y_%m_%d"),".csv")
nome.sint.srag <- paste0(output_folder, "n_casos_data_sintoma_srag_",format(data.base,"%Y_%m_%d"),".csv")
##Obitos covid##
##nome.now.ob.covid <- paste0(output_folder, "nowcasting_obitos_covid_",format(data.base,"%Y_%m_%d"),".rds")
nome.now.df.ob.covid <- paste0(output_folder, "nowcasting_obitos_covid_previstos_",format(data.base,"%Y_%m_%d"),".csv")
nome.now.post.ob.covid <- paste0(output_folder, "nowcasting_obitos_covid_post_",format(data.base,"%Y_%m_%d"),".csv")
nome.not.ob.covid <- paste0(output_folder, "notificacoes_obitos_covid_",format(data.base,"%Y_%m_%d"),".csv")
nome.data.ob.covid <- paste0(output_folder, "n_casos_data_obitos_covid_",format(data.base,"%Y_%m_%d"),".csv")
##SRAG##
##nome.now.ob.srag <- paste0(output_folder, "nowcasting_obitos_srag_",format(data.base,"%Y_%m_%d"),".rds")
nome.now.df.ob.srag <- paste0(output_folder, "nowcasting_obitos_srag_previstos_",format(data.base,"%Y_%m_%d"),".csv")
nome.now.post.ob.srag <- paste0(output_folder, "nowcasting_obitos_srag_post_",format(data.base,"%Y_%m_%d"),".csv")
nome.not.ob.srag <- paste0(output_folder, "notificacoes_obitos_srag_",format(data.base,"%Y_%m_%d"),".csv")
nome.data.ob.srag <- paste0(output_folder, "n_casos_data_obitos_srag_",format(data.base,"%Y_%m_%d"),".csv")


## Grava os objetos
## Output do nowcasting. Uma lista, por isso é salvo em RDS
##COVID##
## saveRDS(now.covid, file = nome.now)
##SRAG##
## saveRDS(now.srag, file = nome.now.srag)
##obitos_COVID##
## saveRDS(now.obitos.covid, file = nome.now.ob.covid)
##obitos_SRAG##
## saveRDS(now.obitos.srag, file = nome.now.ob.srag)

## Previstos pelo nowcasting: faz parte da lista que que a função de nowcasting retorna,
## mas como a lista esta ocupando muito disco agora separamos apenas o que é necessário para so calculos
## Distribuições posteriores dos parametros do nowcasting. Também faz parte da lista retornada pelo nowcasting

##COVID##
if (exists("now.covid")) {
  write.csv(now.covid$estimates,
            file = nome.now.df,
            row.names = FALSE)
  ##COVID# _dist post#
  write.csv(now.covid$params.post,
            file = nome.now.post,
            row.names = FALSE)

}
##SRAG##
if (exists("now.srag")) {
  write.csv(now.srag$estimates,
            file = nome.now.df.srag,
            row.names = FALSE)
  ##SRAG##
  write.csv(now.srag$params.post,
            file = nome.now.post.srag,
            row.names = FALSE)
}
##obitos##
if (exists("now.obitos.covid")) {
  write.csv(now.obitos.covid$estimates,
            file = nome.now.df.ob.covid,
            row.names = FALSE)
  write.csv(now.obitos.covid$params.post,
            file = nome.now.post.ob.covid,
            row.names = FALSE)
}
##obitos srag##
if (exists("now.obitos.srag")) {
  write.csv(now.obitos.srag$estimates,
            file = nome.now.df.ob.srag,
            row.names = FALSE)
  write.csv(now.obitos.srag$params.post,
            file = nome.now.post.ob.srag,
            row.names = FALSE)
}

## N de casos por data de notificações
##COVID##
write.csv(n.notificacoes,
          file = nome.not,
          row.names = FALSE)
##SRAG##
write.csv(n.notificacoes.srag,
          file = nome.not.srag,
          row.names = FALSE)
##OBITOS##
write.csv(n.notificacoes.obitos.covid,
          file = nome.not.ob.covid,
          row.names = FALSE)
write.csv(n.notificacoes.obitos.srag,
          file = nome.not.ob.srag,
          row.names = FALSE)

## N de casos por data de sintoma ou data de óbito
##COVID##
write.csv(n.data.sintoma,
          file = nome.sint,
          row.names = FALSE)
##SRAG##
write.csv(n.data.sintoma.srag,
          file = nome.sint.srag,
          row.names = FALSE)
#OBITOS COVID
write.csv(n.data.obitos.covid,
          file = nome.data.ob.covid,
          row.names = FALSE)
##obitos srag##
write.csv(n.data.obitos.srag,
          file = nome.data.ob.srag,
          row.names = FALSE)

################################################################################
## Comando git: commits e pushs
################################################################################
if (update.git) {#ast isto é segurança para a gente por enquanto, pode sair depois ou ficar como TRUE por default - por enquanto tá FALSE
  system("git pull")
  ##COVID##
  system(paste("git add", nome.now, nome.now.df, nome.not, nome.sint))
  ##SRAG##
  system(paste("git add", nome.now.srag, nome.now.df.srag, nome.not.srag, nome.sint.srag))
  ##obitos covid##
  system(paste("git add", nome.now.ob.covid, nome.now.df.ob.covid, nome.not.ob.covid, nome.data.ob.covid))
  ##obitos srag##
  system(paste("git add", nome.now.ob.srag, nome.now.df.ob.srag, nome.not.ob.srag, nome.data.ob.srag))
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
