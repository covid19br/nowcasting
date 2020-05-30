if(!require(optparse)){install.packages("optparse"); library(optparse)}
if(!require(Hmisc)){install.packages("Hmisc"); library(Hmisc)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}


if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../dados/estado_SP/SRAG_hospitalizados/dados/",
                metavar = "dir"),
    make_option("--tipo", default = "srag",
                help = ("tipo da internação. Só 'srag' ou 'all', srag e covid"),
                metavar = "tipo"),
    make_option("--escala", default = "municipio",
                 help = ("Nível administrativo, um de: municipio, micro, meso, estado, país"),
                 metavar = "escala"),
    make_option("--sigla", default = "SP", # ainda nao estamos usando
                 help = ("Sigla do estado a ser atualizado"),
                 metavar = "sigla"),
    make_option("--geocode", default = 3550308,
                help = ("Geocode de município, micro-mesorregião ou estado"),
                metavar = "geocode"),
    make_option("--window", type = "integer", default = 40,
                help = ("Largura da running window do nowcasting (dias)"),
                metavar = "window"),
    make_option("--dataBase", default = NULL,
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--dataInicial", default = as.Date("2020-03-08"),
                help = ("Data do início dos casos, formato 'yyyy_mm_dd'"),
                metavar = "dataInicial"),
    make_option("--formatoData", default = "%Y_%m_%d",
                help = ("Formato do campo de datas no csv, confome padrão da função as.Date"),#ast antes de tirar checar outras fontes de dados
                metavar = "formatoData"),
    make_option("--fix_dates", default = TRUE,
                help = ("Booleana. Define se as os casos com datas inconsistentes devem ser concertadas (TRUE) ou excluidas (FALSE)"),
                metavar = "fix_dates"),
    make_option("--out_dir", default = NULL,
                help = ("Pasta de saida dos resultados"),
                metavar = "out_dir"),
    make_option("--n_cores", default = 2,
                help = ("Numero de cores usado para a projeção de leitos"),
                metavar = "n_cores"),
    make_option("--nowcasting", default = TRUE,
                help = ("Booleano. Executa ou não nowcasting. Precisa ser feito antes de ajustar os modelos."),
                metavar = "nowcasting"),
    make_option("--fit_models", default = TRUE,
                help = ("Booleano. Ajusta ou não os modelos de projeção. Precisa ser feito antes do relatório."),
                metavar = "fit_models"),
    make_option("--report", default = TRUE,
                help = ("Booleano. Gera ou não relatório automático."),
                metavar = "report"),
    make_option("--report_dir", default = NULL,
                help = ("Paste de saida dos reports."),
                metavar = "report_dir"),
    make_option("--check_report", default = FALSE,
                help = ("Não faz nada se o relatório já existir."),
                metavar = "check_report")
    # make_option("--updateGit", default = "FALSE",
    #             help = ("Fazer git add, commit e push?"),
    #             metavar = "updateGit")
  )
  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para importar csv da sivep gripe,
                                executar nowcasting e salvar os resultados")
  
  ## aliases
  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)
  DATAROOT <- opt$options$dir
  disease <- opt$options$tipo
  escala <- opt$options$escala
  sigla <- opt$options$sigla
  geocode <- opt$options$geocode
  window <- opt$options$window
  data_date <- if(is.null(opt$options$dataBase)) {NULL} else as.Date(opt$options$dataBase, format("%Y_%m_%d"))
  initial_date <- as.Date(opt$options$dataInicial)
  formato.data <- opt$options$formatoData
  fix_missing_dates <- opt$options$fix_dates
  n_cores <- opt$options$n_cores
  make_report <- opt$options$report
  fit_models <- opt$options$fit_models
  nowcasting <- opt$options$nowcasting
  out.root <- if(is.null(opt$options$out_dir)) {"../dados_processados"} else opt$options$out_dir
  report.dir <- opt$options$report_dir
  check_report <- opt$options$check_report
  
}

####################################################
### to run INTERACTIVELY:
#You only have to set up the variables that are not already set up above or the ones that you would like to change #
#geocode <- "3550308" # municipio SP
# geocode <- "3509502" # municipio Campinas
# DATAROOT <- "../dados/municipio_campinas/SRAG_hospitalizados/dados/"
# intial_date <- as.Date("2020-03-16")
#data <- "2020_05_16"
# DATAROOT = "../dados/estado_SP/SRAG_hospitalizados/dados/"
# initial_date = as.Date("2020-03-08")
# data_date = as.Date("2020_05_27", format = "%Y_%m_%d")
# escala  = "drs"
# sigla = "SP"
# geocode = 1  
# out.root =  "../dados_processados/"
# nowcasting = FALSE 
# fit_models = FALSE 
# make_report = TRUE
# report.dir  = "~/Desktop"
#######################################################


if(exists("PRJROOT")){
  if(!is.null(PRJROOT)) 
    PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  
} else 
  PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  

P = function(...) file.path(PRJROOT, ...)

source(P("fct/load_packages.R"))
source(P("_src/funcoes.R"))

name_path <- check.geocode(escala = escala, geocode = geocode, sigla = sigla)#ast falta checar outras escalas e fontes de dados e destinos para push
output.dir <- file.path(out.root, "projecao_leitos", name_path)

if (!file.exists(output.dir))
  dir.create(output.dir, showWarnings = TRUE, recursive = TRUE)
if (!file.exists(file.path(output.dir, "hospitalizados")))
  dir.create(file.path(output.dir, "hospitalizados"), showWarnings = TRUE, recursive = TRUE)
if (!file.exists(file.path(output.dir, "curve_fits")))
  dir.create(file.path(output.dir, "curve_fits"), showWarnings = TRUE, recursive = TRUE)
if(make_report){
  if(is.null(report.dir))
    report.dir = file.path(output.dir, "relatorios")
  if (!file.exists(report.dir))
      dir.create(report.dir, showWarnings = TRUE, recursive = TRUE)
}
update_site = FALSE

if (is.null(data_date)) {
  data_date <- as.Date(get.last.date(DATAROOT), format = "%Y_%m_%d")
}

say(paste("Data date is set to:", format(data_date, "%d %B %Y")), "cow")

if(check_report){
  if(any(grepl(data_date, dir(report.dir)))){
    say(paste("Report exisist for date", format(data_date, "%d %B %Y")), "yoda")
    quit(save="no")
  }
}

registerDoMC(n_cores)

O = function(...) file.path(output.dir, ...)
P = function(...) file.path(PRJROOT, ...)
CODEROOT = paste0(PRJROOT, "/_src/projecao_leitos")
C = function(...) file.path(CODEROOT, ...)	



if(nowcasting){
  source(C("00-read_process_SIVEP_CSV.R"))
  source(C("01-nowcast_inHospital_byAge.R"))
}
  
if(fit_models)
  source(C("02-runFits_generalized_Exponential_Logistic_fit.R"))

if(make_report)
  source(C("03-report_ExpLogistfit.R"))

if(update_site){
  setwd(file.path("../site/_src"))
  source("update_modelogro.R")
}
