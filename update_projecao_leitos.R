if(!require(optparse)){install.packages("optparse"); library(optparse)}
if(!require(Hmisc)){install.packages("Hmisc"); library(Hmisc)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}


if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../dados/municipio_SP/SRAG_hospitalizados/dados/",
                metavar = "dir"),
    make_option("--tipo", default = "srag",
                help = ("tipo da internação. Todos os 'srag' ou só 'covid'"),
                metavar = "tipo"),
    make_option("--escala", default = "municipio",
                 help = ("Nível administrativo, um de: municipio, micro, meso, estado, país"),
                 metavar = "escala"),
    make_option("--sigla", default = "SP", # ainda nao estamos usando
                 help = ("Sigla do estado a ser atualizado"),
                 metavar = "sigla"),
    make_option("--geocode", default = 355030,
                help = ("Geocode de município, micro-mesorregião ou estado"),
                metavar = "geocode"),
    make_option("--window", type = "integer", default = 40,
                help = ("Largura da running window do nowcasting (dias)"),
                metavar = "window"),
    make_option("--dataBase", default = NULL,
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--dataInicial", default = as.Date("2020-03-08"),
                help = ("Data do ínicio dos casos, formato 'yyyy_mm_dd'"),
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
                metavar = "n_cores")
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
  intial_date <- as.Date(opt$options$dataInicial)
  formato.data <- opt$options$formatoData
  fix_missing_dates <- opt$options$fix_dates
  n_cores <- opt$options$n_cores
  out.root <- if(is.null(opt$options$out_dir)) {"dados_processados"} else opt$options$out_dir
}

if(exists("PRJROOT")){
  if(!is.null(PRJROOT)) 
    PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  
} else 
  PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  

P = function(...) file.path(PRJROOT, ...)

source(P("fct/load_packages.R"))
source(P("_src/funcoes.R"))

name_path <- check.geocode(escala = escala,
                           geocode = geocode)#ast falta checar outras escalas e fontes de dados e destinos para push
output.dir <- file.path(out.root, "projecao_leitos", name_path)


if (!file.exists(df.path))
  dir.create(df.path, showWarnings = TRUE, recursive = TRUE)
if (!file.exists(out.path))
  dir.create(out.path, showWarnings = TRUE, recursive = TRUE)


METAROOT = rprojroot::find_root(".here")
make_report = TRUE
update_site = FALSE

registerDoMC(n_cores)

O = function(...) file.path(output.dir, ...)
R = function(...) file.path(PRJROOT, "dados_processados/projecao_leitos/municipio_SP/relatorios", ...)
P = function(...) file.path(PRJROOT, ...)
CODEROOT = paste0(PRJROOT, "/_src/projecao_leitos")
C = function(...) file.path(CODEROOT, ...)	


source(C("00-read_process_SIVEP_CSV.R"))

source(C("01-nowcast_inHospital_byAge.R"))

source(C("02-runFits_generalized_Exponential_Logistic_fit.R"))

if(make_report)
  source(C("03-report_ExpLogistfit.R"))

if(update_site){
  setwd(file.path(METAROOT, "site/_src"))
  source("update_modelogro.R")
}
