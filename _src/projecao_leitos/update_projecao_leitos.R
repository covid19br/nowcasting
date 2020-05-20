if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}

if(exists("PRJROOT")){
  if(!is.null(PRJROOT)) 
    PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  
} else 
  PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  

METAROOT = rprojroot::find_root(".here")
DATAROOT = paste0(METAROOT, "/dados/municipio_SP/SRAG_hospitalizados/dados/")
fix_missing_dates = TRUE
intial_date = as.Date("2020/03/08")
geocode = 355030
n_cores = 2

registerDoMC(n_cores)

P = function(...) file.path(PRJROOT, ...)
O = function(...) file.path(PRJROOT, "dados_processados/projecao_leitos/municipio_SP", ...)
OUT_SITE = function(...) file.path(METAROOT, "site/dados/municipio_SP/projecao_leitos", ...)
R = function(...) file.path(PRJROOT, "dados_processados/projecao_leitos/municipio_SP/relatorios", ...)
P = function(...) file.path(PRJROOT, ...)
CODEROOT = paste0(PRJROOT, "/_src/projecao_leitos")
C = function(...) file.path(CODEROOT, ...)	

source(P("_src/fct/load_packages.R"))

source(C("00-read_process_SIVEP_CSV.R"))
source(C("01-nowcast_inHospital_byAge.R"))
source(C("02-runFits_generalized_Exponential_Logistic_fit.R"))
source(C("03-report_ExpLogistfit.R"))
setwd(file.path(METAROOT, "site/_src"))
source("update_modelogro.R")

