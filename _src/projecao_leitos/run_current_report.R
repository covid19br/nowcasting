PRJROOT =  rprojroot::find_root(criterion=rprojroot::is_git_root)  
CODEROOT = paste0(PRJROOT, "/projecao_leitos")
C = function(...) file.path(CODEROOT, ...)	
source(C("load_packages_set_paths.R"))

registerDoMC(2)

fix_missing_dates = TRUE
source(C("00-read_process_SIVEP_CSV.R"))
source(C("01-nowcast_inHospital_byAge.R"))
source(C("02-runFits_generalized_Exponential_Logistic_fit.R"))
source(C("04-report_ExpLogistfit.R"))
setwd(P("para_o_site"))
source("_src/update_modelogro.R")

# if(!require(googledrive)){install.packages("googledrive"); library(googledrive)}
# drive_upload(media = paste0(getwd(), "/outputs/municipio_SP/projecao_leitos/relatorios/"), 
#              path = "https://drive.google.com/drive/folders/1MBOA7V2xdQLIW7c-0W8ofOQEpbbTrw5X", 
#              name = paste0(data_date, "_relatorio_projecoes_demanda_hospitalar_covid.pdf"), 
#              overwrite = FALSE, 
#              type = "application/pdf")
# drive_upload(media = paste0(getwd(), "/outputs/municipio_SP/projecao_leitos/relatorios/"), 
#              path = "https://drive.google.com/drive/folders/1MBOA7V2xdQLIW7c-0W8ofOQEpbbTrw5X", 
#              name = paste0(data_date, "_relatorio_projecoes_demanda_hospitalar_srag.pdf"), 
#              overwrite = FALSE, 
#              type = "application/pdf")