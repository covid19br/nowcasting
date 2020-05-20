fixUTIDates = function(x, UTI_stay_wait_fit){
  if(!is.na(x$UTI) & x$UTI==1 & is.na(x$dt_saiuti) & !is.na(x$evolucao))
    if(is.na(x$dt_evo)){
      x$dt_saiuti = as.Date(x$dt_entuti + rwaittime(1, UTI_stay_wait_fit))
    } 
  x
}
fixEVODates = function(x, afterUTI_stay_wait_fit, hosp_wait_fit){
  if(is.na(x$dt_evo) & !is.na(x$evolucao)){
    if(!is.na(x$UTI) & x$UTI==1){
      x$dt_evo = as.Date(x$dt_saiuti + rwaittime(1, afterUTI_stay_wait_fit))
    } else{
      x$dt_evo = as.Date(x$dt_int + rwaittime(1, hosp_wait_fit))
    }
  }
  x
}