getTimes = function(x, late, early, censored = FALSE){  
  if(!censored){
    time = as.numeric(x[[late]] - x[[early]])
    data.frame(ID = x$ID, time = time, age_class = x$age_class, 
               early =  x[[early]], late = x[[late]])
  } else{
    if(is.na(x[[late]])){ 
      time = as.numeric(today() - x[[early]])
      censored = 0
    } else{
      time = as.numeric(x[[late]] -x[[early]])
      censored = 1
    } 
    data.frame(ID = x$ID, evolucao = x$evolucao, time = time, age_class = x$age_class, censored = censored,
               early =  x[[early]], late = x[[late]])
  }
}