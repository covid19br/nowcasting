fill.dates <- function(data, column){
  
  if(class(column) == "character"){cases = data[,column]}
  if(class(column) == "numeric"){cases = data[,column]}
  
  sequence = seq(min(data$onset), max(data$onset), by = 1)
  incidence = rep(0, length(sequence))
  
  for(i in 1:length(cases)){
    incidence[which(data$onset[i] == sequence)] = cases[i]
  }
  
  return(data.frame(dates = sequence, incidence = incidence))
}
