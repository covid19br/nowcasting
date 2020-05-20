countByAgeClass = function(df){
  out = data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
  colnames(out) = age_table$ID
  counts = table(df$age_class)
  out = out + counts[names(out)]
  out[is.na(out)] = 0
  return(out)
}