#### Creates a data.frame where the notified number of daily cases is replaced by the nowcasting series
#### in their corresponding dates.
#### nowcasting: a data.frame containing multiple nowcasting series
#### notified: an incidence table with dates and number of cases per day
join_nowcasting <- function(nowcasting, notified){
  prev.dates <- notified$onset[!(notified$onset %in% nowcasting$date)]
  prev.cases = notified$n.casos[!(notified$onset %in% nowcasting$date)]
  prev.cases.table <- data.frame(matrix(rep(prev.cases,each = ncol(nowcasting)-1), ncol = ncol(nowcasting)-1, byrow=TRUE))
  previous <- cbind(date = prev.dates, prev.cases.table)
  all <- rbind(nowcasting, previous)
  all <- all %>% arrange(date)
  return(all)
}
