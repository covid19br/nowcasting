getCurrentInBed = function(df, date, UTI){
  if(!UTI){
    df = df %>% 
      filter(dt_int <= date & (dt_evo >= date | is.na(dt_evo)))
  }else{
    df = df %>% 
      filter(UTI == 1, dt_entuti <= date & (dt_saiuti >= date | is.na(dt_saiuti)))
  }
}