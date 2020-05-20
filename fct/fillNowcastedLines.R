## Using Nowcast and survival analysis to complete covid table with unobserved cases
fillNowcastedLines = function(df, nowcast, hosp_wait_fit, int_wait_fit, 
                              UTI_stay_wait_fit, UTI_after_wait_fit, prob_UTI, ...){
  df.now_casted = dplyr::select(df, dt_sin, dt_int, dt_evo, UTI, dt_entuti, dt_saiuti, age_class)
  nowcasts = data.frame(nowcast$estimates)
  current_date = unique(nowcasts$onset_date)[2]
  dates = as.Date(unique(nowcasts$onset_date))
  createNewInds = function(i, col){
    current_date = dates[i]
    #print(current_date)
    current_now_cast = filter(nowcasts, onset_date == current_date)
    current_now_cast$n.reported = replace_na(current_now_cast$n.reported, 0)
    missing = current_now_cast[[col]] - current_now_cast$n.reported
    missing[missing < 0] = 0
    names(missing) = current_now_cast$stratum
    n_missing = sum(missing, na.rm = TRUE)
    new.df = data.frame(dt_sin    = as.Date(rep("1859-11-24", n_missing)), 
                        dt_int    = as.Date(rep("1859-11-24", n_missing)), 
                        dt_evo    = as.Date(rep("1859-11-24", n_missing)), 
                        UTI       = as.numeric(rep(NA, n_missing)), 
                        dt_entuti = as.Date(rep("1859-11-24", n_missing)), 
                        dt_saiuti = as.Date(rep("1859-11-24", n_missing)),
                        age_class = as.character(rep("OOS", n_missing)), stringsAsFactors = F)
    ll = 1
    if(any(missing >= 1, na.rm = T)){
      to_add = missing[which(missing >= 1)]
      for(current_age in names(to_add)){
        for(i in 1:to_add[current_age]){
          new.df$dt_sin[ll] = current_date
          new.df$dt_int[ll] = as.Date(new.df$dt_sin[ll] + rwaittime(1, int_wait_fit))
          new.df$dt_evo[ll] = as.Date(new.df$dt_int[ll] + rwaittime_age(1, current_age, hosp_wait_fit))
          if(rbernoulli(1, prob_UTI[age_table$ID == current_age])){
            new.df$UTI[ll] = 1
            new.df$dt_entuti[ll] = as.Date(new.df$dt_int[ll] + 1)
            new.df$dt_saiuti[ll] = as.Date(new.df$dt_entuti[ll] + rwaittime(1, UTI_stay_wait_fit))
            new.df$dt_evo[ll] = as.Date(new.df$dt_saiuti[ll] + rwaittime(1, UTI_after_wait_fit) - 1)
          } else{
            new.df$UTI[ll] = 2
            new.df$dt_entuti[ll] = as.Date(NA)
            new.df$dt_saiuti[ll] = as.Date(NA)
          }
          new.df$age_class[ll] = as.character(current_age)
          ll = ll + 1
        }
      }
    }
    return(new.df)
  }
  estimate = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "estimate", ...)) %>% arrange(dt_sin)
  upper    = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "upper", ...)) %>% arrange(dt_sin)
  lower    = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "lower", ...)) %>% arrange(dt_sin)
  return(list(observed = df.now_casted, estimate = estimate, upper = upper, lower = lower))
}