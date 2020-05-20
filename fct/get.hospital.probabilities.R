getProbUTI = function(df){
  df.UTI = filter(df, !is.na(UTI) & UTI!=9)
  UTI_table = as.matrix(table(df.UTI$age_class, df.UTI$UTI))
  UTI_data = data.frame(UTIadmissions = UTI_table[,1], trials = rowSums(UTI_table), age_class = age_table$ID)
  
  UTI_prob_model = brm(data = UTI_data, family = binomial,
                       UTIadmissions | trials(trials) ~ 1 + (1|age_class),
                       c(prior("normal(0, 1)", class = "Intercept"),
                         prior("normal(0, 1)", class = "sd")),
                       control = list(adapt_delta = 0.99))
  
  out = coef(UTI_prob_model) %>%
    {inv_logit_scaled(.$age_class)}
  data.frame(out[,,"Intercept"])
}

# probabilidade de morte de hospitalizado comum, e em UTI, 
getProbDeath = function(df, UTI = FALSE){
  if(UTI){
    df.filtered = filter(df, UTI==1, !is.na(evolucao) & evolucao!=9)
  } else{
    df.filtered = filter(df, UTI!=1, !is.na(evolucao) & evolucao!=9)
  }
  case_table = as.matrix(table(df.filtered$age_class, df.filtered$evolucao))
  
  trial_data = data.frame(deaths = 0, trials = 0, age_class = age_table$ID)
  trial_data[match(rownames(case_table), age_table$ID),1] = case_table[,2]
  trial_data[match(rownames(case_table), age_table$ID),2] = rowSums(case_table)
  trial_data$trials[trial_data$trials == 0] = 1 # Adds one trial if none exist
  death_prob_model = brm(data = trial_data, family = binomial,
                         deaths | trials(trials) ~ 1 + (1|age_class),
                         c(prior("normal(0, 1)", class = "Intercept"),
                           prior("normal(0, 1)", class = "sd")),
                         control = list(adapt_delta = 0.99))
  out = coef(death_prob_model) %>%
    {inv_logit_scaled(.$age_class)}
  data.frame(out[,,"Intercept"])
}