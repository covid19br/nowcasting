plotTimesValidation = function(times_table, fit1, age = TRUE){
  if(age){
    times_table = times_table %>% arrange(age_class)
    tb = table(times_table$age_class)
    sim_times = unlist(sapply(names(tb), function(a) rwaittime_age(tb[a], a, fit1)))
    times_table$sim = sim_times
    times_table$age = age_table$faixas[match(times_table$age_class, age_table$ID)]
    d = pivot_longer(times_table, c(sim, time))
    ggplot(data = d, aes(x = value, group = name, fill = name)) + 
      geom_density(alpha= 0.5) + facet_wrap(~age) + 
      theme_cowplot() + scale_fill_discrete(labels = c("Simulado", "Observado"), name = "Categoria") 
  } else{
    sim_times = rwaittime(nrow(times_table), fit1)
    times_table$sim = sim_times
    d = pivot_longer(times_table, c(sim, time))
    ggplot(data = d, aes(x = value, group = name, fill = name)) + 
      geom_density(alpha= 0.5) + 
      theme_cowplot() + scale_fill_discrete(labels = c("Simulado", "Observado"), name = "Categoria") 
  }
}