if(!require(ggridges)){install.packages("ggridges"); library(ggridges)}

if(exists("PRJROOT")){
  if(!is.null(PRJROOT)) 
    PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  
} else 
  PRJROOT = rprojroot::find_root(criterion=rprojroot::is_git_root)  

P = function(...) file.path(PRJROOT, ...)
CODEROOT = paste0(PRJROOT, "/_src/projecao_leitos")
C = function(...) file.path(CODEROOT, ...)	

source(P("fct/load_packages.R"))
source(P("_src/funcoes.R"))

all_models = TRUE

### Set if looking for specific date
#data_date = as.Date("2020-04-02")

fix_missing_dates = FALSE

DATAROOT = "../dados/estado_SP/SRAG_hospitalizados/dados/"
escala = "estado"
sigla = "SP"
geocode = 35
data_date <- as.Date(get.last.date(DATAROOT), format = "%Y_%m_%d")
source(C("00-read_process_SIVEP_CSV.R"))
EXPORT = function(...) file.path("../dados_processados/parametros_epidemicos", paste0(data_date, "_", ..., ".csv" ))
#probabilidade de hospitalizado ir pra UTI, 

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
  data.frame(out[,-2,"Intercept"])
}

prob_uti_covid = getProbUTI(covid.dt) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything()) 
prob_uti_srag = getProbUTI(srag.dt) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything()) 

write_csv(prob_uti_covid, EXPORT("prob_UTI_covid"))
write_csv(prob_uti_covid, EXPORT("prob_UTI_srag"))

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
                         control = list(adapt_delta = 0.99, max_treedepth = 12),
                         iter = 4000)
  out = coef(death_prob_model) %>%
    {inv_logit_scaled(.$age_class)}
  data.frame(out[,-2,"Intercept"])
}

if(all_models){
  prob_death_UTI_covid = getProbDeath(covid.dt, UTI = T) %>% 
    mutate(faixas = age_table$faixas) %>% 
    select(faixas, everything())
  prob_death_notUTI_covid = getProbDeath(covid.dt, UTI = F) %>% 
    mutate(faixas = age_table$faixas) %>% 
    select(faixas, everything())
  prob_death_UTI_srag = getProbDeath(srag.dt, UTI = T) %>% 
    mutate(faixas = age_table$faixas) %>% 
    select(faixas, everything())
  prob_death_notUTI_srag = getProbDeath(srag.dt, UTI = F) %>% 
    mutate(faixas = age_table$faixas) %>% 
    select(faixas, everything())
  
  write_csv(prob_death_UTI_covid, EXPORT("prob_death_UTI_covid"))
  write_csv(prob_death_notUTI_covid, EXPORT("prob_death_notUTI_covid"))
  write_csv(prob_death_UTI_srag, EXPORT("prob_death_UTI_srag"))
  write_csv(prob_death_notUTI_srag, EXPORT("prob_death_notUTI_srag"))
  
  probsFits = list(covid = list(uti = prob_uti_covid, 
                                death_uti = prob_death_UTI_covid, 
                                death_notuti = prob_death_notUTI_covid),
                   srag = list(uti = prob_uti_srag, 
                               death_uti = prob_death_UTI_srag, 
                               death_notuti = prob_death_notUTI_srag))
}

# df = covid.dt
# 
# df.UTI = filter(df, !is.na(UTI) & UTI!=9)
# UTI_table = as.matrix(table(df.UTI$age_class, df.UTI$UTI))
# UTI_data = data.frame(UTIadmissions = UTI_table[,1], trials = rowSums(UTI_table), age_class = age_table$ID)
# 
# table_twitter = data.frame(faixas = prob_uti_covid$faixas,
#                            hospitalizados =  UTI_data$trials,
#                            hospitalizados_UTI = UTI_data$UTIadmissions,
#                            prob_UTI = prob_uti_covid$Estimate*100,
#                            mort_UTI = prob_death_UTI_covid$Estimate*100,
#                            mort_notUTI = prob_death_notUTI_covid$Estimate*100)
# write_csv(table_twitter, "~/Dropbox/table_twitter.csv")
# 
# xtable::xtable(table_twitter)

##################################
## Survival analysis
##################################

# #save_plot(filename = "plots/survival_dist_byAge.png", p1, base_height = 3, ncol = 3, nrow = 3)
# 
# current_age = age_table$ID[1]
# ldply(age_table$ID, function(current_age) quantile(rwaittime_age(10000, current_age, fit1_hosp), c(0.1, 0.5, 0.9))) %>%
#   round(1) %>% mutate(age = age_table$faixas) %>% select(age, everything()) 

# tempo de hospitalização em leito comum, 

notUTIStay_covid = ddply(filter(covid.dt, UTI != 1), .(ID), getTimes, "dt_evo", "dt_int", censored = TRUE) %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
notUTIStay_srag  = ddply(filter(srag.dt,  UTI != 1), .(ID), getTimes, "dt_evo", "dt_int", censored = TRUE) %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
#qplot(data = notUTIStay_covid, x = time, geom = "histogram") + facet_wrap(~age_class)

fit0_notUTIStay_covid <- brm(time ~ 1, 
                             data = notUTIStay_covid, family = weibull, inits = "0", 
                             prior = c(prior("normal(0, 1)", class = "Intercept"),
                                       prior("normal(0, 0.5)", class = "shape")),
                             control = list(adapt_delta = 0.99))
plotTimesValidation(notUTIStay_covid, fit0_notUTIStay_covid, FALSE)

fit0_notUTIStay_srag <- brm(time ~ 1, 
                            data = notUTIStay_srag, family = weibull, inits = "0", 
                            prior = c(prior("normal(0, 1)", class = "Intercept"),
                                      prior("normal(0, 0.5)", class = "shape")),
                            control = list(adapt_delta = 0.99))
plotTimesValidation(notUTIStay_srag, fit0_notUTIStay_srag, FALSE)


fit1_notUTIStay_covid <- brm(time ~ 1 + (1|age_class), 
                             data = notUTIStay_covid, family = weibull, inits = "0", 
                             prior = c(prior("normal(0, 1)", class = "sd"), 
                                       prior("normal(0, 1)", class = "Intercept"),
                                       prior("normal(0, 0.5)", class = "shape")),
                             control = list(adapt_delta = 0.99))
plotTimesValidation(notUTIStay_covid, fit1_notUTIStay_covid)


fit1_notUTIStay_srag <- brm(time ~ 1 + (1|age_class), 
                            data = notUTIStay_srag, family = weibull, inits = "0", 
                            prior = c(prior("normal(0, 1)", class = "sd"), 
                                      prior("normal(0, 1)", class = "Intercept"),
                                      prior("normal(0, 0.5)", class = "shape")),
                            control = list(adapt_delta = 0.99))
plotTimesValidation(notUTIStay_srag, fit1_notUTIStay_srag)


if(all_models){
  notUTI_stay_times_covid = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                             c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                                  fit1_notUTIStay_covid) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  notUTI_stay_times_srag = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                            c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                                 fit1_notUTIStay_srag) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  
  write_csv(notUTI_stay_times_covid, EXPORT("notUTI_stay_times_covid"))
  write_csv(notUTI_stay_times_srag, EXPORT("notUTI_stay_times_srag"))
}
#em UTI, 

# Tempo entre Entrar e sair da UTI
UTIStay_covid = ddply(filter(covid.dt, UTI == 1), .(ID), getTimes, "dt_saiuti", "dt_entuti", censored = TRUE) %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
UTIStay_srag  = ddply(filter(srag.dt,  UTI == 1), .(ID), getTimes, "dt_saiuti", "dt_entuti", censored = TRUE) %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
#qplot(data = UTIStay_covid, x = time, geom = "histogram") + facet_wrap(~age_class)

fit0_UTIStay_covid <- brm(time ~ 1, 
                          data = UTIStay_covid, family = weibull, inits = "0", 
                          prior = c(prior("normal(0, 1)", class = "Intercept"),
                                    prior("normal(0, 0.5)", class = "shape")),
                          control = list(adapt_delta = 0.99))
plotTimesValidation(UTIStay_covid, fit0_UTIStay_covid, FALSE)


fit0_UTIStay_srag <- brm(time ~ 1, 
                         data = UTIStay_srag, family = weibull, inits = "0", 
                         prior = c(prior("normal(0, 1)", class = "Intercept"),
                                   prior("normal(0, 0.5)", class = "shape")),
                         control = list(adapt_delta = 0.99))
plotTimesValidation(UTIStay_srag, fit0_UTIStay_srag, FALSE)


fit1_UTIStay_covid <- brm(time ~ 1 + (1|age_class), 
                          data = UTIStay_covid, family = weibull, inits = "0", 
                          prior = c(prior("normal(0, 1)", class = "sd"), 
                                    prior("normal(0, 1)", class = "Intercept"),
                                    prior("normal(0, 0.5)", class = "shape")),
                          control = list(adapt_delta = 0.99))
plotTimesValidation(UTIStay_covid, fit1_UTIStay_covid)

fit1_UTIStay_srag <- brm(time ~ 1 + (1|age_class), 
                         data = UTIStay_srag, family = weibull, inits = "0", 
                         prior = c(prior("normal(0, 1)", class = "sd"), 
                                   prior("normal(0, 1)", class = "Intercept"),
                                   prior("normal(0, 0.5)", class = "shape")),
                         control = list(adapt_delta = 0.99))
plotTimesValidation(UTIStay_srag, fit1_UTIStay_srag)


if(all_models){
  UTI_stay_times_covid = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                          c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                               fit1_UTIStay_covid) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  UTI_stay_times_srag = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                         c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                              fit1_UTIStay_srag) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  
  write_csv(UTI_stay_times_covid, EXPORT("UTI_stay_times_covid"))
  write_csv(UTI_stay_times_srag, EXPORT("UTI_stay_times_srag"))
  
  
  meanUTI_stay_times_covid = llply(age_table$ID, 
                                   function(age, fit1){ 
                                     rwaittime_posterior_age(100, age, fit1)
                                   },
                                   fit1_UTIStay_covid) %>%
    llply(colMeans) %>% ldply(function(x) c(mean(x), sd(x))) %>%
    dplyr::rename(media = V1, sd = V2) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
}

## Tempo entre sintomas e internação

int_times_covid = ddply(covid.dt, .(ID), getTimes, late = "dt_int", early = "dt_sin") %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
int_times_srag  = ddply(srag.dt,  .(ID), getTimes, late = "dt_int", early = "dt_sin") %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))  
#qplot(data = int_times_covid, x = time, geom = "histogram") + facet_wrap(~age_class)

fit0_int_covid <- brm(time ~ 1,
                      data = int_times_covid, family = weibull, inits = "0", 
                      prior =  c(prior("normal(0, 1)", class = "Intercept"),
                                 prior("normal(0, 0.5)", class = "shape")), 
                      control = list(adapt_delta = .99))
plotTimesValidation(int_times_covid, fit0_int_covid, FALSE)

fit0_int_srag <- brm(time ~ 1,
                     data = int_times_srag, family = weibull, inits = "0",
                     prior =  c(prior("normal(0, 1)", class = "Intercept"),
                                prior("normal(0, 0.5)", class = "shape")),
                     control = list(adapt_delta = 0.99))
plotTimesValidation(int_times_srag, fit0_int_srag, FALSE)


if(all_models){
  fit1_int_covid <- brm(time ~ 1 + (1|age_class), 
                        data = int_times_covid, family = weibull, inits = "0", 
                        prior =  c(prior("normal(0, 1)", class = "sd"), 
                                   prior("normal(0, 1)", class = "Intercept"),
                                   prior("normal(0, 0.5)", class = "shape")), 
                        control = list(adapt_delta = .99))
  plotTimesValidation(int_times_covid, fit1_int_covid)
  
  
  fit1_int_srag <- brm(time ~ 1 + (1|age_class), 
                       data = int_times_srag, family = weibull, inits = "0",
                       prior =  c(prior("normal(0, 1)", class = "sd"), 
                                  prior("normal(0, 1)", class = "Intercept"),
                                  prior("normal(0, 0.5)", class = "shape")),
                       control = list(adapt_delta = 0.99))
  plotTimesValidation(int_times_srag, fit1_int_srag)
  
  
  sint_int_times_covid = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                          c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                               fit1_int_covid) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  sint_int_times_srag = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                         c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                              fit1_int_srag) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  
  write_csv(sint_int_times_covid, EXPORT("sint_int_times_covid"))
  write_csv(sint_int_times_srag, EXPORT("sint_int_times_srag"))
}

# Tempo entre sair da UTI e evolução

UTIAfter_covid = ddply(filter(covid.dt, !is.na(dt_saiuti), dt_saiuti <= today(), UTI == 1, evolucao == 1), .(ID), getTimes, "dt_evo", "dt_saiuti", censored = FALSE)%>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
UTIAfter_srag  = ddply(filter(srag.dt,  !is.na(dt_saiuti), dt_saiuti <= today(), UTI == 1, evolucao == 2), .(ID), getTimes, "dt_evo", "dt_saiuti", censored = FALSE) %>% 
  mutate(time = time + 1) %>% filter(time >= 1 & time <= today() - as.Date("2020-03-08"))
#qplot(data = UTIAfter_srag, x = time, geom = "histogram", binwidth = 1)

fit0_AfterUTI_covid <- brm(time ~ 1, 
                           data = UTIAfter_covid, family = weibull, inits = "0", 
                           prior =c(prior("normal(0, 0.05)", class = "Intercept"),
                                    prior("normal(0, 0.5)", class = "shape")), 
                           control = list(adapt_delta = 0.99))
plotTimesValidation(UTIAfter_covid, fit0_AfterUTI_covid, FALSE)

fit0_AfterUTI_srag <- brm(time ~ 1, 
                          data = UTIAfter_srag, family = weibull, inits = "0", 
                          prior =c(prior("normal(0, 0.05)", class = "Intercept"),
                                   prior("normal(0, 0.5)", class = "shape")), 
                          control = list(adapt_delta = 0.99))
plotTimesValidation(UTIAfter_srag, fit0_AfterUTI_srag, FALSE)

if(all_models){
  fit1_AfterUTI_covid <- brm(time ~ 1 + (1|age_class), 
                             data = UTIAfter_covid, family = weibull, inits = "0", 
                             prior =c(prior("normal(0, 0.05)", class = "sd"), 
                                      prior("normal(0, 1)", class = "Intercept"),
                                      prior("normal(0, 0.5)", class = "shape")), 
                             control = list(adapt_delta = 0.99))
  plotTimesValidation(UTIAfter_covid, fit1_AfterUTI_covid)
  
  
  fit1_AfterUTI_srag <- brm(time ~ 1 + (1|age_class), 
                            data = UTIAfter_srag, family = weibull, inits = "0", 
                            prior =c(prior("normal(0, 0.05)", class = "sd"), 
                                     prior("normal(0, 1)", class = "Intercept"),
                                     prior("normal(0, 0.5)", class = "shape")), 
                            control = list(adapt_delta = 0.99))
  plotTimesValidation(UTIAfter_srag, fit1_AfterUTI_srag)
  
  
  afterUTI_times_covid = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                          c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                               fit1_AfterUTI_covid) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  afterUTI_times_srag = ldply(age_table$ID, function(age, fit1) quantile(rwaittime_posterior_age(100, age, fit1), 
                                                                         c(0.025, 0.2, 0.5, 0.8, 0.975)), 
                              fit1_AfterUTI_srag) %>% mutate(faixas = age_table$faixas) %>% select(faixas, everything())
  
  write_csv(afterUTI_times_covid, EXPORT("afterUTI_times_covid"))
  write_csv(afterUTI_times_srag, EXPORT("afterUTI_times_srag"))
}



if(all_models){
  time_fits1 = list(covid = list(notUTI   = fit1_notUTIStay_covid, 
                                 UTI      = fit1_UTIStay_covid, 
                                 Int      = fit1_int_covid, 
                                 afterUTI = fit1_AfterUTI_covid),
                    srag  = list(notUTI   = fit1_notUTIStay_srag, 
                                 UTI      = fit1_UTIStay_srag, 
                                 Int      = fit1_int_srag, 
                                 afterUTI = fit1_AfterUTI_srag))
} else{
  load(C("hospitalStatsFits.Rdata"))
  time_fits1$covid$notUTI = fit1_notUTIStay_covid
  time_fits1$srag$notUTI = fit1_notUTIStay_srag
}

time_fits0 = list(covid = list(notUTI   = fit0_notUTIStay_covid, 
                               UTI      = fit0_UTIStay_covid, 
                               Int      = fit0_int_covid, 
                               afterUTI = fit0_AfterUTI_covid),
                  srag  = list(notUTI   = fit0_notUTIStay_srag, 
                               UTI      = fit0_UTIStay_srag, 
                               Int      = fit0_int_srag, 
                               afterUTI = fit0_AfterUTI_srag))

save(time_fits0, time_fits1, probsFits, 
     file = C("hospitalStatsFits.Rdata"))
# sim_hosp = sapply(hospitalization_times$age_class, function(a) rwaittime_age(1, a, fit1_hosp))
# hospitalization_times$sim = sim_hosp
# hospitalization_times$age = age_table$faixas[match(hospitalization_times$age_class, age_table$ID)]
# d = pivot_longer(hospitalization_times, c(sim, time))
# p1 = ggplot(data = d, aes(x = value, group = name, fill = name)) + 
#   geom_density(alpha= 0.5) + facet_wrap(~age) + 
#   theme_cowplot() + scale_fill_discrete(labels = c("Simulado", "Observado"), name = "Categoria") 
# #save_plot(filename = "plots/survival_dist_byAge.png", p1, base_height = 3, ncol = 3, nrow = 3)
# 
# current_age = age_table$ID[1]
# ldply(age_table$ID, function(current_age) quantile(rwaittime_age(10000, current_age, fit1_hosp), c(0.1, 0.5, 0.9))) %>%
#   round(1) %>% mutate(age = age_table$faixas) %>% select(age, everything()) 

int_times_covid = ddply(covid.dt, .(ID), getTimes, late = "dt_int", early = "dt_sin") %>% filter(time > 0, time < 20)
notUTIStay_covid = ddply(filter(covid.dt, UTI != 1), .(ID), getTimes, "dt_evo", "dt_int", censored = TRUE) %>% filter(time >= 0)
UTIStay_covid = ddply(filter(covid.dt, UTI == 1), .(ID), getTimes, "dt_saiuti", "dt_entuti", censored = TRUE) %>% filter(time >= 0)
notUTIStay_covid %>% filter(age_class == "age_1")
srag.20.raw %>%
  filter(nu_idade_n < 10, pcr_sars2 == 1 | classi_fin == 5, uti == 1, evolucao == 2)
times_table = UTIStay_covid
times_table %>% filter(!(is.na(evolucao) & !is.na(late)), age_class == "age_1")

plotTimesCensored = function(times_table, age = TRUE){
  if(age){
    times_table$age = age_table$faixas[match(times_table$age_class, age_table$ID)]
    if(!is.null(times_table$censored)){
      times_table = dplyr::filter(times_table, !(is.na(evolucao) & !is.na(late)))
      #times_table$evolucao[is.na(times_table$evolucao)] = "Caso Ativo"
      times_table = dplyr::filter(times_table, evolucao == 1 | evolucao == 2)
      times_table$evolucao[times_table$evolucao == 1] = "Alta"
      times_table$evolucao[times_table$evolucao == 2] = "Obito"
      ggplot(data = times_table, aes(x = time, y = age, 
                                     fill = evolucao)) + 
        geom_density_ridges2(alpha = 0.6) + #facet_wrap(~age, ncol = 1) +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        scale_fill_viridis_d(name = "Categoria") +
        theme_cowplot() + 
        labs(x = "Tempos (dias)", y = "Classe Etária")
    } else{
      ggplot(data = times_table, aes(x = time, y = age)) + 
        geom_density_ridges2(alpha = 0.6) + scale_fill_viridis_d(labels = c("Casos encerrados", "Casos ativos"), name = "Categoria") +
        theme_cowplot() + 
        labs(x = "Tempos (dias)", y = "Classe Etária")
    }
    
  } else{
    ggplot(data = times_table, aes(x = time, y = 1)) + 
      geom_density_ridges2(alpha = 0.7) + scale_fill_viridis_d(labels = c("Casos encerrados", "Casos ativos"), name = "Categoria") +
      theme_cowplot() + 
      labs(x = "Tempos (dias)", y = "Classe Etária")
  }
}

sintomas_internacao = plotTimesCensored(int_times_covid) + ggtitle("Sintoma -> Internação")
internacao_leito = plotTimesCensored(notUTIStay_covid) + ggtitle("Internação -> Encerramento\n (Leito comum, casos encerrados)")
internacao_UTI = plotTimesCensored(UTIStay_covid) + ggtitle("Internação -> Encerramento\n (Leito de UTI, casos encerrados)")

#plot_grid(sintomas_internacao, internacao_leito + theme(legend.position = "none"), internacao_UTI, ncol = 3)
p = sintomas_internacao + internacao_leito + theme(legend.position = "bottom") + internacao_UTI + theme(legend.position = "bottom")
save_plot("~/Desktop/tempos.png", p, ncol = 3, base_height = 9, base_asp = 0.5)          

