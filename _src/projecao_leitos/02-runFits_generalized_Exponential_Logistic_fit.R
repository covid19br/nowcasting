#PRJROOT = rprojroot::find_root(".here")
#PRJROOT = "~/code"
#P = function(...) file.path(PRJROOT, ...)
fit_generalizedExp = function(y, trim = 2, iter = 2000, project = 7){
  n_days = length(y)
  used_days = n_days-trim
  stan_data = list(T = used_days,                 
                   T_p = n_days + project,
                   y = as.vector(y)[1:used_days],
                   ts = 1:used_days,
                   ts_p = 1:(n_days + project),
                   initial_y = as.vector(y)[1])
  mod <- cmdstan_model(C("stan_files/generalized_exponential_fit.stan"))
  capture.output(fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 0, iter_warmup = iter/2, iter_sampling = iter/2, 
    adapt_delta = 0.99, max_treedepth = 12
  ), type = "message")
  y_pred = fit$summary("cases_pred", "mean", ~quantile(.x, c(0.025, 0.1, 0.2, 0.5, 0.8, 0.9, 0.975)))
  names(y_pred)[3:9] = c("X2.5.", "X10.", "X20.", "X50.", "X80.", "X90.", "X97.5.")
  y_pred$date = as.Date((1:nrow(y_pred))-1, origin = initial_date)
  #list(model=model, pred = y_pred)
  list(pred = as.data.frame(y_pred))
}
fit_generalizedLogistic = function(y, trim = 2, iter = 2000, project = 7){
  n_days = length(y)
  used_days = n_days-trim
  stan_data = list(T = used_days,                 
                   T_p = n_days + project,
                   y = as.vector(y)[1:used_days],
                   ts = 1:used_days,
                   ts_p = 1:(n_days + project),
                   initial_y = as.vector(y)[1],
                   K_prior = last(y))
  mod <- cmdstan_model(C("stan_files/generalized_logistic_fit.stan"))
  capture.output(fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 0, iter_warmup = iter/2, iter_sampling = iter/2, 
    adapt_delta = 0.99, max_treedepth = 12
  ), type = "message")
  y_pred = fit$summary("cases_pred", "mean", ~quantile(.x, c(0.025, 0.1, 0.2, 0.5, 0.8, 0.9, 0.975)))
  names(y_pred)[3:9] = c("X2.5.", "X10.", "X20.", "X50.", "X80.", "X90.", "X97.5.")
  y_pred$date = as.Date((1:nrow(y_pred))-1, origin = initial_date)
  #list(model=model, pred = y_pred)
  list(pred = as.data.frame(y_pred))
}

runExpFit = function(x, trim = 2, iter = 2000, project = 7){
  fit_Exp_estimate = fit_generalizedExp(x$estimate, trim, iter, project)
  fit_Exp_upper    = fit_generalizedExp(x$upper, trim, iter, project)
  fit_Exp_lower    = fit_generalizedExp(x$lower, trim, iter, project)
  fits = list(estimate = fit_Exp_estimate,
              upper    = fit_Exp_upper,
              lower    = fit_Exp_lower)
  fits
}
runLogistFit = function(x, trim = 2, iter = 2000, project = 7){
  fit_Logist_estimate = fit_generalizedLogistic(x$estimate, trim, iter, project)
  fit_Logist_upper    = fit_generalizedLogistic(x$upper, trim, iter, project)
  fit_Logist_lower    = fit_generalizedLogistic(x$lower, trim, iter, project)
  fits = list(estimate = fit_Logist_estimate,
              upper    = fit_Logist_upper,
              lower    = fit_Logist_lower)
  fits
}

hospitalized_files = sort(grep("hopitalized_2020", 
                               dir(O("hospitalizados"), full.names = TRUE), value = TRUE))
UTI_files = sort(grep("hopitalized_UTI_2020", 
                      dir(O("hospitalizados"), full.names = TRUE), value = TRUE))

### Set if looking for specific date
#data_date = as.Date("2020-05-01")

if(is.null(data_date)){
  stop("Target validation date not set.")
} else{
  say(paste("FITTING MODELS FOR DATE:", format(data_date, "%d %B %Y")), "cat")
  current_hosp_table = grep(as.character(data_date), hospitalized_files, value = TRUE)
  current_UTI_table = grep(as.character(data_date), UTI_files, value = TRUE)
  FITSPATH = O("curve_fits", paste0("curve_fits_", data_date,".Rds"))
}

######################################
# reading current data
#####################################

hospital_data = read.csv(current_hosp_table)
hospital_data$date = as.Date(hospital_data$date)

UTI_data = read.csv(current_UTI_table)
UTI_data$date = as.Date(UTI_data$date)

covid = hospital_data %>% filter(type == "covid", date >= initial_date)
covid_UTI = UTI_data %>% filter(type == "covid", date >= initial_date)
srag = hospital_data %>% filter(type == "srag", date >= initial_date)
srag_UTI = UTI_data %>% filter(type == "srag", date >= initial_date)

########################
# Covid
########################

########################
# Hospitalized
########################

print("Exp covid")
fitsExpCovid = runExpFit(covid)    
print("Logist covid")
fitsLogistCovid = runLogistFit(covid)

########################
# UTI
########################

print("Exp covid UTI")
fitsExpCovidUTI = runExpFit(covid_UTI)
print("Logist covid UTI")
fitsLogitCovidUTI = runLogistFit(covid_UTI)

########################
# SRAG
########################

#######################
# Hospitalized
#######################

print("Exp srag")
fitsExpSrag = runExpFit(srag, trim = 2, iter = 4000)
print("Logist srag")
fitsLogistSrag = runLogistFit(srag, trim = 2, iter = 4000)

########################
# UTI
########################

print("Exp srag UTI")
fitsExpSragUTI = runExpFit(srag_UTI, trim = 2, iter = 4000)
print("Logist srag UTI")
fitsLogitSragUTI = runLogistFit(srag_UTI, trim = 2, iter = 4000)

########################
# Output
########################

fits = list(date = data_date,  
            covid = list(Exp = fitsExpCovid,
                         Logist = fitsLogistCovid,
                         UTIExp = fitsExpCovidUTI,
                         UTILogist = fitsLogitCovidUTI),
            srag = list(Exp = fitsExpSrag,
                        Logist = fitsLogistSrag,
                        UTIExp = fitsExpSragUTI,
                        UTILogist = fitsLogitSragUTI))
saveRDS(fits, file = FITSPATH)
#readRDS(FITSPATH)            

