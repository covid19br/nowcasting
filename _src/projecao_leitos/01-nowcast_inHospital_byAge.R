
#PRJROOT  = rprojroot::find_root(".here")
#PRJROOT = "~/code"
#P = function(...) file.path(PRJROOT, ...)

### Set if looking for specific date
#data_date = NULL
#data_date = as.Date("2020-04-12")
#fix_missing_dates = TRUE
#source(P("Curve_fitting/00-read_process_SIVEP_DBF.R"))

## This takes a long time, try loading the Rdata
#source(C("SIVEP_survival_analysis.R"))
load(C("hospitalStatsFits.Rdata"))

### Nowcasting
# Covid
now.Date.covid  <-  max(covid.dt$dt_sin)
covid.now.day <- NobBS.strat(
  data = covid.dt,
  now = now.Date.covid,
  strata = "age_class",
  onset_date = "dt_sin",
  report_date = "dt_rec",
  units = "1 day",
  moving_window =  window,
  specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)

nowcasts.covid <- data.frame(covid.now.day$estimates)
# p1 <-
#   ggplot(nowcasts.covid) + geom_line(aes(onset_date,estimate,col="Estimado"),linetype="longdash") +
#   geom_line(aes(onset_date,n.reported,col="Notificado"),linetype="solid") +
#   scale_colour_manual(name="",values=c("indianred3","black"))+
#   theme_classic()+
#   geom_ribbon(fill="indianred3",aes(x = onset_date,ymin=lower, ymax=upper),alpha=0.3)+
#   xlab("Primeiro dia do sintoma") + ylab("Número de casos") + facet_wrap(~stratum)

# SRAG
now.Date.srag  <-  max(srag.dt$dt_sin)
srag.now.day <- NobBS.strat(
  data = srag.dt,
  now = now.Date.srag,
  strata = "age_class",
  onset_date = "dt_sin",
  report_date = "dt_mnd",
  units = "1 day",
  moving_window =  window,
  specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)

nowcasts.srag <- data.frame(srag.now.day$estimates)
# p2 <-
#   ggplot(nowcasts.srag) + geom_line(aes(onset_date,estimate,col="Estimado"),linetype="longdash") +
#   geom_line(aes(onset_date,n.reported,col="Notificado"),linetype="solid") +
#   scale_colour_manual(name="",values=c("indianred3","black"))+
#   theme_classic()+
#   geom_ribbon(fill="indianred3",aes(x = onset_date,ymin=lower, ymax=upper),alpha=0.3)+
#   xlab("Primeiro dia do sintoma") + ylab("Número de casos") + facet_wrap(~stratum)

## Using Nowcast and survival analysis com complete covid table with unobserved cases
covid.now_casted = fillNowcastedLines(covid.dt, covid.now.day, 
                                      time_fits1$covid$notUTI, 
                                      time_fits0$covid$Int, 
                                      time_fits0$covid$UTI, 
                                      time_fits0$covid$afterUTI, 
                                      probsFits$covid$uti[,"Estimate"],
                                      .parallel = TRUE)
srag.now_casted = fillNowcastedLines(srag.dt, srag.now.day, 
                                     time_fits1$srag$notUTI, 
                                     time_fits0$srag$Int, 
                                     time_fits0$srag$UTI, 
                                     time_fits0$srag$afterUTI, 
                                     probsFits$srag$uti[,"Estimate"],
                                     .parallel = TRUE)

start_date = as.Date("2020-03-01")
march_present = seq(start_date, today(), by = "day")
march_present.covid = seq(start_date, now.Date.covid, by = "day")
march_present.srag  = seq(start_date, now.Date.srag, by = "day")

covid_in_hospital = lapply(covid.now_casted, makeHospitalTable, march_present.covid)
covid_in_UTI      = lapply(covid.now_casted, makeHospitalTable, march_present.covid, UTI = TRUE)

srag_in_hospital = lapply(srag.now_casted, makeHospitalTable, march_present.srag)
srag_in_UTI      = lapply(srag.now_casted, makeHospitalTable, march_present.srag, UTI = TRUE)

hospitalized_totals_covid = data.frame(covid_in_hospital$observed$date, 
                                       t(laply(covid_in_hospital, function(x) rowSums(x[,-1]))),
                                       typo = "covid", stringsAsFactors = F)
names(hospitalized_totals_covid) = c("date", "observed", "estimate", "upper", "lower", "type")
hospitalized_totals_srag = data.frame(srag_in_hospital$observed$date, 
                                      t(laply(srag_in_hospital, function(x) rowSums(x[,-1]))),
                                      typo = "srag", stringsAsFactors = F)
names(hospitalized_totals_srag) = c("date", "observed", "estimate", "upper", "lower", "type")
hospitalized_totals = rbind(hospitalized_totals_covid, hospitalized_totals_srag)

# p3 = ggplot(hospitalized_totals, aes(date, observed, group = type, shape = type, color = type)) +
#   geom_point(size=2) + geom_line(aes(y = estimate)) +
#   geom_ribbon(fill="indianred3",
#               aes(ymin=lower, ymax=upper), alpha=0.3, color = 0) +
#   theme_cowplot() + scale_color_manual(values = c("black", "red")) +
#   scale_x_date(breaks = seq(as.Date("2020-03-08"), today()+7, by = 5),
#                labels = format(seq(as.Date("2020-03-08"), today()+7, by = 5), "%d %b")) +
#   scale_y_continuous(breaks = seq(0, 20000, by = 2000)) +
#   background_grid(major = "xy", minor = "y") +
#   labs(x = "Dia", y = "Número de casos hospitalizados")
# save_plot(filename = O("plots", 
#                        paste0("covid_srag_in_hospital_nowcast_", data_date, ".png")), 
#           p3, base_height = 6.5, base_asp = 1.7)

UTI_totals_covid = data.frame(covid_in_UTI$observed$date, 
                                       t(laply(covid_in_UTI, function(x) rowSums(x[,-1]))),
                                       typo = "covid", stringsAsFactors = F)
names(UTI_totals_covid) = c("date", "observed", "estimate", "upper", "lower", "type")
UTI_totals_srag = data.frame(srag_in_UTI$observed$date, 
                                      t(laply(srag_in_UTI, function(x) rowSums(x[,-1]))),
                                      typo = "srag", stringsAsFactors = F)
names(UTI_totals_srag) = c("date", "observed", "estimate", "upper", "lower", "type")
UTI_totals = rbind(UTI_totals_covid, UTI_totals_srag)

##################
# Output
##################

write_csv(hospitalized_totals, O("hospitalizados", paste0("hopitalized_", data_date,".csv")))
write_csv(UTI_totals, O("hospitalizados", paste0("hopitalized_UTI_", data_date,".csv")))

# p4 = ggplot(UTI_totals, aes(date, observed, group = type, color = type)) + 
#   geom_point(size=2, aes(shape = type)) + geom_line(aes(y = estimate)) + 
#   geom_ribbon(fill="indianred3", 
#               aes(ymin=lower, ymax=upper),alpha=0.3, color = 0) +
#   theme_cowplot() + scale_color_manual(values = c("black", "red")) +
#   scale_x_date(breaks = seq(as.Date("2020-03-08"), today()+7, by = 5), 
#                labels = format(seq(as.Date("2020-03-08"), today()+7, by = 5), "%d %b")) +
#   scale_y_continuous(breaks = seq(0, 20000, by = 1000)) +
#   background_grid(major = "xy", minor = "y") + 
#   labs(x = "Dia", y = "Número de casos hospitalizados em UTI") 
# save_plot(filename = O("plots", 
#                        paste0("covid_srag_in_UTI_nowcast_", data_date, ".png")), 
#           p4, base_height = 6.5, base_asp = 1.7)

# write_csv(srag_in_hospital$observed, 
#           O("hospitalizados", paste0("srag_municipioSP_hospitalizados_porDia_", data_date, ".csv")))
# write_csv(covid_in_hospital$observed, 
#           O("hospitalizados", paste0("covid_municipioSP_hospitalizados_porDia_", data_date, ".csv")))
# write_csv(srag_in_hospital$estimate, 
#           O("hospitalizados", paste0("srag_municipioSP_hospitalizados_nowcast_porDia_", data_date, ".csv")))
# write_csv(srag_in_hospital$estimate, 
#           O("hospitalizados", paste0("covid_municipioSP_hospitalizados_nowcast_porDia_", data_date, ".csv")))

