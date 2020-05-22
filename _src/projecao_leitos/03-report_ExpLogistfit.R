
excape_paths = function(x) gsub(" ", "\\\ ", x, fixed = TRUE)

data = latest_data$hosp
fits = latest_data$fits$srag$Logist
latest_data = data
disease = "srag"
make_ggplot = function(data, latest_data = NULL, fits, disease ="covid", ylabel = "Hospitalizados", 
                       title = "Previsões"){
  last_date = last(filter(data, type == disease)$date)
  mask = fits$estimate$pred$date >= last_date
  
  plot = ggplot(filter(data, type == disease), aes(date, observed)) + 
    geom_ribbon(fill="blue", data = fits$lower$pred[mask,],
                aes(x = date, y = fits$estimate$pred$mean[mask], 
                    ymin = fits$lower$pred[mask, "X20."], 
                    ymax = fits$upper$pred[mask, "X80."]),alpha=0.2, color = 0) + 
    geom_ribbon(fill="indianred3", 
                aes(ymin=lower, ymax=upper),alpha=0.2, color = 0) +
    geom_point(data = filter(latest_data, type == disease), size=2) + geom_line(aes(y = estimate), color = "indianred3") + 
    geom_line(data = fits$lower$pred, aes(y = X20.), linetype= "dotted") + 
    geom_line(data = fits$upper$pred, aes(y = X80.), linetype= "dotted") + 
    geom_line(data = fits$estimate$pred, aes(y = mean), linetype= "dashed") + 
    theme_cowplot() + scale_color_manual(values = c("black", "darkgreen")) +
    scale_x_date(breaks = seq(as.Date("2020-03-08"), today()+7, by = 7), date_labels = "%d/%m/%y") +
    #scale_y_continuous(breaks = seq(0, 30000, by = breaks)) +
    background_grid(major = "xy", minor = "y") + 
    annotate("text", x = last_date - 6, 
             y = data[data$date == (last_date-6),"upper"] * 1.35, 
             label = "Nowcast", color = "indianred3", size = 4.5) +
    annotate("text", x = last_date + 3, 
             y = fits$upper$pred[fits$estimate$pred$date == (last_date+5), "X80."] * 1.1, 
             label = "Modelo", color = "blue", size = 4.5) +
    annotate("text", x = last_date-1, 
             y = data[data$date == (last_date-1),"observed"] * 0.75, 
             label = "Observado", color = "black", size = 4.5)  +
    theme(legend.position = "none") + 
    labs(x = "Dia", y = ylabel) + 
    ggtitle(title)
  
  if(!is.null(latest_data)){
    latest_last_date = last(filter(latest_data, type == disease)$date)
    plot_validation = plot + geom_ribbon(fill="orange",data = latest_data,
                                         aes(ymin=lower, ymax=upper),alpha=0.2, color = 0) +
      geom_line(data = latest_data, aes(y = estimate), color = "orange", size = 1) +
      annotate("text", x = latest_last_date+2, 
               y = latest_data[latest_data$date == (latest_last_date),"estimate"], 
               label = "Nowcast\natual", color = "orange", size = 4.5) 
    plot_validation
  } else
    plot_validation = NULL
  list(current = plot, validation = plot_validation)
}

hospitalized_files = sort(grep("hopitalized_2020", dir(O("hospitalizados"), full.names = TRUE), value = TRUE))
UTI_files = sort(grep("hopitalized_UTI_2020", dir(O("hospitalizados"), full.names = TRUE), value = TRUE))
fit_files = sort(grep("curve_fits_2020", dir(O("curve_fits"), full.names = TRUE), value = TRUE))

date_list = sort(gsub( ".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", fit_files))
date_list = tail(date_list, n = 8)

#######################
# reading vintage and current data
#######################

readAll = function(date){
  print(date)
  current_hosp_table = grep(as.character(date), hospitalized_files, value = TRUE)
  current_UTI_table = grep(as.character(date), UTI_files, value = TRUE)
  current_fit_table = grep(as.character(date), fit_files, value = TRUE)
  hospital_data = read.csv(current_hosp_table)
  hospital_data$date = as.Date(hospital_data$date)
  UTI_data = read.csv(current_UTI_table)
  UTI_data$date = as.Date(UTI_data$date)
  current_fits = readRDS(current_fit_table)
  return(list(hosp = hospital_data,
              UTI = UTI_data,
              fits = current_fits))
}
getCovidPlots = function(current_data){
  covid = current_data$hosp %>% filter(type == "covid")
  latest_covid = latest_data$hosp %>% filter(type == "covid")
  pce = make_ggplot(covid, latest_covid, current_data$fits$covid$Exp, 
                    ylabel = "Número de casos COVID-19 hospitalizados", 
                    title = "COVID-19 - Leitos Totais - Exponencial") 
  pcl = make_ggplot(covid, latest_covid, current_data$fits$covid$Logist,  
                    ylabel = "Número de casos COVID-19 hospitalizados",
                    title = "COVID-19 - Leitos Totais - Logistico")  
  
  covid_UTI = current_data$UTI %>% filter(type == "covid")
  latest_covid_UTI = latest_data$UTI %>% filter(type == "covid")
  pceU = make_ggplot(covid_UTI, latest_covid_UTI, current_data$fits$covid$UTIExp, 
                     ylabel = "Número de casos COVID-19 hospitalizados em UTI",
                     title = "COVID-19 - Leitos UTI - Exponencial") 
  pclU = make_ggplot(covid_UTI, latest_covid_UTI, current_data$fits$covid$UTILogist, 
                     ylabel = "Número de casos COVID-19 hospitalizados em UTI",
                     title = "COVID-19 - Leitos UTI - Logistico") 
  list(exp = pce, 
       lgt = pcl, 
       expU = pceU, 
       lgtU = pclU)
}
getSragPlots = function(current_data){
  srag = current_data$hosp %>% filter(type == "srag")
  latest_srag = latest_data$hosp %>% filter(type == "srag")
  pse = make_ggplot(srag, latest_srag, current_data$fits$srag$Exp, disease = "srag", 
                    ylabel = "Número de casos SRAG hospitalizados",
                    title = "SRAG - Leitos totais - Exponencial")
  
  psl = make_ggplot(srag, latest_srag, current_data$fits$srag$Logist, disease = "srag",
                    ylabel = "Número de casos SRAG hospitalizados",
                    title = "SRAG - Leitos totais - Logistico")
  
  srag_UTI = current_data$UTI %>% filter(type == "srag")
  latest_srag_UTI = latest_data$UTI %>% filter(type == "srag")
  
  pseU = make_ggplot(srag_UTI, latest_srag_UTI, current_data$fits$srag$UTIExp, disease = "srag", 
                     ylabel = "Número de casos SRAG hospitalizados em UTI",
                     title = "SRAG - Leitos UTI - Exponencial")
  
  pslU = make_ggplot(srag_UTI, latest_srag_UTI, current_data$fits$srag$UTILogist, disease = "srag", 
                     ylabel = "Número de casos SRAG hospitalizados em UTI",
                     title = "SRAG - Leitos UTI - Logistico")
  list(exp = pse, 
       lgt = psl, 
       expU = pseU, 
       lgtU = pslU)
}

all_data = lapply(date_list, readAll)
names(all_data) = date_list
latest_data = last(all_data)

covidPlots = lapply(all_data, getCovidPlots)
sragPlots = lapply(all_data, getSragPlots)

outputLatestPlot = function(plot_list){
  today_plots = last(plot_list)
  today_grid = plot_grid(today_plots$exp$current, today_plots$lgt$current,
                         today_plots$expU$current, today_plots$lgtU$current, nrow = 2, scale = 0.9)
  
  title <- ggdraw() + 
    draw_label(
      paste0("Previsões usando Notificações de SRAG Hospitalizados na base SIVEP Gripe de ", last(date_list)),
      fontface = 'bold',
      x = 0,
      hjust = 0, size = 30
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 20)
    )
  output = plot_grid(
    title, today_grid,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  
  # save_plot(filename = O(paste0("plots/temp_report_", last(date_list), ".pdf")), output, 
  #           ncol = 2, nrow = 2, base_height = 7, base_asp = 1.414)
}
outputPlot = function(c_date, plot_list){
  today_plots = plot_list[[c_date]]
  v_grid = plot_grid(today_plots$exp$validation + theme(axis.text.x = element_text(angle = 45, 
                                                                                   hjust = 1)) , 
                     today_plots$lgt$validation + theme(axis.text.x = element_text(angle = 45, 
                                                                                   hjust = 1)),
                     today_plots$expU$validation + theme(axis.text.x = element_text(angle = 45, 
                                                                                    hjust = 1)), 
                     today_plots$lgtU$validation + theme(axis.text.x = element_text(angle = 45, 
                                                                                    hjust = 1)), nrow = 2, scale = 0.9)
  title <- ggdraw() + 
    draw_label(
      paste0("Validação das previsões usando a base do dia ", c_date, "\ncontra observados atuais"),
      fontface = 'bold',
      x = 0,
      hjust = 0, size = 18
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  output = plot_grid(
    title, v_grid,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.12, 1)
  )
  
  # save_plot(filename = O(paste0("plots/temp_report_", c_date, ".pdf")), output, 
  #           ncol = 2, nrow = 2, base_height = 7, base_asp = 1.414)
  return(output)
}

##########
# Covid
##########
outputLatestPlot(covidPlots)
olderPlotsCovid = lapply(date_list[1:(length(date_list)-1)], outputPlot, covidPlots)
# report_files = sort(grep("temp_report_2020", dir(P("Curve_fitting/output/plots"), 
#                                                  full.names = TRUE), value = TRUE), decreasing = T)
# report_filesE = excape_paths(report_files)
# join_comand = paste0("pdfjoin ", paste(report_filesE, collapse = " "), " -o ", 
#                      excape_paths(O("plots")), 
#                      "/projecoes_demanda_hospitalar_", 
#                      last(date_list),"_covid.pdf")
# system(join_comand)
# file.remove(report_files)

##########
# SRAG
##########
outputLatestPlot(sragPlots)
olderPlotsSrag = lapply(date_list[1:(length(date_list)-1)], outputPlot, sragPlots)
# report_files = sort(grep("temp_report_2020", dir(O("plots"), 
#                                                  full.names = TRUE), value = TRUE), decreasing = T)
# report_filesE = excape_paths(report_files)
# join_comand = paste0("pdfjoin ", paste(report_filesE, collapse = " "), " -o ", 
#                      excape_paths(O("plots")), 
#                      "/projecoes_demanda_hospitalar_", 
#                      last(date_list),"_srag.pdf")
# system(join_comand)
# file.remove(report_files)

##############################
# Report
#############################

Sys.setlocale(category = "LC_TIME", locale = "pt_BR.UTF-8")

options(scipen=999)

knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)

data.atual = last(date_list)
disease = "covid"
disease_text = "COVID-19"
label_municipio = check.geocode("label", geocode = geocode)
plots = last(covidPlots)
olderPlots = olderPlotsCovid
render(input = C("relatorio.Rmd"),
       output_file = P(R(paste0(data.atual, "_relatorio_projecoes_demanda_hospitalar_covid.pdf"))),
       encoding = "utf8")

data.atual = last(date_list)
disease = "srag"
disease_text = "SRAG"
label_municipio = check.geocode("label", geocode = geocode)
plots = last(sragPlots)
olderPlots = olderPlotsSrag
render(input = C("relatorio.Rmd"),
       output_file = P(R(paste0(data.atual, "_relatorio_projecoes_demanda_hospitalar_srag.pdf"))),
       encoding = "utf8")

