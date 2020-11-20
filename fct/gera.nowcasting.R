gera.nowcasting <- function(dados, # dados
                            caso = TRUE, # caso = FALSE faz obitos
                            tipo, # covid ou srag
                            hospitalizados = TRUE,
                            trim.now, # corte para nowcasting
                            window, # janela para nowcasting
                            trajectories = FALSE, # retorna trajetórias
                            obito_sin_pri = FALSE, # nowcasting obitos pela data de sintoma primario?)
                            semanal = FALSE){ # nowcasting semanal?
  ## Variavel para fazer o switch entre nowcasting diario ou semanal
  now.units <- ifelse(semanal, "1 week", "1 day")
  if(trajectories)
    NobBS  <- NobBS.posterior
  ## 1. nowcasting de casos ###
  if (caso) {
    if (hospitalizados)
      dados <- dados %>% filter(hospital == 1)
    ## 1.1 casos covid ####
    if (tipo == "covid") {
      ##COVID##
      dados2 <- dados %>%
        filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
        select(dt_notific, dt_sin_pri, dt_pcr, dt_digita) %>%
        mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE))
    }
    ## 1.2. casos srag ####
    if (tipo == "srag") {
      ## %PIP data de registro é data mais recente entre notificação e digitação, não deve incluir data pcr (dt_pcr)
      ## pq SRAG não precisa de teste para ser confirmado
      dados2 <- dados %>%
        select(dt_notific, dt_sin_pri, dt_digita) %>%
        mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) # nome aqui é pcr mas não tem pcr
    }
    if(semanal){
      ## Finds the later date that is a Staurday (end of a epi week)
      last.dates <- as.POSIXlt(sort(unique(dados2$dt_sin_pri), decreasing = TRUE)[1:7])
      max.date <- max(last.dates[last.dates$wday==6])
      ## Convert dates to the date of the last onset day of each epiweek (epi weeks in Brazil start on Sundays)
      dados2 %<>%
        filter(dados2$dt_sin_pri <= max.date) %>%
        mutate(dt_notific = week2date(date2week(dt_notific), floor_day=TRUE, week_start = "Sunday")+6,
               dt_sin_pri = week2date(date2week(dt_sin_pri), floor_day=TRUE, week_start = "Sunday")+6,
               dt_digita = week2date(date2week(dt_digita), floor_day=TRUE, week_start = "Sunday")+6,
               dt_pcr_dig = week2date(date2week(dt_pcr_dig), floor_day=TRUE, week_start = "Sunday")+6)

    }

    if (nrow(dados2) != 0) {
      dados.now <- NobBS(
        data = dados2,
        now = max(dados2$dt_sin_pri, na.rm = TRUE) - trim.now,
        onset_date = "dt_sin_pri",
        report_date = "dt_pcr_dig",
        units = now.units,
        moving_window = window)
    } else {
      dados.now <- NULL
    }
  }
  ## 2. nowcasting de obitos ####
  else {
    ## 2.1. obitos covid ####
    if (tipo == "covid") {
      ##obitos COVID ####
      ## Onset date = data do óbito
      if(!obito_sin_pri){
        dados2 <- dados %>%
          filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
          filter(evolucao == 2) %>%
          filter(!is.na(dt_evoluca)) %>%
          mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                   na.rm = TRUE)) %>%
          select(dt_evoluca, dt_notific, dt_encerra)
      }
      ## Onset date = data do óbito
      if(obito_sin_pri){
        dados2 <- dados %>%
          filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
          filter(evolucao == 2) %>%
          filter(!is.na(dt_sin_pri)) %>%
          mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                   na.rm = TRUE)) %>%
          select(dt_sin_pri, dt_notific, dt_encerra)
      }
    }
    ## 2.2. obitos srag ####
    if (tipo == "srag") {
      ## Onset date = data do óbito
      if(!obito_sin_pri){
        dados2 <- dados %>%
          filter(evolucao == 2) %>%
          filter(!is.na(dt_evoluca)) %>%
          mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                   na.rm = TRUE)) %>%
          select(dt_sin_pri, dt_evoluca, dt_notific, dt_encerra)
      }
      ## Onset date = data do óbito
      if(obito_sin_pri){
        dados2 <- dados %>%
          filter(evolucao == 2) %>%
          filter(!is.na(dt_sin_pri)) %>%
          mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                   na.rm = TRUE)) %>%
          select(dt_sin_pri, dt_evoluca, dt_notific, dt_encerra)
      }
    }
    if(semanal){
      onset <- ifelse(obito_sin_pri, "dt_sin_pri", "dt_evoluca") ## PIP: define o onset date
      ## Finds the later onset date that is a Staurday (end of a epi week)
      last.dates <- as.POSIXlt(sort(unique(c(dados2[,onset])), decreasing = TRUE)[1:7])
      max.date <- max(last.dates[last.dates$wday==6])
      ## Convert dates to the date of the last onset day of each epiweek (epi weeks in Brazil start on Sundays)
      dados2 <- dados2[dados2[,onset]<= max.date,]
      dados2 %<>%
        mutate(dt_notific = week2date(date2week(dt_notific), floor_day=TRUE, week_start = "Sunday")+6,
               dt_evoluca = week2date(date2week(dt_evoluca), floor_day=TRUE, week_start = "Sunday")+6,
               dt_sin_pri = week2date(date2week(dt_sin_pri), floor_day=TRUE, week_start = "Sunday")+6,
               dt_encerra = week2date(date2week(dt_encerra), floor_day=TRUE, week_start = "Sunday")+6)
    }

    if (nrow(dados2) != 0) {
      onset <- ifelse(obito_sin_pri, "dt_sin_pri", "dt_evoluca") ## PIP: define o onset date
      its.now <- max(dados2[,onset], na.rm=TRUE)
      dados.now <- NobBS(
        data = dados2,
        now = its.now - trim.now, ##PIP: nocwasting vai até última data de onset, que pdoe ser a do obito ou do sintoma primario (novo argumento obit_sin_pri)
        onset_date = onset,
        report_date = "dt_encerra",
        units = now.units,
        moving_window = window,
        specs = list(beta.priors = dbinom(0:40, size = 40, p = 15/50)))
    } else {
      dados.now <- NULL
    }
  }
  out <- list(now = dados.now, dados = dados2)
  return(out)
}
