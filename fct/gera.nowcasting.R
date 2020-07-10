gera.nowcasting <- function(dados, # dados
                            caso = TRUE, # caso = FALSE faz obitos
                            tipo, # covid ou srag
                            hospitalizados = TRUE,
                            trim.now, # corte para nowcasting
                            window, # janela para nowcasting
                            trajectories = FALSE, # retorna trajetórias
                            obito_sin_pri = FALSE)  { # nowcasting obitos pela data de sintoma primario?) 
  if(trajectories)
      NobBS  <- NobBS.posterior
  # 1. nowcasting de casos ###
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

    if (nrow(dados2) != 0) {
      dados.now <- NobBS(
        data = dados2,
        now = max(dados2$dt_sin_pri, na.rm = TRUE) - trim.now,
        onset_date = "dt_sin_pri",
        report_date = "dt_pcr_dig",
        units = "1 day",
        moving_window = window)
    } else {
     dados.now <- NULL
    }

    # 2. nowcasting de obitos ####
  } else {
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
                select(dt_evoluca, dt_notific, dt_encerra)
        }
          ## Onset date = data do óbito
        if(obito_sin_pri){
            dados2 <- dados %>%
                filter(evolucao == 2) %>%
                filter(!is.na(dt_sin_pri)) %>%
                mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                         na.rm = TRUE)) %>%
                select(dt_sin_pri, dt_notific, dt_encerra)
        }
      }
      if (nrow(dados2) != 0) {
          onset <- ifelse(obito_sin_pri, "dt_sin_pri", "dt_evoluca") ## PIP: define o onset date
          its.now <- max(dados2[,onset])
          dados.now <- NobBS(
              data = dados2,
              now = its.now - trim.now, ##PIP: nocwasting vai até última data de onset, que pdoe ser a do obito ou do sintoma primario (novo argumrnto obit_sin_pri)
              onset_date = onset,
              report_date = "dt_encerra",
              units = "1 day",
              moving_window = window,
              specs = list(beta.priors = dbinom(0:40, size = 40, p = 15/50)))
      } else {
          dados.now <- NULL
      }
  }
  out <- list(now = dados.now, dados = dados2)
  return(out)
}
