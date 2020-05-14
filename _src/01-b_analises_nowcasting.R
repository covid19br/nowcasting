library(dplyr)
source("funcoes.R")

###############
# 1. COVID ####
###############

if (existe.covid) {
  ## 1.1 projecao ####
  now.proj.zoo <- now.proj(pred = lista.covid$now.pred.zoo,
                           pred.original = lista.covid$now.pred.original,
                           now.params.post = lista.covid$now.params.post)


  ## 1.2 Cálculo do R efetivo ####
  Re.now <- Re.com.data(ncasos = lista.covid$now.pred.zoo$upper.merged,
                        datas = time(lista.covid$now.pred.zoo),
                        delay = 7)
  ## Objeto time series indexado pela data de fim de cada janela de cálculo
  Re.now.zoo <- zoo(Re.now$R[, -(12:13)], Re.now$R[, 13])

  ## 1.3. Cálculo do tempo de duplicação ####
  td.now <- dt.rw(lista.covid$now.pred.zoo$estimate.merged.c, window.width = 5)
  ## Conveniencia: reordena e renomeia as colunas do objeto resultante
  td.now <- td.now[, c(1, 3, 2)]
  names(td.now) <- c("estimativa", "ic.inf", "ic.sup")

  ## 1.4. Corta a partir do dia com >= 10 casos ####
  dia.zero <- time(lista.covid$now.pred.zoo)[min(which(lista.covid$now.pred.zoo$n.casos >= 10, arr.ind = TRUE))]
  if (!is.na(dia.zero)) {
    now.pred.zoo <- window(lista.covid$now.pred.zoo, start = dia.zero)
    now.proj.zoo <- window(now.proj.zoo, start = dia.zero)
    td.now <- window(td.now, start = dia.zero)
  } else {
    now.pred.zoo <- lista.covid$now.pred.zoo
  }

  ## 1.5 Gera df para grafico ####
  df.covid <- formata.now.df(now.pred.zoo,
                             now.proj.zoo,
                             lista.covid)
  df.covid.diario <- df.covid$diario
  df.covid.cum <- df.covid$acumulado
  # salva o df em csv
  df.path <- paste0("../dados/", adm, "_", sigla.adm, "/tabelas_nowcasting_para_grafico/")
  if (!dir.exists(df.path)) dir.create(df.path)
  write.csv(df.covid.cum,
            paste0(df.path, "nowcasting_acumulado_covid_", data.covid, ".csv"),
            row.names = FALSE)
  write.csv(df.covid.diario,
            paste0(df.path, "nowcasting_diario_covid_", data.covid, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(td.now),
            paste0(df.path, "tempo_duplicacao_covid_", data.covid, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(Re.now.zoo),
            paste0(df.path, "r_efetivo_covid_", data.covid, ".csv"),
            row.names = FALSE)
}


###############
# 2. SRAG #####
###############

if (existe.srag) {
  ## 2.1 projecao ####
  now.srag.proj.zoo <-  now.proj(pred = lista.srag$now.pred.zoo,
                                 pred.original = lista.srag$now.pred.original,
                                 now.params.post = lista.srag$now.params.post)

  ## 2.2 Cálculo do R efetivo ####
  ## SRAG ##
  Re.now.srag <- Re.com.data(ncasos = lista.srag$now.pred.zoo$upper.merged,
                             datas = time(lista.srag$now.pred.zoo), delay = 7)
  ## Objeto time series indexado pela data de fim de cada janela de cálculo
  Re.now.srag.zoo <- zoo(Re.now.srag$R[, -(12:13)], Re.now.srag$R[, 13])

  ## 2.3. Cálculo do tempo de duplicação ####
  td.now.srag <- dt.rw(lista.srag$now.pred.zoo$estimate.merged.c, window.width = 5)
  ## Conveniencia: reordena e renomeia as colunas do objeto resultante
  td.now.srag <- td.now.srag[, c(1, 3, 2)]
  names(td.now.srag) <- c("estimativa", "ic.inf", "ic.sup")

  ## 2.4. Corta a partir do dia com >= 10 casos ####
  # corta so se da :P
  dia.zero.srag <- time(lista.srag$now.pred.zoo)[min(which(lista.srag$now.pred.zoo$n.casos >= 10, arr.ind = TRUE))]
  if (!is.na(dia.zero.srag)) {
    now.srag.pred.zoo <- window(lista.srag$now.pred.zoo, start = dia.zero.srag)
    now.srag.proj.zoo <- window(now.srag.proj.zoo, start = dia.zero.srag)
    td.now.srag <- window(td.now.srag, start = dia.zero.srag)
  } else {
    now.srag.pred.zoo <- lista.srag$now.pred.zoo
  }

  ## 2.5. Gera df para grafico ####
  df.srag <- formata.now.df(now.srag.pred.zoo,
                            now.srag.proj.zoo,
                            lista.srag)
  df.srag.diario <- df.srag$diario
  df.srag.cum <- df.srag$acumulado
  # salva o df em csv
  df.path <- paste0("../dados/", adm, "_", sigla.adm, "/tabelas_nowcasting_para_grafico/")
  if (!dir.exists(df.path)) dir.create(df.path)
  write.csv(df.srag.cum,
            paste0(df.path, "nowcasting_acumulado_srag_", data.srag, ".csv"),
            row.names = FALSE)
  write.csv(df.srag.diario,
            paste0(df.path, "nowcasting_diario_srag_", data.srag, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(td.now.srag),
            paste0(df.path, "tempo_duplicacao_srag_", data.srag, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(Re.now.srag.zoo),
            paste0(df.path, "r_efetivo_srag_", data.srag, ".csv"),
            row.names = FALSE)
}

######################
# 3. OBITOS COVID ####
######################

if (existe.ob.covid) {
  ## 3.1 projecao ####
  now.ob.covid.proj.zoo <-  now.proj(pred = lista.ob.covid$now.pred.zoo,
                                     pred.original = lista.ob.covid$now.pred.original,
                                     now.params.post = lista.ob.covid$now.params.post)

  ## 3.2. Cálculo do tempo de duplicação ####
  td.now.ob.covid <- dt.rw(lista.ob.covid$now.pred.zoo$estimate.merged.c, window.width = 5)
  ## Conveniencia: reordena e renomeia as colunas do objeto resultante
  td.now.ob.covid <- td.now.ob.covid[, c(1, 3, 2)]
  names(td.now.ob.covid) <- c("estimativa", "ic.inf", "ic.sup")

  ## 3.3. Corta a partir do dia com >= 10 casos ####
  dia.zero.ob.covid <- time(lista.ob.covid$now.pred.zoo)[min(which(lista.ob.covid$now.pred.zoo$n.casos >= 10, arr.ind = TRUE))]
  if (!is.na(dia.zero.ob.covid)) {
    now.ob.covid.pred.zoo <- window(lista.ob.covid$now.pred.zoo, start = dia.zero.ob.covid)
    now.ob.covid.proj.zoo  <- window(now.ob.covid.proj.zoo, start = dia.zero.ob.covid)
    td.now.ob.covid <- window(td.now.ob.covid, start = dia.zero.ob.covid)
  } else {
    now.ob.covid.pred.zoo <- lista.ob.covid$now.pred.zoo
  }

  ## 3.4. Gera df para grafico ####
  df.ob.covid <- formata.now.df(now.ob.covid.pred.zoo,
                                now.ob.covid.proj.zoo,
                                lista.ob.covid)
  df.ob.covid.diario <- df.ob.covid$diario
  df.ob.covid.cum <- df.ob.covid$acumulado
  df.path <- paste0("../dados/", adm, "_", sigla.adm, "/tabelas_nowcasting_para_grafico/")
  if (!dir.exists(df.path)) dir.create(df.path)
  write.csv(df.ob.covid.cum,
            paste0(df.path, "nowcasting_acumulado_obitos_covid_", data.ob.covid, ".csv"),
            row.names = FALSE)
  write.csv(df.ob.covid.diario,
            paste0(df.path, "nowcasting_diario_obitos_covid_", data.ob.covid, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(td.now.ob.covid),
            paste0(df.path, "tempo_duplicacao_obitos_covid_", data.ob.covid, ".csv"),
            row.names = FALSE)
}

#####################
# 4. OBITOS SRAG ####
######################

if (existe.ob.srag) {
  ## 4.1 projecao ####
  now.ob.srag.proj.zoo <-  now.proj(pred = lista.ob.srag$now.pred.zoo,
                                    pred.original = lista.ob.srag$now.pred.original,
                                    now.params.post = lista.ob.srag$now.params.post)

  ## 4.2. Cálculo do tempo de duplicação ####
  td.now.ob.srag <- dt.rw(lista.ob.srag$now.pred.zoo$estimate.merged.c, window.width = 5)
  ## Conveniencia: reordena e renomeia as colunas do objeto resultante
  td.now.ob.srag <- td.now.ob.srag[, c(1, 3, 2)]
  names(td.now.ob.srag) <- c("estimativa", "ic.inf", "ic.sup")

  ## 4.3. Corta a partir do dia com >= 10 casos ####
  dia.zero.ob.srag <- time(lista.ob.srag$now.pred.zoo)[min(which(lista.ob.srag$now.pred.zoo$n.casos >= 10, arr.ind = TRUE))]
  if (!is.na(dia.zero.ob.srag)) {
    now.ob.srag.pred.zoo <- window(lista.ob.srag$now.pred.zoo, start = dia.zero.ob.srag)
    now.ob.srag.proj.zoo  <- window(now.ob.srag.proj.zoo, start = dia.zero.ob.srag)
    td.now.ob.srag <- window(td.now.ob.srag, start = dia.zero.ob.srag)
  } else {
    now.ob.srag.pred.zoo <- lista.ob.srag$now.pred.zoo
  }

  ## 4.4. Gera df para grafico
  df.ob.srag <- formata.now.df(now.ob.srag.pred.zoo,
                               now.ob.srag.proj.zoo,
                               lista.ob.srag)
  df.ob.srag.diario <- df.ob.srag$diario
  df.ob.srag.cum <- df.ob.srag$acumulado
  df.path <- paste0("../dados/", adm, "_", sigla.adm, "/tabelas_nowcasting_para_grafico/")
  if (!dir.exists(df.path)) dir.create(df.path)
  write.csv(df.ob.srag.cum,
            paste0(df.path, "nowcasting_acumulado_obitos_srag_", data.ob.srag, ".csv"),
            row.names = FALSE)
  write.csv(df.ob.srag.diario,
            paste0(df.path, "nowcasting_diario_obitos_srag_", data.ob.srag, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(td.now.ob.srag),
            paste0(df.path, "tempo_duplicacao_obitos_srag_", data.ob.srag, ".csv"),
            row.names = FALSE)
}


############################
# 4. OBITOS SRAG PROAIM ####
############################
# so importa no caso do municipio SP

if (existe.ob.srag.proaim) {
  ## 4.1 projecao ####
  now.ob.srag.proj.zoo.proaim <-  now.proj(pred = lista.ob.srag.proaim$now.pred.zoo,
                                           pred.original = lista.ob.srag.proaim$now.pred.original,
                                           now.params.post = lista.ob.srag.proaim$now.params.post)

  ## 4.2. Cálculo do tempo de duplicação ####
  td.now.ob.srag.proaim <- dt.rw(lista.ob.srag.proaim$now.pred.zoo$estimate.merged.c, window.width = 5)
  ## Conveniencia: reordena e renomeia as colunas do objeto resultante
  td.now.ob.srag.proaim <- td.now.ob.srag.proaim[, c(1, 3, 2)]
  names(td.now.ob.srag.proaim) <- c("estimativa", "ic.inf", "ic.sup")

  ## 4.3. Corta a partir do dia com >= 10 casos ####
  dia.zero.ob.srag.proaim <- time(lista.ob.srag.proaim$now.pred.zoo)[min(which(lista.ob.srag.proaim$now.pred.zoo$n.casos >= 10, arr.ind = TRUE))]
  if (!is.na(dia.zero.ob.srag.proaim)) {
    now.ob.srag.pred.zoo.proaim <- window(lista.ob.srag.proaim$now.pred.zoo, start = dia.zero.ob.srag.proaim)
    now.ob.srag.proj.zoo.proaim  <- window(now.ob.srag.proj.zoo.proaim, start = dia.zero.ob.srag.proaim)
    td.now.ob.srag.proaim <- window(td.now.ob.srag.proaim, start = dia.zero.ob.srag.proaim)
  } else {
    now.ob.srag.pred.zoo.proaim <- lista.ob.srag.proaim$now.pred.zoo
  }

  ## 4.4. Gera df para grafico
  df.ob.srag.proaim <- formata.now.df(now.ob.srag.pred.zoo.proaim,
                                      now.ob.srag.proj.zoo.proaim,
                                      lista.ob.srag.proaim)
  df.ob.srag.diario.proaim <- df.ob.srag.proaim$diario
  df.ob.srag.cum.proaim <- df.ob.srag.proaim$acumulado

  push_folder <- "../para_o_site/"
  df.path <- paste0(push_folder, "dados/", adm, "_", sigla.adm, "/tabelas_nowcasting_para_grafico/")
  if (!dir.exists(df.path)) dir.create(df.path)
  write.csv(df.ob.srag.cum.proaim,
            paste0(df.path, "nowcasting_acumulado_obitos_srag_proaim_", data.ob.srag.proaim, ".csv"),
            row.names = FALSE)
  write.csv(df.ob.srag.diario.proaim,
            paste0(df.path, "nowcasting_diario_obitos_srag_proaim_", data.ob.srag.proaim, ".csv"),
            row.names = FALSE)
  write.csv(zoo2df(td.now.ob.srag.proaim),
            paste0(df.path, "tempo_duplicacao_obitos_srag_proaim_", data.ob.srag.proaim, ".csv"),
            row.names = FALSE)
}
