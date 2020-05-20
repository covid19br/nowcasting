#####################################################################
## 1. Plot diários, acumulados, tempo de duplicação e Re
## 2. Exporta tabelas com números do dia
###################################################################


# dir para os ler os dados

#data.dir <- paste0("../dados/", escala, "_", sigla, "/", "tabelas_nowcasting_para_grafico/")
df_path <- paste0(output_dir, "tabelas_nowcasting_para_grafico/")
data.dir <- df_path
data <- get.last.date(data.dir)

# dir para os outputs, separados em subpastas
#output.dir <- paste0("../web/", escala, "_", sigla, "/")
plot.dir <- paste0(output_dir, "plots/")

if (!dir.exists(plots_dir)) dir.create(plots_dir)

# testando se existe nowcasting
existe.covid <- existe.nowcasting2(tipo = "covid", data = data, output.dir = data.dir)
existe.srag <- existe.nowcasting2(tipo = "srag", data = data, output.dir = data.dir)
existe.ob.covid <- existe.nowcasting2(tipo = "obitos_covid", data = data, output.dir = data.dir)
existe.ob.srag  <- existe.nowcasting2(tipo = "obitos_srag", data = data, output.dir = data.dir)
#existe.ob.srag.proaim <- existe.nowcasting2(escala = escala, sigla = sigla, tipo = "obitos_srag_proaim")

#############
## COVID ####
#############

if (existe.covid) {
  data.covid <- data
  df.covid.diario <- read.csv(paste0(data.dir, "nowcasting_diario_covid_", data.covid, ".csv"))
  df.covid.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_covid_", data.covid, ".csv"))
  df.td.covid <- read.csv(paste0(data.dir, "tempo_duplicacao_covid_", data.covid, ".csv"))
  df.re.covid <- read.csv(paste0(data.dir, "r_efetivo_covid_", data.covid, ".csv"))
  # PLOTS ####
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.covid <- plot.nowcast.diario(df.covid.diario)

  ### acumulado
  plot.nowcast.cum.covid <- plot.nowcast.acumulado(df.covid.cum)

  ### tempo de duplicação
  plot.tempo.dupl.covid <- plot.tempo.dupl(df.td.covid)

  ### R efetivo
  plot.estimate.R0.covid <- plot.estimate.R0(df.re.covid)

  # TABELAS ####
  ## Tabela que preenche o minimo e o maximo do nowcast, tempo de duplicacao, e r efetivo
  tabelas.web(plot.dir,
              tipo = "covid",
              df.covid.cum,
              df.td.covid,
              df.re.covid)

} else {
  plot.nowcast.covid <- NULL
  plot.nowcast.cum.covid <- NULL
  plot.estimate.R0.covid <- NULL
  plot.tempo.dupl.covid <- NULL
}

############
## SRAG ####
############

if (existe.srag) {
  data.srag <- data
  df.srag.diario <- read.csv(paste0(data.dir, "nowcasting_diario_srag_", data.srag, ".csv"))
  df.srag.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_srag_", data.srag, ".csv"))
  df.td.srag <- read.csv(paste0(data.dir, "tempo_duplicacao_srag_", data.srag, ".csv"))
  df.re.srag <- read.csv(paste0(data.dir, "r_efetivo_srag_", data.srag, ".csv"))
  # PLOTS ####
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.srag <- df.srag.diario %>%
    filter(data > "2020-03-15") %>%
    plot.nowcast.diario()

  ### acumulado
  plot.nowcast.cum.srag <- df.srag.cum %>%
    filter(data > "2020-03-15") %>%
    plot.nowcast.acumulado()

  ### tempo de duplicação
  plot.tempo.dupl.srag <- df.td.srag %>%
    filter(data > "2020-03-15") %>%
    plot.tempo.dupl()

  ### R efetivo
  plot.estimate.R0.srag <- df.re.srag %>%
    filter(data > "2020-03-15") %>%
    plot.estimate.R0()

  # TABELAS ####
  tabelas.web(output.dir,
              tipo = "srag",
              df.srag.cum,
              df.td.srag,
              df.re.srag)
} else {
  plot.nowcast.srag <- NULL
  plot.nowcast.cum.srag <- NULL
  plot.estimate.R0.srag <- NULL
  plot.tempo.dupl.srag <- NULL
}

#####################
## OBTITOS COVID ####
#####################

if (existe.ob.covid) {
  data.ob.covid <- data
  df.ob.covid.diario <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_covid_", data.ob.covid, ".csv"))
  df.ob.covid.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_obitos_covid_", data.ob.covid, ".csv"))
  df.td.ob.covid <- read.csv(paste0(data.dir, "tempo_duplicacao_obitos_covid_", data.ob.covid, ".csv"))
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.ob.covid <- plot.nowcast.diario(df.ob.covid.diario) +
    xlab("Dia") +
    ylab("Número de novos óbitos")

  ### acumulado
  plot.nowcast.cum.ob.covid <- plot.nowcast.acumulado(df.ob.covid.cum) +
    xlab("Dia") +
    ylab("Número acumulado de óbitos")

  ### tempo de duplicação
  plot.tempo.dupl.ob.covid <- plot.tempo.dupl(df.td.ob.covid)

  # TABELAS ####
  tabelas.web(output.dir,
              tipo = "obitos_covid",
              df.ob.covid.cum,
              df.td.ob.covid)
} else {
  plot.nowcast.ob.covid <- NULL
  plot.nowcast.cum.ob.covid <- NULL
  plot.tempo.dupl.ob.covid <- NULL
}

####################
## OBTITOS SRAG ####
####################

if (existe.ob.srag) {
  data.ob.srag <- data
  df.ob.srag.diario <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_srag_", data.ob.srag, ".csv"))
  df.ob.srag.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_obitos_srag_", data.ob.srag, ".csv"))
  df.td.ob.srag <- read.csv(paste0(data.dir, "tempo_duplicacao_obitos_srag_", data.ob.srag, ".csv"))
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.ob.srag <- df.ob.srag.diario %>%
    filter(data > "2020-03-15") %>%
    plot.nowcast.diario() +
    xlab("Dia") +
    ylab("Número de novos óbitos")

  ### acumulado
  plot.nowcast.cum.ob.srag <- df.ob.srag.cum %>%
    filter(data > "2020-03-15") %>%
    plot.nowcast.acumulado() +
    xlab("Dia") +
    ylab("Número acumulado de óbitos")

  ### tempo de duplicação
  plot.tempo.dupl.ob.srag <- df.td.ob.srag %>%
    filter(data > "2020-03-15") %>%
    plot.tempo.dupl()

  # TABELAS ####
  tabelas.web(output.dir,
              tipo = "obitos_srag",
              df.ob.srag.cum,
              df.td.ob.srag)
} else {
  plot.nowcast.ob.srag <- NULL
  plot.nowcast.cum.ob.srag <- NULL
  plot.tempo.dupl.ob.srag <- NULL
}

#########################
# OBITOS SRAG PROAIM ####
#########################

# if (existe.ob.srag.proaim) {
#   data.ob.srag.proaim <- get.data.base2(escala, sigla, "obitos_srag_proaim")
#   df.ob.srag.diario.proaim <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_srag_proaim_",
#                                               data.ob.srag.proaim, ".csv"))
#   df.ob.srag.cum.proaim <- read.csv(paste0(data.dir, "nowcasting_acumulado_obitos_srag_proaim_",
#                                            data.ob.srag.proaim, ".csv"))
#   df.td.ob.srag.proaim <- read.csv(paste0(data.dir, "tempo_duplicacao_obitos_srag_proaim_", data.ob.srag.proaim, ".csv"))
#   ### diario
#   ## N de novos casos observados e por nowcasting
#   ## Com linha de média móvel
#   plot.nowcast.ob.srag.proaim <- plot.nowcast.diario(df.ob.srag.diario.proaim) +
#     xlab("Dia") +
#     ylab("Número de novos óbitos")
#
#   ### acumulado
#   plot.nowcast.cum.ob.srag.proaim <- plot.nowcast.acumulado(df.ob.srag.cum.proaim) +
#     xlab("Dia") +
#     ylab("Número acumulado de óbitos")
#
#   ### tempo de duplicação
#   plot.tempo.dupl.ob.srag.proaim <- plot.tempo.dupl(df.td.ob.srag.proaim)
#   # TABELAS ####
#   tabelas.web(sigla,
#               output.dir,
#               tipo = "obitos_srag_proaim",
#               df.ob.srag.cum.proaim,
#               df.td.ob.srag.proaim)
# } else {
#   plot.nowcast.ob.srag.proaim <- NULL
#   plot.nowcast.cum.ob.srag.proaim <- NULL
#   plot.tempo.dupl.ob.srag.proaim <- NULL
# }
