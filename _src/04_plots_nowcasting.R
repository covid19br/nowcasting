#####################################################################
## 1. Plot diários, acumulados, tempo de duplicação e Re
## 2. Exporta tabelas com números do dia
###################################################################

# dir para os ler os dados
data.dir <- df.path

# dir para os outputs, separados em subpastas
plot.dir <- paste0(output.dir, "plots/")#could be in the script
if (!dir.exists(plot.dir)) dir.create(plot.dir)

# testando se existe nowcasting
existe.covid <- existe.nowcasting2(tipo = "covid",
                                   data = data,
                                   output.dir = data.dir)
existe.srag <- existe.nowcasting2(tipo = "srag",
                                  data = data,
                                  output.dir = data.dir)
existe.ob.covid <- existe.nowcasting2(tipo = "obitos_covid",
                                      data = data,
                                      output.dir = data.dir)
existe.ob.srag <- existe.nowcasting2(tipo = "obitos_srag",
                                     data = data,
                                     output.dir = data.dir)
existe.ob.srag.proaim <- existe.nowcasting2(tipo = "obitos_srag_proaim",
                                            data = data,
                                            output.dir = data.dir)

#############
## COVID ####
#############

if (existe.covid) {
  data.covid <- data
  df.covid.diario <- read.csv(paste0(data.dir, "nowcasting_diario_covid_", data.covid, ".csv"),
                              stringsAsFactors = FALSE)
  df.covid.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_covid_", data.covid, ".csv"),
                           stringsAsFactors = FALSE)
  df.td.covid <- read.csv(paste0(data.dir, "tempo_duplicacao_covid_", data.covid, ".csv"),
                          stringsAsFactors = FALSE)
  df.re.covid <- read.csv(paste0(data.dir, "r_efetivo_covid_", data.covid, ".csv"),
                          stringsAsFactors = FALSE)
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
              df.cum = df.covid.cum,
              df.td = df.td.covid,
              df.re = df.re.covid,
              data_base = data.covid)

} else {
  plot.nowcast.covid <- NULL
  plot.nowcast.cum.covid <- NULL
  plot.estimate.R0.covid <- NULL
  plot.tempo.dupl.covid <- NULL
  data_atualizacao <- NULL
}

############
## SRAG ####
############

if (existe.srag) {
  data.srag <- data
  df.srag.diario <- read.csv(paste0(data.dir, "nowcasting_diario_srag_", data.srag, ".csv"),
                             stringsAsFactors = FALSE)
  df.srag.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_srag_", data.srag, ".csv"),
                          stringsAsFactors = FALSE)
  df.td.srag <- read.csv(paste0(data.dir, "tempo_duplicacao_srag_", data.srag, ".csv"),
                         stringsAsFactors = FALSE)
  df.re.srag <- read.csv(paste0(data.dir, "r_efetivo_srag_", data.srag, ".csv"),
                         stringsAsFactors = FALSE)
  # PLOTS ####
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.srag <- df.srag.diario %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.nowcast.diario()

  ### acumulado
  plot.nowcast.cum.srag <- df.srag.cum %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.nowcast.acumulado()

  ### tempo de duplicação
  plot.tempo.dupl.srag <- df.td.srag %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.tempo.dupl()

  ### R efetivo
  plot.estimate.R0.srag <- df.re.srag %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.estimate.R0()

  # TABELAS ####
  tabelas.web(plot.dir,
              tipo = "srag",
              df.cum = df.srag.cum,
              df.td = df.td.srag,
              df.re = df.re.srag,
              data_base = data.srag)
} else {
  plot.nowcast.srag <- NULL
  plot.nowcast.cum.srag <- NULL
  plot.estimate.R0.srag <- NULL
  plot.tempo.dupl.srag <- NULL
  data_atualizacao <- NULL
}

#####################
## OBITOS COVID ####
#####################

if (existe.ob.covid) {
  data.ob.covid <- data
  df.ob.covid.diario <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_covid_", data.ob.covid, ".csv"),
                                 stringsAsFactors = FALSE)
  df.ob.covid.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_obitos_covid_", data.ob.covid, ".csv"),
                              stringsAsFactors = FALSE)
  df.td.ob.covid <- read.csv(paste0(data.dir, "tempo_duplicacao_obitos_covid_", data.ob.covid, ".csv"),
                             stringsAsFactors = FALSE)
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.ob.covid <- plot.nowcast.diario(df.ob.covid.diario) +
    xlab("Data do óbito") +
    ylab("Número de novos óbitos")

  ### acumulado
  plot.nowcast.cum.ob.covid <- plot.nowcast.acumulado(df.ob.covid.cum) +
    xlab("Data do óbito") +
    ylab("Número acumulado de óbitos")

  ### tempo de duplicação
  plot.tempo.dupl.ob.covid <- plot.tempo.dupl(df.td.ob.covid)

  # TABELAS ####
  tabelas.web(plot.dir,
              tipo = "obitos_covid",
              df.cum = df.ob.covid.cum,
              df.td = df.td.ob.covid,
              data_base = data.ob.covid)
} else {
  plot.nowcast.ob.covid <- NULL
  plot.nowcast.cum.ob.covid <- NULL
  plot.tempo.dupl.ob.covid <- NULL
  data_atualizacao <- NULL
}

####################
## OBITOS SRAG ####
####################

if (existe.ob.srag) {
  data.ob.srag <- data
  df.ob.srag.diario <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_srag_", data.ob.srag, ".csv"),
                                stringsAsFactors = FALSE)
  df.ob.srag.cum <- read.csv(paste0(data.dir, "nowcasting_acumulado_obitos_srag_", data.ob.srag, ".csv"),
                             stringsAsFactors = FALSE)
  df.td.ob.srag <- read.csv(paste0(data.dir, "tempo_duplicacao_obitos_srag_", data.ob.srag, ".csv"),
                            stringsAsFactors = FALSE)
  ### diario
  ## N de novos casos observados e por nowcasting
  ## Com linha de média móvel
  plot.nowcast.ob.srag <- df.ob.srag.diario %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.nowcast.diario() +
    xlab("Dia") +
    ylab("Número de novos óbitos")

  ### acumulado
  plot.nowcast.cum.ob.srag <- df.ob.srag.cum %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.nowcast.acumulado() +
    xlab("Dia") +
    ylab("Número acumulado de óbitos")

  ### tempo de duplicação
  plot.tempo.dupl.ob.srag <- df.td.ob.srag %>%
    dplyr::filter(data > "2020-03-15") %>%
    plot.tempo.dupl()

  # TABELAS ####
    tabelas.web(plot.dir,
                tipo = "obitos_srag",
                df.cum = df.ob.srag.cum,
                df.td = df.td.ob.srag,
                data_base = data.ob.srag)
} else {
  plot.nowcast.ob.srag <- NULL
  plot.nowcast.cum.ob.srag <- NULL
  plot.tempo.dupl.ob.srag <- NULL
  data_atualizacao <- NULL
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

################################################################# ###############
## Atualiza gráficos por estado
################################################################################
print("Atualizando plots...")

# Graficos a serem atualizados
plots.para.atualizar <-
  makeNamedList(
  # covid
  plot.nowcast.covid,
  plot.nowcast.cum.covid,
  plot.estimate.R0.covid,
  plot.tempo.dupl.covid,
  # srag
  plot.nowcast.srag,
  plot.nowcast.cum.srag,
  plot.estimate.R0.srag,
  plot.tempo.dupl.srag,
  # obitos covid
  plot.nowcast.ob.covid,
  plot.nowcast.cum.ob.covid,
  plot.tempo.dupl.ob.covid,
  # obitos srag
  plot.nowcast.ob.srag,
  plot.nowcast.cum.ob.srag,
  plot.tempo.dupl.ob.srag
  #obitos srag.proaim
  #plot.nowcast.ob.srag.proaim,
  #plot.nowcast.cum.ob.srag.proaim,
  #plot.tempo.dupl.ob.srag.proaim
)
# pegando apenas os plots que existem mesmo
plots.true <- sapply(plots.para.atualizar, function(x) !is.null(x))

filenames <- gsub(".", "_", names(plots.para.atualizar), fixed = TRUE)
filenames <- paste0(plot.dir, filenames)

n <- 1:length(plots.para.atualizar)

for (i in n[plots.true]) {
  fig.name <- filenames[i]

  # SVG ####
  # fazendo todos os graficos svg para o site
  graph.svg <- plots.para.atualizar[[i]] +
    theme(axis.text = element_text(size = 6.65)
          #plot.margin = margin(10, 0, 0, 7, "pt")
    )
  ggsave(paste(fig.name, ".svg", sep = ""),
         plot = graph.svg,
         device = svg,
         scale = 1,
         width = 215,
         height = 146,
         units = "mm")
  #ast nao chequei as dimensoes, só tirei o que parece redundante
}
