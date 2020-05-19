## Pacotes necessários
library(zoo)
library(dplyr)
library(stringr)
source("funcoes.R")


# testando se existe nowcasting
existe.covid <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "covid",
                                  data = data.base, output.dir = output.dir)
existe.srag <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "srag",
                                 data = data.base, output.dir = output.dir)
existe.ob.covid <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_covid",
                                     data = data.base, output.dir = output.dir)
existe.ob.srag <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_srag",
                                    data = data.base, output.dir = output.dir)
existe.ob.srag.proaim <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "proaim_obitos_srag",
                                           data = data.base, output.dir = output.dir)


## Usa funcao prepara.dados
################################################################################
## Dados e nowcastings COVID
################################################################################
if (existe.covid) {
  # guarda data mais recente
  if (data.base == "NULL") {
    data.covid <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "covid", output.dir = output.dir)
  } else data.covid <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.covid <- prepara.dados(tipo = "covid",
                               adm = adm,
                               sigla.adm = sigla.adm,
                               data.base = data.covid,
                               output.dir = output.dir)
}
################################################################################
## Dados e nowcastings SRAG
################################################################################
if (existe.srag) {
  # guardando objeto data.base
  if (data.base == "NULL") {
    data.srag <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "srag", output.dir = output.dir)
  } else data.srag <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.srag <- prepara.dados(tipo = "srag",
                              adm = adm,
                              sigla.adm = sigla.adm,
                              data.base = data.srag,
                              output.dir = output.dir)
}
################################################################################
## Dados e nowcastings COVID OBITOS
################################################################################
if (existe.ob.covid) {
  if (data.base == "NULL") {
    data.ob.covid <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_covid",
                                   output.dir = output.dir)
  } else data.ob.covid <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.ob.covid <- prepara.dados(tipo = "obitos_covid",
                                  adm = adm,
                                  sigla.adm = sigla.adm,
                                  data.base = data.ob.covid,
                                  output.dir = output.dir)
}
################################################################################
## Dados e nowcastings SRAG OBITOS
################################################################################
if (existe.ob.srag) {
  if (data.base == "NULL") {
    data.ob.srag <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_srag",
                                  output.dir = output.dir)
  } else data.ob.srag <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.ob.srag <- prepara.dados(tipo = "obitos_srag",
                                 adm = adm,
                                 sigla.adm = sigla.adm,
                                 data.base = data.ob.srag,
                                 output.dir = output.dir)
}

if (existe.ob.srag.proaim) {
  if (data.base == "NULL") {
  data.ob.srag.proaim <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "proaim_obitos_srag",
                                       output.dir = output.dir)
  } else data.ob.srag.proaim <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.ob.srag.proaim <- prepara.dados(tipo = "proaim_obitos_srag",
                                        adm = adm,
                                        sigla.adm = sigla.adm,
                                        data.base = data.ob.srag.proaim,
                                        output.dir = output.dir)
}
