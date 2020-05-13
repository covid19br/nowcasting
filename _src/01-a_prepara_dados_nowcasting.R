## Pacotes necessários
library(zoo)
library(dplyr)
library(stringr)
source("funcoes.R")

# testando se existe nowcasting
existe.covid <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "covid", data = data.base)
existe.srag <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "srag", data = data.base)
existe.ob.covid <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_covid", data = data.base)
existe.ob.srag <- existe.nowcasting(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_srag", data = data.base)

## Usa funcao prepara.dados
################################################################################
## Dados e nowcastings COVID
################################################################################
if (existe.covid) { 
  # guarda data mais recente
  if (data.base == "NULL") {
  data.covid <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "covid")
  } else data.covid <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.covid <- prepara.dados(tipo = "covid", 
                               adm = adm, 
                               sigla.adm = sigla.adm,
                               data.base = data.covid)
}
################################################################################
## Dados e nowcastings SRAG
################################################################################
if (existe.srag) { 
  # guardando objeto data.base
  if (data.base == "NULL") {
  data.srag <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "srag")
  } else data.srag <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.srag <- prepara.dados(tipo = "srag",
                              adm = adm, 
                              sigla.adm = sigla.adm,
                              data.base = data.srag)
}  
################################################################################
## Dados e nowcastings COVID OBITOS
################################################################################
if (existe.ob.covid) {
  if (data.base == "NULL") {
  data.ob.covid <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_covid")
  } else data.ob.covid <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.ob.covid <- prepara.dados(tipo = "obitos_covid", 
                                  adm = adm, 
                                  sigla.adm = sigla.adm,
                                  data.base = data.ob.covid)
}  
################################################################################
## Dados e nowcastings SRAG OBITOS
################################################################################
if (existe.ob.srag) {
  if (data.base == "NULL") {
  data.ob.srag <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = "obitos_srag")
  } else data.ob.srag <- as.Date(data.base) %>%  format("%Y_%m_%d")
  lista.ob.srag <- prepara.dados(tipo = "obitos_srag", 
                                 adm = adm, 
                                 sigla.adm = sigla.adm,
                                 data.base = data.ob.srag)
}

