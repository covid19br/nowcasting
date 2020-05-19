

# testando se existe nowcasting
existe.covid <- existe.nowcasting(escala = escala,
                                  sigla = sigla,
                                  tipo = "covid",
                                  data = data,
                                  output.dir = output.dir)

existe.srag <- existe.nowcasting(escala = escala,
                                 sigla = sigla,
                                 tipo = "srag",
                                 data = data,
                                 output.dir = output.dir)

existe.ob.covid <- existe.nowcasting(escala = escala,
                                     sigla = sigla,
                                     tipo = "obitos_covid",
                                     data = data,
                                     output.dir = output.dir)

existe.ob.srag <- existe.nowcasting(escala = escala,
                                    sigla = sigla,
                                    tipo = "obitos_srag",
                                    data = data,
                                    output.dir = output.dir)

existe.ob.srag.proaim <- existe.nowcasting(escala = escala,
                                           sigla = sigla,
                                           tipo = "proaim_obitos_srag",
                                           data = data,
                                           output.dir = output.dir)


## Usa funcao prepara.dados
################################################################################
## Dados e nowcastings COVID
################################################################################
if (existe.covid) {
  # guarda data mais recente
  if (data == "NULL") {
    data.covid <- get.data.base(tipo = "covid",
                                output.dir = output.dir)
  } else data.covid <- as.Date(data, format = "%Y_%m_%d") %>%
      format("%Y_%m_%d")##ast all this should be relying in format data and will be useful when other sources enter
  lista.covid <- prepara.dados(tipo = "covid",
                               escala = escala,
                               sigla = sigla,
                               data.base = data.covid,
                               output.dir = output.dir)
}
################################################################################
## Dados e nowcastings SRAG
################################################################################
if (existe.srag) {
  # guardando objeto data.base
  if (data == "NULL") {
    data.srag <- get.data.base(tipo = "srag",
                               output.dir = output.dir)
  } else data.srag <- as.Date(data, format = "%Y_%m_%d") %>%
      format("%Y_%m_%d")
  lista.srag <- prepara.dados(tipo = "srag",
                              escala = escala,
                              sigla = sigla,
                              data.base = data.srag,
                              output.dir = output.dir)
}
################################################################################
## Dados e nowcastings COVID OBITOS
################################################################################
if (existe.ob.covid) {
  if (data == "NULL") {
    data.ob.covid <- get.data.base(tipo = "obitos_covid",
                                   output.dir = output.dir)
  } else data.ob.covid <- as.Date(data, format = "%Y_%m_%d") %>%
      format("%Y_%m_%d")
  lista.ob.covid <- prepara.dados(tipo = "obitos_covid",
                                  escala = escala,
                                  sigla = sigla,
                                  data.base = data.ob.covid,
                                  output.dir = output.dir)
}
################################################################################
## Dados e nowcastings SRAG OBITOS
################################################################################
if (existe.ob.srag) {
  if (data == "NULL") {
    data.ob.srag <- get.data.base(tipo = "obitos_srag",
                                  output.dir = output.dir)
  } else data.ob.srag <- as.Date(data, format = "%Y_%m_%d") %>%
      format("%Y_%m_%d")
  lista.ob.srag <- prepara.dados(tipo = "obitos_srag",
                                 escala = escala,
                                 sigla = sigla,
                                 data.base = data.ob.srag,
                                 output.dir = output.dir)
}

if (existe.ob.srag.proaim) {
  if (data == "NULL") {
  data.ob.srag.proaim <- get.data.base(tipo = "proaim_obitos_srag",
                                       output.dir = output.dir)
  } else data.ob.srag.proaim <- as.Date(data, format = "%Y_%m_%d") %>%
      format("%Y_%m_%d")
  lista.ob.srag.proaim <- prepara.dados(tipo = "proaim_obitos_srag",
                                        escala = escala,
                                        sigla = sigla,
                                        data.base = data.ob.srag.proaim,
                                        output.dir = output.dir)
}

