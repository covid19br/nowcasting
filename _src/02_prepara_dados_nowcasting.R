

# testando se existe nowcasting
existe.covid <- existe.nowcasting(tipo = "covid",
                                  data = data,
                                  output.dir = output.dir)

existe.srag <- existe.nowcasting(tipo = "srag",
                                 data = data,
                                 output.dir = output.dir)

existe.ob.covid <- existe.nowcasting(tipo = "obitos_covid",
                                     data = data,
                                     output.dir = output.dir)

existe.ob.srag <- existe.nowcasting(tipo = "obitos_srag",
                                    data = data,
                                    output.dir = output.dir)

existe.ob.srag.proaim <- existe.nowcasting(tipo = "proaim_obitos_srag",
                                           data = data,
                                           output.dir = output.dir)


## Usa funcao prepara.dados
################################################################################
## Dados e nowcastings COVID
################################################################################
if (existe.covid) {
  data.covid <- data
  lista.covid <- prepara.dados(tipo = "covid",
                               data.base = data.covid,
                               output.dir = output.dir)
    }

################################################################################
## Dados e nowcastings SRAG
################################################################################
if (existe.srag) {
    data.srag <- data
    lista.srag <- prepara.dados(tipo = "srag",
                                data.base = data.srag,
                                output.dir = output.dir)
  }

################################################################################
## Dados e nowcastings COVID OBITOS
################################################################################
if (existe.ob.covid) {
      data.ob.covid <- data
      lista.ob.covid <- prepara.dados(tipo = "obitos_covid",
                                      data.base = data.ob.covid,
                                      output.dir = output.dir)
    }
################################################################################
## Dados e nowcastings SRAG OBITOS
################################################################################
if (existe.ob.srag) {
    data.ob.srag <- data
    lista.ob.srag <- prepara.dados(tipo = "obitos_srag",
                                   data.base = data.ob.srag,
                                   output.dir = output.dir)
  }


if (existe.ob.srag.proaim) {
    data.ob.srag.proaim <- data
    lista.ob.srag.proaim <- prepara.dados(tipo = "proaim_obitos_srag",
                                          data.base = data.ob.srag.proaim,
                                          output.dir = output.dir)
}
