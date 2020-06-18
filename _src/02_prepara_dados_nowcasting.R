#####################################################################
## 1. Testa se existe nowcasting para o tipo específico
## 2. Prepara os dados (cria objetos zoo) para análises
###################################################################

# usa funcoes em fct:
## 1. existe.nowcasting
## 2. prepara.dados

# testando se existe nowcasting
existe.covid <- existe.nowcasting(tipo = "covid",
                                  data = data,
                                  output.dir = out.path)

existe.srag <- existe.nowcasting(tipo = "srag",
                                 data = data,
                                 output.dir = out.path)

existe.ob.covid <- existe.nowcasting(tipo = "obitos_covid",
                                     data = data,
                                     output.dir = out.path)

existe.ob.srag <- existe.nowcasting(tipo = "obitos_srag",
                                    data = data,
                                    output.dir = out.path)

existe.ob.srag.proaim <- existe.nowcasting(tipo = "proaim_obitos_srag",
                                           data = data,
                                           output.dir = out.path)


## Usa funcao prepara.dados
################################################################################
## Dados e nowcastings COVID
################################################################################
if (existe.covid) {
  data.covid <- data
  lista.covid <- prepara.dados(tipo = "covid",
                               data.base = data.covid,
                               output.dir = out.path,
                               trajectories = trajectories)
    }

################################################################################
## Dados e nowcastings SRAG
################################################################################
if (existe.srag) {
    data.srag <- data
    lista.srag <- prepara.dados(tipo = "srag",
                                data.base = data.srag,
                                output.dir = out.path,
                                trajectories = trajectories)
  }

################################################################################
## Dados e nowcastings COVID OBITOS
################################################################################
if (existe.ob.covid) {
      data.ob.covid <- data
      lista.ob.covid <- prepara.dados(tipo = "obitos_covid",
                                      data.base = data.ob.covid,
                                      output.dir = out.path)
    }
################################################################################
## Dados e nowcastings SRAG OBITOS
################################################################################
if (existe.ob.srag) {
    data.ob.srag <- data
    lista.ob.srag <- prepara.dados(tipo = "obitos_srag",
                                   data.base = data.ob.srag,
                                   output.dir = out.path)
  }


if (existe.ob.srag.proaim) {
    data.ob.srag.proaim <- data
    lista.ob.srag.proaim <- prepara.dados(tipo = "proaim_obitos_srag",
                                          data.base = data.ob.srag.proaim,
                                          output.dir = out.path)
}
