

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
  # guarda data mais recente#ast data já nao deve ser NULL porque entrou uma base de dados ou o usuário pediu, mas quem sabe, poderia nao ter a tabela especifica, né. o que a gente deveria testar é se data que já vem de antes (ex. 2020_05_19) é também a mais recente para cada tipo, entao seria se data != data.covid. pegar data.covid como get.data.base (aqui tá igual) e checar se são iguais.
  #o que fazer se são diferentes? stop? pular? null por enquanto né. ou talvez nem fazer o check, só que fique claro que é o mais recente. deixei por enquanto. só que isto pode ser redundante com o próprio existe. nãõ sei ainda.
    data.covid <- get.data.base(tipo = "covid",
                                output.dir = output.dir)
    if (data == data.covid) {
  #data.covid <- as.Date(data, format = "%Y_%m_%d") %>%
   #   format("%Y_%m_%d")##ast all this should be relying in format data and will be useful when other sources enter
  lista.covid <- prepara.dados(tipo = "covid",
                               data.base = data.covid,
                               output.dir = output.dir)
    }
}
################################################################################
## Dados e nowcastings SRAG
################################################################################
if (existe.srag) {
  # guardando objeto data.base
    data.srag <- get.data.base(tipo = "srag",
                               output.dir = output.dir)
  if (data.srag == data) {
    lista.srag <- prepara.dados(tipo = "srag",
                                data.base = data.srag,
                                output.dir = output.dir)
  }
}
################################################################################
## Dados e nowcastings COVID OBITOS
################################################################################
if (existe.ob.covid) {
    data.ob.covid <- get.data.base(tipo = "obitos_covid",
                                   output.dir = output.dir)
    if (data.ob.covid == data) {
      lista.ob.covid <- prepara.dados(tipo = "obitos_covid",
                                      data.base = data.ob.covid,
                                      output.dir = output.dir)
      }
    }
################################################################################
## Dados e nowcastings SRAG OBITOS
################################################################################
if (existe.ob.srag) {
    data.ob.srag <- get.data.base(tipo = "obitos_srag",
                                  output.dir = output.dir)
  if (data.ob.srag == data) {
    lista.ob.srag <- prepara.dados(tipo = "obitos_srag",
                                   data.base = data.ob.srag,
                                   output.dir = output.dir)
  }
}

if (existe.ob.srag.proaim) {
  data.ob.srag.proaim <- get.data.base(tipo = "proaim_obitos_srag",
                                       output.dir = output.dir)
  if (data.ob.srag.proaim == data) {
    lista.ob.srag.proaim <- prepara.dados(tipo = "proaim_obitos_srag",
                                          data.base = data.ob.srag.proaim,
                                          output.dir = output.dir)
  }
}
