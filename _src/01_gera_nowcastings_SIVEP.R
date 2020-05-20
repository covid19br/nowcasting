################################################################################
## 1. Leitura de arquivo  do sivep gripe no diretorio SIVEP-Gripe
## 2. Execucao do nowcasting, e criação de tabelas de n de casos por data de sintoma e notificacao
## 3. Dá push destes arquivos no repo do site OBSERVATÓRIO COVID-19 BR
################################################################################


################################################################################
## Importacao de preparacao dos dados
################################################################################

# Se for para baixar de data especifica, usar o argumento data
dados <- read.sivep(dir = dir,
                    escala = escala,
                    geocode = geocode,
                    data = data)


################################################################################
## Nowcastings
################################################################################
## dados necessarios: data de 1o sintoma, maior data entre resultado do teste, digitacao ou notificação
## Guarda data de notificação também para gerar a tabela de n de notificaoes por data
## pois o nowcasting retorna tabela de n de casos por data do 1o sintoma

now.covid <- gera.nowcasting(dados = dados,
                             caso = TRUE,
                             tipo = "covid",
                             trim.now = trim.now,
                             window = window)

now.srag <- gera.nowcasting(dados = dados,
                            caso = TRUE,
                            tipo = "srag",
                            trim.now = trim.now,
                            window = window)

now.ob.covid <- gera.nowcasting(dados = dados,
                                caso = FALSE,
                                tipo = "covid",
                                trim.now = trim.now,
                                window = window)

now.ob.srag <- gera.nowcasting(dados = dados,
                               caso = FALSE,
                               tipo = "srag",
                               trim.now = trim.now,
                               window = window)

################################################################################
## Exporta data frames com totais observados de casos ou obitos
## Exporta data frames com outputs de nowcasting
################################################################################


# COVID ####
write.notificacoes.data(dados = now.covid$dados,
                        tipo = "covid",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.covid$now)) {
  write.nowcasting(now = now.covid$now,
                   output.dir = output.dir,
                   tipo = "covid",
                   data = data)
}

# SRAG ####
write.notificacoes.data(dados = now.srag$dados,
                        tipo = "srag",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.srag$now)) {
  write.nowcasting(now = now.srag$now,
                   output.dir = output.dir,
                   tipo = "srag",
                   data = data)
}

# OBITOS COVID ####

write.notificacoes.data(dados = now.ob.covid$dados,
                        tipo = "obitos_covid",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.ob.covid$now)) {
  write.nowcasting(now = now.ob.covid$now,
                  output.dir = output.dir,
                  tipo = "obitos_covid",
                  data = data)
}


# OBITOS SRAG ####

write.notificacoes.data(dados = now.ob.srag$dados,
                        tipo = "obitos_srag",
                        output.dir = output.dir,
                        data = data)

if (!is.null(now.ob.srag$now)) {
  write.nowcasting(now = now.ob.srag$now,
                  output.dir = output.dir,
                  tipo = "obitos_srag",
                  data = data)
}


