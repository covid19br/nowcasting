#####################################################################
## 1. Leitura de arquivo  do sivep gripe no diretorio SIVEP-Gripe
## 2. Execucao do nowcasting, e criação de tabelas de n de casos
# por data de sintoma e notificacao
## 3. Exporta os arquivos
###################################################################

# usa funcoes em fct:
## 1. read.sivep
## 2. gera.nowcasting
## 3. write.notificacoes.data
## 4.  write.nowcasting
## 5. betas.summary
##    betas.cumsum
## 6. quantile_delay


################################################################################
## Importacao de preparacao dos dados
################################################################################

# Se for para baixar de data especifica, usar o argumento data
dados <- read.sivep(dir = dir,
                    escala = escala,
                    geocode = geocode,
                    data = data,
                    sigla = sigla,
                    residentes = residentes)


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
                             window = window,
                             trajectories = trajectories)

now.srag <- gera.nowcasting(dados = dados,
                            caso = TRUE,
                            tipo = "srag",
                            trim.now = trim.now,
                            window = window,
                            trajectories = trajectories)

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
## Betas
################################################################################
## dados necessários: output nowcasting
## Calculo de betas diáiros, acumulados e medianos
## Covid
# betas.covid<-beta.summary(now.covid)
# betas.covid.sum<-beta.cumsum(now.covid)
# betas.covid.median<-quantile_delay(betas.covid.sum, prob = c(0.25,0.5,0.75,0.95))
# ## SRAG
# betas.srag<-beta.summary(now.srag)
# betas.srag.sum<-beta.cumsum(now.srag)
# betas.srag.median<-quantile_delay(betas.srag.sum, prob = c(0.25,0.5,0.75,0.95))
## Óbitos Covid
betas.ob.covid<-beta.summary(now.ob.covid)
betas.ob.covid.sum<-beta.cumsum(now.ob.covid)
betas.ob.covid.median<-quantile_delay(betas.ob.covid.sum, prob = c(0.25,0.5,0.75,0.95))
## Óbitos SRAG
# betas.ob.srag<-beta.summary(now.ob.srag)
# betas.ob.srag.sum<-beta.cumsum(now.ob.srag)
# betas.ob.srag.median<-quantile_delay(betas.ob.srag.sum, prob = c(0.25,0.5,0.75,0.95))
## comentado porque o primeiro interesse é covid ##


################################################################################
## Exporta data frames com totais observados de casos ou obitos
## Exporta data frames com outputs de nowcasting
################################################################################


# COVID ####
write.notificacoes.data(dados = now.covid$dados,
                        tipo = "covid",
                        output.dir = out.path,
                        data = data)

if (!is.null(now.covid$now)) {
  write.nowcasting(now = now.covid$now,
                   output.dir = out.path,
                   tipo = "covid",
                   data = data)
}

# SRAG ####
write.notificacoes.data(dados = now.srag$dados,
                        tipo = "srag",
                        output.dir = out.path,
                        data = data)

if (!is.null(now.srag$now)) {
  write.nowcasting(now = now.srag$now,
                   output.dir = out.path,
                   tipo = "srag",
                   data = data)
}

# OBITOS COVID ####

write.notificacoes.data(dados = now.ob.covid$dados,
                        tipo = "obitos_covid",
                        output.dir = out.path,
                        data = data)

if (!is.null(now.ob.covid$now)) {
  write.nowcasting(now = now.ob.covid$now,
                  output.dir = out.path,
                  tipo = "obitos_covid",
                  data = data)
}

write.csv(betas.ob.covid,
          file = paste0(out.path, "betas_covid_obitos", data, ".csv"),
          row.names = FALSE)

write.csv(betas.ob.covid.sum,
          file = paste0(out.path, "betas_covid_obitos_cumsum", data, ".csv"),
          row.names = FALSE)

write.csv(betas.ob.covid.median,
          file = paste0(out.path, "betas_covid_obitos_median", data, ".csv"),
          row.names = FALSE)


# OBITOS SRAG ####

write.notificacoes.data(dados = now.ob.srag$dados,
                        tipo = "obitos_srag",
                        output.dir = out.path,
                        data = data)

if (!is.null(now.ob.srag$now)) {
  write.nowcasting(now = now.ob.srag$now,
                  output.dir = out.path,
                  tipo = "obitos_srag",
                  data = data)
}


