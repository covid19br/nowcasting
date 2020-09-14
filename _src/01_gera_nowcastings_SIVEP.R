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
## Calculo de betas diários, acumulados e medianos
## Covid
if (betas == TRUE) {
  if (!is.null(now.covid$now)) {
    betas.covid <- beta.summary(now.covid$now)
    betas.covid.sum <- beta.cumsum(now.covid$now)
    betas.covid.median <- quantile_delay(betas.covid.sum,
                                         prob = seq(0.25, 0.95, 0.05))
    #betas.covid.median$prob<-seq(0.25, 0.95, 0.05)
    write.csv(betas.covid,
              file = paste0(out.path, "betas_covid_", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.covid.sum,
              file = paste0(out.path, "betas_covid_cumsum_", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.covid.median,
              file = paste0(out.path, "betas_covid_median_", data, ".csv"),
              row.names = FALSE)
    }
  # ## SRAG
  if (!is.null(now.srag$now)) {
    betas.srag <- beta.summary(now.srag$now)
    betas.srag.sum <- beta.cumsum(now.srag$now)
    betas.srag.median <- quantile_delay(betas.srag.sum, 
                                        prob = seq(0.25, 0.95, 0.05))
    #betas.srag.median$prob<-seq(0.25, 0.95, 0.05)
    write.csv(betas.srag,
              file = paste0(out.path, "betas_srag_", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.srag.sum,
              file = paste0(out.path, "betas_srag_cumsum_", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.srag.median,
              file = paste0(out.path, "betas_srag_median_", data, ".csv"),
              row.names = FALSE)
    }
  ## Óbitos Covid
  if (!is.null(now.ob.covid$now)) {
    betas.ob.covid <- beta.summary(now.ob.covid$now)
    betas.ob.covid.sum <- beta.cumsum(now.ob.covid$now)
    betas.ob.covid.median <- quantile_delay(betas.ob.covid.sum, 
                                            prob = seq(0.25, 0.95, 0.05))
    #betas.ob.covid.median$prob<-seq(0.25, 0.95, 0.05)
    write.csv(betas.ob.covid,
              file = paste0(out.path, "betas_obitos_covid_", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.ob.covid.sum,
              file = paste0(out.path, "betas_obitos_covid_cumsum", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.ob.covid.median,
              file = paste0(out.path, "betas_obitos_covid_median_", data, ".csv"),
              row.names = FALSE)
    }

  ## Óbitos SRAG
  if (!is.null(now.ob.covid$now)) {
    betas.ob.srag <- beta.summary(now.ob.srag$now)
    betas.ob.srag.sum <- beta.cumsum(now.ob.srag$now)
    betas.ob.srag.median <- quantile_delay(betas.ob.srag.sum, 
                                           prob = seq(0.25, 0.95, 0.05))
    #betas.ob.srag.median$prob<-seq(0.25, 0.95, 0.05)
    write.csv(betas.ob.srag,
              file = paste0(out.path, "betas_obitos_srag_", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.ob.srag,
              file = paste0(out.path, "betas_obitos_srag", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.ob.srag.sum,
              file = paste0(out.path, "betas_obitos_srag_cumsum", data, ".csv"),
              row.names = FALSE)
    write.csv(betas.ob.srag.median,
              file = paste0(out.path, "betas_obitos_srag_median_", data, ".csv"),
              row.names = FALSE)
  }
  }
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

