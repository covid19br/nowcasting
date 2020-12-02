################################################################################
## Funcao para preparacao dos dados de nowcasting
################################################################################
#' Função para automatizar a preparação dos dados de nowcasting por unidade administrativa
#' @details Retira datas dos sufixos dos nomes das bases e identifica a maior data. Só funciona se os nomes das bases forem mantidos no padrão
#' @param tipo Caractere. Nome da base de dados para preparar. Tipos possíveis: `covid` para casos de COVID-19, `srag` para casos de SRAG, `obitos_covid` para óbitos por COVID-19 e `obitos_srag` para óbitos por SRAG
#' @param trajectories bool carregar trajetórias de projeções do nowcasting
prepara.dados <- function(tipo = "covid",
                          data.base,
                          output.dir = output.dir, # tipos possiveis: covid, srag, obitos_covid e obitos_srag
                          trajectories = FALSE,
                          include.post = TRUE,
                          semanal = FALSE) {
    casos <- c("covid", "srag")
    obitos <- c("obitos_covid", "obitos_srag")
    proaim <- c("proaim_obitos_srag")
    nome.dir <- output.dir
    # if (missing(data.base))
    #     data.base <- get.data.base(escala = escala, sigla = sigla, tipo = tipo)


    ## Importa dados em objetos de séries temporais (zoo)
    ## Serie completa de n de notificacoes
    n.notificados <- read.csv(paste0(nome.dir,"notificacoes_", tipo, "_", data.base,".csv"))

    #rename columns forever to unify the analyses
    # srm: colocando tambem condicao para definir no nome em n.sintoma
    if (tipo %in% casos) {
        nome.sint <- "n_casos_data_sintoma_"
    }
    if (tipo %in% c(obitos, proaim)) {
        nome.sint <- "n_casos_data_"
    }
    n.notificados.zoo <- with(n.notificados, zoo(n.notific, as.Date(dt_notific)))

    ## Previsoes de nowcasting e n de casos por data de inicio do sintoma %ast aqui não precisa mudar porque tudo é onset_date
    now.pred.original <- read.csv(paste0(nome.dir, "nowcasting_", tipo, "_previstos_", data.base, ".csv"))
    now.pred.zoo.original <- zoo(now.pred.original[,c("estimate", "lower", "upper")],
                                 as.Date(now.pred.original[,"onset_date"]))

    ## N de casos por data de sintoma
    n.sintoma <- read.csv(paste0(nome.dir, nome.sint, tipo, "_", data.base, ".csv"))
    # adicionando condicao para
    if (tipo %in% casos)
        n.sintoma.zoo <- with(n.sintoma, zoo(n.casos, as.Date(dt_sin_pri)))
    # if (tipo %in% proaim)
    #     n.sintoma$n.casos <- n.sintoma$n.notific
    if (tipo %in% c(obitos, proaim))
        n.sintoma.zoo <- with(n.sintoma, zoo(n.casos, as.Date(dt_evoluca)))#ast aqui igual, dt_sin_pri é dt_evoluca no caso

    ## Junta todos os casos por data de sintoma com previsao do nowcasting (que só tem os ultimos 40 dias)
    ## VERIFICAR: Total de casos reportado por data do nowcasting tem diferenças com total de casos por data de sintoma tabulado
    now.pred.zoo <- merge(n.casos = n.sintoma.zoo, now.pred.zoo.original)
    ## Retira os dias para os quais há n de casos observados mas nao nowcasting
    now.pred.zoo <- window(now.pred.zoo, start = min(time(n.sintoma.zoo), na.rm = TRUE),
                           end = max(time(now.pred.zoo.original), na.rm = TRUE))
    ## Adiciona variavel de novos casos merged:
    ## junta os valores corrigidos por nowcasting (que em geral vai até um certo ponto no passado)
    ## e n de casos observados antes da data em que começa a correção de nowcasting
    now.pred.zoo$estimate.merged <- with(now.pred.zoo, preenche.now(estimate, n.casos))
    now.pred.zoo$lower.merged <- with(now.pred.zoo, preenche.now(lower, n.casos))
    now.pred.zoo$upper.merged <- with(now.pred.zoo, preenche.now(upper, n.casos))
    ## Media movel da estimativa e dos limites superior e inferiors
    ma.width = if (semanal) 4 else 10
    now.pred.zoo$estimate.merged.smooth <- rollapply(now.pred.zoo$estimate.merged, width = ma.width, mean, partial = TRUE)
    # não calcula média móvel no caso semanal
    if (semanal) now.pred.zoo$estimate.merged.smooth <- now.pred.zoo$estimate.merged.smooth + NA
    now.pred.zoo$lower.merged.smooth <- rollapply(now.pred.zoo$lower.merged, width = ma.width, mean, partial = TRUE)
    now.pred.zoo$upper.merged.smooth <- rollapply(now.pred.zoo$upper.merged, width = ma.width, mean, partial = TRUE)

    ## n cumulativo
    now.pred.zoo$estimate.merged.c <- cumsum(now.pred.zoo$estimate.merged)
    now.pred.zoo$lower.merged.c <- cumsum(now.pred.zoo$lower.merged)
    now.pred.zoo$upper.merged.c <- cumsum(now.pred.zoo$upper.merged)

    ## Atualiza tb o data.frame
    now.pred <- as.data.frame(now.pred.zoo)
    now.pred$onset_date <- as.Date(rownames(now.pred))
    now.pred <- now.pred[, c(11, 1:10)]

    ## Lista com todos os resultados no nowcasting
    ##now.lista <- readRDS(paste0(nome.dir, "nowcasting_", tipo, "_", data.base, ".rds"))

    # lista para salvar os objetos
    pred <- list(now.pred = now.pred,
                 now.pred.zoo = now.pred.zoo,
                 ##now.params.post = now.params.post,
                 now.pred.original = now.pred.original,
                 now.pred.zoo.original = now.pred.zoo.original
                 ##now.lista = now.lista
                 )
    ## PIP: argumento que permite incluir ou não as posteriores dos parâmetros do nowcasting
    if(include.post)
        pred[["now.params.post"]] <- read.csv(paste0(nome.dir, "nowcasting_", tipo, "_post_", data.base, ".csv"))
    
    ## Data frame com as trajetórias de projeções do Nowcasting
    if (trajectories) {
        pred[["trajectories"]] <- read.csv(paste0(nome.dir, "nowcasting_", tipo, "_traj_", data.base, ".csv"))
        pred$trajectories$date <- as.Date(pred$trajectories$date)
    }

    return(pred)
}
