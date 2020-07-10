#' Preparacao dos dados de nowcasting a partir de objetos ao inves de csvs
#' @details que recebe os objetos gerados pelas funcoes gera.nowcasting e write.notificaoes.data
## Esta funcao é basicamente a parte de processamento dos objetos apos
## leituas dos csvs que esta na funcao prepara.dados.R.
#' @param lista.now lista retornada pela funcao gera.nowcasting
#' @param lista.not lista retornada pela funcao write.notificacoes.data.r com argumento write.arq = FALSE
prepara_dados2 <- function(lista.now,
                          lista.not){ ## a incluir: argumentos trajectories e include.post
    ## Serie completa de n de notificacoes
    n.notificados <- lista.not$n.not
    n.notificados.zoo <- with(n.notificados, zoo(n.notific, as.Date(dt_notific)))

    ## Previsoes de nowcasting e n de casos por data de inicio do sintoma %ast aqui não precisa mudar porque tudo é onset_date
    now.pred.original <- lista.now$now$estimates
    now.pred.zoo.original <- zoo(now.pred.original[,c("estimate", "lower", "upper")],
                                 as.Date(now.pred.original[,"onset_date"]))
    ## N de casos por data de sintoma
    n.sintoma <- lista.not$n.data
    ## verifica se o campo de nome da onset date é de sintoma ou de evolucao
    if ("dt_sin_pri" %in% names(n.sintoma))
        n.sintoma.zoo <- with(n.sintoma, zoo(n.casos, as.Date(dt_sin_pri)))
    # if (tipo %in% proaim)
    #     n.sintoma$n.casos <- n.sintoma$n.notific
    if ("dt_evoluca" %in% names(n.sintoma))
        n.sintoma.zoo <- with(n.sintoma, zoo(n.casos, as.Date(dt_evoluca)))
    ## Junta todos os casos por data de sintoma com previsao do nowcasting (que só tem os ultimos 40 dias)
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
    now.pred.zoo$estimate.merged.smooth <- rollapply(now.pred.zoo$estimate.merged, width = 10, mean, partial = TRUE)
    now.pred.zoo$lower.merged.smooth <- rollapply(now.pred.zoo$lower.merged, width = 10, mean, partial = TRUE)
    now.pred.zoo$upper.merged.smooth <- rollapply(now.pred.zoo$upper.merged, width = 10, mean, partial = TRUE)
    ## n cumulativo
    now.pred.zoo$estimate.merged.c <- cumsum(now.pred.zoo$estimate.merged)
    now.pred.zoo$lower.merged.c <- cumsum(now.pred.zoo$lower.merged)
    now.pred.zoo$upper.merged.c <- cumsum(now.pred.zoo$upper.merged)
    ## Atualiza tb o data.frame
    now.pred <- as.data.frame(now.pred.zoo)
    now.pred$onset_date <- as.Date(rownames(now.pred))
    now.pred <- now.pred[, c(11, 1:10)]
    # lista para salvar os objetos
    pred <- list(now.pred = now.pred,
                 now.pred.zoo = now.pred.zoo,
                 now.pred.original = now.pred.original,
                 now.pred.zoo.original = now.pred.zoo.original
                 )
    ################################################################################
    ## To be done: adaptar esta parte para esta nova funcao
    ## PIP: argumento que permite incluir ou não as posteriores dos parâmetros do nowcasting
    ## if(include.post)
    ##     pred[["now.params.post"]] <- read.csv(paste0(nome.dir, "nowcasting_", tipo, "_post_", data.base, ".csv"))
    
    ## ## Data frame com as trajetórias de projeções do Nowcasting
    ## if (trajectories) {
    ##     pred[["trajectories"]] <- read.csv(paste0(nome.dir, "nowcasting_", tipo, "_traj_", data.base, ".csv"))
    ##     pred$trajectories$date <- as.Date(pred$trajectories$date)
    ## }
    ################################################################################
    return(pred)
}
