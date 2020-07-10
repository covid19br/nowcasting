#' Médias e ICs das probabilidades de notificação cumulativa a cada dia
#' @param NobBS.output objeto retornado pela função NobBS do pacote de
#'     mesmo nome Este argumento é ignorado se o argumento
#'     NobBS.params.post é usado.
#' @param NobBS.params.post data frame com as distribuicoes
#'     posteriores dos parâmetros estimados pela função NobBS. Está
#'     contido na lista que é retornada pela função.
#' @param samples Número de samples a ser considerado.
#' @return data frame com média e quantis 2.5% e 97.5% das
#'     distribuições a posteriori dos parâmetros de atraso de
#'     notificação pelo método de nowcasting da função NobBS. Os
#'     valores estão convertidos para escala de probabilidade são cumulativos, e
#'     portanto podem ser interpretados como a probabilidade cumulativa de casos
#'     ser notificados D dias após o dias o primeiro sintoma, sendo que
#'     vai de zero ao máximo definido pelos argumentos do nowcasting
beta.cumsum <- function(NobBS.output, NobBS.params.post, samples){    
    if(missing(NobBS.params.post))
        df <- NobBS.output$params.post
    else
        df <- NobBS.params.post
    df1 <- df[, names(df)[grepl("Beta",names(df))]]
    if (samples > nrow(df1))
        stop(paste("samples deve ter tamanho menor ou igual a", nrow(df1)))
    else
        df2<-df1[sample(nrow(df1), samples), ]
    df2<-exp(df2)
    df3<-t(apply(df2, 1, cumsum))
    data.frame(atraso = as.integer(substr(colnames(df3), 6, 8)),
                        mean = apply(df3, 2, mean),
               lower = apply(df3, 2, quantile, 0.025),
               upper = apply(df3, 2, quantile, 0.975),
               row.names = names(df3))
}
