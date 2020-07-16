#' Quantil de atraso, com intervalos de credibilidade
#' @details Recebe o output do nowcasting feito pela função NobBS ou
#'     um dataframe com a distribuição acumulada de atrasos (gerada
#'     pela funão beta.cumsum) e retorna o tempo mediano de atraso,
#'     com limites de 2.5% e 95% do intervalo de credibilidade.
#' @param cum.betas dataframe com CDF dos dias de atraso e seus
#'     intervalos de credibilidade a 95%, retornado pela função
#'     beta.cumsum. Se um dos dois argumentos seguintes deve estar
#'     presente.
#' @param NobBS.output objeto retornado pela função NobBS do pacote de
#'     mesmo nome Este argumento é ignorado se os argumentos cum.betas
#'     ou NobBS.params.post é usado.
#' @param NobBS.params.post data frame com as distribuicoes
#'     posteriores dos parâmetros estimados pela função NobBS. Está
#'     contido na lista que é retornada pela função. Este argumento é
#'     ignorado se os argumento cum.betas é fornecido.
#' @param samples Tamanho da amostra da distribuição posterior de
#'     betas. Necessário se argumento cum.betas é omitido.
#' @return data frame com média e quantis 2.5% e 97.5% do atraso
#'     mediano estimado pelo nowcasting.  Este atraso mediano é obtido
#'     por interpolação das distribuição de probabilidade acumualdas
#'     dos atrasos, estimadas pela função beta.cumsum.
quantile_delay <- function(cum.betas, NobBS.params.post, NobBS.output, samples, prob = 0.5){
    if(missing(cum.betas)){
        if(missing(NobBS.params.post))
            NobBS.params.post <- NobBS.output$params.post
        cum.betas <- beta.cumsum(NobBS.output=NULL, NobBS.params.post = NobBS.params.post, samples = samples)
    }
    result <- sapply(cum.betas[,-1], quantile.cdf, x= cum.betas[,1], prob = prob)
    names(result) <- names(result)[c(1,3,2)] ## quem é lower e upper limit inverte neste calculo, apenas ajeita rotulos.
    return(result)
}


#' Para manter compatibilidade com algum uso antigo da median_delay, agora generalizada pela quantile delay
median_delay <- function(...)
    quantile_delay(..., prob = 0.5)

#' Acessory function: estimates quantile(s) by interporlation of a cumulative probability distribution
#' @param numeric vector of cumulative probabilities
#' @param numeric vector of quantile at each accumulated probability in cdf
#' @param numeric vector of acummalated probablities for which the quantile is to be calculated.
#' @return a vector of quantiles at each value of prob.
quantile.cdf <- function(cdf, x, prob ){
    if(length(cdf)!=length(x))
        stop("cdf and prob should be of the same length")
    f1 <- approxfun(x = cdf, y = x)
    f1(prob)
    }
