if(!require(Hmisc)){install.packages("Hmisc"); library(Hmisc)}
if(!require(plyr)){install.packages("plyr"); library(plyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(rstan)){install.packages("rstan"); library(rstan)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
if(!require(rmarkdown)){install.packages("rmarkdown"); library(rmarkdown)}
if(!require(kableExtra)){install.packages("kableExtra"); library(kableExtra)}
if(!require(knitr)){install.packages("knitr"); library(knitr)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(NobBS)){install.packages("NobBS"); library(NobBS)}
if(!require(brms)){install.packages("brms"); library(brms)}
if(!require(doMC)){install.packages("doMC"); library(doMC)}
if(!require(cowsay)){install.packages("cowsay"); library(cowsay)}
if(!require(aweek)){install.packages("aweek"); library(aweek)}
if(!require(dplyr))    {install.packages("dplyr")    }; library(dplyr)
if(!require(textclean)){install.packages("textclean")}; library(textclean)
if(!require(zoo))      {install.packages("zoo")      }; library(zoo)
if(!require(EpiEstim)) {install.packages("EpiEstim") }; library(EpiEstim)
if(!require(stringr))  {install.packages("stringr")  }; library(stringr)
options(mc.cores = parallel::detectCores())

age_table = structure(list(idade_lower = c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L), 
                           idade_upper = c(9L, 19L, 29L, 39L, 49L, 59L, 69L, 79L, 100L), 
                           ID = c("age_1", "age_2", "age_3", "age_4", "age_5", "age_6", "age_7", "age_8", "age_9"), 
                           faixas = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", 
                                      "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80+")),
                      class = "data.frame", row.names = c(NA, -9L))

classifyAgeFast = function(x){
  laply(x, function(age) paste0("age_", sum(age >= age_table$idade_lower)))
}

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
        df <- nowcasting$params.post
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

#' Médias e ICs das probabilidades de notificação a cada dia
#' @param NobBS.output objeto retornado pela função NobBS do pacote de
#'     mesmo nome Este argumento é ignorado se o argumento
#'     NobBS.params.post é usado.
#' @param NobBS.params.post data frame com as distribuicoes
#'     posteriores dos parâmetros estimados pela função NobBS. Está
#'     contido na lista que é retornada pela função.
#' @return data frame com média e quantis 2.5% e 97.5% das
#'     distribuições a posteriori dos parâmetros de atraso de
#'     notificação pelo método de nowcasting da função NobBS. Os
#'     valores estão convertidos para escala de probabilidade, e
#'     portanto podem ser interpretado como a probabilidade de um caso
#'     ser notificado D dias após o dias o primeiro sintoma, sendo que
#'     vai de zero ao máximo definido pelos argumentos do nowcasting
beta.summary <- function(NobBS.output, NobBS.params.post){
    if(missing(NobBS.params.post))
        df <- NobBS.output$params.post
    else
        df <- NobBS.params.post
    df1 <- df[, names(df)[grepl("Beta",names(df))]]
    data.frame(atraso = as.integer(substr(names(df1), 6, 8)),
               mean = exp(apply(df1, 2, mean)),
               lower = exp(apply(df1, 2, quantile, 0.025)),
               upper = exp(apply(df1, 2, quantile, 0.975)),
               row.names = names(df1))    
}

#' Função para extrair nome do path a partir do geocode
#' @param escala Caractere. Escala do filtro: `"municipio`, `"estado"`, `"micro"`, `"meso"`
#' @param geocode Caractere. Geocode IBGE. Município 6 ou 7 dígitos; microrregião 5 dígitos; mesorregião 4 dígitos; estado 2 dígitos
check.geocode <- function(escala,
                          geocode,
                          sigla,
                          nonascii = TRUE) {

  if (escala == "pais") return(nome <- "/pais/Brasil")
  #url <- paste0("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")
  #df <- jsonlite::fromJSON(url)
  #df$nome.nonascii <- gsub(" ", "_", textclean::replace_non_ascii(df$nome))
  #df$nome.nonascii <- gsub("'", "", df$nome.nonascii)
  #write.csv(df, "./dados/geocode_ibge.csv", row.names = FALSE)
  df <- read.csv("./dados/geocode_ibge.csv")
  #geocode <- as.numeric(geocode)
  municipio.code <- sapply(df$id, function(x) substr(x, start = 1, stop = 6))
  micro.code   <- df$microrregiao.id
  meso.code    <- df$microrregiao.mesorregiao.id
  estado.code  <- df$microrregiao.mesorregiao.UF.id
  estado.sigla <- df$microrregiao.mesorregiao.UF.sigla

    if (escala == "municipio") {
      if (!nchar(geocode) %in% c(6, 7))
        stop("geocode não bate com escala")
      if (nchar(geocode) == 7)
        geocode <- substr(geocode, start = 1, stop = 6)
    if (geocode %in% municipio.code) {
      id <- which(municipio.code == geocode)
      if(nonascii)
        nome <- paste0("municipios/", estado.sigla[id], "/", df$nome.nonascii[id]) # municipios/SP/Sao_Paulo
      else
        nome <- df$nome[id] # municipios/SP/Sao_Paulo
    } else {
      stop("geocode de municipio invalido")
    }
    }

  if (escala == "micro") {
    if (nchar(geocode) != 5)
      stop("geocode não bate com escala")
    if (geocode %in% micro.code) {
      id <- which(micro.code == geocode)[1]
      micro.nome <- gsub("'", "",
                    gsub(" ", "_",
                         replace_non_ascii(df$microrregiao.nome[id])))
      nome <- paste0("microrregioes/", estado.sigla[id], "/", micro.nome) #microrregioes/SP/Sao_Paulo
    } else {
      stop("geocode de microrregião invalido")
    }
  }

  if (escala == "meso") {
    if (nchar(geocode) != 4)
      stop("geocode não bate com escala")
    if (geocode %in% meso.code) {
      id <- which(meso.code == geocode)[1]
      meso.nome <- gsub("'", "",
                    gsub(" ", "_",
                         replace_non_ascii(df$microrregiao.mesorregiao.nome[id])))
      nome <- paste0("mesorregioes/", estado.sigla[id], "/", meso.nome) #mesorregioes/SP/Metropolitana_de_Sao_Paulo
    } else {
      stop("geocode de mesorregião invalido")
    }
  }

  if (escala == "estado") {
    if (nchar(geocode) != 2)
      stop("geocode não bate com escala")
    if (geocode %in% estado.code) {
      id <- which(estado.code == geocode)[1]
      nome <- paste0("estado/", estado.sigla[id])
    } else {
      stop("geocode de estado invalido")
    }
  }

  if (escala == "drs") {
    if(missing(sigla))
      stop("sigla é necessária para escala DRS")
    drs <- read.csv(paste0('./dados/DRS_', sigla, '.csv'))
    if (geocode %in% drs$DRS) {
      id <- which(drs$DRS == geocode)[1]
      if(nonascii)
        nome <- paste0("DRS/", sigla, "/", drs[id, 'DRS.nome.nonascii'])
      else
        nome <- drs[id, 'DRS.nome']
    } else {
      stop("geocode de DRS inválido")
    }
  }

  return(nome)
}

# check.geocode(escala = "municipio", geocode = "3512100")
# check.geocode(escala = "estado", geocode = "21")
# check.geocode(escala = "micro", geocode = "21")
# check.geocode(escala = "micro", geocode = "21008")
# check.geocode(escala = "meso", geocode = "2102")

countByAgeClass = function(df){
  out = data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0)
  colnames(out) = age_table$ID
  counts = table(df$age_class)
  out = out + counts[names(out)]
  out[is.na(out)] = 0
  return(out)
}
#' Conta número de linhas de um arquivo de texto, zipado ou bzipado ou xzeado
#' @param filename Caractere. Caminho do arquivo
count.lines <- function(filename){
    if (endsWith(filename, 'csv'))
        return(as.integer(system2('wc',
                                  args=c('-l', filename, ' | cut -d" " -f1'),
                                  stdout=T)))
    else if (endsWith(filename, 'zip'))
        return(as.integer(system2('unzip', args=c('-c', filename, '| wc -l'),
                                  stdout=T)))
    else if (endsWith(filename, 'bz2'))
        return(as.integer(system2('bzgrep', args=c('-c', '$', filename),
                                  stdout=T)))
    else if (endsWith(filename, 'xz'))
        return(as.integer(system2('xzgrep', args=c('-c', '$', filename),
                                  stdout=T)))
    else {
        print("Tipo de arquivo desconhecido")
        return(0)
    }
}

#' Corta uma série temporal no dia zero.
#' @details Esta função corta um objeto da classe zoo, tirando todos
#'     os valores anteriores ao primeiro valor igual a um certo limite
#'     (n.casos).
#' @param zoo.obj: objeto da classe zoo, com a série temporal
#' @param valor limite para iniciar a série. Todas as datas anteriores
#'     à primeira ocorrência desse valor serão retiradas da série.
diazero <- function(zoo.obj, limite){
    dia.zero <- min(which(zoo.obj>=limite, arr.ind=TRUE))
    zoo.obj[dia.zero:length(zoo.obj)]
}

#' Tempo de duplicação ao longo de uma serie temporal de n de casos
#' @details Toma uma série temporal de n de casos, ajusta a função
#'     fitP.exp a 'running windows' de n de dias fixo ao longo da
#'     série. Retorna um objeto de série temporal com os valores dos
#'     tempos de duplicação em cada janela e seus intervalos de
#'     confiança
#' @param zoo.obj objeto da classe 'zoo' com uma serie temporal
#'     univariada (n de casos)
#' @param window.width largura da janela (em unidades de tempo da
#'     serie temporal, em geral dias)
dt.rw <- function(zoo.obj, window.width){
    if(class(zoo.obj)!="zoo"|!is.null(dim(zoo.obj)))
        stop("'zoo.obj' deve ser um objeto da classe zoo com uma única variável")
    rollapply(zoo.obj,
              width = window.width,
              FUN = function(x) log(2)/fitP.exp(x)[c("coef","coef.low","coef.upp")],
              align="right")
}


#' Estima numero de notificacoes por dia a partir de um vetor de n de
#' casos novos e da distribuição de probabilidades de notificação do
#' nowcasting
#' @param vetor.casos objeto da classe zoo com n de casos
#' @param NobBS.output objeto retornado pela função NobBS do pacote de
#'     mesmo nome. Este argumento é ignorado se o argumento
#'     NobBS.params.post é usado.
#' @param NobBS.params.post data frame com as distribuicoes
#'     posteriores dos parâmetros estimados pela função NobBS. Está
#'     contido na lista que é retornada pela função.
#' @param from posicao do vetor de casos a partir da qual estimar o
#'     numero de notificacões
estima.not <- function(vetor.casos, NobBS.output, NobBS.params.post, from = length(vetor.casos)-30){
    if(missing(NobBS.params.post))
        betas <- beta.summary(NobBS.output)$mean
    else
        betas <- beta.summary(NobBS.params.post = NobBS.params.post)$mean
    i <- length(vetor.casos)-length(betas)
    if(i<0) stop(paste("vetor.casos deve ter comprimento maior ou igual a", length(betas))) 
    else if(i>0)
        y <- vetor.casos[(i+1):length(vetor.casos)]
    else
        y <- vetor.casos
    z <- as.vector(y)
    pred <- rev(cumsum(rev(z*rev(betas))))
    zoo(pred[from:length(z)], time(y)[from:length(z)])
}

###############################################################################
# Estimating R accounting for uncertainty on the serial interval distribution #
###############################################################################
## - the mean of the SI comes from a Normal(4.8, 0.71), truncated at 3.8 and 6.1
## - the sd of the SI comes from  a Normal(2.3, 0.58), truncated at 1.6 and 3.5
## 
##   day0 : day to start the analysis
##   delay : 7 #number of days
estimate.R0 <- function(novos.casos, day0=8, delay=7, ...){
    config <- make_config(list(si_parametric_distr = "L",
                               mean_si = 4.8, std_mean_si = 0.71,
                               min_mean_si = 3.8, max_mean_si = 6.1,
                               std_si = 2.3, std_std_si = 0.58,
                               min_std_si = 1.6, max_std_si = 3.5,
                               t_start = seq(day0,length(novos.casos)-delay),
                               t_end = seq(delay+day0,length(novos.casos))))
    estimate_R(novos.casos, method = "uncertain_si", config = config)
}

## Função para checar se existem dados de nowcasting para a unidade administrativa
existe.nowcasting2 <- function(tipo,
                               data,
                               output.dir) {
  nowcasting.file <- list.files(path = output.dir,
                                pattern = paste0("nowcasting_acumulado_", tipo, "_20"))
  length(nowcasting.file) > 0
}

## Função para checar se existem dados de nowcasting para a unidade administrativa
existe.nowcasting <- function(tipo,
                              data,
                              output.dir) {
  # if (data == "NULL") {
  #   data <- get.data.base(tipo = tipo,
  #                         output.dir = output.dir)
  #   }
  # data_file <- as.Date(data, format = formato.data) %>%
  #   format("%Y_%m_%d")
  nowcasting.file <- list.files(path = output.dir,
                                pattern = paste0("nowcasting_", tipo, ".+", data, ".csv"))
  length(nowcasting.file) > 0
}

fill.dates <- function(data, column){
  
  if(class(column) == "character"){cases = data[,column]}
  if(class(column) == "numeric"){cases = data[,column]}
  
  sequence = seq(min(data$onset), max(data$onset), by = 1)
  incidence = rep(0, length(sequence))
  
  for(i in 1:length(cases)){
    incidence[which(data$onset[i] == sequence)] = cases[i]
  }
  
  return(data.frame(dates = sequence, incidence = incidence))
}

## Using Nowcast and survival analysis to complete covid table with unobserved cases
fillNowcastedLines = function(df, nowcast, hosp_wait_fit, int_wait_fit, 
                              UTI_stay_wait_fit, UTI_after_wait_fit, prob_UTI, ...){
  df.now_casted = dplyr::select(df, dt_sin, dt_int, dt_evo, UTI, dt_entuti, dt_saiuti, age_class)
  nowcasts = data.frame(nowcast$estimates)
  current_date = unique(nowcasts$onset_date)[2]
  dates = as.Date(unique(nowcasts$onset_date))
  createNewInds = function(i, col){
    current_date = dates[i]
    #print(current_date)
    current_now_cast = filter(nowcasts, onset_date == current_date)
    current_now_cast$n.reported = replace_na(current_now_cast$n.reported, 0)
    missing = current_now_cast[[col]] - current_now_cast$n.reported
    missing[missing < 0] = 0
    names(missing) = current_now_cast$stratum
    n_missing = sum(missing, na.rm = TRUE)
    new.df = data.frame(dt_sin    = as.Date(rep("1859-11-24", n_missing)), 
                        dt_int    = as.Date(rep("1859-11-24", n_missing)), 
                        dt_evo    = as.Date(rep("1859-11-24", n_missing)), 
                        UTI       = as.numeric(rep(NA, n_missing)), 
                        dt_entuti = as.Date(rep("1859-11-24", n_missing)), 
                        dt_saiuti = as.Date(rep("1859-11-24", n_missing)),
                        age_class = as.character(rep("OOS", n_missing)), stringsAsFactors = F)
    ll = 1
    if(any(missing >= 1, na.rm = T)){
      to_add = missing[which(missing >= 1)]
      for(current_age in names(to_add)){
        for(i in 1:to_add[current_age]){
          new.df$dt_sin[ll] = current_date
          new.df$dt_int[ll] = as.Date(new.df$dt_sin[ll] + rwaittime(1, int_wait_fit))
          new.df$dt_evo[ll] = as.Date(new.df$dt_int[ll] + rwaittime_age(1, current_age, hosp_wait_fit))
          if(rbernoulli(1, prob_UTI[age_table$ID == current_age])){
            new.df$UTI[ll] = 1
            new.df$dt_entuti[ll] = as.Date(new.df$dt_int[ll] + 1)
            new.df$dt_saiuti[ll] = as.Date(new.df$dt_entuti[ll] + rwaittime(1, UTI_stay_wait_fit))
            new.df$dt_evo[ll] = as.Date(new.df$dt_saiuti[ll] + rwaittime(1, UTI_after_wait_fit) - 1)
          } else{
            new.df$UTI[ll] = 2
            new.df$dt_entuti[ll] = as.Date(NA)
            new.df$dt_saiuti[ll] = as.Date(NA)
          }
          new.df$age_class[ll] = as.character(current_age)
          ll = ll + 1
        }
      }
    }
    return(new.df)
  }
  estimate = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "estimate", ...)) %>% arrange(dt_sin)
  upper    = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "upper", ...)) %>% arrange(dt_sin)
  lower    = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "lower", ...)) %>% arrange(dt_sin)
  return(list(observed = df.now_casted, estimate = estimate, upper = upper, lower = lower))
}

# df = covid.dt
# nowcast = covid.now.day 
# hosp_wait_fit = time_fits1$covid$notUTI
# int_wait_fit = time_fits0$covid$Int
# UTI_stay_wait_fit = time_fits0$covid$UTI
# UTI_after_wait_fit = time_fits0$covid$afterUTI
# prob_UTI = probsFits$covid$uti[,"Estimate"]

## Using Nowcast and survival analysis to complete covid table with unobserved cases
fillNowcastedLinesFast = function(df, nowcast, hosp_wait_fit, int_wait_fit, 
                              UTI_stay_wait_fit, UTI_after_wait_fit, prob_UTI, ...){
  df.now_casted = dplyr::select(df, dt_sin, dt_int, dt_evo, UTI, dt_entuti, dt_saiuti, age_class)
  nowcasts = data.frame(nowcast$estimates)
  current_date = unique(nowcasts$onset_date)[2]
  dates = as.Date(unique(nowcasts$onset_date))
  createNewInds = function(i, col){
    current_date = dates[i]
    #print(current_date)
    current_now_cast = filter(nowcasts, onset_date == current_date)
    current_now_cast$n.reported = replace_na(current_now_cast$n.reported, 0)
    missing = current_now_cast[[col]] - current_now_cast$n.reported
    missing[missing < 0] = 0
    names(missing) = current_now_cast$stratum
    n_missing = sum(missing, na.rm = TRUE)
    new.df = data.frame(dt_sin    = as.Date(rep("1859-11-24", n_missing)), 
                        dt_int    = as.Date(rep("1859-11-24", n_missing)), 
                        dt_evo    = as.Date(rep("1859-11-24", n_missing)), 
                        UTI       = as.numeric(rep(NA, n_missing)), 
                        dt_entuti = as.Date(rep("1859-11-24", n_missing)), 
                        dt_saiuti = as.Date(rep("1859-11-24", n_missing)),
                        age_class = as.character(rep("OOS", n_missing)), stringsAsFactors = F)
    ll = 1
    if(any(missing >= 1, na.rm = T)){
      to_add = missing[which(missing >= 1)]
      for(current_age in names(to_add)){
        current_lines = ll:(ll+to_add[current_age]-1)
        new.df$dt_sin[current_lines] = current_date
        new.df$dt_int[current_lines] = as.Date(new.df$dt_sin[ll] + rwaittime(to_add[current_age], int_wait_fit))
        new.df$dt_evo[current_lines] = as.Date(new.df$dt_int[current_lines] + rwaittime_age(to_add[current_age], current_age, hosp_wait_fit))
        new.df$UTI[current_lines] = as.numeric(rbernoulli(to_add[current_age], prob_UTI[age_table$ID == current_age]))
        new.df$dt_entuti[current_lines] = if_else(as.logical(new.df$UTI[current_lines]), 
                                                  as.Date(new.df$dt_int[current_lines] + 1), 
                                                  NA_Date_)
        new.df$dt_saiuti[current_lines] = if_else(as.logical(new.df$UTI[current_lines]), 
                                                  as.Date(new.df$dt_entuti[current_lines] + 
                                                            rwaittime(to_add[current_age], UTI_stay_wait_fit)), 
                                                  NA_Date_)
        new.df$dt_evo[current_lines] = if_else(as.logical(new.df$UTI[current_lines]), 
                                               as.Date(new.df$dt_saiuti[current_lines] + rwaittime(to_add[current_age], UTI_after_wait_fit) - 1), 
                                               new.df$dt_evo[current_lines])
        new.df$UTI[current_lines] = if_else(as.logical(new.df$UTI[current_lines]), 1, 2)
        new.df$age_class[current_lines] = as.character(current_age)
        ll = ll + to_add[current_age]
      }
    }
    return(new.df)
  }
  estimate = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "estimate", ...)) %>% arrange(dt_sin)
  upper    = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "upper", ...)) %>% arrange(dt_sin)
  lower    = rbind(df.now_casted, ldply(seq_along(dates), createNewInds, "lower", ...)) %>% arrange(dt_sin)
  return(list(observed = df.now_casted, estimate = estimate, upper = upper, lower = lower))
}

#' Ajusta um modelo exponencial para n de casos em função de n de dias
#' passados desde o início da série.
#' @details Ajusta um modelo generalizado misto para contagens
#'     (Poisson) dos valores de uma série temporal em função do número
#'     de dias transcorridos desde o início da série. Como o modelo
#'     Poisson tem função de ligação log, equivale a ajustar uma
#'     função de crescimento exponencial às contagens, mas com erro
#'     não gaussianos e sim Poisson.
#' @param zoo.obj objeto da classe zoo com série temporal univariada
#' @param family nome da distribuição de erros a usar no modelo linear
#'     (ver ajuda em stats::family)
#' @param only.coef se TRUE a função retorna um vetor coms
#'     coeficientes da regressão e seus intervalos de confiança. Se
#'     FALSE retorna o objeto do modelo ajustado, da classe glm.
fitP.exp <- function(zoo.obj, only.coef = TRUE){
    ## Nao funciona com rollaply
    ## if(class(zoo.obj)!="zoo"|!is.null(ncol(zoo.obj)))
    ##    stop("'zoo.obj' deve ser um objeto da classe zoo com uma única variável")
    ndias <- as.vector ( rev(max(time(zoo.obj)) - time(zoo.obj)) )
    fit <- try(glm(zoo.obj ~ ndias, family = poisson))
    if(only.coef){
            ci <- try(confint(fit))
            if(any(class(fit)=="try-error")||any(class(ci)=="try-error"))
                results <- rep(NA,6)
            else
                results <- c(coef(fit),ci[1,], ci[2,])
            names(results) <- c("intercept", "coef", "int.low", "int.upp", "coef.low", "coef.upp")
    }
    if(!only.coef){
        if(class(fit)=="try-error")
            results <- NA
        else
            results  <-  fit
    }
    return(results)
}

fixUTIDates = function(x, UTI_stay_wait_fit){
  if(!is.na(x$UTI) & x$UTI==1 & is.na(x$dt_saiuti) & !is.na(x$evolucao))
    if(is.na(x$dt_evo)){
      x$dt_saiuti = as.Date(x$dt_entuti + rwaittime(1, UTI_stay_wait_fit))
    } 
  x
}
fixEVODates = function(x, afterUTI_stay_wait_fit, hosp_wait_fit){
  if(is.na(x$dt_evo) & !is.na(x$evolucao)){
    if(!is.na(x$UTI) & x$UTI==1){
      x$dt_evo = as.Date(x$dt_saiuti + rwaittime(1, afterUTI_stay_wait_fit))
    } else{
      x$dt_evo = as.Date(x$dt_int + rwaittime(1, hosp_wait_fit))
    }
  }
  x
}
#' Forecast usando regressão Poisson sobre série dos casos acumulados
forecast.exponential <- function(zoo.obj, start, end = length(zoo.obj), days.forecast, ...){
    if(class(zoo.obj)!="zoo"|!is.null(dim(zoo.obj)))
        stop("'zoo.obj' deve ser um objeto da classe zoo com uma única variável")
    if(is.numeric(start))
        inicio <- time(zoo.obj)[start]
    else
        inicio <- start
    if(is.numeric(end))
        fim <- time(zoo.obj)[end]
    else
        fim <- end
    y <- window(zoo.obj, start = inicio, end = fim)
    if(!is.integer(y)) {
        warning("Resposta não está em inteiros, convertendo para inteiro para ajustar glm Poisson")
        y <- as.integer(y)
    }
    fit <- fitP.exp(y, only.coef = FALSE)
    datas.forecast <- fim + (1:days.forecast)
    newdata <- data.frame( ndias = as.vector( datas.forecast - inicio ) )
    pred <- predict(fit, newdata = newdata, se.fit = TRUE)
    df1 <- data.frame(lpred = pred$fit, lse = pred$se.fit)
    df1$predito <- exp(df1$lpred)
    df1$ic.low <-  with(df1, exp(lpred - 2*lse))
    df1$ic.upp <-  with(df1, exp(lpred + 2*lse))
    zoo(df1[,c("predito","ic.low","ic.upp")], datas.forecast)
}

## Funcao para formatar data.frame para o grafico de nowcasting casos diarios e acumulados
formata.now.df <- function(now.pred.zoo, 
                           now.proj.zoo,
                           lista) { # aceita "caso" para casos diarios ou "cum" para acumulados
    # Helper function
    end.time <- function(pred.zoo, pred.zoo.original){
        if (min(time(pred.zoo.original)) < min(time(pred.zoo))) {
            end.time <- min(time(pred.zoo))
        } else {
            end.time <- min(time(pred.zoo.original))
        }
        return(end.time)
    }
    ## PARA O PLOT CASOS DIARIOS ####
    end.time.now <- end.time(now.pred.zoo, lista$now.pred.zoo.original)
    time.now <- time(now.pred.zoo)
    df.zoo <- cbind(as.data.frame(now.pred.zoo), data = as.character(time.now))
    # notificados
    df.not <- as.data.frame(window(now.pred.zoo, end = end.time.now))
    df.not$tipo <- "Notificado"
    df.not$data <- row.names(df.not)
    # nowcasting
    df.now <- as.data.frame(window(now.pred.zoo, start = min(time(lista$now.pred.zoo.original)) + 1))
    df.now$tipo <- "Nowcasting"
    df.now$data <- row.names(df.now)
    # predict
    df.pred <- as.data.frame(window(now.pred.zoo, start = min(time(lista$now.pred.zoo.original)) + 1))
    names(df.pred) <- paste0(names(df.pred), ".pred")
    df.pred$data <- row.names(df.pred)
    ## finalmente gera o df diario
    df.diario <- full_join(df.not[, c('data', 'n.casos')], df.now[, c('data', 'estimate')]) %>%
        full_join(., df.zoo[, c('data', 'estimate.merged', 'estimate.merged.smooth')]) %>%
        full_join(., df.pred[, c('data', 'lower.merged.pred', 'upper.merged.pred')]) %>%
        mutate(data = as.Date(data))
    # PARA O PLOT CASOS ACUMULADOS
    select.cols <- c("data", 
                     'now.mean.c', 
                     'now.mean.c.proj', 'now.low.c.proj', 'now.upp.c.proj',
                     'not.mean.c', 
                     'not.mean.c.proj', 'not.low.c.proj', 'not.upp.c.proj')
    # estimados
    df.cum1 <- as.data.frame(window(now.proj.zoo, end = max(time(now.pred.zoo))))
    df.cum1$data <- row.names(df.cum1)
    # notificados
    df.cum2 <- as.data.frame(window(now.proj.zoo, start = max(time(now.pred.zoo))))
    names(df.cum2) <- paste0(names(df.cum2), ".proj")
    df.cum2$data <- row.names(df.cum2)
    # gera o df para casos acumulados
    df.cum <- full_join(df.cum1, 
                        df.cum2) %>%
        select(select.cols) %>%
        mutate(data = as.Date(data)) %>%
        mutate_all(funs(round(., 0)))
    lista.plot <- list(diario = df.diario, acumulado = df.cum)
    return(lista.plot)
}

gera.nowcasting <- function(dados, # dados
                            caso = TRUE, # caso = FALSE faz obitos
                            tipo, # covid ou srag
                            hospitalizados = TRUE,
                            trim.now, # corte para nowcasting
                            window, # janela para nowcasting
                            trajectories = FALSE) { # retorna trajetórias
  if(trajectories)
      NobBS  <- NobBS.posterior
  # 1. nowcasting de casos ###
  if (caso) {
      if (hospitalizados)
        dados <- dados %>% filter(hospital == 1)
    ## 1.1 casos covid ####
    if (tipo == "covid") {
      ##COVID##
      dados2 <- dados %>%
        filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
        select(dt_notific, dt_sin_pri, dt_pcr, dt_digita) %>%
        mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE))
    }
    ## 1.2. casos srag ####
    if (tipo == "srag") {
      ## %PIP data de registro é data mais recente entre notificação e digitação, não deve incluir data pcr (dt_pcr)
      ## pq SRAG não precisa de teste para ser confirmado
      dados2 <- dados %>%
        select(dt_notific, dt_sin_pri, dt_digita) %>%
        mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) # nome aqui é pcr mas não tem pcr
    }

    if (nrow(dados2) != 0) {
      dados.now <- NobBS(
        data = dados2,
        now = max(dados2$dt_sin_pri, na.rm = TRUE) - trim.now,
        onset_date = "dt_sin_pri",
        report_date = "dt_pcr_dig",
        units = "1 day",
        moving_window = window)
    } else {
     dados.now <- NULL
    }

    # 2. nowcasting de obitos ####
  } else {
    ## 2.1. obitos covid ####
    if (tipo == "covid") {
      ##obitos COVID ####
      dados2 <- dados %>%
        filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
        filter(evolucao == 2) %>%
        filter(!is.na(dt_evoluca)) %>%
        mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                 na.rm = TRUE)) %>%
        select(dt_evoluca, dt_notific, dt_encerra)
    }
    ## 2.2. obitos srag ####
    if (tipo == "srag") {
      dados2 <- dados %>%
        filter(evolucao == 2) %>%
        filter(!is.na(dt_evoluca)) %>%
        mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                 na.rm = TRUE)) %>%
        select(dt_evoluca, dt_notific, dt_encerra)
    }
    if (nrow(dados2) != 0) {
      dados.now <- NobBS(
        data = dados2,
        now = max(dados2$dt_evoluca) - trim.now, ##PIP: nocwasting vai até última data do evento, no caso data do obito
        onset_date = "dt_evoluca",
        report_date = "dt_encerra",
        units = "1 day",
        moving_window = window,
        specs = list(beta.priors = dbinom(0:40, size = 40, p = 15/50)))
    } else {
      dados.now <- NULL
    }
  }
  out <- list(now = dados.now, dados = dados2)
  return(out)
}

getCurrentInBed = function(df, date, UTI){
  if(!UTI){
    df = df %>% 
      filter(dt_int <= date & (dt_evo >= date | is.na(dt_evo)))
  }else{
    df = df %>% 
      filter(UTI == 1, dt_entuti <= date & (dt_saiuti >= date | is.na(dt_saiuti)))
  }
}
# extrai a data mais recente de nowcasting
get.data.base2 <- function(adm, sigla.adm, tipo) {
  nome.dir <- paste0("../dados/", adm, "_", sigla.adm, "/tabelas_nowcasting_para_grafico/")
  data.base <- dir(nome.dir, pattern = paste0("nowcasting_acumulado_", tipo, "_20")) %>% 
    stringr::str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>% #prfct
    as.Date(format = "%Y_%m_%d") %>%
    max() %>%
    format("%Y_%m_%d")
}

# extrai a data mais recente de nowcasting
get.data.base <- function(tipo,
                          output.dir) {
  file <-  dir(output.dir, pattern = paste0("nowcasting", ".+", tipo,".+", "_20"))
  if (length(file) > 0) {
  data.base <- file %>%
    stringr::str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>% #prfct
    as.Date(format = "%Y_%m_%d") %>%
    max() %>%
    format("%Y_%m_%d")
  }
}

getProbUTI = function(df){
  df.UTI = filter(df, !is.na(UTI) & UTI!=9)
  UTI_table = as.matrix(table(df.UTI$age_class, df.UTI$UTI))
  UTI_data = data.frame(UTIadmissions = UTI_table[,1], trials = rowSums(UTI_table), age_class = age_table$ID)
  
  UTI_prob_model = brm(data = UTI_data, family = binomial,
                       UTIadmissions | trials(trials) ~ 1 + (1|age_class),
                       c(prior("normal(0, 1)", class = "Intercept"),
                         prior("normal(0, 1)", class = "sd")),
                       control = list(adapt_delta = 0.99))
  
  out = coef(UTI_prob_model) %>%
    {inv_logit_scaled(.$age_class)}
  data.frame(out[,,"Intercept"])
}

# probabilidade de morte de hospitalizado comum, e em UTI, 
getProbDeath = function(df, UTI = FALSE){
  if(UTI){
    df.filtered = filter(df, UTI==1, !is.na(evolucao) & evolucao!=9)
  } else{
    df.filtered = filter(df, UTI!=1, !is.na(evolucao) & evolucao!=9)
  }
  case_table = as.matrix(table(df.filtered$age_class, df.filtered$evolucao))
  
  trial_data = data.frame(deaths = 0, trials = 0, age_class = age_table$ID)
  trial_data[match(rownames(case_table), age_table$ID),1] = case_table[,2]
  trial_data[match(rownames(case_table), age_table$ID),2] = rowSums(case_table)
  trial_data$trials[trial_data$trials == 0] = 1 # Adds one trial if none exist
  death_prob_model = brm(data = trial_data, family = binomial,
                         deaths | trials(trials) ~ 1 + (1|age_class),
                         c(prior("normal(0, 1)", class = "Intercept"),
                           prior("normal(0, 1)", class = "sd")),
                         control = list(adapt_delta = 0.99))
  out = coef(death_prob_model) %>%
    {inv_logit_scaled(.$age_class)}
  data.frame(out[,,"Intercept"])
}
get.last.date <- function(dir) {
  data.base <- list.files(dir) %>%
    # regex para catar data
    stringr::str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
    as.Date(format = "%Y_%m_%d") %>%
    max(na.rm = TRUE) %>%
    format("%Y_%m_%d")
}

getTimes = function(x, late, early, censored = FALSE){  
  if(!censored){
    time = as.numeric(x[[late]] - x[[early]])
    data.frame(ID = x$ID, time = time, age_class = x$age_class, 
               early =  x[[early]], late = x[[late]])
  } else{
    if(is.na(x[[late]])){ 
      time = as.numeric(today() - x[[early]])
      censored = 0
    } else{
      time = as.numeric(x[[late]] -x[[early]])
      censored = 1
    } 
    data.frame(ID = x$ID, evolucao = x$evolucao, time = time, age_class = x$age_class, censored = censored,
               early =  x[[early]], late = x[[late]])
  }
}
#' Inverso da funcao logito
inv.logit <- function(x)
    exp(x)/(1+exp(x))

#### Creates a data.frame where the notified number of daily cases is replaced by the nowcasting series
#### in their corresponding dates.
#### nowcasting: a data.frame containing multiple nowcasting series
#### notified: an incidence table with dates and number of cases per day
join_nowcasting <- function(nowcasting, notified){
  prev.dates <- notified$onset[!(notified$onset %in% nowcasting$date)]
  prev.cases = notified$n.casos[!(notified$onset %in% nowcasting$date)]
  prev.cases.table <- data.frame(matrix(rep(prev.cases,each = ncol(nowcasting)-1), ncol = ncol(nowcasting)-1, byrow=TRUE))
  previous <- cbind(date = prev.dates, prev.cases.table)
  all <- rbind(nowcasting, previous)
  all <- all %>% arrange(date)
  return(all)
}

makeHospitalTable = function(df.table, dates, UTI = FALSE){
  hospital_table = ldply(dates, 
                         function (date) cbind(data.frame(date = date), 
                                               countByAgeClass(getCurrentInBed(df.table, date, UTI = UTI))))
  hospital_table[is.na(hospital_table)] = 0
  hospital_table
}

makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}

#' substitui NAS por zeros
na.zero <- function(x)
    ifelse(is.na(x), 0, x)

if(!require(coda)){install.packages("coda"); library(coda)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}
if(!require(rjags)){install.packages("rjags"); library(rjags)}

NobBS.posterior <- function(data, now, units, onset_date, report_date, moving_window=NULL, max_D=NULL, cutoff_D=NULL, proportion_reported=1, quiet=TRUE,
                  specs=list(
                    dist=c("Poisson","NB"),
                    alpha1.mean.prior=0,
                    alpha1.prec.prior=0.001,
                    alphat.shape.prior=0.001,
                    alphat.rate.prior=0.001,
                    beta.priors=NULL,
                    param_names=NULL,
                    conf=0.95,
                    dispersion.prior=NULL,
                    nAdapt=1000,
                    nChains=1,
                    nBurnin=1000,
                    nThin=1,
                    nSamp=10000)) {
  
  # Check that "now" is entered as a Date
  if(inherits(now, "Date")==FALSE){
    stop("'Now' argument must be of datatype Date (as.Date)")
  }
  
  # Check that "now" is possible in the sequence of reporting data
  if(dplyr::last(seq(unique(data[,onset_date])[1],now,by=units))!=now){
    stop("The date `now` is not possible to estimate: the possible nowcast dates are seq(unique(data[,onset_date])[1],now,by=units).")
  }
  
  # Print date
  message(paste("Computing a nowcast for ",now))
  # Define "T", the length of dates between the first date of data and "now", making sure that "T" is unaffected by skipped-over dates in the time series
  # If the moving window is specified, "T" considers only the dates within the moving window; otherwise considers all historical data
  now.T <- ifelse(is.null(moving_window),length(seq(min(data[,onset_date]),as.Date(now),by=units)),
                  moving_window)
  
  # Check the default arguments
  if (is.null(moving_window)) {
    moving_window <- now.T
  }
  if (is.null(max_D)) {
    max_D <- now.T-1 # ifelse(is.null(moving_window),now.T-1,moving_window-1)
  }
  if (is.null(cutoff_D)) {
    cutoff_D <- TRUE
  }
  if(quiet==TRUE){
    progress.bar <- "none"
  }
  if(quiet==FALSE){
    progress.bar <- "text"
  }
  
  # Check that proportion_reported is between 0,1
  if (proportion_reported > 1 | proportion_reported<=0){
    stop("The proportion_reported must be a number between (0,1].")
  }
  
  # Manipulate the control arguments
  if ("Poisson"%in%(specs[["dist",exact=TRUE]])) { # if no distribution specified, take Poisson as default
    specs$dist <- "Poisson"
  }
  if (is.null(specs[["dist",exact=TRUE]])) {
    specs$dist <- "Poisson"
  }
  if (is.null(specs[["alpha1.mean.prior",exact=TRUE]])) {
    specs$alpha1.mean.prior <- 0
  }
  if (is.null(specs[["alpha1.prec.prior",exact=TRUE]])) {
    specs$alpha1.prec.prior <- 0.001
  }
  if (is.null(specs[["alphat.shape.prior",exact=TRUE]])) {
    specs$alphat.shape.prior <- 0.001
  }
  if (is.null(specs[["alphat.rate.prior",exact=TRUE]])) {
    specs$alphat.rate.prior <- 0.001
  }
  if (is.null(specs[["beta.priors",exact=TRUE]])) {
    specs$beta.priors <- rep(0.1, times=(max_D)+1)
  }
  if (is.null(specs[["param_names",exact=TRUE]])&(specs[["dist"]]=="Poisson")) {
    specs$param_names <- c( "lambda","alpha","beta.logged","tau2.alpha","sum.n")
  }
  if (is.null(specs[["param_names",exact=TRUE]])&(specs[["dist"]]=="NB")) {
    specs$param_names <- c( "lambda","alpha","beta.logged","tau2.alpha","sum.n","r")
  }
  if (is.null(specs[["conf",exact=TRUE]])) {
    specs$conf <- 0.95
  }
  if (is.null(specs[["dispersion.prior",exact=TRUE]])&(specs[["dist"]]=="NB")) {
    specs$dispersion.prior <- c(0.001,0.001)
  }
  if (is.null(specs[["nAdapt",exact=TRUE]])) {
    specs$nAdapt <- 1000
  }
  if (is.null(specs[["nChains",exact=TRUE]])) {
    specs$nChains <- 1
  }
  if (is.null(specs[["nBurnin",exact=TRUE]])) {
    specs$nBurnin <- 1000
  }
  if (is.null(specs[["nThin",exact=TRUE]])) {
    specs$nThin <- 1
  }
  if (is.null(specs[["nSamp",exact=TRUE]])) {
    specs$nSamp <- 10000
  }
  
  # Warnings
  if(max_D>(moving_window-1)){
    stop("Maximum delay cannot be greater than the length of the moving window minus 1 time unit")
  }
  
  # Prep the data: filter only to observable cases reported at or before "now"
  unit.num <- switch(units, "1 day"=1,"1 week"=7)
  w.days <- max((moving_window-1)*unit.num,(now.T-1)*unit.num) # moving window converted to days
  
  realtime.data <- subset(data,(data[,onset_date]<=now) & (data[,onset_date]>=now-w.days) & (data[,report_date]<=now) & (data[,report_date]>=now-w.days))
  realtime.data$week.t <- (as.numeric(realtime.data[,onset_date]-min(realtime.data[,onset_date]))/unit.num)+1
  realtime.data$delay <- as.numeric(realtime.data[,report_date]-realtime.data[,onset_date])/unit.num
  
  if(cutoff_D==FALSE){
    realtime.data$delay <- ifelse(realtime.data$delay>=max_D,max_D,realtime.data$delay)
  }
  
  if(length(unique(realtime.data$week.t))!=now.T){
    warning("Warning! The line list has zero case reports for one or more possible onset dates at one or more delays. Proceeding under the assumption that the true number of cases at the associated delay(s) and week(s) is zero.")
  }
  
  # Build the reporting triangle, fill with NAs where unobservable
  reporting.triangle <- matrix(NA, nrow=now.T,ncol=(max_D+1))
  
  for(t in 1:now.T){
    for(d in 0:max_D){
      reporting.triangle[t,(d+1)] <- nrow(realtime.data[which(realtime.data$week.t==t & realtime.data$delay==d),])
      if(now.T < (t+d)){
        reporting.triangle[t,(d+1)] <- NA
      }
    }
  }
  
  # Run the JAGS model
  
  if(specs[["dist"]]=="Poisson"){
    params=c( "lambda","alpha","beta.logged","tau2.alpha","n","sum.n","sum.lambda")
  }
  if(specs[["dist"]]=="NB"){
    params=c( "lambda","alpha","beta.logged","tau2.alpha","n","sum.n","sum.lambda","r")
  }
  nAdapt = specs[["nAdapt"]] #default = 1000
  nChains = specs[["nChains"]] # default=1
  nBurnin = specs[["nBurnin"]] # default=1000
  nThin = specs[["nThin"]] # default=1
  nKeep = specs[["nSamp"]] # default=10,000
  nIter = nKeep * nThin
  
  if(specs[["dist"]]=="Poisson"){
    dataList = list(Today = now.T,
                    D = max_D,
                    n = reporting.triangle,
                    alpha1.mean.prior=specs$alpha1.mean.prior,
                    alpha1.prec.prior=specs$alpha1.prec.prior,
                    alphat.rate.prior=specs$alphat.rate.prior,
                    alphat.shape.prior=specs$alphat.shape.prior,
                    beta.priors=specs$beta.priors)
  }
  
  if(specs[["dist"]]=="NB"){
    dataList = list(Today = now.T,
                    D = max_D,
                    n = reporting.triangle,
                    alpha1.mean.prior=specs$alpha1.mean.prior,
                    alpha1.prec.prior=specs$alpha1.prec.prior,
                    alphat.rate.prior=specs$alphat.rate.prior,
                    alphat.shape.prior=specs$alphat.shape.prior,
                    beta.priors=specs$beta.priors,
                    dispersion.prior.shape=specs$dispersion.prior[1],
                    dispersion.prior.rate=specs$dispersion.prior[2])
  }
  
  JAGSmodPois <- (system.file("JAGS", "nowcastPois.txt", package="NobBS")) # file.path(path.package('NobBS'),"nowcastPois.txt")
  JAGSmodNB <- (system.file("JAGS", "nowcastNB.txt", package="NobBS")) #file.path(path.package('NobBS'),"nowcastNB.txt")
  
  nowcastmodel = rjags::jags.model(
    file = ifelse(specs[["dist"]]=="Poisson",JAGSmodPois,JAGSmodNB),
    data = dataList,
    n.chains = nChains,
    n.adapt = nAdapt,
    inits=list(.RNG.seed=1,.RNG.name="base::Super-Duper"),
    quiet=quiet)
  
  update( object = nowcastmodel, n.iter = nBurnin , progress.bar = progress.bar)
  
  lambda.output = coda.samples(
    model = nowcastmodel,
    variable.names =  if("sum.n"%in%specs$param_names) c(specs$param_names) else c(specs$param_names,"sum.n"),
    n.iter = nIter,
    thin = nThin,
    quiet=quiet,
    progress.bar=progress.bar)
  
  mymod.mcmc <- as.mcmc(lambda.output)
  mymod.dat <- as.data.frame(as.matrix(mymod.mcmc))
  
  # Extract all hindcasts and 95% credible intervals
  t.extract <- (now.T-(now.T-1)):(now.T) # nowcast all weeks up through the present
  
  estimates <- matrix(NA, ncol=3, nrow=now.T,dimnames=list(NULL,c("estimate","lower","upper")))
  for(v in t.extract){
    estimates[v,1] <- median(mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)])
    estimates[v,2] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[1]
    estimates[v,3] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[2]
  }
  
  # Estimates inflated by proportion reported
  estimates.inflated <- matrix(NA, ncol=3, nrow=now.T,dimnames=list(NULL,c("estimate_inflated","lower","upper")))
  for(v in t.extract){
    estimates.inflated[v,1] <- median(mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)])/proportion_reported
    estimates.inflated[v,2] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[1]/proportion_reported
    estimates.inflated[v,3] <- quantile((mymod.dat[, grep(paste("sum.n[",v,"]",sep=""), colnames(mymod.dat), fixed=TRUE)]),probs = c((1-specs$conf)/2,1-((1-specs$conf)/2)))[2]/proportion_reported
  }
  
  # Combine nowcast estimates with: dates, number of cases reported at each date
  reported <- data.frame(
    realtime.data %>%
      group_by_(onset_date) %>%
      summarise(n.reported=n())
  )
  names(reported)[1] <- "onset_date"
  
  # # # # #
  estimates <- data.frame(estimates, onset_date=(seq(as.Date(now)-w.days,as.Date(now),by=units))) %>%
    left_join(reported,by="onset_date")
  
  estimates.inflated <- data.frame(estimates.inflated, onset_date=(seq(as.Date(now)-w.days,as.Date(now),by=units))) %>%
    left_join(reported,by="onset_date")
  
  t <- now.T
  
  parameter_extract <- matrix(NA, nrow=10000)
  
  if("lambda"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("lambda[",t,",",sep="")))))
  }
  if("beta.logged"%in%specs$param_names){
    betas.logged<- matrix(NA,nrow=10000,ncol=(max_D+1))
    dimnames(betas.logged) = list(NULL,c(paste("Beta",c(0:max_D))))
    for(d in 0:max_D){
      betas.logged[,(d+1)] <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[",(d+1),"]",sep="")))))[,1]
    }
    parameter_extract <- cbind(parameter_extract,betas.logged)
  }
  if("alpha"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("alpha[",t,sep="")))))
  }
  if("tau2.alpha"%in%specs$param_names){
    parameter_extract <- cbind(parameter_extract,mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("tau2.alpha"))))
  }
  
  #log.beta.td1 <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[1]",sep=""))))
  #log.beta.td2 <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[2]",sep=""))))
  #log.beta.td3 <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("beta.logged[3]",sep=""))))
  #alpha.last <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with(paste("alpha[",t,sep=""))))
  #tau2.alpha <- mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("tau2.alpha")))
  
  #parameter_extract <- cbind(pi.logged.td1,pi.logged.td2,pi.logged.td3,
  #                          alpha.last,tau2.alpha)
  
  nowcast.post.samps <- (mymod.dat %>% dplyr::select(select_vars(names(mymod.dat),starts_with("sum.n"))))
  colnames(nowcast.post.samps) = 1:moving_window
  m_post = melt(nowcast.post.samps)
  m_post$sample = 1:nKeep
  trajetoria = pivot_wider(m_post, names_from = sample, values_from = value) %>%
    dplyr::mutate(variable = as.Date(as.numeric(variable), origin = now-moving_window)) %>%
    dplyr::rename(date = variable)

  list(estimates=estimates,estimates.inflated=estimates.inflated, nowcast.post.samps=nowcast.post.samps,params.post=parameter_extract[,2:ncol(parameter_extract)],trajectories=trajetoria)
}

#now.Date.covid  <-  max(covid.dt$dt_sin)
#covid.nowcast.trajectory <- NobBS.posterior(
  #data = covid.dt,
  #now = now.Date.covid,
  #onset_date = "dt_sin",
  #report_date = "dt_rec",
  #units = "1 day",
  #moving_window =  window,
  #specs = list(nAdapt = 6000, nBurnin = 6000, nThin = 100, nSamp = 100)
#)

################################################################################
## Funcao para projecao do n acumulado de casos por data do sintoma
################################################################################
#' Funcao para projecao do n acumulado de casos por data do sintoma
#' @param pred Data frame. Objeto `now.pred.zoo` gerado em prepara_nowcasting.R
#' @param pred.original Data frame. Objeto `now.pred.original` gerado em prepara_nowcasting.R
#' @param now.lista Lista. Objeto `now.lista` gerado em prepara_dados_nowcasting.R
#' @param now.params.post Data frame. Objeto `now.params.post` gerado em gerado em prepara_dados_nowcasting.R
now.proj <- function(pred,
                     pred.original,
                     now.params.post,
                     n.dias = 5,
                     data) {
    data0 <- as.Date(data, "%Y_%m_%d")
    ## N de dias para projetar: 5 dias a partir da data atual
    ## Adiciona ao forecast dias entre a ultima data de nocasting e o dia atual
    days.to.forecast <- as.integer(data0 - max(time(pred)) + n.dias)
    ## Objeto zoo com n de casos previstos pelo nowcasting concatenados com o n de casos
    ## projetado a partir do nowcasting acumulado com regressão Poisson
    now.proj.zoo <- merge(
        now.mean.c = c(forecast.exponential(pred$estimate.merged.c,
                                            start = length(time(pred)) - 4,
                                            days.forecast = days.to.forecast)$predito,
                       pred$estimate.merged.c),

        now.low.c = c(forecast.exponential(pred$lower.merged.c,
                                           start = length(time(pred)) - 4,
                                           days.forecast = days.to.forecast)$predito,
                      pred$lower.merged.c),

        now.upp.c = c(forecast.exponential(pred$upper.merged.c,
                                           start = length(time(pred)) - 4,
                                           days.forecast = days.to.forecast)$predito,
                      pred$upper.merged.c)
    )
    ## Adiciona vetor com n de casos notificados e os previstos para os proximos dias pela projecao
    ## Previsto de casos notificados é calculado a partir das ditribuicoes de atrasos do nowcasting
    ## N de dias que foram corrigidos por nowcating
    ndias.now <- nrow(pred.original)
    now.proj.zoo$not.mean <- c(pred$n.casos,
                               estima.not(diff(now.proj.zoo$now.mean.c[(nrow(now.proj.zoo) - ndias.now):nrow(now.proj.zoo)]),
                                          NobBS.params.post = now.params.post,
                                          from = ndias.now - days.to.forecast + 1))
    now.proj.zoo$not.low <- c(pred$n.casos,
                              estima.not(diff(now.proj.zoo$now.low.c[(nrow(now.proj.zoo) - ndias.now):nrow(now.proj.zoo)]),
                                         NobBS.params.post = now.params.post,
                                         from = ndias.now - days.to.forecast + 1))
    now.proj.zoo$not.upp <- c(pred$n.casos,
                              estima.not(diff(now.proj.zoo$now.upp.c[(nrow(now.proj.zoo) - ndias.now):nrow(now.proj.zoo)]),
                                         NobBS.params.post = now.params.post,
                                         from = ndias.now - days.to.forecast + 1))
    ##Calcula n de casos cumulativos

    now.proj.zoo$not.mean.c <- cumsum(na.zero(now.proj.zoo$not.mean))
    now.proj.zoo$not.low.c <- cumsum(na.zero(now.proj.zoo$not.low))
    now.proj.zoo$not.upp.c <- cumsum(na.zero(now.proj.zoo$not.upp))
    return(now.proj.zoo)
}

# Funcao para plot do R efetivo
plot.estimate.R0 <- function(df.re){ # df com r efetivo  
    plot <- df.re %>%
        mutate(data = as.Date(data)) %>% 
        ggplot(aes(x = data, y = Mean.R)) +
        geom_ribbon(aes(ymin = Quantile.0.025.R, ymax = Quantile.0.975.R), fill = "lightgrey") +
        geom_line(size = 1.25, colour = RColorBrewer::brewer.pal(4, "Dark2")[3]) +
        scale_x_date( date_labels = "%d/%b", name = "") +
        ylim(min(c(0.8, min(df.re$Quantile.0.025.R))), max(df.re$Quantile.0.975.R)) +
        geom_hline(yintercept = 1, linetype = "dashed", col = "red", size = 1) +          
        ylab("Número de reprodução da epidemia") +
        plot.formatos
    plot
}

if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
plot.formatos <- theme_bw() +
  theme(axis.text = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10, face = "plain"),
        legend.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.margin = margin(0, 0, 0, 0, "pt"),
        panel.border = element_blank(),
        panel.grid = element_line(size = 0.25),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
# Funcao para fazer o plot de nowcasting acumulado, com valores estimados e notificados, com projecao para os proximos 5 dias
plot.nowcast.acumulado <- function(df){
    plot <- df %>%
        mutate(data = as.Date(data)) %>%
        ggplot(aes(x = data)) +
        # ic
        geom_ribbon(aes(ymin = now.low.c.proj, ymax = now.upp.c.proj), fill = "lightgrey") +
        # ic
        geom_ribbon(aes(ymin = not.low.c.proj, ymax = not.upp.c.proj), fill = "lightgrey") +
        # linha e ponto
        geom_line(aes(y = now.mean.c, color = "Estimados"), size = 1) +
        #geom_point(aes(y = now.mean.c, color = "Estimados"), size = 1) +
        # linha e ponto projecao
        geom_line(aes(y = now.mean.c.proj, color = "Estimados"), lty = "longdash") +
        #geom_point(aes(y = now.mean.c.proj, color = "Estimados")) +
        # linha e ponto
        geom_line(aes(y = not.mean.c, color = "Notificados"), size = 1) +
        #geom_point(aes(y = not.mean.c, color = "Notificados"), size = 1) +
        # linha e ponto projecao
        geom_line(aes(y = not.mean.c.proj, color = "Notificados"), lty = "longdash") +
        #geom_point(aes(y = not.mean.c.proj, color = "Notificados"), size = 1) +
        scale_x_date(date_labels = "%d/%b") +
        plot.formatos +
        scale_color_discrete(name = "") +
        scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:2]) +
        xlab("Dia do primeiro sintoma") +
        ylab("Número acumulado de casos") +
        theme(legend.position = "none") +
        scale_y_log10()
    plot
}

# Funcao para fazer plot nowcast de casos diarios (notificado e nowcasting), com linha de tendencia atenuada e IC
plot.nowcast.diario <- function(df) {
    plot <- df %>%
        mutate(data = as.Date(data)) %>%
        ggplot(aes(x = data)) +
        geom_ribbon(aes(ymin = lower.merged.pred, ymax = upper.merged.pred),
                    fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.1) +
        geom_line(aes(y = estimate.merged), lty = 2, col = "grey") +
        geom_point(aes(y = n.casos, col = "Notificado"), size = 2) +
        geom_point(aes(y = estimate, col = "Nowcasting"), size = 2) +
        geom_line(aes(y = estimate.merged.smooth), alpha = 0.6, size = 2) +
        scale_x_date(date_labels = "%d/%b") +
        scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[2:1]) +
        xlab("Dia do primeiro sintoma") +
        ylab("Número de novos casos") +
        plot.formatos +
        theme(legend.position = "none")
    plot
}

# Funcao para calcular o tempo de duplicacao
plot.tempo.dupl <- function(df.td) {#data.frame com tempo de duplicacao
    plot <- df.td %>%
        mutate(data = as.Date(data)) %>%
        ggplot(aes(x = data, y = estimativa)) +
        geom_ribbon(aes(ymin = ic.inf, ymax = ic.sup), fill = "lightgrey") +
        geom_line(size = 1.25, colour = RColorBrewer::brewer.pal(3, "Dark2")[1]) +
        scale_x_date(date_labels = "%d/%b", name = "") +
        # coloca coordenada cartesiana para resolver o problema do lim
        coord_cartesian(ylim = c(0, 100)) +
        #ylim(c(0, max(df.td$ic.sup)))
        ylab("Tempo de duplicação (dias)") +
        plot.formatos
    plot
}

plotTimesValidation = function(times_table, fit1, age = TRUE){
  if(age){
    times_table = times_table %>% arrange(age_class)
    tb = table(times_table$age_class)
    sim_times = unlist(sapply(names(tb), function(a) rwaittime_age(tb[a], a, fit1)))
    times_table$sim = sim_times
    times_table$age = age_table$faixas[match(times_table$age_class, age_table$ID)]
    d = pivot_longer(times_table, c(sim, time))
    ggplot(data = d, aes(x = value, group = name, fill = name)) + 
      geom_density(alpha= 0.5) + facet_wrap(~age) + 
      theme_cowplot() + scale_fill_discrete(labels = c("Simulado", "Observado"), name = "Categoria") 
  } else{
    sim_times = rwaittime(nrow(times_table), fit1)
    times_table$sim = sim_times
    d = pivot_longer(times_table, c(sim, time))
    ggplot(data = d, aes(x = value, group = name, fill = name)) + 
      geom_density(alpha= 0.5) + 
      theme_cowplot() + scale_fill_discrete(labels = c("Simulado", "Observado"), name = "Categoria") 
  }
}
# Estimate posterior probabilities for nowcasting AND SI variation
posteriors <- function(data){
  r_sample = data[-which(row.names(data) %in% c("t_start","t_end"))]
  mean_posterior <- apply(r_sample, 2, mean, na.rm = TRUE)
  std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
  quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                    0.025,
                                    na.rm = TRUE
  )
  quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                   0.05,
                                   na.rm = TRUE
  )
  quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                   0.25,
                                   na.rm = TRUE
  )
  median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
  quantile_0.75_posterior <- apply(r_sample, 2, quantile,
                                   0.75,
                                   na.rm = TRUE
  )
  quantile_0.95_posterior <- apply(r_sample, 2, quantile,
                                   0.95,
                                   na.rm = TRUE
  )
  quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                    0.975,
                                    na.rm = TRUE
  )

  results <- list(R = as.data.frame(
                      cbind(as.numeric(data["t_start", ]),
                            as.numeric(data["t_end", ]),
                            mean_posterior,
                            std_posterior,
                            quantile_0.025_posterior,
                            quantile_0.05_posterior,
                            quantile_0.25_posterior,
                            median_posterior,
                            quantile_0.75_posterior,
                            quantile_0.95_posterior,
                            quantile_0.975_posterior)))

  names(results$R) <- c(
      "t_start",
      "t_end",
      "Mean(R)",
      "Std(R)",
      "Quantile.0.025(R)",
      "Quantile.0.05(R)",
      "Quantile.0.25(R)",
      "Median(R)",
      "Quantile.0.75(R)",
      "Quantile.0.95(R)",
      "Quantile.0.975(R)"
  )

  return(results)  
}

#' Preenche NA's iniciais do vetor de estimado pelo nowcasting
#' @details O nowcasting retornado pela função NobBS usado com janela
#'     (argumento 'moving_window') ou limite máximo de atraso
#'     (argumento 'max_D') produz estimativas para os últimos dias,
#'     definidos por esses argumentos. Esta função preenche os dias
#'     anteriores com os valores de um outro vetor, normalmente o
#'     vetor de n de casos observado
#' @param vetor.now vetor com número de casos estimados pelo
#'     nowcasting, com NAs nas datas para as quais não há estimativas.
#' @param vetor.casos vetor com numero de casos o observados. Deve ter
#'     mesmo comprimento de 'vetor.now'
preenche.now <- function(vetor.now, vetor.casos) {
  if (any(is.na(vetor.now))) {
    index <- max(which(is.na(vetor.now), arr.ind = TRUE))
    vetor.now[1:index] <- vetor.casos[1:index]
  }
    return(vetor.now)
}


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
                          include.post = TRUE) {
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

require(incidence)
require(coarseDataTools)
require(coda)
nd <- read.table("./dados/nishi_si_table.txt", header = TRUE)
nishi_si <- read.table("./dados/nishi_si_posterior.txt", header = TRUE)

estimate.R0.cori <- function(novos.casos, day0 = NA, delay=7, method, parameter.table, 
                             p.distribution, bayes.control, si.data, si.sample, modified = FALSE, n2 = NA, ...) {

  if(is.na(day0)){
    mdif = mean(si.data$SL - si.data$EL) # empirical SI
    day0= min(which(cumsum(novos.casos) > 12 & 
                      1:length(novos.casos) > ceiling(mdif) 
                    & 1:length(novos.casos) > delay)) 
    
  }
  
  if(method == "uncertain_si")  {
    
    parameter.list  <- lapply(parameter.table, function(x) x=x)
    parameter.list$si_parametric_distr = p.distribution 
    parameter.list$t_start = seq(day0,length(novos.casos)-delay)
    parameter.list$t_end = seq(delay+day0,length(novos.casos))
    
    config <- make_config(parameter.list)
    return(estimate_R(novos.casos, method = method, config = config))
  }
  
  if(method == "parametric_si"){
    
    parameter.list  <- lapply(parameter.table, function(x) x=x)
    parameter.list$si_parametric_distr =  p.distribution
    parameter.list$t_start = seq(day0,length(novos.casos)-delay)
    parameter.list$t_end = seq(delay+day0,length(novos.casos))
    
    config <- make_config(parameter.list)
    return(estimate_R(novos.casos, method = "parametric_si", config = config))
  }
  
  if (method == "si_from_data") {
  config <- make_config(incid = novos.casos, 
                        method = method, 
                        list(si_parametric_distr = p.distribution,
                             mcmc_control = make_mcmc_control(burnin = bayes_control$burnin, 
                                                              thin = bayes_control$thin),
                             mean_prior = 5,
                             std_prior = 5,
                             n1 = bayes_control$n1, 
                             n2 = bayes_control$n2,
                             t_start = seq(day0,length(novos.casos)-delay),
                             t_end = seq(delay+day0,length(novos.casos))))
  
  return(estimate_R(novos.casos, method = method, 
                    si_data = si.data, config = config))
  }
  
  if(method == "si_from_sample"){
    
    parameter.list  <- list()
    parameter.list$t_start = seq(day0,length(novos.casos)-delay)
    parameter.list$t_end = seq(delay+day0,length(novos.casos))
    
    if(is.na(n2)){
    parameter.list$n2 = 50 } else {
    parameter.list$n2 = n2  
    }
    
    config <- make_config(parameter.list)
    
    if(modified == FALSE){
    return(estimate_R(novos.casos, method = method,
                                   si_sample = si.sample, 
                                   config = config)) } else {
    return(estimate_R_modified(novos.casos, method = method,
                                   si_sample = si.sample, 
                                   config = config))
                                   }
    
    
  }
  
}

estimate_R_modified <- function(incid,
                       method = c(
                         "non_parametric_si", "parametric_si",
                         "uncertain_si", "si_from_data",
                         "si_from_sample"
                       ),
                       si_data = NULL,
                       si_sample = NULL,
                       config = make_config(incid = incid, method = method)) {
  
  method <- match.arg(method)
  config <- make_config(incid = incid, method = method, config = config)
  config <- EpiEstim:::process_config(config)
  EpiEstim:::check_config(config, method)
  
  if (method == "si_from_data") {
    ## Warning if the expected set of parameters is not adequate
    si_data <- EpiEstim:::process_si_data(si_data)
    config <- EpiEstim:::process_config_si_from_data(config, si_data)
    
    ## estimate serial interval from serial interval data first
    if (!is.null(config$mcmc_control$seed)) {
      cdt <- dic.fit.mcmc(
        dat = si_data,
        dist = config$si_parametric_distr,
        burnin = config$mcmc_control$burnin,
        n.samples = config$n1 * config$mcmc_control$thin,
        init.pars = config$mcmc_control$init_pars,
        seed = config$mcmc_control$seed
      )
    } else {
      cdt <- dic.fit.mcmc(
        dat = si_data,
        dist = config$si_parametric_distr,
        burnin = config$mcmc_control$burnin,
        n.samples = config$n1 * config$mcmc_control$thin,
        init.pars = config$mcmc_control$init_pars
      )
    }
    
    ## check convergence of the MCMC and print warning if not converged
    MCMC_conv <- check_cdt_samples_convergence(cdt@samples)
    
    ## thin the chain, and turn the two parameters of the SI distribution into a
    ## whole discrete distribution
    c2e <- coarse2estim(cdt, thin = config$mcmc_control$thin)
    
    cat(paste(
      "\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@",
      "\nEstimating the reproduction number for these serial interval",
      "estimates...\n",
      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    ))
    
    ## then estimate R for these serial intervals
    
    if (!is.null(config$seed)) {
      set.seed(config$seed)
    }
    
    out <- estimate_R_func(
      incid = incid,
      method = "si_from_data",
      si_sample = c2e$si_sample,
      config = config
    )
    out[["MCMC_converged"]] <- MCMC_conv
  } else {
    if (!is.null(config$seed)) {
      set.seed(config$seed)
    }
    
    out <- estimate_R_func(
      incid = incid, method = method, si_sample = si_sample,
      config = config
    )
  }
  return(out)
}

##########################################################
## estimate_R_func: Doing the heavy work in estimate_R  ##
##########################################################

#'
#' @importFrom stats median qgamma quantile rnorm sd
#'
#' @importFrom incidence as.incidence
#'
estimate_R_func <- function(incid,
                            si_sample,
                            method = c(
                              "non_parametric_si", "parametric_si",
                              "uncertain_si", "si_from_data", "si_from_sample"
                            ),
                            config) {
  
  #########################################################
  # Calculates the cumulative incidence over time steps   #
  #########################################################
  
  calc_incidence_per_time_step <- function(incid, t_start, t_end) {
    nb_time_periods <- length(t_start)
    incidence_per_time_step <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(i) 
      sum(incid[seq(t_start[i], t_end[i]), c("local", "imported")]))
    return(incidence_per_time_step)
  }
  
  #########################################################
  # Calculates the parameters of the Gamma posterior      #
  # distribution from the discrete SI distribution        #
  #########################################################
  
  posterior_from_si_distr <- function(incid, si_distr, a_prior, b_prior,
                                      t_start, t_end) {
    nb_time_periods <- length(t_start)
    lambda <- overall_infectivity(incid, si_distr)
    final_mean_si <- sum(si_distr * (seq(0, length(si_distr) -
                                           1)))
    a_posterior <- vector()
    b_posterior <- vector()
    a_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      a_prior + sum(incid[seq(t_start[t], t_end[t]), "local"]) 
      ## only counting local cases on the "numerator"
    }
    else {
      NA
    })
    b_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      1 / (1 / b_prior + sum(lambda[seq(t_start[t], t_end[t])]))
    }
    else {
      NA
    })
    return(list(a_posterior, b_posterior))
  }
  
  #########################################################
  # Samples from the Gamma posterior distribution for a   #
  # given mean SI and std SI                              #
  #########################################################
  
  sample_from_posterior <- function(sample_size, incid, mean_si, std_si,
                                    si_distr = NULL,
                                    a_prior, b_prior, t_start, t_end) {
    nb_time_periods <- length(t_start)
    
    if (is.null(si_distr)) {
      si_distr <- discr_si(seq(0, T - 1), mean_si, std_si)
    }
    
    final_mean_si <- sum(si_distr * (seq(0, length(si_distr) -
                                           1)))
    lambda <- overall_infectivity(incid, si_distr)
    a_posterior <- vector()
    b_posterior <- vector()
    a_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      a_prior + sum(incid[seq(t_start[t], t_end[t]), "local"]) 
      ## only counting local cases on the "numerator"
    }
    else {
      NA
    })
    b_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      1 / (1 / b_prior + sum(lambda[seq(t_start[t], t_end[t])], na.rm = TRUE))
    }
    else {
      NA
    })
    sample_r_posterior <- vapply(seq_len(nb_time_periods), function(t) 
      if (!is.na(a_posterior[t])) {
        rgamma(sample_size,
               shape = unlist(a_posterior[t]),
               scale = unlist(b_posterior[t])
        )
      }
      else {
        rep(NA, sample_size)
      }, numeric(sample_size))
    if (sample_size == 1L) {
      sample_r_posterior <- matrix(sample_r_posterior, nrow = 1)
    }
    return(list(sample_r_posterior, si_distr))
  }
  
  method <- match.arg(method)
  
  incid <- EpiEstim:::process_I(incid)
  T <- nrow(incid)
  
  a_prior <- (config$mean_prior / config$std_prior)^2
  b_prior <- config$std_prior^2 / config$mean_prior
  
  EpiEstim:::check_times(config$t_start, config$t_end, T)
  nb_time_periods <- length(config$t_start)
  
  if (method == "si_from_sample") {
    if (is.null(config$n2)) {
      stop("method si_from_sample requires to specify the config$n2 argument.")
    }
    si_sample <- EpiEstim:::process_si_sample(si_sample)
  }
  
  min_nb_cases_per_time_period <- ceiling(1 / config$cv_posterior^2 - a_prior)
  incidence_per_time_step <- calc_incidence_per_time_step(
    incid, config$t_start,
    config$t_end
  )
  if (incidence_per_time_step[1] < min_nb_cases_per_time_period) {
    warning("You're estimating R too early in the epidemic to get the desired
            posterior CV.")
  }
  
  if (method == "non_parametric_si") {
    si_uncertainty <- "N"
    parametric_si <- "N"
  }
  if (method == "parametric_si") {
    si_uncertainty <- "N"
    parametric_si <- "Y"
  }
  if (method == "uncertain_si") {
    si_uncertainty <- "Y"
    parametric_si <- "Y"
  }
  if (method == "si_from_data" | method == "si_from_sample") {
    si_uncertainty <- "Y"
    parametric_si <- "N"
  }
  if (si_uncertainty == "Y") {
    if (parametric_si == "Y") {
      mean_si_sample <- rep(-1, config$n1)
      std_si_sample <- rep(-1, config$n1)
      for (k in seq_len(config$n1)) {
        while (mean_si_sample[k] < config$min_mean_si || mean_si_sample[k] >
               config$max_mean_si) {
          mean_si_sample[k] <- rnorm(1,
                                     mean = config$mean_si,
                                     sd = config$std_mean_si
          )
        }
        while (std_si_sample[k] < config$min_std_si || std_si_sample[k] >
               config$max_std_si) {
          std_si_sample[k] <- rnorm(1, mean = config$std_si,
                                    sd = config$std_std_si)
        }
      }
      temp <- lapply(seq_len(config$n1), function(k) sample_from_posterior(config$n2,
                                                                           incid, mean_si_sample[k], std_si_sample[k],
                                                                           si_distr = NULL, a_prior,
                                                                           b_prior, config$t_start, config$t_end
      ))
      config$si_distr <- cbind(
        t(vapply(seq_len(config$n1), function(k) (temp[[k]])[[2]], numeric(T))),
        rep(0, config$n1)
      )
      r_sample <- matrix(NA, config$n2 * config$n1, nb_time_periods)
      for (k in seq_len(config$n1)) {
        r_sample[seq((k - 1) * config$n2 + 1, k * config$n2), which(config$t_end >
                                                                      mean_si_sample[k])] <- (temp[[k]])[[1]][, which(config$t_end >
                                                                                                                        mean_si_sample[k])]
      }
      mean_posterior <- apply(r_sample, 2, mean, na.rm = TRUE)
      std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
      quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                        0.025,
                                        na.rm = TRUE
      )
      quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                       0.05,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.25,
                                       na.rm = TRUE
      )
      median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.75,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.95,
                                       na.rm = TRUE
      )
      quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                        0.975,
                                        na.rm = TRUE
      )
    }
    else {
      config$n1 <- dim(si_sample)[2]
      mean_si_sample <- rep(-1, config$n1)
      std_si_sample <- rep(-1, config$n1)
      for (k in seq_len(config$n1)) {
        mean_si_sample[k] <- sum((seq_len(dim(si_sample)[1]) - 1) * 
                                   si_sample[, k])
        std_si_sample[k] <- sqrt(sum(si_sample[, k] * 
                                       ((seq_len(dim(si_sample)[1]) - 1) - 
                                          mean_si_sample[k])^2))
      }
      temp <- lapply(seq_len(config$n1), function(k) sample_from_posterior(config$n2,
                                                                           incid,
                                                                           mean_si = NULL, std_si = NULL, si_sample[, k], a_prior,
                                                                           b_prior, config$t_start, config$t_end
      ))
      config$si_distr <- cbind(
        t(vapply(seq_len(config$n1), function(k) (temp[[k]])[[2]], 
                 numeric(nrow(si_sample)))),
        rep(0, config$n1)
      )
      r_sample <- matrix(NA, config$n2 * config$n1, nb_time_periods)
      for (k in seq_len(config$n1)) {
        r_sample[seq((k - 1) * config$n2 + 1,k * config$n2), which(config$t_end >
                                                                     mean_si_sample[k])] <- (temp[[k]])[[1]][, which(config$t_end >
                                                                                                                       mean_si_sample[k])]
      }
      mean_posterior <- apply(r_sample, 2, mean, na.rm = TRUE)
      std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
      quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                        0.025,
                                        na.rm = TRUE
      )
      quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                       0.05,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.25,
                                       na.rm = TRUE
      )
      median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.75,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.95,
                                       na.rm = TRUE
      )
      quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                        0.975,
                                        na.rm = TRUE
      )
    }
  } else {
    # CertainSI
    if (parametric_si == "Y") {
      config$si_distr <- discr_si(seq(0,T - 1), config$mean_si, config$std_si)
    }
    if (length(config$si_distr) < T + 1) {
      config$si_distr[seq(length(config$si_distr) + 1,T + 1)] <- 0
    }
    final_mean_si <- sum(config$si_distr * (seq(0,length(config$si_distr) -
                                                  1)))
    Finalstd_si <- sqrt(sum(config$si_distr * (seq(0,length(config$si_distr) -
                                                     1))^2) - final_mean_si^2)
    post <- posterior_from_si_distr(
      incid, config$si_distr, a_prior, b_prior,
      config$t_start, config$t_end
    )
    
    a_posterior <- unlist(post[[1]])
    b_posterior <- unlist(post[[2]])
    mean_posterior <- a_posterior * b_posterior
    std_posterior <- sqrt(a_posterior) * b_posterior
    quantile_0.025_posterior <- qgamma(0.025,
                                       shape = a_posterior,
                                       scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.05_posterior <- qgamma(0.05,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.25,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    median_posterior <- qgamma(0.5,
                               shape = a_posterior,
                               scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.75,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.95,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.975_posterior <- qgamma(0.975,
                                       shape = a_posterior,
                                       scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
  }
  
  results <- list(R = as.data.frame(cbind(
    config$t_start, config$t_end, mean_posterior,
    std_posterior, quantile_0.025_posterior, quantile_0.05_posterior,
    quantile_0.25_posterior, median_posterior, quantile_0.25_posterior,
    quantile_0.25_posterior, quantile_0.975_posterior
  )))
  
  names(results$R) <- c(
    "t_start", "t_end", "Mean(R)", "Std(R)",
    "Quantile.0.025(R)", "Quantile.0.05(R)", "Quantile.0.25(R)",
    "Median(R)", "Quantile.0.75(R)", "Quantile.0.95(R)",
    "Quantile.0.975(R)"
  )
  results$method <- method
  results$si_distr <- config$si_distr
  if (is.matrix(results$si_distr)) {
    colnames(results$si_distr) <- paste0("t", seq(0,ncol(results$si_distr) - 1))
  } else {
    names(results$si_distr) <- paste0("t", seq(0,length(results$si_distr) - 1))
  }
  if (si_uncertainty == "Y") {
    results$SI.Moments <- as.data.frame(cbind(
      mean_si_sample,
      std_si_sample
    ))
  } else {
    results$SI.Moments <- as.data.frame(cbind(
      final_mean_si,
      Finalstd_si
    ))
  }
  names(results$SI.Moments) <- c("Mean", "Std")
  
  
  if (!is.null(incid$dates)) {
    results$dates <- EpiEstim:::check_dates(incid)
  } else {
    results$dates <- seq_len(T)
  }
  results$I <- rowSums(incid[, c("local", "imported")])
  results$I_local <- incid$local
  results$I_imported <- incid$imported
  
  results$r_sample <- r_sample
  
  class(results) <- "estimate_R"
  return(results)
}


default.R.cori <- partial(estimate.R0.cori,
                          delay = 7,
                          day0 = NA,
                          method = "si_from_sample",
                          si.data = nd, 
                          si.sample = nishi_si[,sample(1:ncol(nishi_si), 1)], # Samples 1 SI interval
                          p.distribution = "G",
                          modified = TRUE,
                          n1 = 50,
                          n2 = 50)


#' Função para ler base da sivep
#' @param dir Caractere. Caminho relativo para o diretório onde está o dado
#' @param escala Caractere. Escala de análise aceita: `"pais"`, `"estado"`, `"municipio"` #ö micro e meso um dia
#' @param geocode Caractere. Geocode IBGE do estado ou município. Município pde ter 6 ou 7 dígitos
#' @param data Caractere. Data no formato  "%Y_%m_%d". Quando NULL (padrão) pega a data mais recente
#' @param residentes Filtar por residentes ou por local da notificação
#' @param ... Qualquer parâmetro de `read.csv()`
read.sivep <- function(dir, # diretorio onde esta o dado
                       escala,
                       sigla,
                       geocode,
                       data,  #formato com _
                       residentes = TRUE,
                       ...) {
  # lendo os dados
  file.name <- list.files(dir, pattern = paste0(".*", data, ".(csv|zip)"), full.names = TRUE)
  # múltiplos matches são possíveis
  file.name <- file.name[1]
  # detecta e lida com arquivo zip
  if (endsWith(file.name, '.zip')){
      is_zip = TRUE
      file.name <- unzip(file.name, basename(gsub('.zip$', '.csv', file.name)))
  } else{
      is_zip = FALSE
  }
  # detecta separador
  linha1 <- readLines(file.name, n = 2)[2]
  if (count.fields(textConnection(linha1), sep = ',') >
      count.fields(textConnection(linha1), sep = ';')) {
      sep <- ','
  } else
      sep <- ';'
  #dados <- read_delim(file = file.name, delim = sep, ...)
  dados <- read_delim(file = file.name,
                      delim = sep,
                      escape_double = FALSE,
                      trim_ws = TRUE,
                      n_max = 1)
  nomes <- names(dados)
  cols <- rep("c", length(nomes))
  names(cols) <- nomes
  cols.list <- as.list(cols)
  dados <- read_delim(file = file.name,
                      delim = sep,
                      col_types = cols.list,
                      escape_double = FALSE,
                      trim_ws = TRUE)
  dados <- data.frame(dados)
  # conveniencia mudando para minusculas
  names(dados) <- tolower(names(dados))
  names(dados) <- gsub(" ", "_", names(dados))
  names(dados) <- gsub("\\.", "_", names(dados))

  if(is_zip)
      file.remove(file.name)

  # filtro por estados ou municipio
  ## ö dá pra implementar meso e microrregiao ast: super dá, por enquanto tirei filtro
  if (escala != "pais") {
  df <- read.csv("./dados/geocode_ibge.csv")
  #geocode <- as.numeric(geocode)
  municipio.code <- sapply(df$id, function(x) substr(x, start = 1, stop = 6))
  micro.code   <- df$microrregiao.id
  meso.code    <- df$microrregiao.mesorregiao.id
  estado.code  <- df$microrregiao.mesorregiao.UF.id
  estado.sigla <- df$microrregiao.mesorregiao.UF.sigla
    n.geo <- nchar(geocode)
    n.geocodes <- c(estado = 2, meso = 4, micro = 5, municipio = 7)
    #ö ast estava pensando num check de escala vs geocode tipo "seu geocode não corresponde ao n.char". não sei como renato resolveu isto no site. teria que ter a exceção de municipiom claro. mas a pessoa pode querer vir com o geocode de 7, né.
    # CORRIGINDO GEOCODE DO MUNICIPIO DE SAO PAULO na base "355030" mas oficial é "3550308"
    if (escala == "municipio" & n.geo == 7) {
      geocode <- substr(geocode, start = 1, stop = 6)
      n.geo <- nchar(geocode)
    }

    if (escala == "municipio") {
      if(residentes)
          dados <- dados[which(dados$co_mun_res == as.numeric(geocode)), ]
      ## PIP: está selecionando também linhas que têm  NA no campo co_mun_res.
      ## Não seria então
      ## dados <- dados[dados$co_mun_res == as.numeric(geocode)&!is.na(dados$co_mun_res), ]
      ## ou
      ## dados <- filter(dados, co_mun_res == as.numeric(geocode))
      ## ?
      ## O mesmo para as demais linha análogas
      else
        dados <- dados[which(dados$co_mun_not == as.numeric(geocode)), ]
    }
    if (escala == "estado") {
      estados <- c('12','27','13','16','29','23','53','32','52','21','31','50','51','15','25','26','22','41','33','24','11','14','43','42','28','35','17')
      names(estados) <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
      if (!is.null(sigla) & is.null(geocode)) geocode <-  estados[names(estados) == sigla]#o geocode nao importa tanto para estados porque tem sg_uf
      if (is.null(sigla) & !is.null(geocode)) sigla <- names(estados[estados == geocode])
      dados <- dados[dados$sg_uf == sigla, ]
    }
    if (escala == "micro") {
      co.muns <- municipio.code[micro.code == geocode]
      if(residentes)
        dados <- dados[dados$co_mun_res %in% co.muns, ]
      else
        dados <- dados[dados$co_mun_not %in% co.muns, ]
    }
    if (escala == "meso") {
      co.muns <- municipio.code[meso.code == geocode]
      if(residentes)
        dados <- dados[dados$co_mun_res %in% co.muns, ]
      else
        dados <- dados[dados$co_mun_not %in% co.muns, ]
    }
    if (escala == "drs") {
      drs <- read.csv(paste0('./dados/DRS_', sigla, '.csv'))
      co.muns <- drs[drs$DRS == geocode, 'id']
      if(residentes)
        dados <- dados[dados$co_mun_res %in% co.muns, ]
      else
        dados <- dados[dados$co_mun_not %in% co.muns, ]
    }
  }
  # formata datas
  # muda nome de colunas e formata datas
  dt.cols <- names(dados)[grepl("dt_", names(dados))]
  ## usa lubridate
  dados[, dt.cols] <- lapply(dados[, dt.cols],
                             function(x)
                                 as_date(parse_date_time(x, c("dmy", "ymd", "mdy", "dmy HMs", "ymd HMs"))))
  return(dados)
}

#' Calcula R efetivo sobre a estimativas de nowcasting retornadas pela função NobBS::NobBS
#' @param ncasos vetor de número de novos casos
#' @param datas vetor de datas dos novos casos
Re.com.data <- function(ncasos, datas, dia0 = min(datas), delay = 5){
    if(length(ncasos)!=length(datas)) stop("ncasos e ndatas devem ter o mesmo comprimento")
    day0 <- min(which(datas >= dia0, arr.ind=TRUE))
    if(day0 < delay)
        day0 = delay + 1
    Re <- estimate.R0(as.integer(na.zero(ncasos)), day0 = day0, delay = delay)
    names(Re$R) <- gsub("\\(R\\)", ".R", names(Re$R))
    Re$R$data.inicio <- datas[Re$R$t_start]
    Re$R$data.fim <- datas[Re$R$t_end]
    return(Re)
}

#' Calcula R efetivo sobre as trajetórias de nowcasting retornadas pela função NobBS.posterior
#' @param R.method função parcialmente aplicada que calcula Re de uma trajetória
#' @param trajectories data.frame com trajetórias de nowcasting
#' @param Nsamples int número de amostrar das trajetórias de nowcasting
#' @param quantiles logical retorna quantis da distribuição posterior de R ou as osteriores completas?
#' @param ... argumentos extras são repassados pro ldply: .parallel=T é especialmente útil
Re.nowcasting <- function(R.method,
                          trajectories,
                          Nsamples,
                          quantiles = TRUE,
                          ...) {
  N <- ncol(trajectories)
  if (missing(Nsamples) | Nsamples > N - 1)
    Nsamples <- N - 1
  ## trajetórias são auto-correlacionas, melhor sampling é com maior distância
  ## possível entre os índices - usamos intervalos regulares
  samples <- round(seq(2, N, length.out = Nsamples))
  fun <- function(traj){
    casos <- fill.dates(data.frame(onset = trajectories$date, n.casos = traj), 2)
    res <- R.method(casos$incidence)$r_sample
  }
  re.df <- ldply(trajectories[, samples], fun, .id = NULL, ...)
  # run a single time to get full output
  casos <- fill.dates(data.frame(onset = trajectories$date, n.casos = trajectories[, 2]), 2) ## PIP: estava usando a coluna 1 do trajectories, que eram as datas
  res <- R.method(casos$incidence)
  re.df["t_start", ] <- res$R$t_start
  re.df["t_end", ] <- res$R$t_end

  if (!quantiles)
      return(re.df)
  # Calculate quantiles and moments
  results <- posteriors(re.df)
}

# funcao para gerar tabelas que vao para o site
tabelas.web <- function(output.dir,
                        tipo,
                        df.cum, # data frame com os casos acumulados
                        df.td, # data frame com o tempo de duplicacao
                        df.re = NULL, # data frame com o r efetivo
                        data_base) { #data real da última atualização de cada objeto
  # MIN-MAX ####
  ## com n casos min e maximo na data maxima de projecao
  # minmax.casos <- data.frame(row.names = sigla.adm)
  min <- as.integer(df.cum[max(nrow(df.cum)), 'now.low.c.proj'])
  max <- as.integer(df.cum[max(nrow(df.cum)), 'now.upp.c.proj'])
  data <- format(max(as.Date(df.cum$data)), "%d/%m/%Y")
  minmax.casos <- cbind(min, max, data)
  write.table(minmax.casos,
              file = paste0(output.dir, "data_forecast_exp_", tipo, ".csv"),
              row.names = TRUE, col.names = FALSE)
  # TEMPO DE DUPLICACAO ####
  #temp.dupl <- data.frame(row.names = sigla.adm)
  min.dias <- as.vector(round(df.td[max(nrow(df.td)), "ic.inf"], 1))
  max.dias <- as.vector(round(df.td[max(nrow(df.td)), "ic.sup"], 1))
  temp.dupl <- cbind(min.dias, max.dias)
  write.table(temp.dupl,
              file = paste0(output.dir, "data_tempo_dupli_", tipo, ".csv"),
              row.names = TRUE, col.names = FALSE)
  #data_atualizacao
  data_atualizacao <- format(as.Date(data_base, "%Y_%m_%d"), "%d/%m/%Y")
  write.table(data_atualizacao,
              file = paste0(output.dir, "data_atualizacao_", tipo, ".csv"),
              row.names = TRUE, col.names = FALSE)

  if (tipo %in% c("covid", "srag")) { #so calcula re para covid e srag
    # R EFETIVO ####
    #Re.efe <- data.frame(row.names = sigla.adm)
    min <- as.vector(round(df.re[nrow(df.re), "Quantile.0.025.R"], 2))
    max <- as.vector(round(df.re[nrow(df.re), "Quantile.0.975.R"], 2))
    Re.efe <- cbind(min, max)
    write.table(Re.efe,
                file = paste0(output.dir, "data_Re_", tipo, ".csv"),
                row.names = TRUE, col.names = FALSE)


    lista <- list(minmax.casos = minmax.casos,
                  temp.dupl = temp.dupl,
                  Re = Re.efe,
                  data_atualizacao = data_atualizacao)
  }
  if (tipo %in% c("obitos_covid", "obitos_srag", "obitos_srag_proaim")) {
    lista <- list(minmax.casos = minmax.casos,
                  temp.dupl = temp.dupl,
                  data_atualizacao = data_atualizacao)
  }
  return(lista)
}

rwaittime = function(n, fit){
  mu = fixef(fit)[1,"Estimate"]
  shape = mean(as.data.frame(fit)$shape)
  lambda = exp(mu) / gamma( 1 + 1/shape )
  rweibull(n, shape = shape, scale = lambda)
}

rwaittime_posterior_age = function(n, age, fit){
  model_matrix = as.matrix(fit)
  x = model_matrix[1,]
  aaply(model_matrix, 1, function(x){
    random_int = x[grep(age, names(x))]
    if(length(random_int) == 0) random_int = 0
    mu = x["b_Intercept"] + random_int
    shape = x["shape"]
    lambda = exp(mu) / gamma( 1 + 1/shape )
    rweibull(n, shape = shape, scale = lambda)
  })
}

rwaittime_age = function(n, age, fit){
  mu = coef(fit)$age_class[age,"Estimate","Intercept"]
  shape = summary(fit)$spec_pars[1]
  lambda = exp(mu) / gamma( 1 + 1/shape )
  rweibull(n, shape = shape, scale = lambda)
}


write.notificacoes.data <- function(dados,
                                    output.dir,
                                    tipo = "covid", # covid, srag, obitos_covid, obitos_srag
                                    data
                                    ) {

  obitos <- c("obitos_covid", "obitos_srag")

  n.notificacoes <- dados %>%
    group_by(dt_notific) %>%
    summarise(n.notific = n()) %>%
    as.data.frame()

  nome.not <- paste0(output.dir, "notificacoes_", tipo, "_", data, ".csv")

  if (tipo %in% obitos) {
    n.data  <- dados %>%
      group_by(dt_evoluca) %>%
      summarise(n.casos = n()) %>%
      as.data.frame()
    nome.data <- paste0(output.dir, "n_casos_data_", tipo, "_", data, ".csv")
      } else {
    n.data  <- dados %>%
      group_by(dt_sin_pri) %>%
      summarise(n.casos = n()) %>%
      as.data.frame()
    nome.data <-  paste0(output.dir, "n_casos_data_sintoma_", tipo, "_", data, ".csv")
  }

  write.csv(n.notificacoes,
            nome.not,
            row.names = FALSE)

  write.csv(n.data,
            nome.data,
            row.names = FALSE)

}

write.nowcasting <- function(now,
                            output.dir,
                            tipo = "covid", # covid, srag, obitos_covid, obitos_srag
                            data
                            ) { # objeto com output dos nowcastings

  nome.now.df <- paste0(output.dir, "nowcasting_", tipo, "_previstos_", data, ".csv")
  write.csv(now$estimates,
            file = nome.now.df,
            row.names = FALSE)

  nome.now.post <- paste0(output.dir, "nowcasting_", tipo, "_post_", data, ".csv")
  write.csv(now$params.post,
            file = nome.now.post,
            row.names = FALSE)

  # seria melhor usar um argumento?
  if ("trajectories" %in% names(now)){
    nome.now.traj <- paste0(output.dir, "nowcasting_", tipo, "_traj_", data, ".csv")
    write.csv(now$trajectories,
              file = nome.now.traj,
              row.names = FALSE)
  }
}

# funcao para converter zoo em df
## srm não se dá bem com zoo e write.zoo
zoo2df <- function(zoo) {
  df <- as.data.frame(zoo)
  df$data <- as.Date(row.names(df))
  return(df)
}
