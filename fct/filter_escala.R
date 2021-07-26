#' Função para filtrar dados SIVEP por município, estado, região do IBGE, ou DRS
#' 
#' Esta versão tenta ser mais econômica e eficiente em termos de memória,
#' pré-selecionando as colunas e usando vroom ao invés de readr, ao mesmo tempo
#' em que preserva os filtros de escala, regiões etc.
#' 
#' @param dados Data.frame. Precisa conter os campos sg_uf e co_mun_res
#' @param escala Caractere. Escala de análise aceita: `"pais"`, `"estado"`, `"municipio"`, `"micro"`, `"meso"`, `"macro"`, `"drs"`
#' @param sigla Caractere. Sigla do estado - alternativa ao geocode se `escala = "estado"`, obrigatório caso `escala = "drs"`
#' @param geocode Caractere. Geocode IBGE do estado ou município. Município pode ter 6 ou 7 dígitos
filter.escala <- function(dados, escala, sigla, geocode) {
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
        dados <- dados[which(dados$co_mun_res == as.numeric(geocode)), ]
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
        dados <- dados[dados$co_mun_res %in% co.muns, ]
    }
    if (escala == "meso") {
        co.muns <- municipio.code[meso.code == geocode]
        dados <- dados[dados$co_mun_res %in% co.muns, ]
    }
    if (escala == "drs") {
        drs <- read.csv(paste0('./dados/DRS_', sigla, '.csv'))
        co.muns <- drs[drs$DRS == geocode, 'id']
        dados <- dados[dados$co_mun_res %in% co.muns, ]
    }
}
