check.geocode <- function(escala,
                          geocode){

  # url <- paste0("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")
  # df <- jsonlite::fromJSON(url)
  # df$nome.nonascii <- gsub(" ", "_", replace_non_ascii(df$nome))
  # df$nome.nonascii <- gsub("'", "", df$nome.nonascii)
  # write.csv(df, "../dados/geocode_ibge.csv", row.names = FALSE)
  df <- read.csv("../dados/geocode_ibge.csv")
  geocode <- as.numeric(geocode)
  municipio.code <- df$id
  micro.code <- df$microrregiao.id
  meso.code <- df$microrregiao.mesorregiao.id
  estado.code <- df$microrregiao.mesorregiao.UF.id
  estado.sigla <- df$microrregiao.mesorregiao.UF.sigla

    if (escala == "municipio") {
      if (!nchar(geocode) %in% c(6, 7))
        stop("geocode não bate com escala")
    if (geocode %in% municipio.code) {
      id <- which(municipio.code == geocode)
      nome <- paste0("municipio_",
                    paste(estado.sigla[id], df$nome.nonascii[id], sep = "_")) # SP_Sao_Paulo
    } else {
      stop("geocode de municipio invalido")
    }
  }

  if (escala == "micro") {
    if (nchar(geocode) != 5)
      stop("geocode não bate com escala")
    if (geocode %in% micro.code) {
      id <- which(micro.code == geocode)
      nome <- paste0("micro_",
                    unique(paste(estado.sigla[id], micro.code[id], sep = "_"))) #SP_35035
    } else {
      stop("geocode de microrregião invalido")
    }
  }

  if (escala == "meso") {
    if (nchar(geocode) != 4)
      stop("geocode não bate com escala")
    if (geocode %in% meso.code) {
      id <- which(meso.code == geocode)
      nome <- paste0("meso_",
                     unique(paste(estado.sigla[id], meso.code[id], sep = "_"))) #SP_3508
    } else {
      stop("geocode de mesorregião invalido")
    }
  }

  if (escala == "estado") {
    if (nchar(geocode) != 2)
      stop("geocode não bate com escala")
    if (geocode %in% estado.code) {
      id <- which(estado.code == geocode)
      nome <- unique(paste("estado", estado.sigla[id], sep = "_"))
    } else {
      stop("geocode de estado invalido")
    }
  }
  return(nome)
}

# check.geocode(escala = "municipio",
#               geocode = " 2113009")
#
# check.geocode(escala = "estado",
#               geocode = "21")
#
# check.geocode(escala = "micro",
#               geocode = "21")
#
# check.geocode(escala = "micro",
#               geocode = "21008")
#
# check.geocode(escala = "meso",
#               geocode = "2102")


