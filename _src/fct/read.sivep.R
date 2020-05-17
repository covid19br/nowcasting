read.sivep <- function(dir, # diretorio onde esta o dado
                       filtro = FALSE, # Lógico. filtra por estado, mesorregião, microrregião ou municipio a partir do geocode
                       geocode = NULL, # Caractere. especificar aqui se for filtrar por geocode
                       data = NULL,  #se nao especificada, pega a data mais recente
                       ...){ # qq parametro de read.csv()
  # função acessória: extrai a data mais recente de qualquer nome de arquivo
  get.last.data <- function(dir) {
    data.base <- list.files(dir) %>%
      # regex para catar data
      stringr::str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
      as.Date(format = "%Y_%m_%d") %>%
      max(na.rm = TRUE) %>%
      format("%Y_%m_%d")
  }
  if (is.null(data)) {
    data <- get.last.data(dir)
  }
  # lendo os dados
  file.name <- list.files(dir, pattern = paste0(".*", data, ".csv"), full.names = TRUE)
  dados <- read.csv(file.name, sep = ";", header = TRUE, stringsAsFactors = FALSE)
  # muda nome de colunas e formata datas
  dt.cols <- names(dados)[grepl("dt_", names(dados))]
  dados[, dt.cols] <- lapply(dados[, dt.cols], function(x) as.Date(x, formato.data))
  names(dados) <- tolower(names(dados))
  # filtro por estados ou municipio
  ## ö dá pra implementar meso e microrregiao
  if (filtro) {
    geocodes <- c(estado = 2, meso = 4, micro = 6, municipio = 7)
    n.geo <- nchar(geocode)
    tipo.geo <- names(geocodes[geocodes == n.geo])
    if (tipo.geo == "municipio") {
      dados <- dados[dados$co_mun_res == geocode, ]
    }
    if (tipo.geo == "estado") {
      estados <- c('12','27','13','16','29','23','53','32','52','21','31','50','51','15','25','26','22','41','33','24','11','14','43','42','28','35','17')
      names(estados) <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
      sigla.estado <- names(estados[estados == geocode])
      dados <- dados[dados$sg_uf == sigla.estado, ]
    }
  }
  return(dados)
}
