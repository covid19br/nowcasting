## Função para checar se existem dados de nowcasting para a unidade administrativa
existe.nowcasting <- function(adm = adm,
                              sigla.adm = sigla.adm,
                              tipo,
                              data,
                              output.dir) {
  if (data == "NULL") {
    data <- get.data.base(adm = adm, sigla.adm = sigla.adm, tipo = tipo, output.dir = output.dir)
    }
  else data_file <- as.Date(data, format = formato.data) %>%  format("%Y_%m_%d")
  nowcasting.file <- list.files(path = output.dir,
                                pattern = paste0("nowcasting_", tipo, ".+", data,".csv"))
  length(nowcasting.file) > 0
}
