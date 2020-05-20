## Função para checar se existem dados de nowcasting para a unidade administrativa
existe.nowcasting2 <- function(tipo,
                               data,
                               output.dir) {
  nowcasting.file <- list.files(path = output.dir,
                                pattern = paste0("nowcasting_acumulado_", tipo, "_20"))
  length(nowcasting.file) > 0
}
