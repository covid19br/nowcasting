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
