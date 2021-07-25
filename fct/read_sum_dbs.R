library(vroom)
library(stringr)
library(magrittr)

read_sum_dbs <- function(datas, dir) {
    filenames <- list.files(dir, pattern = paste0(".*(", paste0(datas, collapse="|"), ").(csv|zip|csv.xz)$"),
                            full.names = TRUE)
    print(filenames)
    dados <- vroom(filenames, id = "data")
    # not very efficient but will do
    dados$data %<>% str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
        as.Date(format = "%Y_%m_%d")
    return(as.data.frame(dados))
}
