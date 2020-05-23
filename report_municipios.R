###########################################################################
## Script para rodar relatorio por municipio
## requer que os plots tenham sido gerados
###########################################################################

# bibliotecas
library(dplyr)
source("fct/get.last.date.R")

# srm: ainda fazendo apenas para algumas capitais de interesse, bem especifico mesmo é o que tem pra hoje
# params
uf.vector <- c("BA", "CE", "DF", "PE", "RJ")
muni.vector <- c("Salvador", "Fortaleza", "Brasilia", "Recife", "Rio_de_Janeiro")
output.dir <- "../dados_processados/nowcasting/municipios/"

for (i in 1:length(muni.vector)) {
  uf <- uf.vector[i]
  muni <- muni.vector[i]
  muni.name <- paste0(muni, "-", uf)
  docs.dir <- paste0("docs/municipios/", uf, "/", muni, "/")
  data.base <- get.last.date(paste0("dados_processados/nowcasting/municipios/",
                                    uf, "/", muni, "/output_nowcasting/"))
  rmarkdown::render(input = "docs/report_municipios.Rmd",
                    output_format = "html_document",
                    output_file = paste0("relatorio_", data.base, ".html"),
                    output_dir = docs.dir)
}
