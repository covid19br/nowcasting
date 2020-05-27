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
muni.names <- c("Salvador", "Fortaleza", "Brasília", "Recife", "Rio de Janeiro")
dir <- "dados_processados/nowcasting/municipios/"
relatorio.dir <- "docs/municipios/"
# para campinas
# uf.vector <- "SP"
# muni.vector <- "Campinas"
# muni.names <- "Campinas"
# dir <- "../dados/municipio_campinas/output/nowcasting/municipios/"
# relatorio.dir <- "../dados/municipio_campinas/docs/"

for (i in 1:length(muni.vector)) {
  output.dir <- paste0("../", dir)
  uf <- uf.vector[i]
  muni <- muni.vector[i]
  muni.name <- paste0(muni.names[i], "-", uf)
  docs.dir <- paste0(relatorio.dir, uf, "/", muni, "/")
  data.base <- get.last.date(paste0(dir, uf, "/", muni, "/output_nowcasting/"))
  rmarkdown::render(input = "docs/report_municipios.Rmd",
                    output_format = "html_document",
                    output_file = paste0("relatorio_", data.base, ".html"),
                    output_dir = docs.dir)
}
