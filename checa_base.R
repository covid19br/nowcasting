# Libraries
library(optparse)

# carrega funcoes----
source("_src/funcoes.R")
source("fct/count.lines.R")

################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--dir",
                help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                default = "../dados/SIVEP-Gripe/",
                metavar = "dir"),
    make_option("--dataBase",
                help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                metavar = "dataBase"),
    make_option("--outputDir",
                default = "../dados_processados/nowcasting",
                help = ("Diretório de destino"),
                metavar = "outputDir")
  )
  parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                option_list = option_list,
                                description = "Script para checar base")

  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                    positional_arguments = TRUE)
  dir <- opt$options$dir
  data <- opt$options$dataBase
  out.dir <- opt$options$outputDir
}

# pegando a data mais recente
if (is.null(data)) {
    data <- get.last.date(dir)
}

dados <- read.sivep(dir = dir, escala = "pais", data = data)
size.read <- dim(dados)[1]

file.name <- list.files(dir, pattern = paste0(".*", data, ".(csv|zip|bz2|xz)"),
                        full.names = TRUE)
# múltiplos matches são possíveis
file.name <- file.name[1]
size.file <- count.lines(file.name)

cat(paste0("Registros lidos: ", size.read, "\n", "Linhas da base: ", size.file,
           "\n"))

