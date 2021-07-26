# Libraries
library(optparse)
library(tidyr)
library(plyr)
library(dplyr)
# carrega funcoes----
source("fct/read.sivep3.R")
source("fct/age_table.R")

################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
    option_list <- list(
        make_option("--dir",
                    help = ("Caminho até o diretório com os arquivos csv com base sivep gripe"),
                    default = "../dados/SIVEP-Gripe",
                    metavar = "dir"),
        make_option("--dataBase",
                    help = ("Data da base de dados, formato 'yyyy_mm_dd'"),
                    metavar = "dataBase"),
        make_option("--outputDir",
                    default = "../dados/sumario_SIVEP",
                    help = ("Diretório de destino"),
                    metavar = "outputDir"),
        make_option("--updateGit", default = "FALSE",
                    help = ("Fazer git add, commit e push?"),
                    metavar = "updateGit")
    )
    parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n",
                                  option_list = option_list,
                                  description = "Script que computa dados agregados de uma base SIVEP")

    opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                      positional_arguments = TRUE)
    dir <- opt$options$dir
    data <- opt$options$dataBase
    out.dir <- opt$options$outputDir
    update.git <- opt$options$updateGit
}

print(paste("Lendo base de dados de", data))
dados <- read.sivep(dir = dir, escala = "pais",
                    data = data,
                    extra_cols = c("NU_IDADE_N")) %>%
        select(dt_notific, dt_sin_pri, dt_pcr, dt_digita, sg_uf, nu_idade_n,
               dt_evoluca, dt_encerra, pcr_sars2, classi_fin, evolucao,
               co_mun_res) %>%
        dplyr::mutate(nu_idade_n = as.numeric(nu_idade_n)) %>%
        plyr::mutate(age_class = classifyAgeFast(nu_idade_n))

## 1.1 casos covid ####
dados_covid <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
    select(dt_notific, dt_sin_pri, dt_pcr, dt_digita, age_class, sg_uf,
           co_mun_res) %>%
    mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE)) %>%
    filter(!is.na(dt_pcr_dig)) %>%
    group_by(dt_sin_pri, age_class, co_mun_res) %>%
    summarise(sg_uf=first(sg_uf), covid = n()) %>%
    as.data.frame()

## 1.2. casos srag ####
dados_srag <- dados %>%
    select(dt_notific, dt_sin_pri, dt_digita, age_class, co_mun_res, sg_uf) %>%
    mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) %>% # nome aqui é pcr mas não tem pcr
    filter(!is.na(dt_pcr_dig)) %>%
    group_by(dt_sin_pri, age_class, co_mun_res) %>%
    summarise(sg_uf = first(sg_uf), srag = n()) %>%
    as.data.frame()

## 2.1. obitos covid ####
dados_ob_covid <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca, na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, age_class, co_mun_res, sg_uf) %>%
    filter(! is.na(dt_encerra)) %>%
    group_by(dt_evoluca, age_class, co_mun_res) %>%
    summarise(sg_uf = first(sg_uf), ob.covid = n()) %>%
    as.data.frame()

## 2.2. obitos srag ####
dados_ob_srag <- dados %>%
    filter(evolucao == 2) %>%
    filter(!is.na(dt_evoluca)) %>%
    mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                             na.rm = TRUE)) %>%
    select(dt_evoluca, dt_notific, dt_encerra, age_class, co_mun_res, sg_uf) %>%
    filter(! is.na(dt_encerra)) %>%
    group_by(dt_evoluca, age_class, co_mun_res) %>%
    summarise(sg_uf = first(sg_uf), ob.srag = n()) %>%
    as.data.frame()

dados_all <- dados_srag %>%
    full_join(dados_covid, by = c("dt_sin_pri", "age_class", "co_mun_res", "sg_uf")) %>%
    full_join(dados_ob_srag, by = c("dt_sin_pri" = "dt_evoluca", "age_class", "co_mun_res", "sg_uf")) %>%
    full_join(dados_ob_covid, by = c("dt_sin_pri" = "dt_evoluca", "age_class", "co_mun_res", "sg_uf")) %>%
    rename(dt_evento = dt_sin_pri) %>%
    replace_na(list(srag = 0, covid = 0, ob.srag = 0, ob.covid = 0))

filename <- paste0(out.dir, "/sum_dados_", data, ".csv.xz")
write.csv(dados_all, file=xzfile(filename), row.names=FALSE)

if (update.git) {
    system(paste("cd", out.dir, "&&",
                 "git pull &&",
                 "git add", basename(filename), "&&",
                 "git commit", basename(filename), " -m ':robot: sumário da base SIVEP de", data, "' &&",
                 "git push"
                 )
    )
}

