write.notificacoes.data <- function(dados,
                                    output.dir="./",
                                    tipo = c("covid", "srag", "obitos_covid", "obitos_srag",
                                             "obitos_covid_data_sin", "obitos_srag_data_sin"),    
                                    data="_",
                                    write.arq = TRUE) {
    tipo <- match.arg(tipo)
    obitos <- c("obitos_covid", "obitos_srag") ## PI: casos em que o onset date é a data de evolução
    
    n.notificacoes <- dados %>%
        group_by(dt_notific) %>%
        dplyr::summarise(n.notific = n()) %>%
        as.data.frame()
    
    nome.not <- paste0(output.dir, "notificacoes_", tipo, "_", data, ".csv")
    
    if (tipo %in% obitos) {
        n.data  <- dados %>%
            group_by(dt_evoluca) %>%
            dplyr::summarise(n.casos = n()) %>%
            as.data.frame()
        nome.data <- paste0(output.dir, "n_casos_data_", tipo, "_", data, ".csv")
    } else {
        n.data  <- dados %>%
            group_by(dt_sin_pri) %>%
            dplyr::summarise(n.casos = n()) %>%
            as.data.frame()
        nome.data <-  paste0(output.dir, "n_casos_data_sintoma_", tipo, "_", data, ".csv")
    }
    if(write.arq){
        write.csv(n.notificacoes,
                  nome.not,
                  row.names = FALSE)
        
        write.csv(n.data,
                  nome.data,
                  row.names = FALSE)
    }
    if(!write.arq) ## PIP: nova opção para ter a lista com objetos ao inves de csvs
        list(n.not = n.notificacoes, n.data = n.data)
}
