write.notificacoes.data <- function(dados,
                                    output.dir,
                                    tipo = "covid", # covid, srag, obitos_covid, obitos_srag
                                    data
                                    ){

  obitos <- c("obitos_covid", "obitos_srag")

  dir.create(output.dir, showWarnings = FALSE)

  n.notificacoes <- dados %>%
    group_by(dt_notific) %>%
    summarise(n.notific = n()) %>%
    as.data.frame()

  nome.not <- paste0(output.dir, "notificacoes_", tipo, "_", data, ".csv")

  if (tipo %in% obitos) {
    n.data  <- dados %>%
      group_by(dt_evoluca) %>%
      summarise(n.casos = n()) %>%
      as.data.frame()
  } else {
    n.data  <- dados %>%
      group_by(dt_sin_pri) %>%
      summarise(n.casos = n()) %>%
      as.data.frame()
  }

  nome.data <-  paste0(output.dir, "notificacoes_", tipo, "_", data, ".csv")

  write.csv(n.notificacoes,
            nome.not,
            row.names = FALSE)

  write.csv(n.data,
            nome.data,
            row.names = FALSE)

}
