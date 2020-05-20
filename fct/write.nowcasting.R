write.nowcasting <- function(now,
                            output.dir,
                            tipo = "covid", # covid, srag, obitos_covid, obitos_srag
                            data
                            ) { # objeto com output dos nowcastings

  nome.now.df <- paste0(output.dir, "nowcasting_", tipo, "_previstos_", data, ".csv")
  write.csv(now$estimates,
            file = nome.now.df,
            row.names = FALSE)

  nome.now.post <- paste0(output.dir, "nowcasting_", tipo, "_post_", data, ".csv")
  write.csv(now$params.post,
            file = nome.now.post,
            row.names = FALSE)
}
