# Estimate posterior probabilities for nowcasting AND SI variation
posteriors <- function(data){
  r_sample = data[-which(row.names(data) %in% c("t_start","t_end")),]
  mean_posterior <- apply(r_sample, 2, mean, na.rm = TRUE)
  std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
  quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                    0.025,
                                    na.rm = TRUE
  )
  quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                   0.05,
                                   na.rm = TRUE
  )
  quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                   0.25,
                                   na.rm = TRUE
  )
  median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
  quantile_0.75_posterior <- apply(r_sample, 2, quantile,
                                   0.75,
                                   na.rm = TRUE
  )
  quantile_0.95_posterior <- apply(r_sample, 2, quantile,
                                   0.95,
                                   na.rm = TRUE
  )
  quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                    0.975,
                                    na.rm = TRUE
  )

  results <- list(R = as.data.frame(
                      cbind(as.numeric(data["t_start", ]),
                            as.numeric(data["t_end", ]),
                            mean_posterior,
                            std_posterior,
                            quantile_0.025_posterior,
                            quantile_0.05_posterior,
                            quantile_0.25_posterior,
                            median_posterior,
                            quantile_0.75_posterior,
                            quantile_0.95_posterior,
                            quantile_0.975_posterior)))

  names(results$R) <- c(
      "t_start",
      "t_end",
      "Mean(R)",
      "Std(R)",
      "Quantile.0.025(R)",
      "Quantile.0.05(R)",
      "Quantile.0.25(R)",
      "Median(R)",
      "Quantile.0.75(R)",
      "Quantile.0.95(R)",
      "Quantile.0.975(R)"
  )

  return(results)  
}
