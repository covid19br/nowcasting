#' Calcula R efetivo sobre as trajetórias de nowcasting retornadas pela função NobBS.posterior
#' @param R.method função parcialmente aplicada que calcula Re de uma trajetória
#' @param trajectories data.frame com trajetórias de nowcasting
#' @param Nsamples int número de amostrar das trajetórias de nowcasting
#' @param quantiles logical retorna quantis da distribuição posterior de R ou as osteriores completas?
#' @param ... argumentos extras são repassados pro ldply: .parallel=T é especialmente útil
Re.nowcasting <- function(R.method,
                          trajectories,
                          Nsamples,
                          quantiles = TRUE,
                          ...) {
  N <- ncol(trajectories)
  if (missing(Nsamples) | Nsamples > N - 1)
    Nsamples <- N - 1
  ## trajetórias são auto-correlacionas, melhor sampling é com maior distância
  ## possível entre os índices - usamos intervalos regulares
  samples <- round(seq(2, N, length.out = Nsamples))
  fun <- function(traj){
    casos <- fill.dates(data.frame(onset = trajectories$date, n.casos = traj), 2)
    res <- R.method(casos$incidence)$r_sample
  }
  re.df <- ldply(trajectories[, samples], fun, .id = NULL, ...)
  # run a single time to get full output
  casos <- fill.dates(data.frame(onset = trajectories$date, n.casos = trajectories[, 2]), 2) ## PIP: estava usando a coluna 1 do trajectories, que eram as datas
  res <- R.method(casos$incidence)
  re.df["t_start", ] <- res$R$t_start
  re.df["t_end", ] <- res$R$t_end

  if (!quantiles)
      return(re.df)
  # Calculate quantiles and moments
  results <- posteriors(re.df)
}
