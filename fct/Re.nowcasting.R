#' Calcula R efetivo sobre as trajetórias de nowcasting retornadas pela função NobBS.posterior
#' @param R.method função parcialmente aplicada que calcula Re de uma trajetória
#' @param trajectories data.frame com trajetórias de nowcasting
#' @param Nsamples int número de amostrar das trajetórias de nowcasting
#' @param argumentos extras são repassados pro ldply: .parallel=T é especialmente útil
Re.nowcasting <- function(R.method,
                          trajectories,
                          Nsamples,
                          ...) {
  if (missing(Nsamples))
      Nsamples <- ncol(trajectories) - 1
  # trajetórias são auto-correlacionas, melhor sampling é com maior distância
  # possível entre os índices - usamos intervalos regulares
  samples <- round(seq(1, N, length.out = Nsamples))
  fun <- function(traj){
    casos <- fill.dates(data.frame(onset = trajectories$date, n.casos = traj), 2)
    res <- R.method(casos$incidence)$r_sample
  }
  re.df <- ldply(samples, fun, ...)
  if (is.na(quantiles))
      return(re.df)
  # Calculate quantiles and moments
  results <- posteriors(re.df)
}
