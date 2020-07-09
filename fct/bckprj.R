#' Back-projection function to reconstruct the infectiousness period on
#' epidemiological case series
#'
#' @param nowc The series, preferiancially nowcasted
#' @param dmax Maximum size of the series
#' @param parB Number of bootstraps
#' @return the 2 cases series, the original one and the
#' the back-projected
#'@importFrom surveillance new backprojNP 
bckprj<-function(nowc,
                 dmax=25,
                 parB){
  if(missing(dmax))
    dmax<-length(nowc)
  ## Adding the zeroes##
  nowc2<-rbind(rep(0,length(nowc)), nowc)
  ## Creating a distribution ##
  f1 <- function(x) plnorm(x, meanlog = 1.434065, sdlog = 0.6612)
  inc.pmf <- c(0,(f1(1:dmax) - f1(0:(dmax-1)))/f1(dmax))
  ## Converting nowc in a sts object ##
  sts <- new("sts", epoch=1:length(nowc2),observed=matrix(nowc2,ncol=1))
  ## List of parameters to backproj function ##
  bpnp.control <- list(k=0,eps=rep(0.005,2),iter.max=rep(250,2),B=parB,eq3a.method="C", hookFun=NULL,verbose=TRUE)
  ## Back-projecting ##
  sts.bp<-backprojNP(sts, incu.pmf=inc.pmf, control=bpnp.control)
  ## Picking a sample of bootstraps ##
  chos.bp<-sts.bp@lambda[,, sample(1:parB, 1)]
  ## Applying a Poisson distribution to resolve the back-projection ##
  chos.bp.f <- rpois(n = length(chos.bp), lambda = chos.bp)
  ## Returning the final version with the original series ##
  return(chos.bp.f)
}