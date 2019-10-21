#' @title Plot of the  difference (or ratio) between the number of upper and lower records
#' @import ggplot2
#' @importFrom stats rnorm
#' @importFrom stats quantile
#' @description This function constructs a ggplot object  to display the difference (or ratio) between the mean number of  upper records and
#' the mean number of lower records.
#' The expected values and  confidence intervals  of   \eqn{E(N_t)-E(N_t^{low})} (or  \eqn{E(N_t)/E(N_t^{low})}),
#'    under the hypothesis of the classical record model, are also plotted.
#'
#' @details First, this function  calculates    the difference (or ratio) between the mean number of  upper records and
#' the  mean number of lower records up to time t, that is  \eqn{\bar N_t- \bar N_t^{low}} (or \eqn{\bar N_t/ \bar N_t^{low}}),
#'  The sample means  \eqn{\bar N_t} and \eqn{\bar N_t^{low}} are calculated from the sample of values  \eqn{ N_t} and \eqn{N_t^{low}},
#'   obtained from \eqn{M} vectors (columns of matrix \code{XM_T}). Then, these values are plotted and  compared with the functions of the expected values
#' \eqn{E(N_t)-E(N_t^{low})} (or \eqn{E(N_t)/E(N_t^{low})})   and their  confidence intervals  under the hypothesis of the classical record model,which are
#'  obtained using a bootstrap approach.

#' @param XM_T A  matrix.
#' @param type A character string indicating the type of statistic to be plotted, "ratio" or "difference".
#' @param interval A character string indicating the type of display of the confidence intervals,  \code{"ribbon"} (grey area) or \code{"errorbar"} (vertical lines).
#' @param conf Numeriv value in \eqn{(0,1)}. Confidence level of the confidence intervals.
#' @param samples An integer  giving the number of replicates used  to calculate Monte Carlo  confidence intervals.
#' @param colour Colour used to plot the expected values  and the CI. See  \code{\link{ggplot}} for valid values.
#' @return A ggplot graph object.
#' @seealso  \code{\link{N_joint.test}}, \code{\link{N.plot}}, \code{\link{N_normal.test}}
#' @examples
#' N_joint.plot(ZaragozaSeries, samples=200)
#' @export N_joint.plot

N_joint.plot <-
  function(XM_T, type='ratio', interval='ribbon', conf=0.95,   samples=1000, colour='salmon'){

  XM_T <- as.matrix(XM_T)
  Trows <- dim(XM_T)[1]
  Mcols <- dim(XM_T)[2]

  if(type=='difference') XM_T <- Nmean.rec(XM_T) - Nmean.rec(-XM_T)
  else if(type=='ratio') XM_T <- Nmean.rec(XM_T) / Nmean.rec(-XM_T)
  else stop("method has to be 'ratio' or 'difference'")

  # Monte Carlo CI
  xx.N_lower <- matrix(nrow=Trows,ncol=samples)
  xx.N_upper <- matrix(nrow=Trows,ncol=samples)
  for (j in 1:samples){
    xx <- matrix(rnorm(Mcols*Trows), nrow=Trows, ncol=Mcols)
    xx.N_lower[,j] <- Nmean.rec(-xx)
    xx.N_upper[,j] <- Nmean.rec(xx)
  }

  if(type=='difference') xx.statistic <- xx.N_upper - xx.N_lower
  else xx.statistic <- xx.N_upper / xx.N_lower

  statistic <- apply(xx.statistic,1,mean)

  IC <- matrix(nrow=2,ncol=Trows)
  q_<-(1-conf)/2
  q<-1-q_
  for (i in 1:Trows){
    IC[1,i] <- quantile(xx.statistic[i,], prob = q_) #lower bound
    IC[2,i] <- quantile(xx.statistic[i,], prob = q)  #upper bound
  }
  ##################

  graf <- ggplot(data=data.frame(XM_T), aes(x=1:Trows, y=XM_T)) + geom_point() +
    theme_bw() + geom_line(aes(x=1:Trows, y=statistic),colour=colour) +
    theme(axis.title=element_blank())
  if (interval=='ribbon') graf <- graf +
    geom_ribbon(aes(ymin=IC[1,], ymax=IC[2,]), alpha=0.05, colour=colour)
  else if (interval=='errorbar') graf <- graf +
    geom_errorbar(aes(ymin=IC[1,], ymax=IC[2,]), width=0.2)
  else warning("interval has to be 'ribbon' or 'errorbar'")

  return(graf)
}
