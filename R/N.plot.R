#' @title Plot of the  mean number of records  up to time t
#' @import ggplot2
#' @importFrom stats rnorm
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @description This function constructs a ggplot object  to compare  the  sample means  of the number of records
#' in a vector  up to time  \eqn{t}, \eqn{\bar N_t},   and the expected values  \eqn{E(N_t)} under the classical record model.
#'
#' @details  First, this function  calculates   the  sample means  of the number of records
#' in a vector  up to time \eqn{t}. These sample means  \eqn{\bar N_t}  are calculated from the sample of \eqn{M}  values obtained from \eqn{M} vectors,
#' the columns of matrix \code{XM_T}. Then, these values are plotted and  compared with the expected values
#'   \eqn{E(N_t)} and their  confidence intervals (CI),  under the hypothesis of the classical record model.
#'
#'  Two types of CI  of \eqn{E(N_t)}  can be built. The first uses the fact that,  under the classical record model,
#'  the statistic \eqn{\bar N_t} is asymptotically Normal.  The second is obtained  using a bootstrap approach,
#' @param XM_T A  matrix.
#' @param record A character string indicating the type of record,   \code{"upper"}, \code{"lower"} or \code{"both"}.
#' @param interval A character string indicating the type of display of the confidence intervals,  \code{"ribbon"} (grey area) or \code{"errorbar"} (vertical lines).
#' @param conf Confidence level of the confidence intervals.
#' @param bootstrap A logical flag. If \code{FALSE} calculates an asymptotically normal CI,
#' if \code{TRUE}  calculates a  Monte Carlo CI.
#' @param samples An integer  giving the number of replicates used  to calculate Monte Carlo  CI. Only used if \code{bootstrap = TRUE}.
#' @param colour Colour used to plot the expected values of \eqn{N_t}, and the CI. See  \code{\link{ggplot}} for valid values.
#' @return A ggplot object.
#' @seealso  \code{\link{N_joint.test}},  \code{\link{N_normal.test}}

#' @examples
#' Zplot<-N.plot(ZaragozaSeries,  interval='errorbar', bootstrap=TRUE, samples=200)
#' Zplot
#' library(ggplot2)
#' Zplot +
#'   theme(axis.text = element_text(size = 18), legend.position = c(0.8,0.2),
#'     legend.text = element_text(size = 25), axis.title = element_text(size = 30)) +
#'   labs(title = expression(paste("Normal CI of ", N[t])),
#'     caption = "Zaragoza Data",
#'     x = expression(paste("t")),
#'     y = expression(paste("Mean number of records ", bar(N)[t]))) +
#'   scale_y_continuous(limits = c(1, 4.5)) +
#'   guides(colour = guide_legend(order = 1,reverse=TRUE), shape = guide_legend(order = 2))

#'
#' @export N.plot

N.plot <-
  function(XM_T, record='both', interval='ribbon', conf=0.95, bootstrap=FALSE,
           samples=1000, colour='salmon'){

  XM_T <- as.matrix(XM_T)
  Trows <- dim(XM_T)[1]
  Mcols <- dim(XM_T)[2]

  # Monte Carlo CI
  if(bootstrap){
    xx.N_ <- matrix(nrow=Trows,ncol=samples)
    for (j in 1:samples){
      xx <- matrix(rnorm(Mcols*Trows), nrow=Trows, ncol=Mcols)
      xx.N_[,j] <- Nmean.rec(xx)
    }
    expectation <- apply(xx.N_,1,mean)

    CI <- matrix(nrow=2,ncol=Trows)
    q_<-(1-conf)/2
    q<-1-q_
    for (i in 1:Trows){
      CI[1,i] <- quantile(xx.N_[i,], prob = q_) #lower bound
      CI[2,i] <- quantile(xx.N_[i,], prob = q)  #upper bound
    }
  }
  ###################################

  # Normal CI
  else{
    expectation <- 1
    variance <- 0
    for (i in 2:Trows){
      expectation[i] <- expectation[i-1] + 1/i
      variance[i] <- variance[i-1] + 1/i - 1/(i*i)
    }

    sigma<-sqrt(variance/Mcols)
    CI <- matrix(nrow=2,ncol=Trows)
    q<-qnorm((1-conf)/2)
    for (i in 1:Trows){
      CI[1,i] <- expectation[i] + q*sigma[i] #lower bound
      CI[2,i] <- expectation[i] - q*sigma[i] #upper bound
    }
  }
  ###################################

  if(record=='both'){
    N_upper <- Nmean.rec(XM_T, record='upper')
    N_lower <- Nmean.rec(XM_T, record='lower')

    graf <- ggplot(data=data.frame(N_upper,N_lower), mapping = aes(x=1:Trows)) +
      geom_point(aes(y=N_upper, colour='Upper record')) +
      geom_point(aes(y=N_lower, colour='Lower record')) +
      theme_bw() + geom_line(aes(y=expectation, linetype='CI'), colour=colour) +
      theme(axis.title=element_blank(), legend.position='none') +
      scale_colour_manual(name = "", values = c('Upper record' = 'black', 'Lower record'='red')) +
      scale_linetype_manual(name = "", values = c('CI' = 1))
  }
  else {
    N <- Nmean.rec(XM_T, record=record)

    graf <- ggplot(data=data.frame(N), aes(x=1:Trows, y=N)) + geom_point() +
      theme_bw() + geom_line(aes(x=1:Trows, y=expectation),colour=colour) +
      theme(axis.title=element_blank())
  }

  if (interval=='ribbon') graf <- graf +
    geom_ribbon(aes(ymin=CI[1,], ymax=CI[2,]), alpha=0.05, colour=colour)
  else if (interval=='errorbar') graf <- graf +
    geom_errorbar(aes(ymin=CI[1,], ymax=CI[2,]), width=0.2, colour=colour)
  else warning("interval has to be 'ribbon' or 'errorbar'")

  return(graf)
}

