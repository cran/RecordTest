#' @title Plot of  record probabilities
#' @import ggplot2
#' @importFrom stats rnorm
#' @description This function constructs a ggplot object  to display different functions of the  record probabilities at time t, \eqn{p_t}.
#' @details  Three different types of plots which aim to analyse the hypothesis of the record classic model  using the record probabilities
#' are implemented. Estimations of the record probabilities \eqn{\hat p_t} used in the plots are obtained as the proportion of records at time t
#' in M vectors (columns of matrix \code{XM_T}).
#'
#' Type 1 is the plot of  the estimated  record probabilities \eqn{p_t} versus time.   The expected probabilities
#' under the record classic model, \eqn{p_t=1/t},  are also plotted, together with bootstrap confidence  intervals.  Type 3 is
#' the same  plot but on a logarithmic scale, so that the expected value is \eqn{-log(t)}.
#'
#'  Type  2 is the plot of the observed values \eqn{t \hat p_t}  versus time.  The expected values
#' under the  classical record model are 1 for any value \eqn{t}, so that a cloud of points around 1 and  with no trend should be expected.  The estimated values
#' are plotted, together with  bootstrap confidence  intervals. In addition,  a regression line  is fitted to the cloud of points and
#' plotted together with  confidence  intervals of the response. If the classical record model is  true, the  confidence  band (in grey)
#' should contain  the horizontal line equal to 1. Plots of type 2 are  easier to interpret than types 1 and 3.
#'
#'
#' @param XM_T A matrix.
#' @param record A character string indicating the type of record to be calculated, \code{"upper"} or \code{"lower"}.
#' @param plot One of the values 1, 2 or 3. It determines the type of plot to be displayed.   See Details.
#' @param interval A character string indicating the type of display of the confidence intervals,  \code{"ribbon"} (grey area) or \code{"errorbar"} (vertical lines).
#' @param conf Numeric value in (0,1). Confidence level of the confidence intervals.
#' @param samples An integer  giving the number of replicates used  to calculate Monte Carlo  CI.
#' @param colour_point Colour used to plot the points. See  \code{\link{ggplot}} for valid values.
#' @param colour_CI Colour used to plot the expected values  and the CI.
#' @param color_lm Colour used to plot the regression line. Only  used if \code{plot==2}.
#' @return  A ggplot object.
#' @seealso \code{\link{P_regression.test}}

#' @examples
#' P_regression.plot(ZaragozaSeries,  plot=2, interval='errorbar', samples=200)
#'
#' @export

P_regression.plot <- function(XM_T, record='upper', plot=2, interval='ribbon', conf=0.95, samples=5000,
                    colour_point='black', colour_CI='salmon', color_lm='royalblue4'){

  XM_T <- as.matrix(XM_T)
  Trows <- nrow(XM_T)
  Mcols <- ncol(XM_T)
  t <- 1:Trows

  Pt_ <- P.rec(XM_T, record = record)

  # Monte Carlo CI
  xx.Pt_ <- matrix(nrow=Trows,ncol=samples)
  for (j in 1:samples){
    xx <- matrix(rnorm(Mcols*Trows), nrow=Trows, ncol=Mcols)
    xx.Pt_[,j] <- P.rec(xx)
  }

  CI <- matrix(nrow=2,ncol=Trows)
  q_<-(1-conf)/2
  q<-1-q_
  for (i in 1:Trows){
    CI[1,i] <- quantile(xx.Pt_[i,], prob = q_) # lower bound
    CI[2,i] <- quantile(xx.Pt_[i,], prob = q)  # upper bound
  }
  ####################

  if (plot==1){
    pt <- 1/t
    graf <- ggplot(data=data.frame(t,Pt_), aes(x=t, y=Pt_)) +
      theme_bw() + geom_line(aes(x=t, y=pt),colour=colour_CI) +
      geom_point(colour=colour_point)  +xlab("t")+ylab("p_t")
  #    theme(axis.title=element_blank())+xlab("t")
  }
  ###################################
  else if (plot==2){
    t_ <- t*Pt_
    CI[1,] <- t*CI[1,]
    CI[2,] <- t*CI[2,]
    VAR <- Mcols/(1:(Trows-1))
    graf <- ggplot(data=data.frame(t,t_), aes(x=t, y=t_)) +
      theme_bw() + geom_line(aes(x=t, y=rep(1,Trows)),colour=colour_CI) +
      geom_smooth(method = lm, mapping = aes(weight = c(0,VAR)), colour=color_lm) +
      geom_point(colour=colour_point) +xlab("t")+ylab("t*p_t")
#      theme(axis.title=element_blank())
  }
  ###################################
  else if (plot==3) {
    pt <- 1/t
    CI[1,] <- log(CI[1,])
    CI[2,] <- log(CI[2,])
    graf <- ggplot(data=data.frame(log(t),log(Pt_)), aes(x=log(t), y=log(Pt_))) +
      theme_bw() + geom_line(aes(x=log(t), y=log(pt)),colour=colour_CI) +
      geom_point(colour=colour_point)  +xlab("log(t)")+ylab("log(p_t)")
#      theme(axis.title=element_blank())
  }
  ###################################
  else stop("plot has to be 1, 2 or 3")

  if (interval=='ribbon') graf <- graf +
    geom_ribbon(aes(ymin=CI[1,], ymax=CI[2,]), alpha=0.05, colour=colour_CI)
  else if (interval=='errorbar') graf <- graf +
    geom_errorbar(aes(ymin=CI[1,], ymax=CI[2,]), width=0.2, colour=colour_CI)
  else warning('interval not found')

  return(graf)
}
