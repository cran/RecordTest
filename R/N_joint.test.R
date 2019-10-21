#' @title Monte Carlo test on the number of upper and lower records
#' @importFrom stats rnorm
#' @description This function performs a Monte Carlo test  based on  the number of upper and lower  records, \eqn{N_t} and  \eqn{N_t^{low}},
#' to study the hypothesis of the classical record model.
#'

#' @details The null  hypothesis  of this  test is that  the  expected value of the number of upper and lower records  in the observation period
#' is the same \eqn{E(N_T)=E(N_T^{low})}. Two statistics can be used,   \eqn{\bar N_t- \bar N_t^{low}},  or its standardized version;
#' \eqn{\bar N_t} and \eqn{\bar N_t^{low}} are the mean of the number of records up to time \eqn{t}, calculated from a samples of \eqn{M} vectors (columns in \code{XM_T}).
#'
#'  These statistics are useful  when the alternative hypotehsis is that the  sequences of variables in the vectors have a monotonou trend. If the trend is increasing
#'   the statistic will take high values while if it is decreasing, it will take low values. The type of trend has to be specified at
#'   argument \code{trend} to calculate  the adequate p-value.
#'
#' The distribution  of the statistic  under the hypothesis of the classical record model is obtained using a bootstrap approach.
#'
#'
#' @param XM_T A  matrix.
#' @param stand Logical flag. If \code{TRUE}  uses a standarized version of the test statistic.
#' @param samples An integer specifying the number of replicates used in the Monte Carlo approach.
#' @param trend A character string indicating the type of trend ("increasing" or "decreasing")  of the alternative hypothesis.
#' @return A list with class \code{"htest"} containing the following components:
#' \item{statistic}{Value of the likelihood ratio statistic.}
#' \item{p.value}{P-value.}
#' \item{samples}{Number of samples  used in the Monte Carlo approach.}
#' \item{method}{A character string indicating if the statistic is standardized or not.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso  \code{\link{N_joint.plot}}, \code{\link{N.plot}}, \code{\link{N_normal.test}}
#' @examples
#' N_joint.test(ZaragozaSeries, stan=TRUE, samples=200)
#' @export N_joint.test

N_joint.test <- function(XM_T, stand = FALSE, samples = 1000, trend="increasing"){
  DNAME <- deparse(substitute(XM_T))

  if((trend!='increasing')&(trend!='decreasing'))  stop("trend has to be 'increasing' or 'decreasing'")

  XM_T <- as.matrix(XM_T)
  Trows <- nrow(XM_T)
  Mcols <- ncol(XM_T)

  if(stand){
    METHOD <- "Standardized statistic"
    NN <- meanNNsd(XM_T)
    NMT0 <- NN[1,]*sqrt(Mcols) / NN[2,]
  }
  else {
    METHOD <- "Non-standardized statistic"
    NMT0 <- meanNNsd(XM_T)[1,]
  }

  NMT0B <- rep(0,samples)
  if(stand){
    for (j in 1:samples){
      xx <- matrix(rnorm(Mcols*Trows), nrow=Trows, ncol=Mcols)
      NNB <- meanNNsd(xx)
      NMT0B[j] <- NNB[1,]*sqrt(Mcols) / NNB[2,]
    }}
  else{
    for (j in 1:samples){
      xx <- matrix(rnorm(Mcols*Trows), nrow=Trows, ncol=Mcols)
      NMT0B[j] <- meanNNsd(xx)[1,]
    }}

   if (trend=="increasing")     I <- ifelse((NMT0B>NMT0),1,0)
   else    I <- ifelse((NMT0B<NMT0),1,0)
    pvalue <- sum(I)/samples


  names(NMT0) <- "NMT0"
  names(samples) <- "samples"

  structure(list(statistic = NMT0, samples = samples,
                 p.value = pvalue, method = METHOD, data.name = DNAME), class='htest')
}

meanNNsd <- function(XM_T){
  NMin <- N.rec.matrix(XM_T, record = 'lower')[nrow(XM_T),]
  NMax <- N.rec.matrix(XM_T, record = 'upper')[nrow(XM_T),]
  NSub <- NMax - NMin
  NN <- rbind(mean(NSub), sd(NSub))
  return(NN)
}
