#' @title  Extracts a subset of uncorrelated  vectors
#' @importFrom stats cor.test
#' @description  Given a  a set of \eqn{M} vectors,  this function extracts  a subset of  them which are uncorrelated.
#' @details This function is used in the data preparation  (or pre-processing) often
#' required to apply the record inference tools in this package.
#'
#'  Given a  a set of \eqn{M} vectors, which are the columns of matrix \code{XM_T}, this function  extracts the  biggest subset of uncorrelated vectors (columns),
#'  using the following procedure: starting from  column \code{m},
#' the test \code{\link{cor.test}} is applied  to study the correlation between columns \code{m} and \code{m} + 1, \code{m} + 2, ... an so on  up to find  a
#' column \code{m} + k which  is not significantly correlated with column \code{m}. Then,  the process is repeated starting at column \code{m} + k.
#' @param XM_T A numeric matrix  where the uncorrelated vectors are extracted from.
#' @param m  Integer valu giving the starting column.
#' @param alpha   Numeric value in \eqn{(0,1)}. It gives the  sgnificance level of the correlation test.
#' @return A vector with the index of the uncorrelated columns in the matrix.

#' @seealso \code{\link{DaySeries.fun}}, \code{\link{double.fun}}

#' @examples
#' ZM_T <- DaySeries.fun(TX_Zaragoza$TX)
#' mintime.cor.test(ZM_T)
#' @export mintime.cor.test
#'

mintime.cor.test <- function(XM_T, m = 1, alpha = 0.05){
  Mcols <- dim(XM_T)[2]
  sep <- c()
  while (m<=(Mcols-1)){
    sep <- c(sep,m)
    separation <- 0
    pv <- 0
    while (pv < alpha & m+separation<=(Mcols-1)){
      separation <- separation+1
      pv <- cor.test(XM_T[,m],XM_T[,m+separation])[[3]]
    }
    m <- m+separation
  }
  # correlacion del ultimo con el primero
  a <- c(XM_T[,1],NA)
  b <- c(NA,XM_T[,sep[length(sep)]])
  pv<-cor.test(a,b)[[3]]
  while (pv < alpha){
    sep <- sep[-length(sep)]
    b <- c(NA,XM_T[,sep[length(sep)]])
    pv<-cor.test(a,b)[[3]]
  }
  return(sep)
}
