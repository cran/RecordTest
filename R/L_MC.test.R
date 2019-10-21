#' @title  Monte Carlo likelihood ratio test on record times
#' @importFrom stats pchisq
#' @description  This function  performs a likelihood ratio test  based on the record times \eqn{L_i} to study the hypothesis of the classical record model using
#' a Monte Carlo approach.
#' @details The null  hypothesis  of this likelihood ratio test is that  in all the series, \eqn{m=1, ..., M}, the probability of record at time \eqn{t} is \eqn{1/t},
#' and the alternative that the probability at time \eqn{t} is any value, in any of the \eqn{M} series.  The alternative hypothesis
#'  is more general than the one  in \code{\link{L_lr.test}}.
#'
#'
#'    The statistic is the likelihood ratio statistic,  and the p-value
#' is obtained using a Monte Carlo approach.
#' @param XM_T A matrix.
#' @param record A character string indicating the type of record to be calculated,  "upper" or "lower".
#' @param samples An integer specifying the number of replicates used in the Monte Carlo approach.
#' @return A  \code{"htest"} object  with elements:
#' \item{statistic}{Value of the likelihood ratio statistic.}
#' \item{p.value}{P-value}
#' \item{method}{A character string indicating the type of test.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso \code{\link{L.rec}}, \code{\link{L.plot}},  \code{\link{L_lr.test}}
#' @examples
#' L_MC.test(ZaragozaSeries, samples = 200)
#' @export L_MC.test


L_MC.test <- function(XM_T, record = 'upper', samples = 1000){
  METHOD <- "Monte Carlo likelihood ratio test"
  DNAME <- deparse(substitute(XM_T))
  XM_T <- as.matrix(XM_T)
  Trows <- dim(XM_T)[1]
  Mcols <- dim(XM_T)[2]
  XM_T <- as.matrix(I.rec.matrix(XM_T, record = record)[-1,])

  # moments of occurrence - 1
  a <- list()
  L0 <- rep(0,Mcols)
  for (i in 1:Mcols){
    a[[i]] <- which(XM_T[,i]==1)
    # likelihood function
    L0[i] <- 1/prod(c(a[[i]],Trows))
  }

  ELL0 <- -2*sum(log(L0))

  ############################
  ELL0B <- rep(0,samples)
  for (j in 1:samples){
    xx <- matrix(rnorm(Mcols*Trows), nrow=Trows, ncol=Mcols)
    xx <- as.matrix(I.rec.matrix(xx)[-1,])
    aB <- list()
    L0B <- rep(0,Mcols)
    for (i in 1:Mcols){
      aB[[i]] <- which(xx[,i]==1)
      L0B[i] <- 1/prod(c(aB[[i]],Trows))
    }
    ELL0B[j] <- -2*sum(log(L0B))
  }

  if(record=='upper') I <- ifelse((ELL0B>ELL0),1,0)
  else I <- ifelse((ELL0B<ELL0),1,0)

  pvalue <- sum(I)/samples
  ############################
  names(ELL0) <- "ELL0"

  structure(list(statistic = ELL0, p.value = pvalue,
                 method = METHOD, data.name = DNAME), class='htest')
}
