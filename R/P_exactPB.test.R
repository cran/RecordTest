#' @name P_exactPB.test
#' @rdname P_exactPB.test
#' @title Exact Poisson binomial  test on  record probabilities
#' @importFrom Smisc pkbinom
#'
#' @description This function performs an exact test  based on the record probabiliteis \eqn{p_t} to study the hypothesis of the classical record model.
#' @details The null  hypothesis  of this likelihood ratio test is that  in all the vectors (columns of matrix \code{XM_T}), the probability of record at time \eqn{t} is \eqn{1/t}.
#' The test statistic is the  total  number of records at times \eqn{t=2, ..., T}, in  the \eqn{M} vectors. Under the null, this is the sum of \eqn{M(T-1)} independent Bernoulli variables,
#' with probabilities \eqn{p_2, ...,p_2, ..., p_T, ...p_T} with \eqn{p_t=1/t}, so that its distribution is  a Poisson-Binomial.
#'
#'
#'  Only unilateral  alternative hypotehesis \eqn{p_t > 1/t, t=2, ..., T}  or  \eqn{p_t <1/t, t=2, ..., T} are valid, since
#'  otherwise the statistic is not able to detect deviations from the null hypothesis.
#'
#' \code{\link{N_exactPB.test}}  is the same  test, but applied to only one vector, instead of M. Note in this case this test considers the probability
#' at time \eqn{t=1} (by definition of \eqn{N_t}), but the p-value  is the same.
#' @param XM_T A  matrix.
#' @param record A character string indicating the type of record to be calculated, "upper" or "lower".
#' @param method A character string indicating  the method  to calculate the Poisson binomial
#' distribution,  \code{"butler"}, \code{"naive"} or \code{"fft"}. See argument  \code{method} in \code{\link[Smisc]{pkbinom}}.
#' @return A  \code{"htest"} object  with elements:
#' \item{statistic}{Value of the likelihood ratio statistic.}
#' \item{parameter}{Number of Bernoulli independent variables summed in the statistic.}
#' \item{p.value}{P-value.}
#' \item{method}{A character string indicating the type of test performed.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso  \code{\link{P_chisq.test}},\code{\link{P_regression.test}}

#' @examples
#' P_exactPB.test(ZaragozaSeries)
#' N_exactPB.test(ZaragozaSeries[,23])

#' @rdname P_exactPB.test
#' @export P_exactPB.test


P_exactPB.test <- function(XM_T, record='upper', method = 'butler'){
  DNAME <- deparse(substitute(XM_T))

  XM_T <- as.matrix(XM_T)
  Trows <- nrow(XM_T)
  Trows_ <- Trows - 1
  Mcols <- ncol(XM_T)

  MN0 <- sum(M.rec(XM_T, record=record)[-1])
  size <- Mcols*Trows_

  if(record=='lower'){
    METHOD <- "(lower) Record indicator's exact test"
    pvalue <- pkbinom(MN0-1,size=rep(Mcols,Trows_),prob=1/(2:Trows), method = method)
  }
  else{
    METHOD <- "Record indicator's exact test"
    pvalue <- 1 - pkbinom(MN0,size=rep(Mcols,Trows_),prob=1/(2:Trows), method = method)
  }

  names(MN0) <- "Poisson-Binomial"
  names(size) <- "size"

  structure(list(statistic = MN0, parameter = size,
                 p.value = pvalue, method = METHOD, data.name = DNAME),class='htest')
}


#' @rdname P_exactPB.test
#' @export N_exactPB.test

N_exactPB.test <- function(XM_T, record = 'upper', method = 'butler'){
  DNAME <- deparse(substitute(XM_T))

  Trows <- length(XM_T)

  NT0 <- N.rec(XM_T, record=record)[Trows]

  if(record=='lower'){
    METHOD <- "(lower) Record counting process' exact test"
    pvalue <- pkbinom(NT0-1,size=rep(1,Trows),prob=1/1:Trows, method = method)
  }
  else{
    METHOD <- "Record counting process' exact test"
    pvalue <- 1 - pkbinom(NT0,size=rep(1,Trows),prob=1/1:Trows, method = method)
  }

  names(NT0) <- "Poisson-Binomial"
  names(Trows) <- "T"

  structure(list(statistic = NT0, parameter = Trows,
                 p.value = pvalue, method = METHOD, data.name = DNAME),class='htest')
}

