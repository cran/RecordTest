#' @title Normal test on the number of records
#' @importFrom stats pnorm
#' @description  This function performs a test  based on  \eqn{N_T}, the  number of records in the observation period,  to study the
#' hypothesis of the classical record model.
#' @details  In this test the null hypothesis is that the expected value of \eqn{N_T}, the number of records in the observation period (0,T) is
#' \eqn{\mu_T =\sum_{i=1}^T 1/i}
#' and the variance \eqn{\sigma^2_T=\sum_{i=1}^T (1/i-1/i^2)}; these are the values obtained under the classical record model (see  Arnold et al. (2008)).
#' The test statistic is based on  \eqn{\bar N_t}, the mean of the number of records up to time t, calculated from a sample of M vectors (columns in \code{XM_T}).
#' The distribution of the statistic  under the null is   asymptotically Normal.
#'
#' If the sequences of variables  in the vectors are not i.i.d, but they have a monotonous increasing trend, an unilateral alternative hypothesis must be stated, which in the case
#'  of upper records is \eqn{\mu_T >\sum_{i=1}^T 1/i} and \eqn{\sigma^2_T>\sum_{i=1}^T (1/i-1/i^2)},
#'  and in the case of lower records is  \eqn{\mu_T <\sum_{i=1}^T 1/i} and \eqn{\sigma^2_T<\sum_{i=1}^T (1/i-1/i^2)}.

#' @param XM_T A numeric matrix.
#' @param record A character string indicating the type of record to be calculated, "upper" or "lower".
#' @param type A character string indicating the type of alternative hypothesis, unilateral "uni" or bilateral "bi".
#' @return A  \code{"htest"} object  with elements:
#' \item{statistic}{Value  of the test statistic.}
#' \item{parameter}{Length of the observation  period \eqn{T}.}
#' \item{p.value}{P-value.}
#' \item{type}{A character string indicating the type of test performed.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso \code{\link{N.plot}}
#' @references
#' Arnold, B.C., Balakrishnan, N. and  Nagaraja, H.N. (2008). Record Values. En R.E. O’Malley (Ed.) A First Course in Order Statistics. SIAM.
#' @examples
#' N_normal.test(ZaragozaSeries)
#' @export N_normal.test


#'

N_normal.test <- function(XM_T, record='upper', type = 'uni'){
  if(type!='uni' && type!='bi'){
    stop(' not valid type')}
  DNAME <- deparse(substitute(XM_T))

  XM_T <- as.matrix(XM_T)
  Trows <- dim(XM_T)[1]
  Mcols <- dim(XM_T)[2]

  NT <- Nmean.rec(XM_T, record=record)[Trows]
  mu <- sum(1/(1:Trows))
  sigma <- sqrt(mu-sum(1/(1:Trows)^2))
  NT <- (NT-mu)/(sigma/sqrt(Mcols))

  if(record=='lower' && type=='uni'){
    METHOD <- "Lower records.   Unilateral  alternative hypotehsis."
    pvalue <- pnorm(NT, mean=0, sd=1, lower.tail=TRUE)
  }
  else if(record=='upper' && type=='uni'){
    METHOD <- "Upper records.   Unilateral  alternative hypotehsis."
    pvalue <- pnorm(NT, mean=0, sd=1, lower.tail=FALSE)
  }
  else{
    METHOD <- "Bilateral  alternative hypotehsis."
    pvalue <- 2*pnorm(NT, mean=0, sd=1, lower.tail=FALSE)
  }

  names(NT) <- "Z"
  names(Trows) <- "instant"

  structure(list(statistic = NT, parameter = Trows,
                 p.value = pvalue, type = METHOD, data.name = DNAME), class='htest')
}
