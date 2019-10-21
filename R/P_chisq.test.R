#' @title Chi-Square test on  record probabilities
#'
#' @description This function performs a chi-square test  based on the record probabiliteis \eqn{p_t} to study the hypothesis of the classical record model.
#' @details The null  hypothesis  of this likelihood ratio test is that  in all the vectors (columns of matrix \code{XM_T}), the probability of record at time \eqn{t} is \eqn{1/t},
#' and the alternative that the probabilities are not equal to those values.  First,  the chi-square  goodness of fit
#' statistics to study the  null hypotehsis  \eqn{H_0: p_t=1/t} are calculated for  each time \eqn{t=2, ...,T}, where the observed value is the number of records  at time \eqn{t}
#' in the \eqn{M} vectors and the expected value under the null is \eqn{M/t}.   The test staitstic is the sum
#' of the previous \eqn{T-1} statistics and  its distribution under the null is  approximately \eqn{\chi^2_{T-1}}.
#'
#' The chi-square approximation may not be valid with low \eqn{M}, since it requires expected values \eqn{> 5} or up to 20 \% of the expected values is between 1 and 5.
#' If  this condition is not satisfied,  a warning is displayed.
#'
#' @param XM_T A matrix.
#' @param record A character string indicating the type of record to be calculated,  "upper" or "lower".
#' @return A  \code{"htest"} object  with elements:
#' \item{statistic}{Value of the likelihood ratio statistic.}
#' \item{df}{Degrees of freedom of the approximate chi-squared.}
#' \item{p.value}{P-value.}
#' \item{method}{A character string indicating the type of test performed.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso \code{\link{P_exactPB.test}}, \code{\link{P_regression.test}}
#' @examples
#' P_chisq.test(ZaragozaSeries)
#' @export P_chisq.test
#'

P_chisq.test <- function(XM_T, record = 'upper'){
  METHOD <- "Approximate chi-squared test"
  DNAME <- deparse(substitute(XM_T))

  XM_T <- as.matrix(XM_T)
  Trows <- nrow(XM_T)
  Trows_ <- Trows - 1
  Mcols <- ncol(XM_T)
  chi <- rep(0,Trows_)
  Mt <- M.rec(XM_T, record = record)

  for (i in 2:Trows){
    esperado1 <- Mcols/i
    esperado2 <- Mcols - esperado1
    chi[i-1] <- (Mt[i]-esperado1)^2/esperado1 + (Mcols-Mt[i]-esperado2)^2/esperado2
  }
  chi <- sum(chi)
  pvalue <- pchisq(chi, df=Trows_, lower.tail=FALSE)


  if ((Mcols/Trows < 5 && length(which(Mcols/2:Trows < 5)) > 0.2*Trows_ ) ||
      Mcols/Trows < 1 || Mcols < 30)
    warning("Chi-squared approximation may  not be valid")

  names(chi) <- "X-squared"
  names(Trows_) <- "df"

  structure(list(statistic = chi, df = Trows_,
                 p.value = pvalue, method = METHOD, data.name = DNAME),class='htest')
}
