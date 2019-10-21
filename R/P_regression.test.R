#' @title Regression test on  record probabilities
#' @importFrom car linearHypothesis
#' @importFrom stats lm
#' @importFrom stats coef
#' @description This function performs a  test   based on a regression on the  record probabilities \eqn{p_t} to study the hypothesis of the classical record model.
#' @details The null  hypothesis  of this likelihood ratio test is that  in all the vectors (columns in matrix \code{XM_T}), the probability of record at time \eqn{t} is \eqn{1/t},
#'  so that  \eqn{tp_t=1}. Then,   hypothesis \eqn{H_0: p_t=1/t,  \ t=2, ..., T} is equivalent to \eqn{H_0: \beta_0=1,\beta_1=0}
#' where  \eqn{\beta_0}  and \eqn{\beta_1} are the coefficients of the regression model \eqn{t  p_t=\beta_0+ \beta_1  t}.
#' The  model has to be estimated by weighted  least squares since the response is heteroskedastic.
#'
#' The  F statistic is used to compare the regression model under the null and a linear regression model with no restriction (the alterantive hypotehsis is then that
#' \eqn{tp_t} is a linear function of time). This alternative hypotehsis may be reasonable in  many real examples, but not always.
#'
#' @param XM_T A  matrix
#' @param record A character string indicating the type of records to be calculated, "upper" or "lower".
#' @return A  \code{"htest"} object  with elements:
#' \item{statistic}{Value of the likelihood ratio statistic.}
#' \item{intercept}{Estimated intercept of the regression line \eqn{\hat \beta_0}.}
#' \item{slope}{Estimated slope of the regression \eqn{\hat \beta_1}.}
#' \item{p.value}{P-value.}
#' \item{method}{A character string indicating the type of test performed.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso \code{\link{P_exactPB.test}}, \code{\link{P_chisq.test}},\code{\link{P_regression.plot}}
#'
#' @examples
#' P_regression.test(ZaragozaSeries)
#' @export P_regression.test
#'

P_regression.test <- function(XM_T, record = 'upper'){
  METHOD <- "Regression F test. Upper records"
  if (record=='lower') METHOD <- "Regression F test. Lower records"
  DNAME <- deparse(substitute(XM_T))

  XM_T <- as.matrix(XM_T)
  Trows <- nrow(XM_T)
  Mcols <- ncol(XM_T)

  # weigths
  VAR <- Mcols/(1:(Trows-1))
  Pt_ <- P.rec(XM_T, record=record)[-1]
  t <- 2:Trows
  t.Pt_ <- t*Pt_

  model <- lm(t.Pt_~t, weights = VAR)

  pv <- linearHypothesis(model, matrix(c(1,0,0,1), 2, 2, byrow=TRUE), rhs=c(1,0))[[6]][2]

  intercept <- coef(model)[1]
  coefficient <- coef(model)[2]

  names(intercept) <- "(intercept)"
  names(coefficient) <- "t"

  structure(list(intercept = intercept, slope = coefficient,
                 p.value = pv, method = METHOD, data.name = DNAME), class='htest')
}
