#' @title  Asymptotic  likelihood ratio test on record times
#' @importFrom stats pchisq
#' @description  This function performs an asymptotic likelihood ratio test  based on the record times \eqn{L_i} to study the hypothesis of the classical record model.
#' @details The null  hypothesis  of this likelihood ratio test is that  in all the series, \eqn{m=1, ...M}, the probability of record at time \eqn{t} is \eqn{1/t}, and the alternative that the probability
#' at time \eqn{t} is any value, but equal in the \eqn{M} series.   The alternative hypothesis
#'  is more specific than the one  in \code{\link{L_MC.test}}.
#'
#'
#' Under the null, the likelihood ratio statistic has an asymptotic \eqn{\chi^2} distribution with \eqn{T-1}
#' degrees of  freedom.
#' @param XM_T A numeric matrix.
#' @param record A character string indicating the type of record, "upper" or "lower".
#' @return A list of class \code{"htest"}  with the following elements:
#' \item{statistic}{Value of the  statistic.}
#' \item{parameter}{Degrees of freedom of the approximate \eqn{\chi^2}distribution.}
#' \item{p.value}{P-value.}
#' \item{method}{A character string indicating the type of test.}
#' \item{data.name}{A character string giving the name of the data.}
#' @seealso \code{\link{L.rec}}, \code{\link{L.plot}},  \code{\link{L_MC.test}}
#' @examples
#' L_lr.test(ZaragozaSeries)
#' @export L_lr.test

L_lr.test <- function(XM_T, record = 'upper'){
  METHOD <- "Record time's likelihood ratio test"
  DNAME <- deparse(substitute(XM_T))
  XM_T <- cbind(XM_T)
  Trows <- dim(XM_T)[1]
  Mcols <- dim(XM_T)[2]
  XM_T <- cbind(I.rec.matrix(XM_T, record=record)[-1,])

  # instantes de ocurrencia - 1
  a <- list()
  L0 <- rep(0,Mcols)
  for (i in 1:Mcols){
    a[[i]] <- which(XM_T[,i]==1)
    # funcion de verosimilitud
    L0[i] <- 1/prod(c(a[[i]],Trows))
  }

  a <- unlist(a)
  alpha <- c()
  L1 <- rep(0,Trows-1)
  for (i in 1:(Trows-1)){
    alpha[i] <- length(which(a==i))
    L1[i] <- (alpha[i]/Mcols)^alpha[i]*(1-alpha[i]/Mcols)^(Mcols-alpha[i])
  }

  ELL0 <- -2*(sum(log(L0))-sum(log(L1)))

  df <- (Trows-1) # degrees of freedom

  pvalue <- pchisq(ELL0, df=df, lower.tail=FALSE)

  names(ELL0) <- "X-squared"
  names(df) <- "df"

  structure(list(statistic = ELL0, parameter = df,
                 p.value = pvalue, method = METHOD, data.name = DNAME), class='htest')
}
