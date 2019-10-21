#' @name M.rec
#' @rdname M.rec
#' @aliases P.rec
#'
#' @title Number of records  in M  vectors at each time
#' @description  This function calculates the number of records at each time  \eqn{t}  in a set of \eqn{M} vectors.
#' @details Given  a matrix  formed by  \eqn{M} vectors (columns),  measured at  \eqn{T}  times  (rows), this function  calculates
#'  the number of records  in the \eqn{M} vectors at each  observed time \eqn{t}.
#'
#'  Summaries for both upper and lower  records can be calculated.
#'
#'  The function \code{P.rec}  is equivalent, but calculates the proportion of records at each time, that is the ratio:  \eqn{(number of records)/M}.
#'  This proportion is an estimation of the probability of record at that time.

#'
#' @param XM_T A numeric matrix.
#' @param record A character string indicating the type of record to be calulated,  "upper" or "lower".
#' @return  A vector with the number (proportion in the case of  \code{P.rec}) of records at each time (row).
#' @seealso \code{\link{I.rec}},  \code{\link{N.rec}}, \code{\link{value.rec}}


#' @examples
#' Y1<-c(1,5,3,6,6,9,2)
#' Y2<-c(10,5,3,6,6,9,2)
#' Y3<-c(5,7,3,6,19,2,20)
#' M.rec(cbind(Y1,Y2,Y3))
#' M.rec(ZaragozaSeries)
#' P.rec(ZaragozaSeries, record = 'lower')
#' @export M.rec


M.rec <- function(XM_T, record = 'upper'){
  XM_T <- I.rec.matrix(XM_T, record = record)
  XM_T <- apply(XM_T,1,sum)
  return(XM_T)
}

#' @rdname M.rec
#' @export P.rec

P.rec <- function(XM_T, record = 'upper'){
  XM_T <- I.rec.matrix(XM_T, record = record)
  XM_T <- apply(XM_T,1,mean)
  return(XM_T)
}

