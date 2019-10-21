#' @name Nmean.rec
#' @rdname Nmean.rec

#'
#' @title Mean number of records
#' @description  This function calculates the mean  number of records up to time \eqn{t} in a vector.
#' @details    If the argument \code{XM_T} is a matrix,  the approach to obtain  the mean number of records is applied to each
#' column of the matrix, and the return value is  a matrix  formed by the corresponding vectors.
#'
#' @param XM_T  A numeric vector or  matrix.
#' @param record A character string indicating the type of record  to be calculated, "upper" or "lower".
#' @return A numeric vector   or matrix (depending on the input \code{XM_T})  with the mean  number of records up to  each time (row).
#'
#' @seealso \code{\link{N.rec}}, \code{\link{L.rec}}, \code{\link{I.rec}}
#'
#' @examples
#' Y1<-c(1,5,3,6,6,9,2)
#' Y2<-c(10,5,3,6,6,9,2)
#' Y3<-c(5,7,3,6,19,2,20)
#' Nmean.rec(Y1)
#' Nmean.rec(cbind(Y1,Y2,Y3))


#' @export Nmean.rec



Nmean.rec <- function(XM_T, record = 'upper'){
  XM_T <- N.rec.matrix(XM_T, record = record)
  XM_T <- apply(XM_T, 1, mean)
  return(XM_T)
}


