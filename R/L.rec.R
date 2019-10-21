#' @name L.rec
#' @rdname L.rec
#'
#' @title  Times of records
#' @description   This function  calculates   the  time (position in the vector)  where  records occur.
#'
#' @details
#' The function \code{L.rec.matrix} is equivalent to \code{L.rec} but it is applied to a matrix. The approach to obtain  times of records is applied to each
#' column of the matrix.  In this function, argument \code{X_T} is  a matrix called \code{XM_T},
#' and the return value is  a list formed by the  resulting vectors.

#'
#' @param X_T   A numeric vector.
#' @param XM_T  A numeric matrix.
#' @param record A character string indicating the type of record  to be calculated,  "upper" or "lower".
#' @return  A  vector,   containing the times of records.

#' @seealso  \code{\link{I.rec}},\code{\link{M.rec}},  \code{\link{N.rec}}, \code{\link{value.rec}}

#' @examples
#' Y1<-c(1,5,3,6,6,9,2)
#' Y2<-c(10,5,3,6,6,9,2)
#' Y3<-c(5,7,3,6,19,2,20)
#' L.rec(Y1)
#' L.rec.matrix(cbind(Y1,Y2,Y3))
#' @export L.rec
#'

L.rec <- function(X_T, record = 'upper'){
  X_T <- I.rec(X_T, record = record)
  L <- which(X_T==1)
  return(L)
}

#' @rdname L.rec
#' @export L.rec.matrix

L.rec.matrix <- function(XM_T, record = 'upper'){
  XM_T <- as.matrix(XM_T)
  L <- apply(XM_T, 2, L.rec, record = record)
  return(L)
}





