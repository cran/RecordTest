#' @name N.rec
#' @rdname N.rec
#'
#' @title Number of records up to time t
#' @description  This function calculates  \eqn{N_t},  the number of records  up to time \eqn{t} in a vector.
#' @details   The function \code{N.rec.matrix} is equivalent to \code{N.rec} but  the input is a matrix, and the previous procedure to obtain
#' the number of records is applied to each column of the matrix.
#' In this function, argument \code{X_T} is  a matrix called \code{XM_T}, and
#' the return value is  a matrix  formed by the output vectors obtained from each column.
#'
#' @param X_T A numeric vector.
#' @param XM_T  A numeric matrix.
#' @param record A character string indicating the type of record  to be calculated, "upper" or "lower".
#' @return A numeric vector  with the number of records up to each time (row).
#' @seealso \code{\link{Nmean.rec}}, \code{\link{L.rec}}, \code{\link{I.rec}}
#'
#' @examples
#' Y1<-c(1,5,3,6,6,9,2)
#' Y2<-c(10,5,3,6,6,9,2)
#' Y3<-c(5,7,3,6,19,2,20)
#' N.rec(Y1)
#' N.rec.matrix(cbind(Y1,Y2,Y3))
#'
#' @export N.rec

N.rec <- function(X_T, record = 'upper'){
  X_T <- cumsum(I.rec(X_T, record = record))
  return(X_T)
}

#' @rdname N.rec
#' @export N.rec.matrix

N.rec.matrix <- function(XM_T, record = 'upper'){
  XM_T <- as.matrix(XM_T)
  XM_T <- apply(XM_T, 2, N.rec, record = record)
  return(XM_T)
}



