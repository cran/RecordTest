#' @name I.rec
#' @rdname I.rec
#'
#' @title Indicator  of record  occurrence
#' @description  Given  a vector,  this function calculates  a binay variable  which takes the value 1 if the corresponding value in the vector is
#' a record and 0 otherwise.
#' @details  Indicators of both upper and lower  records can be calculated.
#'
#' The function \code{I.rec.matrix} is equivalent to \code{I.rec} but it is applied to a matrix. The  previous procedure to obtain a binary variable is applied to each
#' column of the matrix. In this function, argument \code{X_T} is  a matrix called \code{XM_T}, and
#' the return value is  a matrix  formed by the corresponding binary variables.

#'
#' @param X_T  A numeric vector.
#' @param XM_T  A numeric matrix.
#' @param record A character string indicating the type of record  to be calculated,  "upper" or "lower".
#' @return  A binary vector,  indicating the  record occurrence.
#' @seealso \code{\link{M.rec}},  \code{\link{N.rec}}, \code{\link{I.rec}}, \code{\link{value.rec}}

#' @examples
#' Y1<-c(5,7,3,6,19,2,20)
#' I.rec(Y1)
#' I.rec.matrix(ZaragozaSeries)


#' @export I.rec
#'
#'
#'



I.rec <- function(X_T, record = 'upper'){
  if(record!='upper' & record!='lower')
    stop("record has to be 'upper' or 'lower'")

  if(record=='lower'){
    I <- c(1,(diff(cummin(X_T))<0)) }
  else
    I <- c(1,(diff(cummax(X_T))>0))

  return(I)
}

#' @rdname I.rec
#' @export I.rec.matrix
#'

I.rec.matrix <- function(XM_T, record = 'upper'){
  XM_T <- as.matrix(XM_T)
  I <- apply(XM_T, 2, I.rec, record = record)
  return(I)
}

