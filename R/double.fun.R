#' @title  Transforms a \eqn{T \times M}  in a \eqn{[T/2] \times 2M} matrix
#' @description  It  changes the format of a matrix, in  the following way.
#'  First, the matrix is divided into two matrices \eqn{[T/2] \times M} ,  containing the odd  and the even rows of the original matrix, respectively,
#'  and secondly those matrices are cbinded.
#' @param mm A numeric  matrix.
#' @details   This function is used in the data preparation  (or pre-processing) often
#' required to apply the record inference tools in this package.
#'
#'  Most of the  record inference tools   require a high   number of independent series \eqn{M} (number of columns) to be applied.
#'  If \eqn{M} is low and the time period of observation, \eqn{T},  is  high enough,  the following procedure can be applied in order to double  the value \eqn{M}.
#' The approach  consists of  considering that the observations at two consesutive times, \eqn{t} and \eqn{t+1}, are independent observations   measured at the same time unit.
#' That means that we are doubling the original  time unit of the records, so that the length of the observation period  will be \eqn{[T/2]}.
#' This function rearranges  the original  data matrix  into the  new format.
#'
#' If  the  number of rows of the original matrix is even,  the first row  is deleted.
#'
#' @return  A  \eqn{[T/2] \times 2M} matrix.
#' @seealso \code{\link{DaySeries.fun}}, \code{\link{mintime.cor.test}}

#' @examples
#' double.fun(matrix(1:100,10,10))
#' @export double.fun


double.fun <- function(mm){
  nrows <- dim(mm)[1]

  if(nrows==1) stop('The matrix  must have at least two rows')
  if(nrows%%2==1) {
    mm <- mm[2:nrows,]
    nrows <- nrows-1
  }

  mm <- cbind(mm[seq(1, nrows, 2),],mm[seq(2, nrows, 2),])

  return(mm)
}
