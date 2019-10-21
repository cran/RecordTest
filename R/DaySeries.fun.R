#' @title  Transforms a daily values vector  into  a matrix
#' @description   This function rearranges a vector of consecutive  daily values
#' into a matrix format, where each column is the vector of values at the same  day of the year.
#'

#' @details   This function is used in the data preparation  (or pre-processing) often
#' required to apply the record inference tools in this package.
#'
#'  This function transforms a daily values vector  into  a matrix, applying the following procedure: the first row  of the matrix is made up of
#' the first \code{ncols} elements of the vector, the second row by the \code{ncols} following elements, and so on.
#' The length of the vector must be  a multiple of  \code{ncols}.
#'
#' In the case of  a vector of daily values, \code{ncols} is usually 365,
#' so that the first column corresponds  to  all the values observed at the  first of January, the second to the second of January, etc,
#'

#' @param vect A  numeric vector.
#' @param ncols An integer number, giving  the number of columns in the  final matrix.
#' @return  A matrix.
#' @seealso \code{\link{double.fun}}, \code{\link{mintime.cor.test}}

#' @examples
#' DaySeries.fun(1:100, ncols = 10)
#'
#' @export DaySeries.fun

DaySeries.fun <- function(vect, ncols = 365){
  nrows <- length(vect)/ncols
  if(nrows%%1!=0) stop('Invalid number of columns')

  mm <- t(matrix(vect, ncols, nrows))

  return(mm)
}
