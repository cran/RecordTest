#' @title  Removing  seasonal  components of a vector
#' @description  This  function removes a yearly  seasonal   behaviour of a vector of daily data by subtracting the means and dividing
#'   by the standard deviations, which  define a seasonal pattern, that is the means and standard deviations  calculated for
#'  each day of the year.
#'
#' @details   This function can be  used in the data preparation  (or pre-processing) often
#' required to apply the record inference tools in this package.
#'
#'  Two  approaches are implemented to calculate the mean and standard deviations defining the seasonal pattern, one based on centered moving averages and
#'    the other on a linear combination of harmonics.
#' @param X_T  A numeric vector.
#' @param method A character string indicating the  estimation procedure, \code{"harmonic"} or \code{"moving"}.
#' @param harmonics Integer number of harmonics used in the estimation. Only used if \code{method=='harmonic'}.
#' @param window  Integer width of the moving window, which must be  an odd number. Only used if \code{method=='moving'}.
#' @param Nyear Integer number of  observed years.   First and last year may   be incomplete.
#' @param day.year Vector  of integer  values in (1:365) giving the day  of the year where the observation was measured.
#'  If it is null,  365 observations per year are assumed.
#'
#' @return Input vector, standardized by the  seasonal components.
#' @seealso \code{\link{dailymean.fun}}, \code{\link{dailysd.fun}}
#' @examples
#' std.fun(TX_Zaragoza$TX, method = 'harmonic', harmonics = 2, Nyear=66)
#' @export std.fun
#'

std.fun <- function(X_T, method = 'moving', Nyear, day.year=NULL,  harmonics = 3,
                    window = 3){

  dailymean <- dailymean.fun(X_T, method=method,  Nyear=Nyear, day.year=day.year,  harmonics=harmonics, window=window)
  dailysd <- dailysd.fun(X_T, method=method,   Nyear=Nyear, day.year=day.year, harmonics=harmonics, window=window)

  StdX_T <- (X_T - dailymean) / dailysd

  return(StdX_T)
}
