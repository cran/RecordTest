#' @name dailymean.fun
#' @rdname dailymean.fun
#'
#' @title Seasonal pattern  of the mean values
#' @importFrom stats lm
#' @description  This function estimates   the seasonal pattern  of the mean in a daily sequence of variables, that is the mean value of the variable at each day of the year.
#'
#' @details   This function can be used in a preliminary analysis of data, in order to  determine if the data show a seasonal behaviour, and
#' to estimate  the seasonal component.
#'
#'  Two  approaches are implemented to calculate the seasonal  pattern of the means, one based on linear combination of harmonics
#'  and the other on centered moving averages. In the last case, if \code{window=2p+1}, values obsered at  days in the interval \eqn{(t-p, t+p)}
#'  over all the years are used to calculate the mean  at \eqn{t}.
#' If \code{window=1}, only values obsered at  day \eqn{t} are used to calculate the mean
#
#' @param X_T  A numeric vector  of daily observations.   Its length  has to be higher than 365.
#' @param method A character string indicating the estimation procedure:
#'   \code{"harmonic"} or \code{"moving"}.
#' @param Nyear Integer. Number of  observed years.   First and last year may  have incomplete records.
#' @param day.year Vector  of integer  values in (1:365) giving the day  of the year where the observation was recorded.
#' If it is null,  records of complete years  are assumed.
#' @param harmonics Integer number of harmonics used in the estimation. Only used if \code{method=='harmonic'}.
#' @param window  Integer width of the moving window, which must be  an odd number. Only used if \code{method=='moving'}.
#' @return A numeric vector of length 365. Each element  is the estimated  mean  at a day of the year.

#' @examples
#' dailymean.fun(TX_Zaragoza$TX, method = 'moving', window = 1, Nyear = 66)
#' @seealso \code{\link{std.fun}}, \code{\link{dailysd.fun}}
#' @export dailymean.fun


dailymean.fun <- function(X_T,  method = 'moving',   Nyear, day.year=NULL, harmonics = 3,
                          window = 3){


 if (length(X_T)<=365 ) stop("X_T's length must be  higher than 365")
 year<- c(1, Nyear)
 if (is.null(day.year)){
   if (length(X_T)%%365!=0) stop("if day.year is NULL, X_T's length must be a multiple of 365")
   day.year <- rep(1:365, Nyear)
 }

 if (method=='harmonic') {
    aux.df <- as.data.frame(X_T)
    for (i in 1:harmonics){
      a <- day.year*2*i*pi/365
      aux.df[,2*i] <- sin(a)
      aux.df[,2*i+1] <- cos(a)
    }

    aux.lm <- lm( X_T ~ ., data=aux.df )

    dailymean <- predict( aux.lm, data=aux.df )[1:365]
  }


  else if (method=='moving') {

    if (window%%2==0) stop("window must be odd")

    window2 <- (window-1)/2
    year <- year-1
    dailymean <- rep(0,365)

     for (day in 1:365){
      V <- c()
      for (count in year[1]:year[2]){
        V <- c(V,X_T[max(0,(365*count-window2+day)):min(length(X_T),(365*count+window2+day))])
      }
      dailymean[day] <- mean(V)

    }
  }

  else stop("method must be 'harmonic' or 'moving'")

  return(dailymean)
}

