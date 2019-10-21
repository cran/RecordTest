#' @name dailysd.fun
#' @rdname dailysd.fun
#'
#' @title Seasonal pattern  of the standar deviation
#' @importFrom stats sd
#' @importFrom stats lm
#' @importFrom stats predict
#' @description  This function estimates   the seasonal pattern of the standard deviation in a daily sequence of variables, that is the  standard deviation
#' of the variable at each day of the year.
#' @details   This function can be used in a preliminary analysis of data, in order to  determine if the data show a seasonal behaviour, and
#' to estimate  the seasonal component.
#'
#'  Two  approaches are implemented to calculate the seasonal  pattern of the standard deviation, one based on linear combination of harmonics
#'  and the other on centered moving standard deviations. In the last case, if \code{window=2p+1}, values obsered at  days in the interval\eqn{(t-p, t+p)}
#'  over all the years are used to calculate the standard deviation  at \eqn{t}.
#' If \code{window=1}, only values obsered at  day \eqn{t} are used to calculate the standard deviation
#' @param X_T  A numeric vector  of daily observations.   Its length  has to be higher than 365.
#' @param method A character string indicating the estimation procedure:
#'   \code{"harmonic"} or \code{"moving"}.
#' @param Nyear An integer. Number of  observed years.   First and last year may  have incomplete records.
#' @param day.year A vector  of integer  values in (1: 365) giving the day  of the year where the observation was recorded. If it is null,  records of complete years  are assumed.
#' @param harmonics  Integer number of harmonics used in the estimation. Only used if \code{method=='harmonic'}.
#' @param window Integer width of the moving window, which must be  an odd number. Only used if \code{method=='moving'}.
#' @return A numeric vector of length 365. Each element  is the estimated  standard deviation  at a day of the year.
#' @seealso \code{\link{std.fun}}, \code{\link{dailymean.fun}}
#' @examples dailysd.fun(TX_Zaragoza$TX, method = 'moving', window = 1, Nyear = 66)

#'
#' @export dailysd.fun




dailysd.fun <- function(X_T, method = 'moving',  Nyear, day.year=NULL, harmonics = 3,
                        window = 3){

  if (length(X_T)<=365 ) stop("X_T's length must be  higher than 365")
  year<- c(1, Nyear)
  if (is.null(day.year)){
    if (length(X_T)%%365!=0) stop("if day.year is NULL, X_T's length must be a multiple of 365")
    day.year <- rep(1:365, Nyear)
  }

  if (method=='harmonic') {
    X_T <- X_T[(365*(year[1]-1)+1):(365*year[2])]
    aux.df <- as.data.frame(X_T)
    day.year <- rep(1:365, length(X_T)/365)

    for (i in 1:harmonics){
      a <- day.year*2*i*pi/365
      aux.df[,2*i] <- sin(a)
      aux.df[,2*i+1] <- cos(a)
    }

    aux.lm <- lm( X_T ~ ., data=aux.df )
    aux.mean <- predict( aux.lm, data=aux.df  )
    aux.df$resid2 <- (X_T-aux.mean)^2
    aux.lm <- lm( resid2 ~ . - X_T, data=aux.df )
    dailysd <- sqrt( predict( aux.lm, data=aux.df ) )[1:365]
  }

  else if (method=='moving') {

    if (window%%2==0) stop("window must be odd")

    window2 <- (window-1)/2
    year <- year-1
    dailysd <- rep(0,365)

    for (day in 1:365){
      V<-c()
      for (count in year[1]:year[2]){
        V <- c(V,X_T[max(0,(365*count-window2+day)):min(length(X_T),(365*count+window2+day))])
      }
      dailysd[day]<-sd(V)
    }
  }

  else stop("method must be 'harmonic' or 'moving'")

  return(dailysd)
}
