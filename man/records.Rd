% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/records.R
\name{records}
\alias{records}
\title{Record Values and Record Times}
\usage{
records(
  X,
  plot = TRUE,
  direction = c("forward", "backward", "both"),
  variable,
  type = c("lines", "points"),
  col = c(T = "black", U = "salmon", L = "skyblue", O = "black"),
  alpha = c(T = 1, U = 1, L = 1, O = 1),
  shape = c(F = 19, B = 4, O = 19),
  linetype = c(F = 1, B = 2)
)
}
\arguments{
\item{X}{A numeric vector.}

\item{plot}{Logical. If \code{TRUE} (the default) the records are plotted.}

\item{direction}{A character string indicating the type of record to show 
in the plot if \code{plot == TRUE}: \code{"forward"}, \code{"backward"} or
\code{"both"} (see Details).}

\item{variable}{Optional. A vector, containing other variable related 
to \code{X} and measured at the same times. Only used if 
\code{plot = FALSE}.}

\item{type}{Character string indicating if \code{X} is shown with 
\code{"lines"} or \code{"points"}.}

\item{col, alpha}{Character and numeric vectors of length four, respectively.
These arguments represent respectively the colour and transparency of the
points or lines: trivial record, upper records, lower records and 
observations respectively. 
Vector names in the default are only indicative.}

\item{shape}{If \code{type == "points"}. 
Integer vector of length 3 indicating the shape of the points
for forward records, backward records and observations.
Vector names in the default are only indicative.}

\item{linetype}{Integer vector of length 2 indicating the line type of the
step functions in the forward and backward records, respectively.
Vector names in the default are only indicative.}
}
\value{
If \code{plot = TRUE} a ggplot object, otherwise a list with four 
  data frames where the first column are the record times, the second the 
  record values and, if \code{variable} is not null, the third column are 
  their values at the record times, respectively for upper and lower records
  in forward and backward series.
}
\description{
This function identifies (and plots if argument 
  \code{plot = TRUE}) the record values (\eqn{R_i}), and the record times 
  (\eqn{L_i}) in a vector, for all upper and lower records in forward and
  backward directions.
}
\details{
Customarily, the records in a time series \eqn{(X_t)} observed in \eqn{T} 
  instances \eqn{t = 1, 2, \ldots, T} can be obtained using chronological 
  order. Besides, we could also compute the records in similar sequences of 
  random variables if we consider reversed chronological order starting 
  from the last observation, i.e., \eqn{t' = T, \ldots, 2, 1}. The analysis 
  of series with reversed order is customarily referred to as backward, as 
  opposed to a forward analysis.
}
\examples{
Y <- c(5, 7, 3, 6, 19, 2, 20)
records(Y, plot = FALSE, variable = seq_along(Y))

# Show the whole series and its upper and lower records
records(TX_Zaragoza$TX)
# Compute tables for the whole series
TxZ.record <- records(TX_Zaragoza$TX, plot = FALSE, variable = TX_Zaragoza$DATE)
TxZ.record
names(TxZ.record)
# To show the Forward Upper records
TxZ.record[[1]]
plot(TxZ.record[[1]]$Times, TxZ.record[[1]]$Values)

# Annual maximum daily maximum temperatures
TxZ <- apply(series_split(TX_Zaragoza$TX), 1, max)
# Plot for the records in forward and backward directions
records(TxZ, direction = "both")
# Compute tables for the annual maximum 
records(TxZ, plot = FALSE, variable = 1951:2020)

}
\seealso{
\code{\link{I.record}}, \code{\link{series_double}}, 
  \code{\link{series_rev}}, \code{\link{series_split}}, 
  \code{\link{series_uncor}}, \code{\link{series_untie}}
}
\author{
Jorge Castillo-Mateo
}
