#' @title Plot of the  times of record
#' @import ggplot2
#' @description  This function constructs a ggplot object  to display  the  times of record.
#' @details  The function  can be applied to plot the record times in a vector (if argument \code{XM_T} is a vector)
#'  or to plot and compare the record times in a set of vectors (if argument \code{XM_T} is a matrix). In the latter case,
#'  the approach to obtain  the times of records is applied to each column of the matrix.
#'
#'
#' @param XM_T A numeric vector or matrix.
#' @param record A character string indicating the type of record to be calculated, "upper" or "lower".
#' @param backward Logical flag. If  \code{TRUE}, the  input vector is  reversed before calculating the  times of records.
#' @param colour_point Colour  to plot  points.
#' @param colour_line Colour  to plot lines.
#' @return A ggplot object.
#' @seealso \code{\link{L.rec}}, \code{\link{L_lr.test}}
#' @export L.plot



#' @examples
#'
#' Y1<-c(1,5,3,6,6,9,2, 11, 17, 8)
#' L.plot(Y1)
#' L.plot(ZaragozaSeries)


L.plot <-
  function(XM_T, record='upper', backward=FALSE, colour_point='salmon', colour_line='grey95'){
  XM_T <- as.matrix(XM_T)
  Mcols <- dim(XM_T)[2]

  if(backward) XM_T <- apply(XM_T, 2, rev)

  XM_T <- I.rec.matrix(XM_T, record = record)

  L <- cbind(which(XM_T[,1]==1)-1,1)
  if(Mcols > 1)  for (i in 2:Mcols) L <- rbind(L, cbind(which(XM_T[,i]==1)-1,i))

  y_breaks <- ceiling(sqrt(Mcols))

  graf <- ggplot(data=data.frame(L), aes(x=L[,1], y=L[,2]))  + theme_classic() +
    geom_hline(aes(yintercept = L[,2]), colour=colour_line) + geom_point(colour=colour_point) +
    scale_y_continuous(breaks=seq(1, Mcols, y_breaks)) +
    theme(axis.title=element_blank())

  return(graf)
}
