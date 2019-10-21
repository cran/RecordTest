#' @title Record values
#' @description  This function identifies  the record values  (\eqn{R_i}), and the record times (\eqn{L_i}),  in a vector.
#' @param X_T A numeric vector.
#' @param record A character string indicating the type of record  be calculated, "upper" or "lower".
#' @param variables Optional. A matrix, containing other variables related to  \code{X_T} and measured at the same times.
#' @return A data frame where the first column are the record times,  the second  the record values and, if \code{variables} is not null,
#' the third column   are their  values  at the record times.
#' @examples
#'  Y1<-c(5,7,3,6,19,2,20)
#' value.rec(Y1)
#' value.rec(TX_Zaragoza$TX,  variable= TX_Zaragoza$DATE)
#' @export value.rec

 value.rec <- function(X_T, record = 'upper', variables = NULL){
  if (!is.null(variables)){
    variables<- as.matrix(variables)
    if(length(X_T)!=dim (variables)[1])
    stop("X_T and variables must have the same  number of rows")}
  if(record!='upper' & record!='lower')
    stop("record has to be 'upper' or 'lower'")


  I_T <- I.rec(X_T, record = record)
  L <- which(I_T==1)
  R<-X_T[L]
  if (!is.null(variables)){
    Var_L<-variables[L,]
    PosRec <- data.frame(L, R, Var_L)
  }
  else PosRec <- data.frame(L, R)


  return(PosRec)
}
