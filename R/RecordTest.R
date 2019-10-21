#' @title   Record test: A package for testing the classical record model
#' @description  Record test provides three families of tests to study the hypothesis  of the classical record model. It also provides some preprocessing tools
#' often needed to adequately  prepare a dataset  to apply those tests.
#' @details  This package  provides three families of tests to study the hypothesis  of the classical record model, that is that the records from  a series of
#' values observed  at regular time units (a vector) come  from  an i.i.d. series of continous random variables. A particular problem,
#' common for example  in climate problems,  where these tools can be useful, is to detect the existence of a trend in a series of variables. If   we have sequences
#' of  uncorrelated  variables with no seasonal component, the hypothesis of i.i.d.  variables is equivalent to  test the hypothesis of no trend.
#'
#' A sample of \eqn{M} vectors  uncorrelated between them is needed   to   apply   the implemented tests.
#' Then, the input  of  the functions to perform  the statistical tools is a matrix  \code{XM_T} where each column corresponds to  a vector
#' formed by the values of a series \eqn{X_t}, from \eqn{t=1, ...,T}, so that each row of the matrix correspond to a time \eqn{t}.
#'
#' In  many real problems, such as those related to environmental phenomena, the series of variables to analyze  show a seasonal behaviour,
#' and only one realization is available. In order to  be able to apply the suggested tools to detect the existence of a  trend,
#' the seasonal component has to be removed and  a sample of  \eqn{M} uncorrelated series has to be obtained, with a high enough value \eqn{M}.
#' Those  problems can be solved, preprocessing the data adequately. A wide
#'  set of tools to carry out a preliminary analysis  and to preprocess daily data  with a yearly seasonal pattern are implemented:
#' \code{\link{std.fun}}, \code{\link{dailymean.fun}},  \code{\link{dailysd.fun}},\code{\link{DaySeries.fun}},
#' \code{\link{mintime.cor.test}} and \code{\link{double.fun}}.
#'
#' There is also available a set of functions to characterize the occurrence time, the value and other statistics related to the occurrence of records:
#' \code{\link{I.rec}},  \code{\link{N.rec}},   \code{\link{Nmean.rec}}, \code{\link{M.rec}},  \code{\link{L.rec}}, \code{\link{value.rec}}.
#'
#'
#'
#' All the tests are based on the  occurrence  of records, but  three families are distinguished:  the first is based on the indicators variables of record \eqn{I_t},
#' (\code{\link{P_exactPB.test}}, \code{\link{P_chisq.test}},\code{\link{P_regression.test}}), and the second on the times of records
#' \eqn{L_i} (\code{\link{L_MC.test}}, \code{\link{L_lr.test}}). In both cases the null hypothesis is that the probabilies of record at time t are \eqn{p_t=1/t}.
#' The third family is based on  the number of records up to time t, \eqn{N_t} (\code{\link{N_normal.test}}, \code{\link{N_joint.test}}) and the null hypotehsis
#' is related to the values of  \eqn{E(N_t)} and \eqn{Var(N_t)} under the  classical record model.
#'
#' Finally, there is another set of functions aiming to plot different features related to records: \code{\link{L.plot}},  \code{\link{N.plot}},
#'  \code{\link{N_joint.plot}}, and \code{\link{P_regression.plot}}.
#'
#' All the tests and preprocessing tools can be applied to both upper and lower  records,  using the  corresponding value in the argument \code{record}.
#' @docType package
#' @name RecordTest-package
NULL
