#' Error functions
#' 
#' Functions for calculating error metrics from data: RMSE, \eqn{R^2}, bias and Lin's concordance coefficient
#' 
#' @name error-functions
#' @aliases rmse bias ccc
#' @param data A dataframe containing columns of predicted and measured data.
#' @param pred character or numeric. Column name in \code{data} corresponding to predicted values.
#' @param meas as above, corresponding to measured values
#' @param ... Further arguments passed on to \code{\link{epi.ccc}}
#' 
#' @importFrom epiR epi.ccc

#' @rdname error-functions
#' @export rmse
#' @export bias
#' @export ccc

rmse <- function(data, pred, meas){
  c(RMSE=sqrt(mean((data[,pred]-data[,meas])^2, na.rm=TRUE)))
}

#' @rdname error-functions
#' @export
bias <- function(data, pred, meas){
  c(bias=mean(data[,pred]-data[,meas], na.rm=TRUE))
}

#' @rdname error-functions
#' @export
ccc <- function(data, pred, meas, ...){
  c("Lin's CCC" = epi.ccc(data[, pred], data[, meas], ...)$rho.c$est)
}