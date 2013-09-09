\name{error-functions}
\alias{bias}
\alias{ccc}
\alias{error-functions}
\alias{rmse}
\title{Error functions}
\usage{
  rmse(data, pred, meas)

  bias(data, pred, meas)

  ccc(data, pred, meas, ...)
}
\arguments{
  \item{data}{A dataframe containing columns of predicted
  and measured data.}

  \item{pred}{character or numeric. Column name in
  \code{data} corresponding to predicted values.}

  \item{meas}{as above, corresponding to measured values}

  \item{...}{Further arguments passed on to
  \code{\link{epi.ccc}}}
}
\description{
  Functions for calculating error metrics from data: RMSE,
  \eqn{R^2}, bias and Lin's concordance coefficient
}

