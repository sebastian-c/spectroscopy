\name{fit_bag_plsr}
\alias{fit_bag_plsr}
\title{Fit bootstrap aggregated PLSR}
\usage{
  fit_bag_plsr(soilv, spec, nbag, maxc)
}
\arguments{
  \item{soilv}{soil variables of interest}

  \item{spec}{matrix or dataframe containing spectra. Each
  row represents a spectrum, each column a wavelength.}

  \item{nbag}{integer. Number of bootstrap parameters.}

  \item{maxc}{integer. Maximum number of components for
  PLSR.}
}
\description{
  Fit bootstrap aggregated PLSR
}
\author{
  Budiman Minasny
}
\seealso{
  \code{\link{predict_bag_plsr}}
}

