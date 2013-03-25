\name{chBLC}
\alias{chBLC}
\title{Continuum removal by removing the convex hull}
\usage{
  chBLC(spectra)
}
\arguments{
  \item{spectra}{a matrix or data.frame with wavelengths as
  columns and spectra as rows}
}
\description{
  This function applies a linearly interpolated convex hull
  to the spectra and returns the ratio of the deviation to
  the hull value
}

