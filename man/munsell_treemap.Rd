\name{munsell_treemap}
\alias{munsell_treemap}
\title{Create Munsell treemap}
\usage{
  munsell_treemap(spectra, wavelengths, coltext = TRUE,
    otherArgs = NULL, textrange = c(1, 15))
}
\arguments{
  \item{spectra}{dataframe or matrix where each row is a
  spectrum and each column is a wavelength}

  \item{wavelengths}{integer wavelengths corresponding to
  the columns of \code{spectra}}

  \item{coltext}{logical, whether or not to plot Munsell
  colours as text}

  \item{otherArgs}{list of additional elements to be added
  to ggplot2 call}

  \item{textrange}{vector of length two indicating minimum
  and maximum text size respectively}
}
\description{
  Creates a treemap with frequency of munsell colours as
  the size of the tree rectangles using \code{treemap} and
  actual Munsell colours as the colours of rectangles using
  \code{ggplot2}.
}

