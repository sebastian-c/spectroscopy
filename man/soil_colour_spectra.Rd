\name{soil_colour_spectra}
\alias{soil_color_spectra}
\alias{soil_colour_spectra}
\title{Load spectra and extract RGB and Munsell colour}
\usage{
  soil_colour_spectra(spectra, wavelengths,
    include_id = FALSE)
}
\arguments{
  \item{spectra}{matrix or data.frame with spectra as rows
  and wavelengths as columns}

  \item{wavelengths}{vector of wavelengths corresponding to
  the columns of spectra}

  \item{include_id}{if TRUE, adds an id column which is
  simply an integer from \code{1:nrow(spectra)}}
}
\description{
  Converts spectra reflectance into RGB and Munsell colours
}
\author{
  Michael Nelson
}

