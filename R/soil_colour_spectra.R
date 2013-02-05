#' Load spectra and extract RGB and Munsell colour
#' 
#' Converts spectra reflectance into RGB and Munsell colours
#' 
#' @aliases soil_color_spectra
#' @importFrom plyr adply
#' @importFrom plyr splat
#' @importFrom plyr llply
#' @importFrom munsell rgb2mnsl
#' 
#' @param spectra matrix or data.frame with spectra as rows and wavelengths as columns
#' @param wavelengths vector of wavelengths corresponding to the columns of spectra
#' 
#' @export soil_colour_spectra
#' @author Michael Nelson

soil_colour_spectra <- function(spectra, wavelengths){
  ## this function loads spectra
  ## and returns the rgb colour and munsell colour
  ##
  ## find r,g,b colour
  rgb_colours <- adply(spectra, 1, spectra_to_RGB, all_wavelengths = wavelengths)
  ##
  ## get munsell colour
  munsell_colours <- splat(function(red,green,blue, ...){rgb2mnsl(R=red,G=green,B=blue)})(rgb_colours)
  ##
  ## return
  data.frame(rgb_colours, munsell = munsell_colours)
}


in_interval <- function(.all, .interval,...){
  ## index for an interval
  ## a function to subset a particular waveband interval
  .in_interval = .all %in% .interval
}

mean_interval <- function(.data, .index){
  ## returns the mean for given indices
  mean(.data[.index])
}

spectra_to_RGB <- function(.spectra, all_wavelengths,
                           rgb_list = list(blue = list(.interval=450:520), red = list(.interval=600:690), green=list(.interval=520:600)), ...) {
  ## a function to return the average values in the
  ## red, green and blue sections of the spectrum
  ## would work on any intervals
  ##
  ## get the appropriate indices
  interval_list <- llply(rgb_list, splat(in_interval), .all=all_wavelengths)
  ##
  ## get the average in these subsets
  rgb_values <- lapply(interval_list, mean_interval, .data=.spectra)
  ##
  ## convert to colour
  colour <- with(rgb_values, rgb(red, green, blue))
  ##
  ## return data frame
  with(rgb_values, data.frame(red, green, blue, colour))
}
