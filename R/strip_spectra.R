#' Stripping wavelengths from spectra
#' 
#' This function subsets a chosen range of wavelengths from the spectra. It can 
#' 
#' @param spectra a matrix of spectra where each row represents a spectrum and each column a wavelength
#' @param datawavs a numeric or integer vector of all wavelengths
#' @param wavlimits minimum and maximum wavelengths desired. Must be present in \code{datawavs}
#' @param which take every nth measurement of \code{wavlimits}
#' 
#' @export strip_spectra
#' 
#' @author Sebastian Campbell
#' 

strip_spectra <- function(spectra, datawavs, wavlimits=range(datawavs), which=1){
  stopifnot(length(wavlimits) == 2)
    
  datawavs <- as.numeric(datawavs)  
  
  limits <- which(datawavs %in% wavlimits)
  if(length(limits) != 2) stop("There should be 2 limits. Are both elements of wavlimits present in datawavs?")
  
  kept_index <- seq(limits[1], limits[2], which)
  
  trimmed_spectra <- spectra[,kept_index]
  kept_names <- datawavs[kept_index]
  
  colnames(trimmed_spectra) <- kept_names
  attr(trimmed_spectra, "kept_names") <- kept_names
  attr(trimmed_spectra, "discarded_names") <- datawavs[!seq_along(datawavs) %in% kept_index]
  
  trimmed_spectra  
}
