#' Continuum removal by removing the convex hull
#' 
#' This function applies a linearly interpolated convex hull to the spectra and returns the ratio of the deviation to the hull value
#' 
#' @param spectra a matrix or data.frame with wavelengths as columns and spectra as rows
#' @export

chBLC<- function(spectra){
  interval<- c(1:ncol(spectra))
  hull_spectra<- matrix(NA,ncol=ncol(spectra),nrow=nrow(spectra))
  for (i in 1:nrow(spectra)){
    tempSpect= as.matrix(spectra[i,])
    data1 <- sortedXyData(interval, tempSpect)
    ## calculate convex hull
    c_hull <- chull(data1)
    ## get the appropriate region
    c_hull <- c_hull[which(c_hull == 1):length(c_hull)]
    ## calculate linear approximation between hull points
    linear_approx <- approx(data1[c_hull,], xout = interval, method = 'linear', ties = 'mean')
    ## calculate the deviation from the convex hull
    hull_spectra[i,] <- ( linear_approx[[2]] - tempSpect )/linear_approx[[2]]}
  colnames(hull_spectra) <- colnames(spectra)
  return(hull_spectra)}