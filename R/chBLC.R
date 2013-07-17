#' Continuum removal by removing the convex hull
#' 
#' This function applies a linearly interpolated convex hull to the spectra and returns the ratio of the deviation to the hull value
#' 
#' @param spectra a matrix or data.frame with wavelengths as columns and spectra as rows
#' @export

chBLC<- function(spectra){
  interval <- seq_len(ncol(spectra))
  hull_spectra <- matrix(NA,ncol=ncol(spectra),nrow=nrow(spectra))
  for (i in seq_len(nrow(spectra))){
    tempSpect <- as.matrix(spectra[i,])
    data1 <- sortedXyData(interval, tempSpect)
    ## calculate convex hull
    c_hull <- chull(data1)
    ## get the appropriate region: the points of the polygon over the spectra
    if(tail(c_hull, 1) == 1L){
      ## If the first point is the last point, just use the first and last point
      c_hull <- c_hull[c(1, length(c_hull))]
      } else {
        c_hull <- c_hull[which(c_hull == 1):length(c_hull)]
      }
    ## calculate linear approximation between hull points
    linear_approx <- approx(data1[c_hull,], xout = interval, method = 'linear', ties = 'mean')
    ## calculate the deviation from the convex hull
    hull_spectra[i,] <- ( linear_approx[[2]] - tempSpect )/linear_approx[[2]]}
  colnames(hull_spectra) <- colnames(spectra)
  return(hull_spectra)}