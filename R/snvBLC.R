#' Standard Normal Variate (SNV) transformation
#' 
#' @param spectra a matrix or data.frame with wavelengths as columns and spectra as rows
#' @export

snvBLC<- function(spectra){
  spectra<-as.matrix(spectra)
  snvMat<- matrix(NA,ncol=ncol(spectra),nrow=nrow(spectra))
  for (i in 1: nrow(spectra)){
    snvMat[i,]<-(spectra[i,]- mean(spectra[i,]))/ sd(spectra[i,])}
  colnames(snvMat) <- colnames(spectra)
  return(snvMat)}