#' Reduce column dimensions
#' 
#' This function reduces the column dimensions
#' 
#' 
#' @param spectra matrix where each row is a spectrum and each column a wavelength
#' @param window window size over which the spectra will be averaged
#' @param colab ???
#'
#' @author Brendan Malone
#'   
#' @export

compSpec<- function(spectra, window, colab){
  compMat<- matrix(NA,ncol=(ncol(spectra))/window,nrow=nrow(spectra))
  cc<-1
  for (i in 1:ncol(compMat)) {
    compMat[,i]<-rowMeans(spectra[,cc:(cc+(window-1))])
    cc<-cc+window}
  colnames(compMat)<- t(as.matrix(colab))
  return(compMat)}