#'  Multiplicative scatter correction (MSC)
#'  
#'  @param spectra a matrix or data.frame with wavelengths as columns and spectra as rows
#'  @export

mscBLC<- function(spectra){
  #calculate the mean spectrum
  meanSpec<-as.matrix(colMeans(spectra))
  mscMat<- matrix(NA,ncol=ncol(spectra),nrow=nrow(spectra))
  for (i in 1:nrow(spectra)){
    #determine the slope and intercept co-efficents 
    specLM<- lm(spectra[i,]~ meanSpec)
    specCE<-t(as.matrix(specLM$coefficients))
    #Adjust the spectra
    mscMat[i,]<- t(as.matrix((spectra[i,]-specCE[1,1])/specCE[1,2]))}
  colnames(mscMat) <- colnames(spectra)
  return(mscMat)}