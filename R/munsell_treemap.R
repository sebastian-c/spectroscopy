#' Create Munsell treemap
#' 
#' Creates a treemap with frequency of munsell colours as the size of the tree rectangles using \code{treemap} and actual Munsell colours as the colours of rectangles using \code{ggplot2}.
#' 
#' @importFrom treemap tmPlot
#' @importFrom munsell rgb2mnsl
#' @importFrom munsell mnsl
#' @importFrom plyr splat
#' @import ggplot2
#' @export munsell_treemap
#' 
#' @param spectra dataframe or matrix where each row is a spectrum and each column is a wavelength
#' @param wavelengths integer wavelengths corresponding to the columns of \code{spectra}.
#' @param coltext logical, whether or not to plot Munsell colours as text.

munsell_treemap <- function(spectra, wavelengths, coltext=TRUE){
  treemap_data <- munsell_tm(spectra, wavelengths)
  
  treemap <- ggplot(treemap_data, aes(xmin=x0, xmax=x0+w, ymin=y0, ymax=y0+h))+
    geom_rect(aes(fill=hex))+
    scale_fill_identity()+
    scale_size_continuous()+
    scale_size_log10()+
    theme(axis.title=element_blank())
    
  if(coltext) treemap <- treemap + geom_text(aes(label=munsell, x=x0+0.5*w, y=y0+0.5*h, size=Freq))
    
  print(treemap)
}

munsell_tm <- function(spectra, wavelengths){
  rgb_colours <- adply(spectra, 1L, spectra_to_RGB, all_wavelengths = wavelengths)
  munsell <- splat(function(red,green,blue, ...){rgb2mnsl(R=red, G=green, B=blue)})(rgb_colours)
    
  munsell_table <- as.data.frame(table(munsell))
  
  pdf(file=NULL)
  raw_tmdata <- tmPlot(munsell_table, index="munsell", vSize="Freq")[[1L]][[1L]]
  dev.off()
  
  ordered_munsell <- sapply(raw_tmdata[,-c(ncol(raw_tmdata)-0:3)], levels)
  names(ordered_munsell) <- NULL
  
  rect_coords <- data.frame(munsell=ordered_munsell, 
                       raw_tmdata[,c(ncol(raw_tmdata)-3:0)])
  rect_coords$hex <- mnsl(rect_coords$munsell)
  rect_coords <- merge(rect_coords, munsell_table)
  
  rect_coords

}