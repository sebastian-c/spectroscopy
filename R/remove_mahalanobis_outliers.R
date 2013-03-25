#' Removal of Mahalanobis outliers
#' 
#' This function removes Mahalanobis outliers, assessing them on the basis of plsr scores
#' 
#' @param .spectra matrix where each row is a spectrum and each column is a separate wavelength. It is important that these wavelengths be at least very nearly evenly spaced.
#' @param response reponse you are trying to predict from the spectra
#' @param plsr_scores plsr scores from 1 to value to be used for calculating outliers
#' @param critical_value Chi-square critical value at which to exclude outliers
#' #@importFrom pls plsr scores
#' @importFrom mvoutlier sign1
#' @export remove_mahalanobis_outliers
#' @author Michael Nelson
#' @return A list containing the spectra and the response with outliers removed with the outlier indices

remove_mahalanobis_outliers <- function(.spectra, response, plsr_scores = 5, critical_value = 0.975){
  ## organize data
  pls_data <- data.frame(response, spectra = I(as.matrix(.spectra)))
  ##
  ## fit the model
  pls_model <- plsr(response ~spectra, data = pls_data, ncomp = plsr_scores)
  ##
  ## get the pls scores scores
  variable_scores <- scores(pls_model)
  ##
  ## calculate Mahalanobis distance on scores and test
  ## for significance
  ## uses sign1 from the mvoutlier pacakge
  outlier_test <- sign1(variable_scores, qcrit = critical_value)
  ##
  ## return data frame showing which samples are outliers
  potential_outlier <- !as.logical(outlier_test$wfinal01)
  ##
  ## return
  return_spectra <- list(spectra=.spectra[!potential_outlier,], response=response[!potential_outlier])
  attr(return_spectra, "potential_outliers") <- potential_outlier
  
  return_spectra
  ##
}
