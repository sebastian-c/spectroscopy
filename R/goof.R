#' Goodness of fit
#' 
#' @author Brendan Malone
#' 
#' @param observed numeric vector of observed values
#' @param predicted numeric vector of predicted values
#' @export

goof <- function(observed,predicted, ...){
  # Coefficient of determination
  rLM <- lm(predicted ~ observed)
  R2 <- as.matrix(summary(rLM)$adj.r.squared)
  
  # Standard error of prediction ^2
  SEP2 <- mean((observed - predicted)^2)
  
  # Standard error of prediction
  SEP <- sqrt(SEP2)
  
  #Bias
  bias <- mean(predicted) - mean(observed)
  
  # residual  variance
  SEP2c <- sum(((predicted - bias - observed)^2) / length(observed))
  SEPc <- sqrt(SEP2c)
  
  # ratio of performance to deviation
  RPD <- sd(observed) / SEP
  
  # Ratio of performance to interquartile distance
  IQ <- c(quantile(observed))[3] - c(quantile(observed))[2]
  RPIQ <- IQ / SEP
  
  # Concordance
  mx <- mean(observed)
  my < mean(predicted)
  s2x <- var(observed)
  s2y <- var(predicted)
  sxy <- mean((observed-mx) * (predicted-my))
  ccc <- 2 * sxy / (s2x + s2y + (mx - my)^2)
  
  eqscplot(observed,predicted, ...)
  abline(a = 0, b = 1, col = "brown4")
  
  gf <- data.frame(R2=R2, concordance=ccc, MSE=SEP2, RMSE=SEP, bias=bias, 
                   MSEc=SEP2c,RMSEc=SEPc, RPD=RPD, RPIQ=RPIQ, row.names=NULL)
  gf
}
