#' Calculating the residual sum of squares with predicted values and the raw data of contrast
#' sensitivity
#'
#' @description
#' Using the four parameters from the Quick-CSF method, this function
#' compares the differences between the raw data (y) and the predicted data. The
#' predicted data are calculated from the four parameters at the given
#' spatial frequencies (x). This difference is computed as the residual sum of squares (RSS).
#' If the RSS is low, then the predicted values from the given model
#' are highly representative of the actual raw data.
#'
#' @param param
#' This is a vector that should contain four values in this order:
#' 1) peak gain (log10 units), 2) peak spatial frequency (log10 units),
#' 3) bandwidth (log10 units), and 4) truncation value (log10 units).
#'
#' @param x
#'
#' A vector of spatial frequencies from which the contrast sensitivity data
#' have been collected. This should be the linear values of the tested spatial frequencies, not log.
#'
#' @param y
#'
#' Linear units of contrast sensitivity data, not log.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(0.25,0.35,0.48,0.68,0.94,1.31,1.83,2.54) # spatial frequency
#' y <- c(141,172,190,187,164,129,90.3,57.1) # averaged contrast sensitivity
#' params <- sm_params(x,y)
#' sm_fitCSF(params,x,y) # sum of squares between y and the predicted y
#' from the CSF model
#;
#' }
#'
sm_fitCSF <- function(param, x, y) {
  y1 <- sm_findQCSF(log10(x), param[[1]], param[[2]], param[[3]], param[[4]])
  rss <- sum((y1 - log10(y))^2)
  return(rss)
}
