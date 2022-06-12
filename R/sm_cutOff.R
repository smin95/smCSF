#' Calculating log cutoff spatial frequency
#'
#' @description
#' This is used to analyze whether sensitivity at higher spatial frequency
#' is improved. The higher the cutoff SF, the better the sensitivity at higher
#' spatial frequency. This value, by definition, is the spatial frequency where
#' log-sensitivity = 0.
#'
#' @param logGain
#' This refers to peak gain (i.e., peak sensitivity) of the contrast sensitivity function.
#' It should be provided in log10 units.
#' @param logCenter
#' This refers to peak spatial frequency (i.e., center of the contrast sensitivity function).
#' It should be provided in log10 units.
#' @param octaveWidth
#' This refers to the bandwidth of the contrast sensitivity function. It should be provided in log10
#' units.
#'
#' @examples
#' \dontrun{
#'
#' x <- c(0.6,1,2,4,8,16,24,36) # spatial frequency
#' y <- c(30, 50, 35, 25, 6, 4, 2, 1) # contrast sensitivity
#' df <- data.frame(x=x,y=y)
#' param0 = log10(c(100, 1, 2, 0.5))
#' param_upLimit = log10(c(2000, 20, 9, 2)) # lower limits of the parameters
#' param_lowLimit= log10(c(2, 0.2, 1, 0.02)) # upper limits of the parameters
#' params <- sm_params(df$x, df$y, param0, param_upLimit, param_lowLimit)
#' sm_cutOff(params[[1]], params[[2]], params[[3]]) # cutoff spatial frequency
#' }}
#'
#' @export
#'
sm_cutOff <- function(logGain, logCenter, octaveWidth) {

  cutoffSF <- 10^logCenter * 10^(octaveWidth * sqrt(logGain/log10(2)))
  logCutOffSF <- log10(cutoffSF)

  return(logCutOffSF)
}
