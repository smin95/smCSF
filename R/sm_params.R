#' Calculating the four + one parameters of the contrast sensitivity function
#'
#' @description
#'
#' This function calculates the logGain, logPeakSF, log octaveWidth,
#' log Truncation factor and the cut-off spatial frequency.
#'
#' @param x
#' A vector that contains spatial frequency.
#' It is recommended that spatial frequency in linear units is provided.
#'
#' @param y
#' Numerical data of contrast sensitivity. The length of x and y must be equal.
#' It is recommended that contrast sensitivity in linear units is provided.
#'
#' @param param0
#' A vector that contains initial four parameters in log10 units in this order:
#' 1) peak gain, 2) peak spatial frequency, 3) bandwidth, and 4) truncation value. The
#' defaults are set to log10(c(100, 1, 2, 0.5)).
#'
#' @param param_upLimit
#' A vector that contains the upper limit of the four parameters (in log10 units)
#' in this order: 1) peak gain, 2) peak spatial frequency, 3) bandwidth, and
#' 4) truncation value. The defaults are set to log10(c(2000, 20, 9, 2)).
#'
#' @param param_lowLimit
#' A vector that contains the lower limit of the four parameters (in log10 units)
#' in this order: 1) peak gain, 2) peak spatial frequency, 3) bandwidth, and
#' 4) truncation value. The defaults are set to log10(c(2, 0.2, 1, 0.02)).
#'
#' @param sensCutOff
#' This refers to the sensitivity level that the user wishes to measure the cut-off spatial frequency.
#' The default is set to 1. This means that the cut-off SF is measured when the linear
#' sensitivity equals to 1.
#'
#' @importFrom stats optim
#' @export
#'
#' @examples
#' \dontrun{
#'
#' x <- c(0.6,1,2,4,8,16,24,36) # spatial frequency
#' y <- c(30, 50, 35, 25, 6, 4, 2, 1) # contrast sensitivity
#' df <- data.frame(x=x,y=y)
#'
#' sm_params(df$x, df$y)
#' }
#'
sm_params <- function(x, y,
                      param0 = log10(c(100, 1, 2, 0.5)),
                      param_upLimit = log10(c(2000, 20, 9, 2)),
                      param_lowLimit = log10(c(2, 0.2, 1, 0.02)),
                      sensCutOff = 1) {

  res <- stats::optim(par = param0, sm_fitCSF, x = x, y = y,
               lower = param_lowLimit, upper = param_upLimit,
               method = 'L-BFGS-B')

  output <- vector('double', length(param0))

  output[[1]] <- res$par[[1]] # logGain
  output[[2]] <- res$par[[2]] # log peak SF
  output[[3]] <- res$par[[3]] # log octaveWidth
  output[[4]] <- res$par[[4]] # logTrunc

  output[[5]] <- sm_cutOff(output[[1]], output[[2]], output[[3]],
                           sensCutOff = sensCutOff) # cutoff SF

  names(output) <- c('logGain', 'logPeakSF', 'logOctaveWidth',
                     'logTrunc', 'logCutOffSF')
  return(output)

}
