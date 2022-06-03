#' Calculating the four parameters of the contrast sensitivity function
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
#' 1) peak gain, 2) peak spatial frequency, 3) bandwidth, and
#' 4) truncation value.
#'
#' @param param_upLimit
#' A vector that contains the upper limit of the four parameters (in log10 units)
#' in this order: 1) peak gain, 2) peak spatial frequency, 3) bandwidth, and
#' 4) truncation value.
#'
#' @param param_lowLimit
#' A vector that contains the lower limit of the four parameters (in log10 units)
#' in this order: 1) peak gain, 2) peak spatial frequency, 3) bandwidth, and
#' 4) truncation value.
#'
#' @return
#' @export
#' @importFrom stats optim
#'
#' @examples
sm_params <- function(x, y,
                          param0, param_upLimit,
                          param_lowLimit) {

  res <- stats::optim(par = param0, sm_fitCSF, x = x, y = y,
               lower = param_lowLimit, upper = param_upLimit,
               method = 'L-BFGS-B')

  output <- vector('double', length(param0))

  output[[1]] <- res$par[[1]] # logGain
  output[[2]] <- res$par[[2]] # logCenter
  output[[3]] <- res$par[[3]] # octaveWidth
  output[[4]] <- res$par[[4]] # logTrunc

  names(output) <- c('logGain', 'logCenter', 'logOctaveWidth',
                     'logTrunc')
  return(output)

}
