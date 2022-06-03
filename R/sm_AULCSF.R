#' Calculation of the Area under a Curve (area under the log CSF, i.e., AULCSF)
#'
#' @param x
#' This is the scalar spacing of the coordinate, such as the spatial frequency.
#' It is recommended that spatial frequency in linear units is provided.
#'
#' @param y
#' Numerical data of contrast sensitivity. The length of x and y must be equal.
#' It is recommended that contrast sensitivity in linear units is provided.
#'
#' @param logXY
#' When this argument is set to TRUE, the function will convert the x and y values
#' into log10 units, and then perform the integration. If it is set to FALSE,
#' the fuction will keep the data values as they are.
#'
#' @param n
#' Number of predicted data that are generated from the given model.
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
#'
#' @importFrom zoo rollmean
#'
#' @examples
sm_AULCSF <- function(x,y, logXY = TRUE, n = 500,
                      param0 = log10(c(100, 1, 2, 0.5)),
                      param_upLimit = log10(c(2000, 20, 9, 2)),
                      param_lowLimit= log10(c(2, 0.2, 1, 0.02))) {
  if (logXY) {
    x <- log10(x)
    y <- log10(y)

    rng <- range(x, na.rm = TRUE)
    grid <- data.frame(x = seq(rng[1], rng[2], length = n))
    #return(grid)
    params <- sm_params(10^x, 10^y, param0, param_upLimit,
                            param_lowLimit)

    grid$y <- sm_findQCSF(grid$x, params[[1]], params[[2]],
                       params[[3]], params[[4]])
  } else {

    rng <- range(x, na.rm = TRUE)
    grid <- data.frame(x = seq(rng[1], rng[2], length = n))
    #return(grid)
    params <- sm_params(x, y, param0, param_upLimit,
                            param_lowLimit)

    grid$y <- 10^(sm_findQCSF(log10(grid$x), params[[1]], params[[2]],
                           params[[3]], params[[4]]))

  }
  id <- order(grid$x)
  res <- sum(diff(grid$x[id])* zoo::rollmean(grid$y[id],2))
  return(res)
}
