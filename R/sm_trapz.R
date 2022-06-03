#' Calculation of the Area under a Curve (Trapezoidal
#' numerical integration)
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
#' @importFrom zoo rollmean
#'
#' @examples
#'
sm_trapz <- function(x,y, logXY = TRUE) {
  if (logXY) {
    x <- log10(x)
    y <- log10(y)
  }
  id <- order(x)
  res <- sum(diff(x[id])* zoo::rollmean(y[id],2))
  return(res)
}
