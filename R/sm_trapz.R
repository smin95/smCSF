#' Calculation of the Area under a Curve (Trapezoidal
#' numerical integration)
#'
#' The default setting of the function converts the x and y data into
#' log units. If this is not wanted, set `logXY = FALSE`.
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
#' When this argument is set to TRUE (default), the function will convert the x and y values
#' into log10 units, and then perform the integration. If it is set to FALSE,
#' the fuction will keep the data values as they are.
#'
#' @importFrom zoo rollmean
#' @export
#' @examples
#' \dontrun{
#'
#' # trapezoidal area from linear spatial frequencies
#' # and contrast sensitivities
#' x <- c(0.6,1,2,4,8,16,24,36) # spatial frequency
#' y <- c(30, 50, 35, 25, 6, 4, 2, 1) # contrast sensitivity
#' sm_trapz(x,y,logXY=F)
#'
#' # trapezoidal area from log10 spatial frequencies and
#' # log10 contrast sensitivities
#' sm_trapz(x,y) #logXY = TRUE is the default
#' }
#'
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
