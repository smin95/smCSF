#' Contrast sensitivity function
#'
#' @description
#' It plots the contrast sensitivity function.
#'
#' The function's default converts the x and y coordinates into log scales.
#'
#' If logXY = TRUE, the plotted function will be in log xy scales.
#'
#' @param ...
#' Other arguments that are passed to control for the appearance of the
#' contrast sensitivity function, such as `alpha = 0.5` or
#' `color = sm_color('blue')`.
#'
#' @param logXY
#' When this argument is set to TRUE (default), the function will convert the x and y values
#' into log10 units, and then plot the function in log10 scales. If it is set to FALSE,
#' the function will keep the data values as they are in the linear scale.
#'
#'
#' @export
#'
#' @examples
sm_CSF <- function(..., logXY = TRUE) {
  if (logXY) {
    list(
      sm_CSF_linear(..., logXY = logXY),
      ggplot2::scale_x_continuous(trans = 'log10'),
      ggplot2::scale_y_continuous(trans = 'log10')
    )
  } else {
    sm_CSF_linear(..., logXY = logXY)
  }

}
