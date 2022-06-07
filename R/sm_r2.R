#' Calculating R-squared of the contrast sensitivity data by comparing them
#' to the predicted values based on the given model
#'
#' @description
#' This compares the log data of the contrast sensitivity function (x- and y-axes both log)
#' to the predicted values based on the contrast sensitivity function model. This fuction
#' enables the users to assess whether the given model is appropriate to plot the
#' contrast sensitivity function with their data.
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
#' @export
#'
#' @examples
#' \dontrun{
#'
#' x <- c(0.6,1,2,4,8,16,24,36) # spatial frequency
#' y <- c(30, 50, 35, 25, 6, 4, 2, 1) # contrast sensitivity
#'
#' sm_r2(x,y)
#' }
#'
sm_r2 <- function(x,y,
                  param0 = log10(c(100, 1, 2, 0.5)),
                  param_upLimit = log10(c(2000, 20, 9, 2)),
                  param_lowLimit= log10(c(2, 0.2, 1, 0.02))) {

  if (length(y) == 0) {
    return(NA)
  } else {

    params <- sm_params(x,y, param0, param_upLimit, param_lowLimit)

    y1 <- sm_findQCSF(log10(x), params[[1]], params[[2]], params[[3]], params[[4]])
    sse <- sum((y1 - log10(y))^2)
    ssr <- sum((y1- mean(log10(y)))^2)
    sst <- ssr+sse
    r2 <- ssr/sst
    return(round(r2),4)
  }

}
