#' Contrast sensitivity function
#'
#' @description
#' It plots the contrast sensitivity function.
#'
#' The function's default converts the x and y coordinates into log scales.
#'
#' @param mapping
#' This argument is to establish the x and y axes. The columns which represent
#' x and y should be inside `aes()`.
#'
#' @param data
#' This argument refers to the variable that stores the data frame that contains
#' the data that is to be plotted.
#'
#' @param position
#' Position adjustment.
#'
#' @param na.rm
#' This argument should be set to TRUE or FALSE. If TRUE, the missing values
#' are removed without warnings. If FALSE< the missing values are removed
#' with warnings.
#'
#' @param show.legend
#' This argument should be set to TRUE or FALSE. If TRUE, the aesthetics that are
#' mapped in `aes()` will be included in the legend. If FALSE, these will
#' not be included.
#'
#' @param inherit.aes
#' If `FALSE`,the function bypasses the default aesthetics. The default
#' is set to `TRUE`.
#'
#' @param n
#' Number of predicted data that are generated from the given model.
#'
#' @param logXY
#' When this argument is set to TRUE (default), the function will convert the x and y values
#' into log10 units, and then perform the integration. If it is set to FALSE,
#' the function will keep the data values as they are.
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
#' @param ...
#' Other arguments that are passed to control for the appearance of the
#' contrast sensitivity function, such as `alpha = 0.5` or
#' `color = sm_color('blue')`.
#'
#' @importFrom ggplot2 ggproto Geom draw_key_polygon layer Stat aes
#' @importFrom grid polygonGrob gList
#' @importFrom stats complete.cases
#'
#' @export
#' @examples
#' \dontrun{
#' x <- c(0.6,1,2,4,8,16,24,36) # spatial frequency
#' y <- c(30, 50, 35, 25, 6, 4, 2, 1) # contrast sensitivity
#' df <- data.frame(x=x,y=y)
#'
#' # linear plot of CSF
#' ggplot(data=df,aes(x,y)) +
#' geom_point() +
#' sm_CSF(logXY=FALSE)
#'
#' # log plot of CSF
#' ggplot(data=df,aes(log10(x), log10(y))) +
#'  geom_point() +
#'  sm_CSF()
#'
#'  # log plot of CSF
#'  ggplot(data=df,aes(x,y)) +
#'  geom_point() +
#'  sm_CSF() +
#'  scale_x_continuous(trans='log10')+
#'  scale_y_continuous(trans='log10')
#'
#' }
#'
sm_CSF <- function(mapping = NULL, data = NULL,
                   position = 'identity', na.rm = FALSE,
                   show.legend = NA, inherit.aes = TRUE, n = 100,
                   logXY = TRUE,
                   param0 = log10(c(100, 1, 2, 0.5)),
                   param_upLimit = log10(c(2000, 20, 9, 2)),
                   param_lowLimit= log10(c(2, 0.2, 1, 0.02)),
                   ...) {

  ggplot2::layer(
    stat = StatSmCSF, data = data, mapping = mapping, geom = GeomSmCSF,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, param0 = param0,
                  param_upLimit = param_upLimit,
                  param_lowLimit = param_lowLimit,
                  logXY = logXY, ...)
  )

}
#'
#' @rdname sm_CSF
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
#'
StatSmCSF <- ggplot2::ggproto('StatSmCSF', ggplot2::Stat,
                     required_aes = c('x','y'),

                     compute_group = function(data, scales, param0,
                                              param_upLimit,
                                              param_lowLimit,
                                              logXY,
                                              n) {

                       rng <- range(data$x, na.rm = TRUE)
                       grid <- data.frame(x = seq(rng[1], rng[2], length = n))

                       if (logXY) {
                         params <- sm_params(10^data$x, 10^data$y,
                                                 param0, param_upLimit,
                                                 param_lowLimit)
                         grid$y <- sm_findQCSF(grid$x, params[[1]], params[[2]],
                                  params[[3]], params[[4]])
                       } else {
                         params <- sm_params(data$x, data$y,
                                                 param0, param_upLimit,
                                                 param_lowLimit)
                         grid$y <- 10^sm_findQCSF(log10(grid$x), params[[1]], params[[2]],
                                               params[[3]], params[[4]])
                       }

                       grid

                     })

#' @rdname sm_CSF
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom
#' @export

GeomSmCSF <- ggplot2::ggproto('GeomSmCSF', ggplot2::Geom,
                     default_aes = ggplot2::aes(
                       fill = 'gray80',
                       color = 'black',
                       size = 1,
                       linetype = 1,
                       weight = 1,
                       alpha = 0.6,
                     ),

                     setup_params = function(data, params) {
                       params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE, ambiguous = TRUE)
                       params
                     },


                     setup_data = function(data, params) {
                       GeomLine$setup_data(data, params)
                     },

                     draw_group = function(data, panel_params, coord,
                                           flipped_aes = FALSE) {

                       path <- transform(data, alpha = NA)


                       grid::gList(
                         GeomLine$draw_panel(path, panel_params, coord)
                       )

                     },
                     draw_key = ggplot2::draw_key_smooth,
                     required_aes = c('x','y')
)


