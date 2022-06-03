#' Filling the area of the contrast sensitivity function
#'
#' @description
#' Conventionally, the area of the contrast sensitivity function is filled
#' to describe how one has conducted data analysis or just for aesthetic
#' purposes. This function fills the area of the contrast sensitivity function.
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
#' @param outline.type
#' This argument specifies whether the upper and lower lines of the area
#' should be drawn (`both`), or upper line (`upper`) or lower line (`lower`).
#' `Full` draws a polygon.
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
#' When this argument is set to TRUE, the function will convert the x and y values
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
#'
#' @importFrom ggplot2 ggproto Geom draw_key_polygon layer Stat aes
#' @importFrom grid polygonGrob gList
#' @importFrom stats complete.cases
#'
#'
#' @examples
#'
sm_areaCSF <- function(mapping = NULL, data = NULL,
                       position = 'identity', na.rm = FALSE,
                       outline.type = 'upper',
                       show.legend = NA, inherit.aes = TRUE, n = 100,
                       logXY = TRUE,
                       param0 = log10(c(100, 1, 2, 0.5)),
                       param_upLimit = log10(c(2000, 20, 9, 2)),
                       param_lowLimit= log10(c(2, 0.2, 1, 0.02)),
                       ...) {

  outline.type <- rlang::arg_match0(outline.type,
                                    c('both', 'upper', 'lower', 'full'))


  ggplot2::layer(
    stat = StatSmAreaCSF, data = data, mapping = mapping, geom = GeomSmAreaCSF,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, param0 = param0,
                  param_upLimit = param_upLimit,
                  param_lowLimit = param_lowLimit,
                  logXY = logXY, ...)
  )


}

#' @rdname sm_areaCSF
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
#'
StatSmAreaCSF <- ggplot2::ggproto('StatSmAreaCSF', ggplot2::Stat,
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

                           grid$ymax <- grid$y
                           grid$ymin <- rep(0, length(grid$y))
                           grid
                         })

#' @rdname sm_areaCSF
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom aes
#' @export
#'
GeomSmAreaCSF <- ggplot2::ggproto('GeomSmAreaCSF', ggplot2::Geom,
                         default_aes = ggplot2::aes(
                           fill = 'gray80',
                           color = NA,
                           size = 0.5,
                           linetype = 1,
                           alpha = NA
                         ),

                         setup_params = function(data, params) {
                           params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE, ambiguous = TRUE)
                           params
                         },

                         setup_data = function(data, params) {
                           GeomLine$setup_data(data, params)
                         },

                         draw_group = function(data, panel_params, coord,
                                               lineend = 'butt',
                                               linejoin = 'round',
                                               linemitre = 10,
                                               na.rm = FALSE,
                                               flipped_aes = FALSE,
                                               outline.type = 'both') {

                           path <- transform(data, alpha = NA)

                           data <- flip_data(data, flipped_aes)
                           if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
                           data <- data[order(data$group), ]

                           # Check that aesthetics are constant
                           aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
                           if (nrow(aes) > 1) {
                             cli::cli_abort("Aesthetics can not vary along a ribbon")
                           }
                           aes <- as.list(aes)

                           missing_pos <- !stats::complete.cases(data[c('x','ymin','ymax')])
                           ids <- cumsum(missing_pos) + 1
                           ids[missing_pos] <- NA

                           positions_upper <- vctrs::new_data_frame(list(
                             x = data$x,
                             y = data$y,
                             id = ids
                           ))

                           positions_lower <- vctrs::new_data_frame(list(
                             x = rev(data$x),
                             y = rev(data$ymin),
                             id = rev(ids)
                           ))

                           positions_upper <- flip_data(positions_upper, flipped_aes)
                           positions_lower <- flip_data(positions_lower, flipped_aes)

                           munched_upper <- coord_munch(coord, positions_upper, panel_params)
                           munched_lower <- coord_munch(coord, positions_lower, panel_params)

                           munched_poly <- rbind(munched_upper, munched_lower)

                           is_full_outline <- identical(outline.type, 'full')


                           g_poly <- grid::polygonGrob(
                             munched_poly$x, munched_poly$y, id = munched_poly$id,
                             default.units = 'native',
                             gp = grid::gpar(
                               fill = alpha(aes$fill, aes$alpha),
                               col = if (is_full_outline) aes$colour else NA,
                               lwd = if (is_full_outline) aes$size * .pt else 0,
                               lty = if (is_full_outline) aes$linetype else 1,
                               lineend = lineend,
                               linejoin, linejoin,
                               linemitre = linemitre
                             )
                           )

                           if (is_full_outline) {
                             return(g_poly)
                           }

                           munched_lower$id <- munched_lower$id +
                             max(ids, na.rm = TRUE)

                           munched_lines <- switch(outline.type,
                                                   both = rbind(munched_upper, munched_lower),
                                                   upper = munched_upper,
                                                   lower = munched_lower,
                                                   cli::cli_abort(c(
                                                     'invalid {.arg outline.type}: {.val {outline.type}}',
                                                     'i' = 'use either {.val upper}, {.val lower}, or {.val both}'
                                                   ))
                           )

                           g_lines <- grid::polylineGrob(
                             munched_lines$x, munched_lines$y, id = munched_lines$id,
                             default.units = 'native',
                             gp = grid::gpar(
                               col = aes$colour,
                               lwd = aes$size * .pt,
                               lty = aes$linetype,
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre
                             )
                           )
                           grid::grobTree(g_poly, g_lines)
                         },

                         draw_key = ggplot2::draw_key_polygon,
                         required_aes = c('x','y'),
                         optional_aes = c('ymin','ymax')
)

