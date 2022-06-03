#' Customising the aesthetics of the facet header.
#'
#' @param font_size
#' Font size of the facet header. Number referring to the font size
#' should be provided.
#'
#' @param head_fill
#' The color that will fill the head of each facet. The hex code of a color,
#' or a character string of the color, should be provided here.
#'
#' @param font_weight
#' The font weight, such as 'bold', 'plain', 'italic', 'bold.italic' can
#' be specified here.
#'
#'
#' @return
#' @export
#' @importFrom ggplot2 theme
#'
#' @examples
sm_facet_header <- function(font_size = 10, head_fill = 'white',
                            font_weight = 'bold') {
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = head_fill),
    strip.text = ggplot2::element_text(size = font_size,
                              face = font_weight)
  )
}
