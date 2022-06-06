#' Customizing the aesthetics of the facet header.
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

#' @importFrom ggplot2 theme
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- c(0.25,0.35,0.48,0.68,0.94,1.31,1.83,2.54) # spatial frequency
#' y <- c(141,172,190,187,164,129,90.3,57.1) # averaged contrast sensitivity of subjects
#' se <- c(9.6,11,11.1,9.9,7.9,6.1,4.8,3.8) # standard error across subjects
#' gr <- c(rep(1,length(x)),rep(2,length(x)))
#' df <- data.frame(x=x,y=y, se=se, gr=gr)
#' df[1:length(x)+1,-c(1,4)] <- df[1:length(x)+1,-c(1,4)]+round(rnorm(10),1)
#'
#' df$gr <- factor(df$gr)

#'
#'  ggplot(data=df,aes(x, y, group = gr,
#'             color = gr)) +
#'  facet_wrap(~ gr,
#'             labeller = labeller(gr = sm_facet_label('Group',
#'                                                     sep_by = ' ',
#'                                                     df.S1$Repetition))) +
#'  sm_ribbonCSF(aes(ymin = y-se, ymax = y+se)) +
#'  sm_CSF() +
#'  scale_x_continuous(trans = 'log10') +
#'  scale_y_continuous(trans = 'log10') +
#'  sm_facet_header(font_weight = 'bold')
#'
#' }
sm_facet_header <- function(font_size = 10, head_fill = 'white',
                            font_weight = 'bold') {
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = head_fill),
    strip.text = ggplot2::element_text(size = font_size,
                              face = font_weight)
  )
}
