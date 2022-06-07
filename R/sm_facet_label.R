#' Facet header labeling
#'
#' @param str1
#' Character string that precedes the factor level as specified by the
#' column of the data frame `df_column`.
#' @param sep_by
#' Character string that separates `str1` from the factor level as specified
#' by the column of the data frame `df_column`.
#' @param df_column
#' Column of the data frame that provides the factor levels.
#' @param sep_by2
#' Character string that separates `str2` from the factor level as specified
#' by the column of the data frame `df_column`.
#' @param str2
#' Character string that comes after the factor level as specified by the
#' column of the data frame `df_column`.
#' @export
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
#'
sm_facet_label <- function(str1, sep_by  = " ", df_column,
                           sep_by2 = NULL, str2 = NULL){
  lev <- levels(as.factor(df_column))
  lab <- paste0(str1, sep_by, lev, sep_by2, str2)
  names(lab) <- lev
  return(lab)
}

