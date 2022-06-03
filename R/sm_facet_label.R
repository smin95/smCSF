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
#'
#' @return
#' @export
#'
#' @examples
sm_facet_label <- function(str1, sep_by  = " ", df_column,
                           sep_by2 = NULL, str2 = NULL){
  lev <- levels(as.factor(df_column))
  lab <- paste0(str1, sep_by, lev, sep_by2, str2)
  names(lab) <- lev
  return(lab)
}

