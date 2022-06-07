#' Calculating R-squared of the contrast sensitivity function fit
#' across multiple conditions, groups and subjects
#'
#' @description
#' This function returns a data frame containing r2s from a data frame
#' that contains the original raw data. One of the two arguments 'groups'
#' and 'conditions' must be filled.
#'
#' @param data
#' Name of the variable that stores the data frame that contains
#' the columns with the specified column names. It is recommended that the
#' data frame contains data of contrast sensitivity and spatial frequency
#' in linear units.
#'
#' @param subjects
#' The name of the column of the data frame that contains subjects.
#' It must be strings.
#'
#' @param groups
#' The name of the column of the data frame that contains each group.
#' It must be strings.
#'
#' @param conditions
#' The name of the column of the data frame that contains each condition.
#' It must be strings.
#'
#' @param x
#' The name of the column of the data frame that contains the
#' x-axis points/x coordinates (i.e., spatial frequency) from
#' which the AULCSF can be calculated. It must be strings.
#' The column must not have characters.
#'
#' @param values
#' The name of the column of the data frame that contains the
#' actual data (i.e., contrast sensitivity), which are the y-axis
#' points from which the AULCSF can be calculated. It must be strings.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' set.seed(1)
#' x <- c(0.25,0.35,0.48,0.68,0.94,1.31,1.83,2.54) # spatial frequency
#' y <- c(141,172,190,187,164,129,90.3,57.1) # averaged contrast sensitivity
#' se <- c(9.6,11,11.1,9.9,7.9,6.1,4.8,3.8) # standard error
#' gr <- c(rep(1,length(x)),rep(2,length(x)))
#' subj <- rep(paste0('S',1:2),each=8) # subject number
#' df <- data.frame(subj=subj,x=x,y=y, se=se, gr=gr)
#' df[1:length(x)+1,-c(1,2,5)] <- df[1:length(x)+1,-c(1,2,5)]+round(rnorm(10),1)

#' df$gr <- factor(df$gr)
#' sm_r2_list(data=df, subjects = 'subj', groups ='gr',x='x', values='y')

#' }
sm_R2_list <- function(data, subjects, groups, conditions, x, values) {

  x_val <- unique(data[[x]])
  subjects_list <- unique(base::as.character(data[[subjects]]))
  subj_num <- length(subjects_list)
  x_length <- length(unique(x_val))

  if (missing(groups) && missing(conditions)) {
    stop('conditions or groups argument has to be filled')
  } else if (missing(groups)) {
    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    r2_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
    names(r2_list) <- c(subjects, conditions, paste0('R2'))

    for (iCond in seq_along(1:cond_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        r2_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
        r2_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
        r2_list[,3][(cond_num*(iSubj-1))+(iCond)] <- sm_r2(x_val,data[[values]][ind])
      }
    }
    r2_list[[conditions]] <- base::as.factor(r2_list[[conditions]])
    levels(r2_list[[conditions]]) <- levels(data[[conditions]])

  } else if (missing(conditions)) {
    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])


    r2_list <- data.frame(matrix(ncol = 3, nrow = subj_num*group_num))
    names(r2_list) <- c(subjects, groups, paste0('R2'))

    for (iGroup in seq_along(1:group_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        r2_list[,1][(group_num*(iSubj-1))+(iGroup)] <- subjects_list[iSubj]
        r2_list[,2][(group_num*(iSubj-1))+(iGroup)] <- group_list[iGroup]
        r2_list[,3][(group_num*(iSubj-1))+(iGroup)] <- sm_r2(x_val,data[[values]][ind])
      }
    }
    r2_list[[groups]] <- base::as.factor(r2_list[[groups]])
    levels(r2_list[[groups]]) <- levels(data[[groups]])

  } else {

    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])

    r2_list <- data.frame(matrix(ncol = 4, nrow = subj_num*cond_num*group_num))

    names(r2_list) <- c(subjects, conditions, groups, paste0('R2'))

    for (iGroup in seq_along(1:group_num)) {
      for (iCond in seq_along(1:cond_num)) {
        for (iSubj in seq_along(1:subj_num)) {
          ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                         data[[conditions]] == unique(cond_list)[iCond] &
                         data[[subjects]] == unique(subjects_list)[iSubj])
          if (length(ind)!=0) {
            r2_list[,1][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- subjects_list[iSubj]
            r2_list[,2][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- cond_list[iCond]
            r2_list[,3][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- group_list[iGroup]
            r2_list[,4][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_r2(x_val,data[[values]][ind], )
          }

        }
      }
    }

    r2_list[[groups]] <- base::as.factor(r2_list[[groups]])
    levels(r2_list[[groups]]) <- levels(data[[groups]])
    r2_list[[conditions]] <- base::as.factor(r2_list[[conditions]])
    levels(r2_list[[conditions]]) <- levels(data[[conditions]])

  }
  r2_list <- stats::na.omit(r2_list)
  print(paste('R2 (0-worst, 1-best) for each subject'))
  return(r2_list)
}
