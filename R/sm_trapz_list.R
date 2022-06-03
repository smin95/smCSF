#' Calculating Area under Curve (trapezoidal method)
#' across multiple conditions, groups and subjects
#'
#' @description
#'
#' This function returns a data frame containing AUCs from a data frame
#' that contains the original raw data. One of the two arguments 'groups'
#' and 'conditions' must be filled. The function will throw an error
#' if both arguments are empty. If 'logXY' is set TRUE, it will convert the
#' x and y data into log10 units, and then perform the trapezoidal integration.
#'
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
#' which the AUC can be calculated. It must be strings.
#' The column must not have characters.
#'
#' @param values
#' The name of the column of the data frame that contains the
#' actual data (i.e., contrast sensitivity), which are the y-axis
#' points from which the AUC can be calculated. It must be strings.
#'
#' @param logXY
#' When this argument is set to TRUE, the function will convert the x and y values
#' into log10 units, and then perform the integration. If it is set to FALSE,
#' the function will keep the data values as they are.
#'
#' @return
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
sm_trapz_list <- function(data, subjects, groups, conditions, x, values, logXY = TRUE) {

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

    trapz_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
    names(trapz_list) <- c(subjects, conditions, paste0('AUC_', values))

    for (iCond in seq_along(1:cond_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        trapz_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
        trapz_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
        trapz_list[,3][(cond_num*(iSubj-1))+(iCond)] <- sm_trapz(x_val,data[[values]][ind], logXY = logXY)
      }
    }
    trapz_list[[conditions]] <- base::as.factor(trapz_list[[conditions]])
    levels(trapz_list[[conditions]]) <- levels(data[[conditions]])

  } else if (missing(conditions)) {
    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])


    trapz_list <- data.frame(matrix(ncol = 3, nrow = subj_num*group_num))
    names(trapz_list) <- c(subjects, groups, paste0('AUC_', values))

    for (iGroup in seq_along(1:group_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        trapz_list[,1][(group_num*(iSubj-1))+(iGroup)] <- subjects_list[iSubj]
        trapz_list[,2][(group_num*(iSubj-1))+(iGroup)] <- group_list[iGroup]
        trapz_list[,3][(group_num*(iSubj-1))+(iGroup)] <- sm_trapz(x_val,data[[values]][ind], logXY = logXY)
      }
    }
    trapz_list[[groups]] <- base::as.factor(trapz_list[[groups]])
    levels(trapz_list[[groups]]) <- levels(data[[groups]])

  } else {

    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])

    trapz_list <- data.frame(matrix(ncol = 4, nrow = subj_num*cond_num*group_num))

    names(trapz_list) <- c(subjects, conditions, groups, paste0('AUC_', values))

    for (iGroup in seq_along(1:group_num)) {
      for (iCond in seq_along(1:cond_num)) {
        for (iSubj in seq_along(1:subj_num)) {
          ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                         data[[conditions]] == unique(cond_list)[iCond] &
                         data[[subjects]] == unique(subjects_list)[iSubj])
          if (length(ind)!=0) {
            trapz_list[,1][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- subjects_list[iSubj]
            trapz_list[,2][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- cond_list[iCond]
            trapz_list[,3][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- group_list[iGroup]
            trapz_list[,4][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_trapz(x_val,data[[values]][ind], logXY = logXY)
          }

        }
      }
    }

    trapz_list[[groups]] <- base::as.factor(trapz_list[[groups]])
    levels(trapz_list[[groups]]) <- levels(data[[groups]])
    trapz_list[[conditions]] <- base::as.factor(trapz_list[[conditions]])
    levels(trapz_list[[conditions]]) <- levels(data[[conditions]])

  }
  trapz_list <- stats::na.omit(trapz_list)
  print(paste('Trapezoid AUC =', values, '*', x))
  return(trapz_list)
}
