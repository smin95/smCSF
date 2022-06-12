#' Calculation the four parameters of the contrast sensitivity function
#' across multiple groups, conditions and subjects.
#'
#' @description
#' This function returns a data frame containing the four parameters of the
#' contrast sensitivity function based on the raw data in the given data frame.
#' One of the two arguments 'groups' and 'conditions' must be filled. The function will throw an error
#' if both arguments are empty.
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
#' which the four parameters can be calculated. It must be strings.
#' The column must not have characters.
#'
#' @param values
#' The name of the column of the data frame that contains the
#' actual data (i.e., contrast sensitivity), which are the y-axis
#' points from which the four parameters can be calculated.
#' It must be strings.
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
#'
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- c(0.25,0.35,0.48,0.68,0.94,1.31,1.83,2.54) # spatial frequency
#' y <- c(141,172,190,187,164,129,90.3,57.1) # averaged contrast sensitivity
#' se <- c(9.6,11,11.1,9.9,7.9,6.1,4.8,3.8) # standard error
#' gr <- c(rep(1,length(x)),rep(2,length(x)))
#' subj <- rep(paste0('S',1:2),each=8) # subject number
#' df <- data.frame(subj=subj,x=x,y=y, se=se, gr=gr)
#' df[1:length(x)+1,-c(1,2,5)] <- df[1:length(x)+1,-c(1,2,5)]+round(rnorm(10),1)
#' sm_params_list(data=df, subjects = 'subj',
#'               groups='gr',x='x', values='y')
#' }
#'
sm_params_list <- function(data, subjects, groups, conditions, x,
                           values,
                           param0 = log10(c(100, 1, 2, 0.5)),
                           param_upLimit = log10(c(2000, 20, 9, 2)),
                           param_lowLimit= log10(c(2, 0.2, 1, 0.02))) {

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

    params_list <- data.frame(matrix(ncol = 7, nrow = subj_num*cond_num))
    names(params_list) <- c(subjects, conditions, 'logGain', 'logPeakSF',
                            'logOctaveWidth', 'logTrunc', 'logCutOffSF')

    for (iCond in seq_along(1:cond_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        if (length(ind)!=0) {

          params_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
          params_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
          params_list[,3][(cond_num*(iSubj-1))+(iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                     param0=param0,
                                                                     param_upLimit=param_upLimit,
                                                                     param_lowLimit=param_lowLimit)[[1]]
          #return(sm_params(x_val,data[[values]][ind],
          #                     param0, param_upLimit,
          #                     param_lowLimit)[['logGain']])
          #return(x_val)
          params_list[,4][(cond_num*(iSubj-1))+(iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                     param0=param0,
                                                                     param_upLimit=param_upLimit,
                                                                     param_lowLimit=param_lowLimit)[[2]]
          params_list[,5][(cond_num*(iSubj-1))+(iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                     param0=param0,
                                                                     param_upLimit=param_upLimit,
                                                                     param_lowLimit=param_lowLimit)[[3]]
          params_list[,6][(cond_num*(iSubj-1))+(iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                     param0=param0,
                                                                     param_upLimit=param_upLimit,
                                                                     param_lowLimit=param_lowLimit)[[4]]
          params_list[,7][(cond_num*(iSubj-1))+(iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                     param0=param0,
                                                                     param_upLimit=param_upLimit,
                                                                     param_lowLimit=param_lowLimit)[[5]]
          #return(params_list)
        }
      }
    }
    params_list[[conditions]] <- base::as.factor(params_list[[conditions]])
    levels(params_list[[conditions]]) <- levels(data[[conditions]])

  } else if (missing(conditions)) {
    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])


    params_list <- data.frame(matrix(ncol = 7, nrow = subj_num*group_num))
    names(params_list) <- c(subjects, groups,  'logGain', 'logPeakSF',
                            'logOctaveWidth', 'logTrunc', 'logCutOffSF')

    for (iGroup in seq_along(1:group_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        if (length(ind)!=0) {

          params_list[,1][(group_num*(iSubj-1))+(iGroup)] <- subjects_list[iSubj]
          params_list[,2][(group_num*(iSubj-1))+(iGroup)] <- group_list[iGroup]
          params_list[,3][(group_num*(iSubj-1))+(iGroup)] <- sm_params(x_val,data[[values]][ind],
                                                                       param0=param0,
                                                                       param_upLimit=param_upLimit,
                                                                       param_lowLimit=param_lowLimit)[[1]]
          params_list[,4][(group_num*(iSubj-1))+(iGroup)] <- sm_params(x_val,data[[values]][ind],
                                                                       param0=param0,
                                                                       param_upLimit=param_upLimit,
                                                                       param_lowLimit=param_lowLimit)[[2]]
          params_list[,5][(group_num*(iSubj-1))+(iGroup)] <- sm_params(x_val,data[[values]][ind],
                                                                       param0=param0,
                                                                       param_upLimit=param_upLimit,
                                                                       param_lowLimit=param_lowLimit)[[3]]
          params_list[,6][(group_num*(iSubj-1))+(iGroup)] <- sm_params(x_val,data[[values]][ind],
                                                                       param0=param0,
                                                                       param_upLimit=param_upLimit,
                                                                       param_lowLimit=param_lowLimit)[[4]]
          params_list[,7][(group_num*(iSubj-1))+(iGroup)] <- sm_params(x_val,data[[values]][ind],
                                                                       param0=param0,
                                                                       param_upLimit=param_upLimit,
                                                                       param_lowLimit=param_lowLimit)[[5]]
        }
      }
    }
    params_list[[groups]] <- base::as.factor(params_list[[groups]])
    levels(params_list[[groups]]) <- levels(data[[groups]])

  } else {

    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])

    params_list <- data.frame(matrix(ncol = 8, nrow = subj_num*cond_num*group_num))

    names(params_list) <- c(subjects, conditions, groups, 'logGain', 'logPeakSF',
                            'logOctaveWidth', 'logTrunc', 'logCutOffSF')

    for (iGroup in seq_along(1:group_num)) {
      for (iCond in seq_along(1:cond_num)) {
        for (iSubj in seq_along(1:subj_num)) {
          ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                         data[[conditions]] == unique(cond_list)[iCond] &
                         data[[subjects]] == unique(subjects_list)[iSubj])

          if (length(ind)!=0) {
            params_list[,1][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- subjects_list[iSubj]
            params_list[,2][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- cond_list[iCond]
            params_list[,3][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- group_list[iGroup]


            params_list[,4][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                                        param0=param0,
                                                                                        param_upLimit=param_upLimit,
                                                                                        param_lowLimit=param_lowLimit)[[1]]
            params_list[,5][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                                        param0=param0,
                                                                                        param_upLimit=param_upLimit,
                                                                                        param_lowLimit=param_lowLimit)[[2]]
            params_list[,6][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                                        param0=param0,
                                                                                        param_upLimit=param_upLimit,
                                                                                        param_lowLimit=param_lowLimit)[[3]]
            params_list[,7][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                                        param0=param0,
                                                                                        param_upLimit=param_upLimit,
                                                                                        param_lowLimit=param_lowLimit)[[4]]
            params_list[,8][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_params(x_val,data[[values]][ind],
                                                                                        param0=param0,
                                                                                        param_upLimit=param_upLimit,
                                                                                        param_lowLimit=param_lowLimit)[[5]]
          }

        }
      }
    }

    params_list[[groups]] <- base::as.factor(params_list[[groups]])
    levels(params_list[[groups]]) <- levels(data[[groups]])
    params_list[[conditions]] <- base::as.factor(params_list[[conditions]])
    levels(params_list[[conditions]]) <- levels(data[[conditions]])

  }
  params_list <- stats::na.omit(params_list)
  print(paste('CSF parameters =', values, '~', x))

  params_list_SF <- list(params_list, x_val)
  names(params_list_SF) <- c('parameters', 'spatFreq')
  return(params_list_SF)
}


