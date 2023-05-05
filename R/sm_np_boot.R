#' Non-parametric bootstrapping with replacement for contrast sensitivity data
#'
#' This function randomly samples qCSF parameters from observers, then generates
#' fitted values as a function of spatial frequency using the sampled qCSF parameters.
#' It returns a matrix, which can then be directly used for functions that perform
#' factor analysis.
#'
#' It then computes the eigenvalues from factor analysis/principal component for many
#' iterations, and the returns the mean and the 95% confidence intervals of the
#' eigenvalues. Eigenvalues represent the proportion of the total variance that can be
#' described by a specific factor/principal component. The higher the eigenvalue of the factor/
#' component, the more important it is in the data.
#'
#' If the eigenvalues' 95% confidence interval from the bootstrap does not overlap with
#' the eigenvalues from the random matrix (i.e., significantly higher than the eigenvalues from the
#' random matrix), then it means that the factors are significant
#' and should be included in the model/dimension reduction.
#'
#' These values are returned as a data frame, which can then be used to plot
#' the eigenvalues using ggplot2.
#'
#' @param param_list
#' This is the result from sm_param_list(), which is a list that contains
#' qCSF parameters of all observers, experimental conditions and groups, as well
#' as the tested spatial frequencies.
#'
#' @param n
#' Number of sample for bootstrap. The default is set to 50.
#'
#' @param nSim
#' Number of non-parametric bootstrap simulations (number of samplings)
#'
#' @param ci_range
#' Range of the confidence interval. The default is set to 0.95.
#'
#' @param fa
#' If set to TRUE (default), it computes eigenvalues from the factor model,
#' which accounts for the random error. If FALSE, it computes from the principal component.
#' @importFrom stats quantile
#' @importFrom psych fa.parallel scree
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- read_csv('https://www.smin95.com/data_ACh.csv') %>%
#' group_by(Subject,SpatialFreq) %>%
#'  summarise(Sensitivity = mean(Sensitivity),
#'            Repetition = 'avg')
#'
# 'res <- sm_params_list(subjects = 'Subject',
#'                      conditions = 'Repetition',
#'                      x = 'SpatialFreq',
#'                      values = 'Sensitivity',
#'                      data = df)
#'
#' sm_np_boot(res, n=51)
#' }
#'
sm_np_boot <- function(param_list, n = 50, nSim = 1000,
                       ci_range = 0.95, fa = TRUE) {

  upper_ci <- (1+ci_range)/2
  lower_ci <- (1-ci_range)/2
  #set.seed(seed)

  params_df <- param_list[[1]]
  sf_list <- param_list[[2]]

  sens.sim.list <- lapply(1:nSim, function(i) {

    logGain.i <- sample(params_df$logGain, n, replace=TRUE)
    logPeakSF.i <- sample(params_df$logPeakSF, n, replace=TRUE)
    logOctveWidth.i <- sample(params_df$logOctaveWidth, n, replace=TRUE)
    logTrunc.i <- sample(params_df$logGain, n, replace=TRUE)


    qCSF_est <- sapply(sf_list, function(sf) {
      10^sm_findQCSF(log10(sf), logGain.i, logPeakSF.i, logOctveWidth.i,
                     logTrunc.i)})

    return(as.matrix(qCSF_est))
  })


  if (fa == TRUE) {
    res_all <- lapply(1:nSim, function(iSim) { # eigenval from FA
      scree(sens.sim.list[[iSim]], factors=T, pc = F)$fv
    })

    rnd_all <- lapply(1:nSim, function(iSim) { # eigenval from PC
      fa.parallel(sens.sim.list[[iSim]], plot=FALSE)$fa.sim
    })

  } else { # PCA
    res_all <- lapply(1:nSim, function(iSim) { # eigenval from PCA random
      scree(sens.sim.list[[iSim]], factors=F, pc = T)$pcv
    })

    rnd_all <- lapply(1:nSim, function(iSim) { # eigenval from PC random
      fa.parallel(sens.sim.list[[iSim]])$pc.sim
    })
  }


  df1 <- rowMeans(sapply(res_all, unlist, 1)) # mean from real matrices
  df1_down <- apply(sapply(res_all,unlist,1),1,quantile,lower_ci) # lower 95% CI
  df1_up <- apply(sapply(res_all,unlist,1),1,quantile,upper_ci) # upper 95% CI

  df1_all <- data.frame(mean = df1,
                        downCI = df1_down,
                        upCI = df1_up,
                        group = 'bootstrap',
                        nFac = seq(length(df1)))


  df2 <- rowMeans(sapply(rnd_all, unlist, 1)) # mean of the eigenvalues from random matrices
  df2_down <- apply(sapply(rnd_all,unlist,1),1,quantile,lower_ci) # lower 95% CI
  df2_up <- apply(sapply(rnd_all,unlist,1),1,quantile,upper_ci) # upper 95% CI

  df2_all <- data.frame(mean = df2,
                        downCI = df2_down,
                        upCI = df2_up,
                        group = 'random',
                        nFac = seq(length(df2)))

  df_all <- rbind(df1_all,df2_all)

  return(df_all)
}
