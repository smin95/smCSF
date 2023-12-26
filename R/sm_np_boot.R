#' Non-parametric bootstrap factor analysis for contrast sensitivity data
#'
#' This function randomly samples qCSF parameters from observers, then generates
#' fitted values as a function of spatial frequency using the sampled qCSF parameters.
#' Then using the fitted values, it performs parallel analysis to generate eigenvalues
#' both from the data and random matrices. For more information about parallel analysis
#' please check out `fa.parallel` from `psych` package.
#'
#' It returns a data frame, which can then be directly used to plot the eigenvalues
#' using `sm_plot_boot`, which is an wrapper around ggplot2.
#'
#' Essentially, it computes the eigenvalues from factor analysis/principal component for many
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
#'
#' @param param_list
#' This is the result from sm_param_list(), which is a list that contains
#' qCSF parameters of all observers, experimental conditions and groups, as well
#' as the tested spatial frequencies.
#'
#' @param sf
#'  Numerical vector that contains tested spatial frequencies. If missing,
#'  it will automatically be filled with tested spatial frequencies based on the
#'  data frame.
#'
#' @param n
#' Number of observers (ie sample size) for bootstrap. The default is set to 50.
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
#'
#'
#' @param noiseSize
#' Degree of noise that is added to the simulated data. It uses rnorm() to generate noise,
#' and noiseSize determines the mean and SD of the noise. Its default is set to 1e-6, which
#' is very small to affect the raw sensitivity data but enough to prevent correlation coefficients to
#' become infinite or NA.
#'
#' @importFrom stats quantile rnorm
#' @importFrom psych fa.parallel scree
#' @importFrom utils capture.output
#' @importFrom pracma zeros
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
sm_np_boot <- function(param_list, sf, n = 50, nSim = 1000,
                       ci_range = 0.95, fa = TRUE,
                       noiseSize = 1e-6) {


  upper_ci <- (1+ci_range)/2
  lower_ci <- (1-ci_range)/2
  #set.seed(seed)

  params_df <- param_list[[1]]

  if (missing(sf)) {
    sf_list <- param_list[[2]]
  } else {
    sf_list <- sf
  }


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

  suppressMessages({
    suppressWarnings({
      if (fa == TRUE) {
        res_all <- lapply(1:nSim, function(iSim) { # eigenval from FA
          invisible(capture.output(a <- fa.parallel(sens.sim.list[[iSim]], plot=FALSE)$fa.values))
          a
        })

        rnd_all <- lapply(1:nSim, function(iSim) { # eigenval from FA
          invisible(capture.output(a <- fa.parallel(sens.sim.list[[iSim]], plot=FALSE)$fa.sim))
          a
        })

      } else { # PCA
        res_all <- lapply(1:nSim, function(iSim) { # eigenval from PCA random
          invisible(capture.output(a <- fa.parallel(sens.sim.list[[iSim]], plot=FALSE)$pc.values))
          a
        })

        rnd_all <- lapply(1:nSim, function(iSim) { # eigenval from PC random
          invisible(capture.output(a <- fa.parallel(sens.sim.list[[iSim]], plot=FALSE)$pc.sim))
          a
        })
      }
    })
  })


  df1 <- rowMeans(sapply(res_all, unlist, 1)) # mean from real matrices
  df1_down <- apply(sapply(res_all,unlist,1),1,quantile,lower_ci) # lower 95% CI
  df1_up <- apply(sapply(res_all,unlist,1),1,quantile,upper_ci) # upper 95% CI

  df1_all <- data.frame(mean = df1,
                        downCI = df1_down,
                        upCI = df1_up,
                        group = 'data',
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
