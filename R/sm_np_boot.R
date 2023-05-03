#' Non-parametric bootstrapping with replacement
#'
#' This function randomly samples qCSF parameters from observers, then generates
#' fitted values as a function of spatial frequency using the sampled qCSF parameters.
#' It returns a matrix, which can then be directly used for functions that perform
#' factor analysis.
#'
#' @param param_list
#' This is the result from sm_param_list(), which is a list that contains
#' qCSF parameters of all observers, experimental conditions and groups, as well
#' as the tested spatial frequencies.
#'
#' @param n
#' Number of bootstraps (i.e., simulated observers). The default is set to 1000.
#'
#' @param seed
#' This determines the seed for generating random numbers.
#'
#' @return
#' @importFrom stats optim
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
#' # samples 1000 times with replacement
#' sm_np_boot(res, 1000)
#' }
#'
sm_np_boot <- function(param_list, n = 1000, seed=2223) {
  set.seed(seed)

  params_df <- param_list[[1]]
  sf_list <- param_list[[2]]

  logGain.i <- sample(params_df$logGain, n, replace=TRUE)
  logPeakSF.i <- sample(params_df$logPeakSF, n, replace=TRUE)
  logOctveWidth.i <- sample(params_df$logOctaveWidth, n, replace=TRUE)
  logTrunc.i <- sample(params_df$logGain, n, replace=TRUE)

  qCSF_est <- sapply(sf_list, function(sf) {
    10^sm_findQCSF(log10(sf), logGain.i, logPeakSF.i, logOctveWidth.i,
                   logTrunc.i)})

  return(as.matrix(qCSF_est))

}
