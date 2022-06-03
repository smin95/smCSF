#' Contrast sensitivity function with the form of the truncated
#' log-parabola and four parameters.
#'
#' @description
#'
#' This is equation of the contrast sensitivity function with the
#' form of the truncated log-parabola (Lesmes et al., 2010; Hou et al., 2010).
#' This function can compute the predicted value if the tested spatial frequencies
#' and the four parameters are provided, which can be computed by the Quick CSF method.
#'
#' The four parameters are the peak gain, the peak spatial frequency,
#' the bandwidth and the truncation value. The peak gain refers to the peak
#' (highest) sensitivity of the given contrast sensitivity function across spatial
#' frequency. Essentially it is the highest y-axis value of the CSF function (y-axis:
#' sensitivity, x-axis: spatial frequency). The peak spatial frequency denotes the
#' spatial frequency that yields the peak gain (highest sensitivity). For instance,
#' if the contrast sensitivity is the highest (i.e., about 200) across an observer's
#' contrast sensitivity function at a spatial frequency of 4 cycles/degree, then one can say that
#' the peak gain is 200 and the peak spatial frequency is 4 cycles/degree. The bandwidth
#' refers to how wide the contrast sensitivity function is. The function typically has an semi-oval shape,
#' and the area where it protrudes has a certain width. When this width is measured at the sensitivity level
#' that is half of the peak sensitivity, this is called the bandwidth. The unit of the bandwidth is
#' in log2 scale, known as the octave. Lastly, the truncation value creates a plateau at a low spatial
#' frequency. This factor often plays a minute role and its absence does not significantly affect the fit. It mainly
#' resolves the issue of the contrast sensitivity function's symmetry and asymmetry (Lesmes et al., 2010).
#'
#'
#' @param spatFreq
#'
#' A vector of spatial frequencies from which the contrast sensitivity data
#' have been collected. This should be the linear values of the tested spatial frequencies, not log.
#'
#' @param logGain
#'
#' This refers to peak gain (i.e., peak sensitivity) of the contrast sensitivity function.
#' It should be provided in log10 units.
#' @param logCenter
#' This refers to peak spatial frequency (i.e., center of the contrast sensitivity function).
#' It should be provided in log10 units.
#'
#' @param octaveWidth
#' This refers to the bandwidth of the contrast sensitivity function. It should be provided in log10
#' units.
#'
#' @param logTrunc
#' This refers to the truncation value. It should be provided in log10 units.
#'
#' @return
#' @export
#'
#' @references
#'
#' Lesmes, L. A., Lu, Z. L., Baek, J., & Albright, T. D. (2010). Bayesian adaptive estimation of the contrast sensitivity function: The quick CSF method. Journal of vision, 10(3), 17-17.
#'
#' Hou, F., Huang, C. B., Lesmes, L., Feng, L. X., Tao, L., Zhou, Y. F., & Lu, Z. L. (2010). qCSF in clinical application: efficient characterization and classification of contrast sensitivity functions in amblyopia. Investigative ophthalmology & visual science, 51(10), 5365-5377.
#'
#' @examples
sm_findQCSF <- function(spatFreq, logGain, logCenter, octaveWidth, logTrunc) {
  linTrunc <- 10^logTrunc; # log10
  tauDecay <- 0.5
  K <- log10(tauDecay)
  logWidth <- (10^octaveWidth*log10(2))/2
  logP <- logGain + K * ((1/logWidth) * (spatFreq - logCenter))^2 # log Parabola
  truncHalf <- logGain - linTrunc

  leftCSF <- ((logP < truncHalf) & (spatFreq < logCenter)) * truncHalf
  rightCSF <- ((logP >= truncHalf) | (spatFreq > logCenter)) * logP

  logCSF <- (leftCSF + rightCSF)


  if (any(logCSF < 0)) {
    ind <- which(logCSF < 0)
    logCSF[ind] <- 0
  }
  return(logCSF)

}
