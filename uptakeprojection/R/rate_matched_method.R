#' Run uptake projection under rate-matched method assumptions
#'
#' In the rate-matched (formerly "pessimistic") method, the rate of booster uptake
#' will decline at the same rate as it did in the reference season, so the uptake
#' rate is started from the matching point in that season, then the reference season
#' rates are used moving forward.
#'
#' @param obs_uptake current season's uptake curve
#' @param ref_uptake reference uptake curve (a time series)
#' @param obs_slope slopes estimated for current uptake to date this season
#' @param use_total_slope option to choose slope over the full season or just
#' from the past `n` weeks for rate matching
#' @param n_observations to use when not using the total slope - this is the number
#' of weeks of data to use, starting from the most recent
#'
#' @return vector of cumulative uptakes
#
#' @export
rate_matched_method <- function(obs_uptake, ref_uptake, obs_slope,
                                use_total_slope = FALSE,
                                n_observations = 4) {
  # create "recent slope" (different from observed slope)
  if (use_total_slope) {
    pessimistic_rate <- obs_slope
  } else {
    obs_rate <- diff(c(0, obs_uptake))
    pessimistic_rate <- mean(tail(obs_rate, n_observations - 1))
  }

  # find the date that the ref rate is closest to obs_slope
  ref_rate <- diff(c(0, ref_uptake))
  closest_idx <- which.min(abs(ref_rate - pessimistic_rate))

  obs_rate <- diff(c(0, obs_uptake))

  c(obs_rate, ref_rate[(closest_idx + 1):length(ref_rate)]) %>%
    # should be no longer than reference uptake
    head(length(ref_uptake)) %>%
    # if shorter, pad with zeros
    pad(length(ref_uptake), 0) %>%
    cumsum()
}
