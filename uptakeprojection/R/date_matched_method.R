#' Run uptake projection using date-matched method
#'
#' In the date-matched (formerly 'intermediate") method, the uptake rate continues
#' as it did in the reference season, based on the calendar date, regardless of the
#' uptake observed so far in the current season.
#'
#' @param obs_uptake current season's uptake curve
#' @param ref_uptake reference uptake curve (a time series)
#' @param obs_slope slopes estimated for current uptake to date this season
#'
#' @returns vector of cumulative uptakes
#'
#' @export
date_matched_method <- function(obs_uptake, ref_uptake, obs_slope) {
  stopifnot(length(ref_uptake) >= length(obs_uptake))

  obs_rate <- diff(c(0, obs_uptake))
  ref_rate <- diff(c(0, ref_uptake))

  out <- c(
    obs_rate,
    ref_rate[(length(obs_rate) + 1):length(ref_rate)]
  ) %>%
    cumsum()

  stopifnot(length(out) == length(ref_uptake))

  out
}
