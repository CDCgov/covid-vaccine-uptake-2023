#' Prepares tibble of uptake projection outputs for plotting
#'
#' @param obs_slopes df containing initial slope generated from fit_slope.R,
#' with one column of `trajectories` containing `estimate`, `lci`,  `uci`, and
#' one column of `values`
#' @param estimate vector of the current year's observations
#' @param ref_data tibble of reference year data
#' @param dates lubridate::Date object specifying dates of the observations in estimate
#' @param method character specifying vectors of projection methods to use
#' @param trajectory character used for accessing obs_slope values
#'
#' @return A tibble for plotting
#' @export
prepare_projections_for_plot <- function(
    obs_slopes,
    ref_data,
    estimate,
    dates,
    method = c("rate_matched", "date_matched"),
    trajectory = c("estimate", "lci", "uci")) {
  tidyr::expand_grid(
    method = method,
    trajectory = trajectory
  ) %>%
    left_join(obs_slopes, by = "trajectory") %>%
    mutate(
      proj_uptake = purrr::map2(method, obs_slope, function(sc, os) {
        get_projection(
          sc,
          estimate,
          get_ref_uptake(dates, ref_data),
          os
        )
      }),
      date = list(dates)
    ) %>%
    select(method, trajectory, date, proj_uptake) %>%
    tidyr::unnest(c(date, proj_uptake))
}
