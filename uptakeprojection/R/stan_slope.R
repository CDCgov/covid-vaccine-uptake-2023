#' Linear regression: fit y = mx + b line to x,y pairs to estimate the uptake slope
#'
#' Model: y_i ~ Normal(slope * x_i + intercept, sd)
#'        intercept ~ Normal(0, prior_intercept_sd)
#'        slope ~ normal(0, prior_slope_sd)
#'        sd_i := a priori estimate of standard deviation (provided by NIS)
#' @param df data frame including x (week) and y (estimate) as well as sd for each data point
#' @param chains integer number of chains for rstan to run
#' @param iter integer chain length for rstan
#' @param prior_intercept_sd double (must be positive) see model above
#' @param prior_slope_sd double (must be positive) see model above
#' @param ... further arguments to spass to `rstan::stan`
#'
#' @return MCMC samples as rstan::stanfit object
#' @export
stan_slope <- function(
    df,
    chains = 2,
    iter = 2000,
    prior_intercept_sd = 10,
    prior_slope_sd = 10,
    ...) {
  rstan::stan(
    system.file("stan", "slope.stan", package = "uptakeprojection", mustWork = TRUE),
    data = c(
      N = nrow(df),
      prior_intercept_sd = df$prior_intercept_sd,
      prior_slope_sd = df$prior_slope_sd
    ),
    chains = chains,
    iter = iter,
    ...
  )
}

#' Extract a single parameter from a Stan fit
#'
#' @param x Stan fit object
#' @param nm name of parameter
#' @return vector of draws
extract1 <- function(x, nm) {
  rstan::extract(x, nm)[[nm]] %>%
    as.vector()
}

#' Summarize stan slopes
#'
#' @param df data frame including x (week) and y (estimate) as well as sd for each data point
#' @param name name of the column to extract. Default: `"slope"`
#' @param ... further arguments passed to `stan_slope()`
#' @return named vector with estimate, lci, uci
#'
#' @export
stan_slope_summary <- function(df, name = "slope", ...) {
  draws <- stan_slope(df, ...) %>%
    extract1(name)

  c(
    estimate = stats::median(draws),
    lci = unname(stats::quantile(draws, 0.05 / 2)),
    uci = unname(stats::quantile(draws, 1 - 0.05 / 2))
  )
}
