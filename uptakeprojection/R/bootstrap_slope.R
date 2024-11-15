#' Nonparametric bootstrapping for fitting a line to x,y pairs to estimate the uptake slope
#'
#' Bootstrap alternative to stan_slope() in stan_slope.R
#'
#' @param data data frame including x (week) and y (estimate) as well as std for each data point
#' @param n number of bootstrap replicates
#' @param conf confidence level for bootstrap quantities (1 - alpha)
#' @param type passed to boot::boot.ci, type of bootstrap used (e.g. percentile, studentized, etc.)
#'
#' @return named vector; slope estimate with upper and lower 95% confidence intervals
#' @export
bootstrap_slope <- function(data, n = 1000, conf = 0.95, type = "bca", min_n_rows = 3) {
  if (nrow(data) < min_n_rows) {
    return(NULL)
  }

  fit <- boot::boot(
    data,
    function(df, idx) {
      model <- lm(y ~ x, data = slice(df, idx))
      coef(model)["x"]
    },
    R = n
  )

  bci <- boot::boot.ci(fit, type = type, conf = conf)

  c("estimate" = unname(bci$t0), "lci" = bci[[type]][4], "uci" = bci[[type]][5])
}
