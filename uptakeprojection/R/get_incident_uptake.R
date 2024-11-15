#' Compute incident uptake from cumulative
#'
#' Estimated standard error is an overestimate, if cumulative uptakes are positively correlated
#'
#' @param x cumulative uptake, ordered
#' @param sd standard error in `x`
#'
#' @return named list with incident uptake `$x` and standard errors `$sd`
#' @export
get_incident_uptake <- function(x, sd) {
  # check that both vectors are same length
  n <- length(x)
  stopifnot(length(sd) == n)
  # check for no NAs
  if (any(is.na(x))) stop("Can't handle NAs")
  if (any(is.na(sd))) stop("Can't handle NAs")
  # check that sds are positive
  stopifnot(all(sd > 0 | is.na(sd)))

  out_x <- diff(x)
  # if Y1 = X1 - X0, then
  # Var[Y1] = Var[X1] + Var[X0] - Cov[X1, X0]  #nolint
  # sd[Y1] >= sqrt{sd[X1] + sd[X0]}
  s2 <- sd**2
  out_sd <- sqrt(s2[-n] + s2[-1])

  # check that output is the right length
  stopifnot(length(out_x) == n - 1)
  stopifnot(length(out_sd) == n - 1)

  list(x = out_x, sd = out_sd)
}

#' Augment a dataframe with incident uptake
#'
#' @param df Dataframe with columns `date`, `estimate`, `sd`
augment_with_incident <- function(df) {
  # check for column names
  stopifnot("date" %in% names(df))
  stopifnot("estimate" %in% names(df))
  stopifnot("sd" %in% names(df))

  # ensure we're sorted by date
  x <- as.integer(df$date)
  stopifnot(all(x == cummax(x)))
  if (any(duplicated(df$date))) stop("Duplicated dates")

  incident <- get_incident_uptake(df$estimate, df$sd)

  df %>%
    mutate(
      inc_estimate = c(NA, incident$x),
      inc_sd = c(NA, incident$sd)
    )
}
