#' Make a vector nondecreasing
#'
#' If vector is nondecreasing, do nothing. If it decreases at some step, keep the
#' output value the same, but accumulate a "debt". Pay down the debt with future,
#' positive increases.
#'
#' @param x vector
#'
#' @return vector
#' @export
make_nondecreasing <- function(x) {
  n <- length(x)
  dx <- diff(c(0, x))
  out <- rep(NA, n)
  debt <- 0

  for (i in 1:n) {
    out[i] <- max(dx[i] + debt, 0)
    debt <- min(dx[i] + debt, 0)
  }

  cumsum(out)
}
