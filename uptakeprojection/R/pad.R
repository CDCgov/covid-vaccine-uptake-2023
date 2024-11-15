#' Pad a vector with a constant value
#'
#' @param x Vector to pad
#' @param len Length to pad out to
#' @param with Value to pad with
#' @param side Side to pad onto (only `"right"` is supported)
#'
#' @return Vector
#' @export
#'
#' @examples
#' pad(1:3, 5, 0)
pad <- function(x, len, with, side = "right") {
  stopifnot(length(x) <= len)
  stopifnot(length(with) == 1)
  stopifnot(side == "right")
  c(x, rep(with, len - length(x)))
}
