#' Propagate uncertainty
#'
#' @param mu1 mean estimate of first variable
#' @param mu2 mean estimate of second variable
#' @param sigma1 standard deviation of first variable
#' @param sigma2 standard deviation of second variable
#' @details
#' At multiple points in this workflow we need to propegate uncertainty across either
#' the product or the sum of two different estimates. We compute the error in the
#' product of those two by assuming that the two metrics are independent, which is
#' a conservative assumption.
#'
#' @export
product_sd <- function(mu1, mu2, sigma1, sigma2) {
  # https://en.wikipedia.org/wiki/Distribution_of_the_product_of_two_random_variables#Variance_of_the_product_of_independent_random_variables # nolint
  sqrt((sigma1**2 + mu1**2) * (sigma2**2 + mu2**2) - (mu1**2 * mu2**2))
}

divide_sd <- function(mu1, mu2, sigma1, sigma2) {
  # https://en.wikipedia.org/wiki/Propagation_of_uncertainty # nolint
  abs(mu1 / mu2) * sqrt((sigma1 / mu1)**2 + (sigma2 / mu2)**2)
}

sum_sd <- function(sigma1, sigma2) {
  # https://en.wikipedia.org/wiki/Propagation_of_uncertainty # nolint
  sqrt(sigma1**2 + sigma2**2)
}
