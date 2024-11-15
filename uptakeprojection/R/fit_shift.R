#' Least squares alignment of nis2022 and nis2023 data to find estimated delay
#'
#' @param nis2022 tibble containing nis2022 data
#' @param nis2022 tibble containing nis2023 data
#' @return shift (in days) which best aligns the two time series
#' @export
fit_shift <- function(
    nis2022,
    nis2023) {
  optimize(
    function(lag) {
      obs <- nis2023$estimate
      pred <- get_ref_uptake(nis2023$date + ddays(lag), nis2022)
      sum((obs - pred)**2)
    },
    interval = c(-25, 25)
  )$minimum
}
