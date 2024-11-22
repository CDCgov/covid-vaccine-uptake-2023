#' Create splined reference uptake from 2022
#'
#' @param date week to interpolate for
#' @param nis2022 data input
#' @param ref_date 2023/2024 date to use for offset
#' @param historical_ref_date 2022/2023 season reference date
#'
#' @return uptake on a date
#'
#' @export
get_ref_uptake <- function(
    date,
    nis2022,
    ref_date = ymd("2023-09-15"),
    historical_ref_date = ymd("2022-09-15")) {
  # fit a smooth spline to last year's uptake
  # for the spline function, let historical_ref_date be the "zero" point
  spline22 <- nis2022 %>%
    mutate(x = (date - historical_ref_date) / ddays(7)) %>%
    with(stats::smooth.spline(x, make_nondecreasing(estimate)))

  x <- (lubridate::as_date(date) - ref_date) / ddays(7)
  stats::predict(spline22, x)$y
}
