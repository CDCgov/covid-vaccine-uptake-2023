#' Get public NIS 2023 data
#'
#' Download 2023 bivalent booster uptake data from publicly available
#' dataset `pakc-hru3`
#' @param indicator default: `"booster"`, option: `"sentiment"`
#' @details
#' The data gives the percent of people who completed their primary series, and the
#' percent of those people who got a bivalent booster. We compute the error in the
#' product of those two by assuming that the two metrics are independent, which is
#' a kludge.
#'
#' @export
get_nis_2023 <- function(indicator = "booster") {
  raw <- get_socrata(
    "https://data.cdc.gov/resource/pakc-hru3.json",
    select = paste0(
      "vaccine,geographic_level,geographic_name,demographic_level,demographic_name,",
      "indicator_label,indicator_category_label,",
      "week_ending,month_week,nd_weekly_estimate,ci_half_width_95pct,n_unweighted"
    ),
    where = c(
      "vaccine='COVID'",
      str_glue("indicator_label='4-level vaccination and intent'")
    )
  )

  data <- raw |>
    mutate(
      indicator_category = case_match(
        indicator_category_label,
        "Definitely will get a vaccine" ~ "yes",
        "Probably will get a vaccine/unsure" ~ "maybe",
        "Received a vaccination" ~ "boosted",
        "Definitely or probably will not get a vaccine" ~ "no"
      ),
      indicator = indicator,
      estimate = as.numeric(nd_weekly_estimate),
      # clean dates
      date = lubridate::as_date(week_ending),
      # calculate sd from CI
      sd = as.numeric(ci_half_width_95pct) / 1.96,
      season = "2023/2024",
      time_type = "Weekly"
    ) |>
    select(time_type, date, indicator, indicator_category,
      geography = geographic_name, group_name = demographic_level,
      group_category = demographic_name, estimate,
      sd, season
    ) |>
    arrange(date) |>
    filter(!group_category %in% c(
      "18-49 years",
      "60+ years",
      "65+ years",
      "Multiple Race/Other (Excludes Asian, AIAN, PI/NH), Non-Hispanic"
    )) |>
    mutate(
      group_category = case_when(
        group_category == "18+ years" ~ "Overall",
        TRUE ~ group_category
      )
    )

  if (indicator == "booster") {
    data <- data |>
      filter(indicator_category == "boosted")
  }

  return(data)
}
