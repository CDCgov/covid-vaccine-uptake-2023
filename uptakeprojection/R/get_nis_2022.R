#' Get NIS 2022 data
#'
#' Download 2022 bivalent booster uptake data from publicly available
#' dataset `udsf-9v7b`
#' @param indicator default: `"booster"`, option: `"sentiment"`
#' @details
#' The data gives the percent of people who completed their primary series, and the
#' percent of those people who got a bivalent booster. We compute the error in the
#' product of those two by assuming that the two metrics are independent, which is
#' a kludge.
#'
#' @export
get_nis_2022 <- function(indicator = "booster") {
  if (indicator == "booster") {
    indicator_names <- c(
      "Received updated bivalent booster (among adults who completed primary series)",
      "Received bivalent booster (among adults who completed primary series)",
      # note trailing space vv
      "Completed primary series "
    ) %>%
      str_c("'", ., "'", collapse = ", ")
  } else if (indicator == "sentiment") {
    indicator_names <- c(
      "Bivalent Booster Uptake and Intention",
      # note trailing space vv
      "Completed primary series "
    ) %>%
      str_c("'", ., "'", collapse = ", ")
  }

  raw <- get_socrata(
    "https://data.cdc.gov/resource/udsf-9v7b.json",
    select = paste0(
      "geography,group_name,group_category,indicator_name,indicator_category,",
      "time_type,time_year,time_period,estimate,coninf_95"
    ),
    where = c(
      "time_year>='2022'",
      str_glue("indicator_name in({indicator_names})")
    )
  )

  if (indicator == "booster") {
    data <- raw %>%
      # simplify indicator labels
      mutate(
        indicator_category = case_match(
          indicator_name,
          # nolint start
          "Received updated bivalent booster (among adults who completed primary series)" ~ "booster",
          # nolint end
          "Received bivalent booster (among adults who completed primary series)" ~ "booster",
          "Completed primary series " ~ "primary"
        )
      ) %>%
      # separate dates
      tidyr::separate(time_period, c("time_start", "time_end"), sep = " - ") %>%
      mutate(across(
        c(time_start, time_end),
        function(x) lubridate::mdy(paste0(x, ", ", time_year))
      )) %>%
      # separate CIs
      tidyr::separate(coninf_95, c("lci", "uci"), sep = " - ") %>%
      mutate(
        # convert estimates and CIs to percents, to proportions
        across(c(estimate, lci, uci), function(x) readr::parse_number(x) / 100),
        # compute SD
        sd = (uci - lci) / 1.96 / 2
      ) %>%
      # compute % bivalent as %primary * %bivalent|primary
      select(
        time_type,
        date = time_end, geography, group_name, group_category, indicator_category,
        estimate, sd
      ) %>%
      tidyr::pivot_wider(names_from = "indicator_category", values_from = c("estimate", "sd")) %>%
      mutate(
        estimate = estimate_booster * estimate_primary,
        sd = product_sd(estimate_primary, estimate_booster, sd_primary, sd_booster),
        season = "2022/2023",
        indicator = indicator,
        indicator_category = "boosted"
      ) %>%
      filter(!is.na(estimate)) %>%
      # convert back to percents
      mutate(across(c(estimate, sd), function(x) x * 100)) %>%
      select(
        time_type, date, indicator, indicator_category, geography, group_name,
        group_category, estimate, sd, season
      )
  } else if (indicator == "sentiment") {
    data <- raw %>%
      mutate(
        indicator_category = case_match(
          indicator_category,
          # nolint start
          "Definitely will get an updated bivalent booster (among adults who completed primary series)" ~ "yes",
          "Probably will get an updated bivalent booster or are unsure (among adults who completed primary series)" ~ "maybe",
          "Received updated bivalent booster dose (among adults who completed primary series)" ~ "boosted",
          "Probably or definitely will not get an updated bivalent booster (among adults who completed primary series)" ~ "no",
          "Completed primary series" ~ "primary"
          # nolint end
        )
      ) %>%
      # separate dates
      tidyr::separate(time_period, c("time_start", "time_end"), sep = " - ") %>%
      mutate(across(
        c(time_start, time_end),
        function(x) lubridate::mdy(paste0(x, ", ", time_year))
      )) %>%
      # separate CIs
      tidyr::separate(coninf_95, c("lci", "uci"), sep = " - ") %>%
      mutate(
        across(c(estimate, lci, uci), function(x) readr::parse_number(x) / 100),
        # compute SD
        sd = (uci - lci) / 1.96 / 2,
        season = "2022/2023",
        indicator = indicator,
      ) %>%
      select(
        time_type,
        date = time_end, geography, group_name, group_category, indicator, indicator_category,
        estimate, sd, season
      ) %>%
      tidyr::pivot_wider(names_from = "indicator_category", values_from = c("estimate", "sd")) |>
      mutate(
        estimate_boosted = estimate_boosted * estimate_primary,
        sd_boosted = product_sd(estimate_primary, estimate_boosted, sd_primary, sd_boosted),
        estimate_maybe = estimate_maybe * estimate_primary,
        sd_maybe = product_sd(estimate_primary, estimate_maybe, sd_primary, sd_maybe),
        estimate_yes = estimate_yes * estimate_primary,
        sd_yes = product_sd(estimate_primary, estimate_yes, sd_primary, sd_yes),
        estimate_no = estimate_no * estimate_primary + (1 - estimate_primary),
        sd_no = product_sd(estimate_primary, estimate_no, sd_primary, sd_no)
      ) |>
      tidyr::pivot_longer(c(starts_with("estimate")),
        names_to = "indicator_category",
        values_to = "estimate"
      ) |>
      tidyr::pivot_longer(c(starts_with("sd")),
        names_to = "indicator_category_sd",
        values_to = "sd"
      ) |>
      mutate(
        indicator_category = stringr::str_split_i(indicator_category, pattern = "_", i = 2),
        indicator_category_sd = stringr::str_split_i(indicator_category_sd,
          pattern = "_", i = 2
        )
      ) |>
      filter(
        indicator_category == indicator_category_sd,
        indicator_category != "primary",
        !is.na(estimate)
      ) |>
      # convert back to percents
      mutate(across(c(estimate, sd), function(x) x * 100)) %>%
      select(
        time_type, date, indicator, indicator_category, geography, group_name,
        group_category, estimate, sd, season
      )
  }

  saved <- data %>%
    arrange(date) %>%
    filter(!group_category %in% c(
      "18 – 49 years", "60+ years", "65+ years"
    )) %>%
    mutate(
      group_category = case_when(
        group_category == "18 – 29 years" ~ "18-29 years",
        group_category == "30 – 39 years" ~ "30-39 years",
        group_category == "40 – 49 years" ~ "40-49 years",
        group_category == "50 – 64 years" ~ "50-64 years",
        group_category == "65 – 74 years" ~ "65-74 years",
        group_category == "American Indian/Alaska Native, non-Hispanic" ~
          "American Indian/Alaska Native, Non-Hispanic",
        group_category == "Asian, non-Hispanic" ~ "Asian, Non-Hispanic",
        group_category == "Black, non-Hispanic" ~ "Black, Non-Hispanic",
        group_category == "Other or multiple races, non-Hispanic" ~ "Other, Non-Hispanic",
        group_category == "Native Hawaiian/Pacific Islander, non-Hispanic" ~
          "Pacific Islander/Native Hawaiian, Non-Hispanic",
        group_category == "White, non-Hispanic" ~ "White, Non-Hispanic",
        group_category == "Above poverty, income <$75k" ~ "Above Poverty, Income < $75k",
        group_category == "Above poverty, income >=$75k" ~ "Above Poverty, Income >= $75k",
        group_category == "Below poverty" ~ "Below Poverty",
        group_category == "Unknown income" ~ "Poverty Status Unknown",
        group_category == "All adults age 18+ years" ~ "Overall",
        group_category == "18+ years" ~ "Overall",
        TRUE ~ group_category
      ),
      group_name = ifelse(group_name == "Race/Ethnicity (7 level)", "Race/Ethnicity", group_name),
      group_name = ifelse(group_name == "Insurance status", "Health Insurance", group_name),
      group_category = ifelse(group_category == "Not insured", "Uninsured", group_category),
      group_name = ifelse(group_name == "Poverty status", "Poverty Status", group_name),
      group_name = ifelse(group_name == "All adults 18+", "Overall", group_name)
    )
}
