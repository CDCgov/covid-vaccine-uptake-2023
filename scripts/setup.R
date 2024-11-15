# pull data from public API

nis2022_raw <- get_nis_2022(indicator = "sentiment")
nis2023_raw <- get_nis_2023(indicator = "sentiment")

# clean uptake-only data -------------------------------------------------------------

nis2022 <- nis2022_raw %>%
  filter(
    indicator_category == "boosted",
    time_type == "Weekly",
    group_name == "Overall",
    geography == "National"
  ) %>%
  mutate(
    lci = estimate - 1.96 * sd,
    uci = estimate + 1.96 * sd
  ) %>%
  select(date, estimate, sd, lci, uci)

nis2023 <- nis2023_raw %>%
  filter(
    indicator_category == "boosted",
    time_type == "Weekly",
    group_name == "Overall",
    geography == "National"
  ) %>%
  mutate(
    lci = estimate - 1.96 * sd,
    uci = estimate + 1.96 * sd
  ) %>%
  select(date, estimate, sd, lci, uci)
