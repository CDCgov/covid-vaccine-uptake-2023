# - Fit slopes to some number of data points
# - optionally drop the first data point
# This script requires that you first run `scripts/setup.r`

# drop the first 2022 data point, due to weird concavity
drop_first_2022_point <- TRUE
# last data point to use for fitting slope

# convert into number of data points, to allow for shifting from last year
n_weeks <- floor((max_date - min(nis2023$date)) / lubridate::dweeks(1))

# format data --------------------------------------------------------------------
# reference season NIS data:
nis2022_dropped <- nis2022 %>%
  arrange(date) %>%
  # drop that point or not
  (function(df) {
    if (drop_first_2022_point) {
      tail(df, -1)
    } else {
      df
    }
  })

fit_data <- bind_rows(
  `NIS_2022` = nis2022_dropped,
  `NIS_2023` = nis2023,
  .id = "dataset"
) %>%
  group_by(dataset) %>%
  mutate(
    season = case_when(
      lubridate::year(min(date)) == 2023 ~ "2023/2024",
      lubridate::year(min(date)) == 2022 ~ "2022/2023"
    ),
    x = (date - min(date)) / lubridate::ddays(7)
  ) %>%
  ungroup() %>%
  arrange(dataset, date) %>%
  select(season, dataset, date, x, y = estimate, std = sd)

# stan and boot slopes for each dataset ----------------------------------------

slopes <- fit_data %>%
  tidyr::nest(data = !c(season, dataset)) %>%
  mutate(
    fit_data = purrr::map(
      data,
      \(df) filter(df, date <= min(date) + lubridate::dweeks(n_weeks))
    ),
    stan = purrr::map(fit_data, stan_slope_summary),
    boot = purrr::map(fit_data, bootstrap_slope)
  ) %>%
  select(season, dataset, stan, boot) %>%
  tidyr::pivot_longer(c(stan, boot), names_to = "method") %>%
  tidyr::unnest_longer(value, values_to = "value", indices_to = "name") %>%
  tidyr::pivot_wider()

plot_slopes(slopes, data_sources = c("NIS_2022", "NIS_2023"))
ggsave("output/slopes_plot.png", width = 6, height = 4, bg = "white")

slopes %>%
  readr::write_csv(paste0("output/slopes/slopes_", max_date, ".csv"))
