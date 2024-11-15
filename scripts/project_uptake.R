## This script requires that you first run `scripts/setup.r`

## merge and filter
nis_data <- nis2022_raw |>
  bind_rows(nis2023_raw) |>
  filter(
    time_type == "Weekly",
    geography == "National",
    group_name == "Overall"
  ) |>
  mutate(
    lci = estimate - 1.96 * sd,
    uci = estimate + 1.96 * sd,
    # add 54 weeks to 2022/2023 season so we can plot on same x axis
    adjusted_date = case_when(
      season == "2022/2023" ~ date + lubridate::weeks(54),
      TRUE ~ date
    )
  )

full_season <- min(nis2023$date) + lubridate::dweeks(0:30)

begin_month <- lubridate::month(min(full_season))
end_month <- lubridate::month(max(full_season))

# get seasons begin weeks and end weeks
week_df <- nis_data %>%
  filter(
    indicator == "sentiment",
    lubridate::month(date) %in% c(begin_month, end_month)
  ) %>%
  group_by(season) %>%
  summarise(start_week = min(date), end_week = max(date))

knitr::kable(week_df)

### sentiment projection ratio estimate

multiplier <- nis_data |>
  filter(
    adjusted_date <= max(full_season),
    group_name == "Overall",
    season == "2022/2023",
    indicator_category %in% c("boosted", "yes")
  ) |>
  filter(
    (date == min(date) & indicator_category == "yes") |
      (date == max(date) & indicator_category == "boosted")
  ) |>
  select(indicator_category, estimate) |>
  tidyr::pivot_wider(
    names_from = indicator_category,
    values_from = estimate
  ) |>
  mutate(ratio = boosted / yes) |>
  select(ratio) |>
  pull()

intention <- nis_data |>
  filter(
    group_name == "Overall",
    season == "2023/2024"
  ) |>
  filter(date == min(date) & indicator_category == "yes") |>
  select(estimate) |>
  pull()

dates <- min(nis2023$date) + lubridate::dweeks(0:30)

sentiment_projection <- data.frame(
  method = "sentiment",
  trajectory = "estimate",
  date = max(dates),
  proj_uptake = multiplier * intention
)

actual_final_uptake <- nis_data |>
  filter(
    indicator_category == "boosted",
    adjusted_date == max(dates)
  ) |>
  select(season, estimate, lci, uci, date)

## plot uptake projections over time
results <- c()
projection_dates <- dates[seq(4, 16, by = 1)]
plot_dates <- c(min(nis2023$date) + lubridate::dweeks(-3:-1), dates)

for (i in seq_along(projection_dates)) {
  projection_date <- lubridate::as_date(projection_dates[i])

  reference_data <- nis_data |>
    filter(
      season == "2022/2023",
      indicator_category == "boosted",
      adjusted_date <= max(dates) + lubridate::dweeks(2)
    )

  available_uptake_data <- nis_data |>
    filter(
      indicator_category == "boosted",
      season == "2023/2024",
      date <= projection_date
    )

  full_uptake_data <- nis_data |>
    filter(
      season == "2023/2024",
      indicator_category == "boosted",
      adjusted_date <= max(dates)
    )

  max_date <- projection_date

  source("scripts/fit_slopes.R")

  obs_slopes <- slopes %>%
    filter(
      season == "2023/2024",
      method == "boot",
      dataset == "NIS_2023"
    ) %>%
    select(estimate, lci, uci) %>%
    tidyr::pivot_longer(everything(), names_to = "trajectory", values_to = "obs_slope")

  projections <- obs_slopes %>%
    prepare_projections_for_plot(
      ref_data = reference_data,
      estimate = available_uptake_data$estimate,
      dates = dates
    )

  projections |>
    bind_rows(sentiment_projection) |>
    filter(
      method != "optimistic",
      date > max(available_uptake_data$date)
    ) |>
    tidyr::pivot_wider(names_from = "trajectory", values_from = "proj_uptake") |>
    mutate(Method = case_when(
      method == "date_matched" ~ "Date-based",
      method == "rate_matched" ~ "Rate-based",
      method == "sentiment" ~ "Sentiment-based"
    )) |>
    ggplot(aes(date, estimate)) +
    # projections
    geom_point(aes(shape = Method), color = "#E16462FF", size = 3) +
    geom_line(aes(group = Method), color = "#E16462FF", linewidth = .3, alpha = 0.4) +
    # last season's data
    scale_linetype_manual(values = c(2, 6)) +
    geom_pointrange(
      data = reference_data,
      aes(
        x = date + lubridate::years(1),
        ymin = lci, ymax = uci
      ),
      color = "grey", size = 0.2
    ) +
    # this season's data
    geom_pointrange(
      data = available_uptake_data,
      aes(ymin = lci, ymax = uci),
      color = "#E16462FF", size = 1.5,
      alpha = 0.6
    ) +
    geom_pointrange(
      data = full_uptake_data,
      aes(ymin = lci, ymax = uci),
      fill = "black", size = 0.2
    ) +
    scale_x_date(
      breaks = lubridate::as_date(plot_dates[seq(3, 31, by = 4)]),
      date_labels = "%b %d",
      minor_breaks = lubridate::as_date(plot_dates)
    ) +
    labs(
      title = "2023/2024 Covid Booster Uptake Projections",
      y = "Uptake (%)"
    ) +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position = "bottom")

  ggsave(paste0("output/projections/", projection_date, "_plot.png"),
    width = 8.5, height = 5.5, bg = "white"
  )

  available_uptake_data <- available_uptake_data |>
    mutate(
      method = "actual",
      trajectory = "estimate"
    ) |>
    select(
      method,
      trajectory,
      date,
      proj_uptake = estimate
    )

  results <- projections |>
    filter(date > projection_date) |>
    bind_rows(available_uptake_data) |>
    mutate(projection_date = projection_date) |>
    bind_rows(results)
}

results %>%
  readr::write_csv(paste0("output/projections/full_results.csv"))

true_final_uptake <- nis_data |>
  filter(date == "2024-04-27", indicator_category == "boosted") |>
  select(estimate, lci, uci) |>
  slice(rep(1, each = length(projection_dates))) |>
  cbind(projection_date = projection_dates)

cumulative_uptake <- results |>
  filter(
    date == max(dates),
    method != "actual"
  ) |>
  tidyr::pivot_wider(names_from = trajectory, values_from = proj_uptake) |>
  mutate(
    estimate = round(estimate, digits = 3),
    lci = round(lci, digits = 3),
    uci = round(uci, digits = 3)
  ) |>
  select(projection_date, method, estimate, date) |>
  arrange(projection_date) |>
  readr::write_csv(paste0("output/paper_results.csv"))

sentiment_projection |>
  mutate(projection_date = lubridate::as_date("2023-10-21")) |>
  tidyr::pivot_wider(names_from = trajectory, values_from = proj_uptake) |>
  select(projection_date, method, estimate, date) |>
  bind_rows(cumulative_uptake) |>
  mutate(Method = case_when(
    method == "date_matched" ~ "Date-based",
    method == "rate_matched" ~ "Rate-based",
    method == "sentiment" ~ "Sentiment-based"
  )) |>
  ggplot() +
  geom_ribbon(
    data = true_final_uptake,
    aes(
      ymin = lci,
      ymax = uci,
      x = projection_date
    ),
    fill = "grey", alpha = 0.3
  ) +
  geom_line(
    data = true_final_uptake,
    aes(x = projection_date, y = estimate),
    color = "grey", linetype = "dashed"
  ) +
  geom_point(aes(x = projection_date, y = estimate, color = Method)) +
  geom_line(aes(x = projection_date, y = estimate, color = Method)) +
  scale_x_date(
    breaks = lubridate::as_date(plot_dates[seq(3, 31, by = 1)]),
    date_labels = "%b %d",
    minor_breaks = lubridate::as_date(plot_dates)
  ) +
  labs(
    title = "2023/2024 COVID Vaccine Uptake Projections by Projection Date",
    y = "Projected Cumulative Uptake (%)", x = "Projection Date"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(paste0("output/projections/cumulative_uptake_supplement.png"),
  width = 9, height = 5, bg = "white"
)

supplement_figure_dates <- dates[seq(4, 16, by = 4)]

sentiment_projection_df <- sentiment_projection |>
  slice(rep(1, each = 4)) |>
  cbind(projection_date = supplement_figure_dates)

results |>
  bind_rows(sentiment_projection_df) |>
  filter(
    projection_date %in% supplement_figure_dates,
    method != "actual"
  ) |>
  tidyr::pivot_wider(names_from = "trajectory", values_from = "proj_uptake") |>
  mutate(Method = case_when(
    method == "date_matched" ~ "Date-based",
    method == "rate_matched" ~ "Rate-based",
    method == "sentiment" ~ "Sentiment-based"
  )) |>
  ggplot(aes(date, estimate)) +
  # projections
  geom_point(aes(shape = Method), color = "#E16462FF", size = 2) +
  geom_line(aes(group = Method), color = "#E16462FF", linewidth = .3, alpha = 0.4) +
  # last season's data
  scale_linetype_manual(values = c(2, 6)) +
  geom_pointrange(
    data = reference_data,
    aes(
      x = date + lubridate::years(1),
      ymin = lci, ymax = uci
    ),
    color = "grey", size = 0.2
  ) +
  # this season's data
  geom_point(
    data = filter(
      results, method == "actual",
      projection_date %in% supplement_figure_dates
    ),
    aes(x = date, y = proj_uptake),
    color = "#E16462FF", size = 5, shape = 16,
    alpha = 0.6
  ) +
  geom_pointrange(
    data = full_uptake_data,
    aes(ymin = lci, ymax = uci),
    fill = "black", size = 0.2
  ) +
  scale_x_date(
    breaks = lubridate::as_date(plot_dates[seq(3, 31, by = 4)]),
    date_labels = "%b %d",
    minor_breaks = lubridate::as_date(plot_dates)
  ) +
  labs(
    title = "2023/2024 Covid Booster Uptake Projections by Projection Date",
    y = "Uptake (%)"
  ) +
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "bottom") +
  facet_wrap(~projection_date)

ggsave(paste0("output/projections/projection_plot_supplement.png"),
  width = 10.5, height = 7, bg = "white"
)
