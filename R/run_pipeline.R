library(tidyverse)

setwd("~/BestBaseWorstModel")

# run pipeline
source("R/data_cleaning.R")
source("R/calculate_errors.R")
source("R/futures_error_model.R")
source("R/add_rolling_mean.R")
source("R/monte_carlo_forecast.R")

# Edit contracts here
contracts <- tibble(
  delivery_month = c("Jan", "May"),
  months_out     = c(3, 1),
  futures_price  = c(75, 47.40)
)

# Run forecasts
results <- contracts |>
  rowwise() |>
  mutate(sim = list(forecast_prices(
    model_bundle   = model_bundle,
    delivery_month = delivery_month,
    months_out     = months_out,
    futures_price  = futures_price
  ))) |>
  ungroup() |>
  mutate(summary = map(sim, summarize_forecast)) |>
  select(summary) |>
  unnest(summary) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))