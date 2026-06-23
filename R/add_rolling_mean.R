# add_rolling_mean.R
# Adds rolling mean adjustment to existing model bundle
library(tidyverse)
library(zoo)

# Load existing model bundle
model_bundle <- readRDS("model_bundle.rds")

# Compute rolling mean
errors_rolling <- errors |>
  arrange(delivery_month, current_year, current_month_num) |>
  group_by(delivery_month) |>
  mutate(
    rolling_mean_error = lag(
      rollmean(error, k = 6, fill = NA, align = "right")
    )
  ) |>
  ungroup() |>
  filter(!is.na(rolling_mean_error))

# Fit new error model with rolling mean on top of existing model data
model_data_rolling <- model_data |>
  left_join(
    errors_rolling |>
      group_by(delivery_month, months_out, futures_price) |>
      summarise(rolling_mean_error = mean(rolling_mean_error, na.rm = TRUE),
                .groups = "drop"),
    by = c("delivery_month", "months_out", "futures_price")
  ) |>
  filter(!is.na(rolling_mean_error))

error_model_rolling <- lm(
  error ~ factor(delivery_month) * splines::ns(months_out, df = 6) + rolling_mean_error,
  data = model_data_rolling
)

# Get most recent rolling mean for each delivery month
current_rolling_means <- errors_rolling |>
  group_by(delivery_month) |>
  slice_max(
    order_by = interaction(current_year, current_month_num, delivery_year),
    n = 1,
    with_ties = FALSE
  ) |>
  select(delivery_month, rolling_mean_error) |>
  ungroup() |>
  mutate(rolling_mean_error = pmin(pmax(rolling_mean_error, -20), 20))

# Add to existing model bundle — vol model unchanged
model_bundle$error_model           <- error_model_rolling
model_bundle$current_rolling_means <- current_rolling_means
model_bundle$errors_rolling        <- errors_rolling

saveRDS(model_bundle, "model_bundle.rds")