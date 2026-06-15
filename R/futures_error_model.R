# Load Packages
library(tidyverse)
select <- dplyr::select

prep_futures_data <- function(errors) {
  
  model_data <- errors |>
    select(c(delivery_month, months_out, futures_price, error)) |>
    group_by(delivery_month, months_out) |>
    # remove small sample size combinations to reduce variability
    filter(n() >= 20) |>
    ungroup()
  
  model_data

}

# Predict error based on month and months out
model_data <- prep_futures_data(errors)

# Fit error model
error_model <- lm(
  # Spline (df=6) accounts for nonlinear relationship with errors and vol
  error ~ factor(delivery_month) * splines::ns(months_out, df = 6),
  data = model_data
)

# Calculate residuals
  model_data  <- model_data |>
  mutate(
  fitted_error = predict(error_model),
  residual = error - fitted_error
    ) |>
  mutate(residual_sq = residual^2)

# Fit volatility model EXPLAIN WHY
  vol_model <- glm(
    residual_sq ~ factor(delivery_month) * splines::ns(months_out, df = 6) + futures_price,
    family = Gamma(link = "log"),
    data = model_data
  )

# Fit volatility model to data
  model_data <- model_data |>
    mutate(
      fitted_var = predict(
        vol_model,
        type = "response"
      ),
      fitted_sd = sqrt(fitted_var)
    )
  
  # Standardize residuals
  model_data <- model_data |>
    mutate(
      z = residual / fitted_sd,
      season = case_when(
        delivery_month %in% c("Dec", "Jan", "Feb") ~ "winter",
        delivery_month %in% c("Jun", "Jul", "Aug") ~ "summer",
        TRUE                                        ~ "shoulder"
      )
    )
  
  # Build seasonal inverse CDFs
  build_inv_cdf <- function(z_vals) {
    z_density <- density(z_vals, from = min(z_vals), to = max(z_vals))
    cdf <- cumsum(z_density$y)
    cdf <- cdf / max(cdf)
    approxfun(x = cdf, y = z_density$x, rule = 2)
  }
  
  inv_cdf_winter   <- build_inv_cdf(model_data$z[model_data$season == "winter"])
  inv_cdf_summer   <- build_inv_cdf(model_data$z[model_data$season == "summer"])
  inv_cdf_shoulder <- build_inv_cdf(model_data$z[model_data$season == "shoulder"])
  
  # Save model components
  model_bundle <- list(
    error_model      = error_model,
    vol_model        = vol_model,
    inv_cdf_winter   = inv_cdf_winter,
    inv_cdf_summer   = inv_cdf_summer,
    inv_cdf_shoulder = inv_cdf_shoulder
  )
  
saveRDS(model_bundle, "model_bundle.rds")