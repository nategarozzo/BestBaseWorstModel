# Load Packages
library(tidyverse)

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
  # log(months) accounts for nonlinear relationship with errors and vol
  error ~ factor(delivery_month) * log(months_out),
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
    residual_sq ~ factor(delivery_month) * log(months_out) + futures_price,
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
      z = residual/fitted_sd
    )

# Create smooth density curve representing pdf of z distribution
  z_density <- density(
    model_data$z,
    from = min(model_data$z),
    to   = max(model_data$z)   # inv_cdf can't exceed what training saw
  )

# Create an inverse CDF of the z distribution

cdf <- cumsum(z_density$y)
cdf <- cdf / max(cdf)

inv_cdf <- approxfun(
  x = cdf,
  y = z_density$x,
  rule = 2
)

# Save model components

model_bundle <- list(
  error_model = error_model,
  vol_model = vol_model,
  z_density = z_density,
  inv_cdf = inv_cdf
)
