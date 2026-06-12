# Load packages
library(tidyverse)

forecast_prices <- function(
    model_bundle,
    delivery_month,
    months_out,
    futures_price,
    n_sim = 10000
) {
  
  contract <- tibble(
    delivery_month = delivery_month,
    months_out = months_out,
    futures_price = futures_price
  )
  
  mu_hat <- predict(
    model_bundle$error_model,
    newdata = contract
  )
  
  var_hat <- predict(
    model_bundle$vol_model,
    newdata = contract,
    type = "response"
  )
  
  sd_hat <- sqrt(var_hat)
  
  inv_cdf <- switch(
    case_when(
      delivery_month %in% c("Dec", "Jan", "Feb") ~ "winter",
      delivery_month %in% c("Jun", "Jul", "Aug") ~ "summer",
      TRUE                                        ~ "shoulder"
    ),
    winter   = model_bundle$inv_cdf_winter,
    summer   = model_bundle$inv_cdf_summer,
    shoulder = model_bundle$inv_cdf_shoulder
  )
  
  z_sim <- inv_cdf(runif(n_sim))
  
  # Eliminates negative DA LMP
  settlement_sim <- pmax(
    futures_price + mu_hat + sd_hat * z_sim,
    0
  )
  
  # Return simulation info
  list(
    forecast = settlement_sim,
    delivery_month = delivery_month,
    months_out = months_out,
    futures_price = futures_price
  )
}

summarize_forecast <- function(simulation_results) {
  
  simulations <- simulation_results$forecast
  
  tibble(
    delivery_month        = simulation_results$delivery_month,
    months_out            = simulation_results$months_out,
    futures_price         = simulation_results$futures_price,
    
    expected_settlement   = mean(simulations),
    expected_error        = mean(simulations) - simulation_results$futures_price,
    settlement_volatility = sd(simulations),
    
    p10_settlement = quantile(simulations, 0.10),  # bearish case
    p25_settlement = quantile(simulations, 0.25),  # mild downside
    p50_settlement = quantile(simulations, 0.50),  # median
    p75_settlement = quantile(simulations, 0.75),  # mild upside
    p95_settlement = quantile(simulations, 0.95)   # bullish case
  )
  
}
