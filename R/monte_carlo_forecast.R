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
    months_out = months_out
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
  
  z_sim <- model_bundle$inv_cdf(
    runif(n_sim)
  )
  
  settlement_sim <-
    futures_price +
    mu_hat +
    sd_hat * z_sim
  
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
    delivery_month = simulation_results$delivery_month,
    months_out = simulation_results$months_out,
    futures_price = simulation_results$futures_price,
    
    expected_settlement = mean(simulations),
    expected_error =
      mean(simulations) - simulation_results$futures_price,
    
    settlement_volatility = sd(simulations),
    
    downside_99 = quantile(simulations, 0.01),
    downside_95 = quantile(simulations, 0.05),
    downside_50 = quantile(simulations, 0.25),
    
    median_settlement = quantile(simulations, 0.50),
    
    upside_50 = quantile(simulations, 0.75),
    upside_95 = quantile(simulations, 0.95),
    upside_99 = quantile(simulations, 0.99)
  )
  
}