# Load packages and data
library(tidyverse)

futures <- read_csv("data/clean/futures.csv")
monthly_lmps <- read_csv("data/clean/settled_avg_da_lmps.csv")

 
calculate_errors <- function(futures, monthly_lmps){
 # Calculate errors for each futures price
  errors <- futures |>
    # Remove observations where delivery month is same as current month
    filter(!(current_month == delivery_month & 
               current_year == delivery_year)) |>
    left_join(monthly_lmps,
              by = c(
                "delivery_month" = "month",
                "delivery_year" = "year"
              )) |>
    mutate(
      error = round(settled_avg_da_lmp - futures_price, 2)
    ) |>
    filter(!is.na(error)) |> # Removes months with no contracts
    mutate(
      current_month_num = match(current_month, month.abb),
      delivery_month_num = match(delivery_month, month.abb),
      months_out =
        (delivery_year - current_year) * 12 +
        (delivery_month_num - current_month_num)
    ) |>
    filter(months_out > 0) # Removes expired contracts
  
  errors
}

errors <- calculate_errors(futures, monthly_lmps)