# load packages and data
library(tidyverse)
library(lubridate)

raw_settled_da_lmps <- read_csv(
  "data/raw/isone_settled_avg_da_lmp_2013_2026.csv"
  )

raw_op_futures <- read_csv(
  "data/raw/off_peak_isone_historical_futures_2022_2026.csv"
  )

raw_p_futures <- read_csv(
  "data/raw/peak_isone_historical_futures_2022_2026.csv"
  ) |>
  mutate(
    `Delivery Month` = gsub("-", "", as.character(`Delivery Month`))
  ) |>
  mutate(
    `Delivery Month` = paste0(
      substr(`Delivery Month`, 3, 5),
      substr(`Delivery Month`, 1, 2)
    )
  )
  
# Function for cleaning the monthly avg lmps
tidy_settled_da_lmps <- function(raw_settled_da_lmps){
  
  clean_settled_da_lmps <- raw_settled_da_lmps |>
    rename("month" = "Month") |>
    select("month":`2026`) |>
    pivot_longer(cols = `2013`:`2026`,
                 names_to = "year",
                 values_to = "settled_avg_da_lmp")
  
  clean_settled_da_lmps

}

# Function for cleaning the futures data
tidy_futures <- function(raw_futures){
  
  clean_futures <- raw_futures |>
    rename("delivery_month" = `Delivery Month`) |>
    pivot_longer(cols = -delivery_month,
                 names_to = "date",
                 values_to = "price") |>
    filter(!is.na(price)) |>
    separate(
      delivery_month,
      into = c("delivery_month", "delivery_year"),
      sep = 3
    ) |>
    mutate(
      delivery_year = paste0("20", delivery_year)
    ) |>
    mutate(
      date = mdy(date),
      current_month = month(date, label = TRUE, abbr = TRUE),
      current_day = day(date),
      current_year = year(date)
    ) |>
    mutate(
      delivery_year = as.integer(delivery_year),
      current_month = as.character(current_month),
      current_year = as.integer(current_year)
    ) |>
    select(c(current_month,
             current_day, 
             current_year,
             delivery_month,
             delivery_year,
             price))
  
  clean_futures
  
}

# Execute functions on raw data and merge peak/off-peak prices to get avg price

clean_settled_da_lmps <- tidy_settled_da_lmps(raw_settled_da_lmps)
clean_op_futures <- tidy_futures(raw_op_futures)
clean_p_futures <- tidy_futures(raw_p_futures)

futures <- clean_op_futures |> 
  left_join(
    clean_p_futures,
    by = c(
      "current_month",
      "current_day",
      "current_year",
      "delivery_month",
      "delivery_year"
    )
  ) |>
  mutate(futures_price = round(((as.numeric(price.x) + as.numeric(price.y))/2),
                               2)) |>
  select(-c(price.x, price.y)) |>
  filter(!is.na(futures_price))

# Write clean data to new folder

write_csv(clean_settled_da_lmps, "data/clean/settled_avg_da_lmps.csv")
write_csv(futures, "data/clean/futures.csv")


