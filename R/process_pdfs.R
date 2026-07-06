library(tidyverse)
library(pdftools)

parse_ice_rec_pdf <- function(file) {
  
  # Read PDF text
  txt <- pdf_text(file)
  
  # Split into lines and clean spacing
  lines <- txt |>
    str_split("\n") |>
    unlist() |>
    str_squish()
  
  lines <- lines[lines != ""]
  
  # Extract report date
  report_date_line <- lines[str_detect(lines, "^\\d{2}-[A-Za-z]{3}-\\d{4}$")][1]
  report_date <- as.Date(report_date_line, format = "%d-%b-%Y")
  
  # Extract contract header
  header_line <- lines[str_detect(lines, "^[A-Z]{2,5}-")][1]
  if (is.na(header_line)) stop("Could not find contract header line.")
  
  contract_code <- str_extract(header_line, "^[A-Z]{2,5}")
  contract_name <- str_remove(header_line, "^[A-Z]{2,5}-")
  
  # Keep only contract rows
  contract_rows <- lines[
    str_detect(lines, paste0("^", contract_code, "\\s+[A-Z][a-z]{2}\\d{2}\\s"))
  ]
  
  if (length(contract_rows) == 0) {
    warning("No contract rows found for ", contract_code, " in file: ", basename(file))
    return(tibble(
      report_date    = as.Date(character()),
      contract_code  = character(),
      contract_name  = character(),
      contract_month = character(),
      settle_price   = double(),
      change         = double()
    ))
  }
  
  # Parse rows using regex
  # Note: first extracted number is always the year digits from contract month (e.g. 26 from Jul26)
  # Without OHLC (n_nums = 10 or 11): year | settle | change | volume...
  # With OHLC    (n_nums = 15):        year | open | high | low | close | settle | change | volume...
  tibble(raw = contract_rows) |>
    mutate(
      contract_code  = str_extract(raw, "^[A-Z]{2,5}"),
      contract_month = str_extract(raw, "[A-Z][a-z]{2}\\d{2}"),
      nums           = str_extract_all(raw, "-?\\d+\\.\\d+|-?\\d+"),
      n_nums         = map_int(nums, length),
      settle_price   = map2_dbl(nums, n_nums, ~ {
        vals <- as.numeric(.x)
        if (.y == 15) vals[6] else vals[2]
      }),
      change = map2_dbl(nums, n_nums, ~ {
        vals <- as.numeric(.x)
        if (.y == 15) vals[7] else vals[3]
      }),
      report_date   = report_date,
      contract_name = contract_name
    ) |>
    select(report_date, contract_code, contract_name,
           contract_month, settle_price, change)
}