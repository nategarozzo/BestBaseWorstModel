# load packages and data
library(tidyverse)

settled_da_lmps <- read_csv("~/BestBaseWorstModel/data/raw/
                              isone_settled_avg_da_lmp_2013_2026.csv")

op_futures <- read_csv("~/BestBaseWorstModel/data/raw/
                         off_peak_isone_historical_futures_2022_2026.csv")

p_futures <- read_csv("~/BestBaseWorstModel/data/raw/
                         off_peak_isone_historical_futures_2022_2026.csv")


clean_settled_lmps <- function(settled_da_lmps){

  settled_da_lmps <- read_csv("~/BestBaseWorstModel/data/raw/
                              isone_settled_da_lmp_2013_2026.csv")
  
}

