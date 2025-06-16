library(censusapi)
library(readr)
library(dplyr)
library(httr)
library(ggplot2)
library(ggtext)
library(stringr)
library(zoo)
library(lubridate)
library(purrr)
library(rlang)
library(scales)
library(tidyr)

# Importing R file with custom functions
source("functions.R")

### Objects Needed ###
# API Keys
con <- file(description = ".api_keys/census.txt", open = "rt", blocking = F)
CENSUS_API_KEY <- readLines(con, n = 1)
close(con)

con <- file(description = ".api_keys/fred.txt", open = "rt", blocking = F)
FRED_API_KEY <- readLines(con, n = 1)
close(con)

# Vector of the dates of the recessionary periods defined by the NBER from here:
# https://fred.stlouisfed.org/series/USREC
recession_dates_df <- get_fred_data("USREC", FRED_API_KEY)

recession_dates <- filter(recession_dates_df, value == 1L) %>% 
  pull(date)

# Reference Files
marts_cat_ref <- read_csv(
  file = "./reference_files/marts_category_codes_reference.csv",
  col_names = T,
  col_types = "cc"
)

# Getting total MARTS data from 1992 to present for monthly sales (data_type_code SM)
# Adding in econanalyzr variables
marts_total_raw <- getCensus(
  name = "timeseries/eits/marts",
  key = CENSUS_API_KEY,
  vars = c("cell_value", 
           "program_code",
           "category_code"),
  data_type_code = "SM",
  seasonally_adj = "yes",
  time = "from 1992",
  show_call = T,
  convert_variables = F
) %>% 
  as_tibble() %>% 
  mutate(date = base::as.Date(paste0(time, "-01")),
         date_period_text = "Monthly",
         value = as.numeric(cell_value) * 1e6,
         data_measure_text = "Level",
         date_measure_text = "Current",
         data_transform_text = "Raw",
         geo_entity_type_text = "Nation",
         geo_entity_text = "US",
         seas_adj_text = "Seasonally adjusted") %>% 
  left_join(marts_cat_ref, by = "category_code") %>% 
  arrange(category_desc, desc(date))

marts_total_raw
# TODO: 
# Charts to make:
# 1. Time series line chart of year-over-year change and month-over-month change annualized for every category
# 2. Time series line chart of month-over-month change and month-over-month trailing 3 month average change for every category
# 3. Bar chart of year-over-year change for every category
# 4. Bar chart of month-over-month change for every category
# Do 3 and 4 need to be trailing 3 month averages?

### CENSUS API NOTES ###
# How to list available census apis and get api metdata in a tibble
# census_apis <- listCensusApis()
# 
# marts_variables <- listCensusMetadata(
#   name = "timeseries/eits/marts",
#   type = "variables"
# )