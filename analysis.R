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

# Basic viz caption citation:
base_viz_caption <- "Adjusted for inflation by CPI-U. Seasonally adjusted as of MMM. 'YY\nSource: Census Bureau Advance Monthly Retail Trade Survey | Chart: Adrian Nesta"

# Final df columns to select
final_cols <- c("date", "date_period_text", "value", "data_element_text", "data_measure_text", 
                "date_measure_text", "data_transform_text", "geo_entity_type_text",
                "geo_entity_text", "seas_adj_text", "viz_type_text")

# Vector of the dates of the recessionary periods defined by the NBER from here:
# https://fred.stlouisfed.org/series/USREC
recession_dates_df <- get_fred_data("USREC", FRED_API_KEY)

recession_dates <- filter(recession_dates_df, value == 1L) %>% 
  pull(date)

# Data frame of the dates and values of monthly CPI-U all items inflation from
# the BLS via FRED here: 
cpi_u_df <- get_fred_data("CPIAUCSL", FRED_API_KEY)

# Calculating period-to-current inflation adjustment
cpi_u_inf_adj_df <- cpi_u_df %>% 
  arrange(desc(date)) %>% 
  mutate(inf_adj = value[1] / value) %>% 
  select(date, inf_adj)

# Reference Files
marts_cat_ref <- read_csv(
  file = "./reference_files/marts_category_codes_reference.csv",
  col_names = T,
  col_types = "cc"
)

# Getting total MARTS data from 1992 to present for monthly sales (data_type_code SM)
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
  as_tibble()
  
# Adding in econanalyzr variables and calculating YoY, MoM, MoM annualized change
marts_total_cur_yoy_mom_momann_df <- marts_total_raw %>% 
  mutate(date = base::as.Date(paste0(time, "-01")),
         date_period_text = "Monthly",
         value = as.numeric(cell_value) * 1e6,
         data_measure_text = "Sales",
         geo_entity_type_text = "Nation",
         geo_entity_text = "US",
         seas_adj_text = "Seasonally adjusted") %>% 
  left_join(marts_cat_ref, by = "category_code") %>%
  arrange(data_element_text, desc(date)) %>% 
  left_join(cpi_u_inf_adj_df, by = "date") %>%
  group_by(data_element_text) %>% 
  mutate(`Year-over-year|Percent change` = (value / lead(value, n = 12)) - 1,
         `Month-over-month|Percent change` = (value / lead(value, n = 1)) - 1,
         `Month-over-month|Percent change;Annualized` = ((value / lead(value, n = 1)) ^ 12) - 1,
         `Current|Raw` = value) %>% 
  ungroup() %>% 
  select(-value) %>% 
  pivot_longer(cols = contains("|"),
               names_to = "date_measure_text-data_transform_text",
               values_to = "value") %>% 
  separate_wider_delim(cols = `date_measure_text-data_transform_text`,
                       delim = "|",
                       names = c("date_measure_text", "data_transform_text")) %>% 
  select(date, date_period_text, value, data_element_text, data_measure_text, 
         date_measure_text, data_transform_text, geo_entity_type_text,
         geo_entity_text, seas_adj_text)

# Making list of data frames split by element
marts_total_cur_yoy_mom_momann_df_list <- marts_total_cur_yoy_mom_momann_df %>% 
  mutate(viz_type_text = "Time series line") %>% 
  filter((date_measure_text == "Year-over-year" & 
            data_transform_text == "Percent change") | 
           (date_measure_text == "Month-over-month" & 
              data_transform_text == "Percent change;Annualized")) %>% 
  group_split(data_element_text, .keep = T)

# Creating list of non-recession averages of all measures in the list of data frames
marts_total_yoy_non_recession_avg_list <- map(marts_total_cur_yoy_mom_momann_df_list, 
                                             function(x) {
                                               y <- x %>% filter(date_measure_text == "Year-over-year")
                                               get_avg_col_val(
                                                 y, recession_dates, value, "exclusive")
                                             })

# Creating list of recession averages of all measures in the list of data frames
marts_total_yoy_recession_avg_list <- map(marts_total_cur_yoy_mom_momann_df_list, 
                                          function(x) {
                                            y <- x %>% filter(date_measure_text == "Year-over-year")
                                            get_avg_col_val(
                                              y, recession_dates, value, "exclusive")
                                          })
# Combining these lists
marts_total_yoy_avg_list <- map2(
  marts_total_yoy_non_recession_avg_list, 
  marts_total_yoy_recession_avg_list, ~c(.x, .y))

# Editing data frame list to filter dates to only past two years.
marts_total_cur_yoy_mom_momann_ts_df_list <- map(marts_total_cur_yoy_mom_momann_df_list, ~filter_recent_dates(.x, 48, "month"))
  
  
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