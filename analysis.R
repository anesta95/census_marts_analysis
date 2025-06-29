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

# More info on MARTS here:
# https://www.census.gov/retail/index.html
# https://www.census.gov/retail/sales.html
# https://www.census.gov/retail/marts/about_the_surveys.html
# https://www.census.gov/retail/marts/how_surveys_are_collected.html
# https://www.census.gov/retail/marts_faqs.html

# Coverage: Retail and food service companies with one or more establishments that sell merchandise and associated services to final consumers (NAICS Sector 44-45 & Sector 72, subsector 722).

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
# NOTE these still use 2017 NAICS codes: https://www.census.gov/naics/?input=44&chart=2017
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

### Analysis & Visualization ###
## Time Series Line Graphs ##
# YoY inflation adjusted change trailing 3 month average
# Adding in econanalyzr variables and calculating YoY, and MoM change
marts_total_cur_yoy_mom_df <- marts_total_raw %>% 
  mutate(date = base::as.Date(paste0(time, "-01")),
         date_period_text = "Monthly",
         value = as.numeric(cell_value) * 1e6,
         data_measure_text = "Level",
         geo_entity_type_text = "Nation",
         geo_entity_text = "US",
         seas_adj_text = "Seasonally adjusted") %>% 
  left_join(marts_cat_ref, by = "category_code") %>%
  arrange(data_element_text, desc(date)) %>% 
  left_join(cpi_u_inf_adj_df, by = "date") %>%
  mutate(value = value * inf_adj) %>% 
  group_by(data_element_text) %>% 
  mutate(`Year-over-year|Percent change` = (value / lead(value, n = 12)) - 1,
         `Month-over-month|Percent change` = (value / lead(value, n = 1)) - 1,
         #`Month-over-month|Percent change;Annualized` = ((value / lead(value, n = 1)) ^ 12) - 1,
         `Current|Raw` = value) %>% 
  ungroup() %>% 
  select(-value) %>% 
  pivot_longer(cols = contains("|"),
               names_to = "date_measure_text-data_transform_text",
               values_to = "value") %>% 
  separate_wider_delim(cols = `date_measure_text-data_transform_text`,
                       delim = "|",
                       names = c("date_measure_text", "data_transform_text")) %>% 
  rename(industry_code = category_code) %>% 
  select(date, date_period_text, value, data_element_text, data_measure_text, 
         date_measure_text, data_transform_text, geo_entity_type_text,
         geo_entity_text, seas_adj_text, industry_code)

# Making list of data frames split by element
marts_total_yoy_df_list <- marts_total_cur_yoy_mom_df %>% 
  mutate(viz_type_text = "Time series line") %>% 
  filter(date_measure_text == "Year-over-year") %>%
  group_by(data_element_text) %>% 
  make_trail_avg_col(3, T) %>% 
  filter(str_detect(data_transform_text, "Trail")) %>% 
  group_split()

# Creating list of non-recession averages of all measures in the list of data frames
marts_total_yoy_non_recession_avg_list <- marts_total_cur_yoy_mom_df %>% 
  filter(date_measure_text == "Year-over-year") %>% 
  group_split(data_element_text, .keep = T) %>% 
  map(~get_avg_col_val(.x, recession_dates, value, "exclusive"))
  
# Creating list of recession averages of all measures in the list of data frames
marts_total_yoy_recession_avg_list <- marts_total_cur_yoy_mom_df %>% 
  filter(date_measure_text == "Year-over-year") %>% 
  group_split(data_element_text, .keep = T) %>% 
  map(~get_avg_col_val(.x, recession_dates, value, "inclusive"))

# Combining these lists
marts_total_yoy_avg_list <- map2(
  marts_total_yoy_non_recession_avg_list, 
  marts_total_yoy_recession_avg_list, ~c(.x, .y))

# Editing data frame list to filter dates to only past two years.
marts_total_yoy_ts_df_list <- map(marts_total_yoy_df_list, ~filter_recent_dates(.x, 24, "month"))

# Writing out each data frame that will be visualized in a time series chart
# to a CSV
walk(marts_total_yoy_ts_df_list, ~econ_csv_write_out(.x, "./data"))

# Making a list of ggplot line charts from the list of time series data frames
marts_total_cur_yoy_mom_momann_ts_viz_list <- map2(
  marts_total_yoy_ts_df_list, 
  marts_total_yoy_avg_list,
  function(x, y) {
    non_rec_avg <- y[1]
    rec_avg <- y[2]
    
    viz_title <- paste(unique(x$data_element_text), "Sales")
    
    make_ts_line_chart(
      viz_df = x,
      x_col = date,
      y_col = value,
      rec_avg_line = rec_avg,
      non_rec_avg_line = non_rec_avg,
      y_data_type = "percentage",
      viz_title = viz_title,
      viz_subtitle = "<b style = \"color: #1f78b4\">Yearly change</b> trailing 3 month average",
      viz_caption = paste("Average lines for data since Jan. '92.",
                          base_viz_caption)
    )
  }
)  

# Saving list of ggplot line charts to PNGs
walk(marts_total_cur_yoy_mom_momann_ts_viz_list, ~save_chart(.x, "./charts/"))

## Bar graphs ##
# month-over-month inflation-adjusted major categories
# Making and sorting data frame
marts_total_mom_df <- marts_total_cur_yoy_mom_df %>% 
  filter(date_measure_text == "Month-over-month", 
         date == max(date),
         str_detect(data_element_text, "Retail Trade")) %>% 
  arrange(desc(value)) %>% 
  mutate(viz_type_text = "Bar")

# Writing out month-over-month changes data frame to CSV
econ_csv_write_out(marts_total_mom_df, "./data")

# Making ggplot of month-over-month change for each category
marts_total_mom_bar <- make_pct_chg_bar_chart(
  viz_df = marts_total_mom_df,
  x_col = value,
  y_col = data_element_text,
  viz_title = "Overall US Retail Sales",
  viz_subtitle = "Month-over-month percent change",
  viz_caption = base_viz_caption
)

save_chart(marts_total_mom_bar, "./charts/")

# month-over-month inflation-adjusted minor categories
# Bar chart with the GAFO retail sales category found here: https://www.census.gov/retail/definitions.html & https://csimarket.com/glossary/term_GAFO.html
# Combined FRED month-over-month series here: https://fred.stlouisfed.org/series/MRTSMPCSM4400CUSN
# Making and sorting data frame
marts_gafo_mom_df <- marts_total_cur_yoy_mom_df %>% 
  filter(date_measure_text == "Month-over-month", 
         date == max(date),
         data_element_text %in% c(
           "General Merchandise Stores",
           "Department Stores",
           "Clothing and Clothing Access. Stores",
           "Furniture and Home Furnishings Stores",
           "Electronics and Appliance Stores",
           "Sporting Goods, Hobby, Musical Instrument, and Book Stores",
           "Miscellaneous Store Retailers"
         )
  ) %>% 
  arrange(desc(value)) %>% 
  mutate(viz_type_text = "Bar")

# Writing out month-over-month changes data frame to CSV
econ_csv_write_out(marts_gafo_mom_df, "./data")

# Making ggplot of month-over-month change for each category
marts_gafo_mom_bar <- make_pct_chg_bar_chart(
  viz_df = marts_gafo_mom_df,
  x_col = value,
  y_col = data_element_text,
  viz_title = "US Retail Sales at GAFO+ Stores",
  viz_subtitle = "Month-over-month percent change",
  viz_caption = paste("GAFO+ includes stores that sell merchandise normally sold in department stores plus general merchandise stores\n", base_viz_caption)
)

save_chart(marts_gafo_mom_bar, "./charts/")

# month-over-month inflation-adjusted minor categories
# Bar chart with the other 3 digit NAICS categories
# Making and sorting data frame
marts_categories_mom_df <- marts_total_cur_yoy_mom_df %>% 
  filter(date_measure_text == "Month-over-month", 
         date == max(date),
         data_element_text %in% c(
           "Motor Vehicle and Parts Dealers",
           "Auto and Other Motor Vehicles",
           "Building Mat. and Garden Equip. and Supplies Dealers",
           "Food and Beverage Stores",
           "Grocery Stores",
           "Health and Personal Care Stores",
           "Gasoline Stations",
           "Nonstore Retailers",
           "Food Services and Drinking Places"
         )
  ) %>% 
  arrange(desc(value)) %>% 
  mutate(viz_type_text = "Bar")

# Writing out month-over-month changes data frame to CSV
econ_csv_write_out(marts_categories_mom_df, "./data")

# Making ggplot of month-over-month change for each category
marts_categories_mom_bar <- make_pct_chg_bar_chart(
  viz_df = marts_categories_mom_df,
  x_col = value,
  y_col = data_element_text,
  viz_title = "US Retail Sales by Category",
  viz_subtitle = "Month-over-month percent change for stores by NAICS industry",
  viz_caption = base_viz_caption
)

save_chart(marts_categories_mom_bar, "./charts/")