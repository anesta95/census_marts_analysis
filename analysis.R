library(censusapi)
library(readr)

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

census_apis <- listCensusApis()

marts_variables <- listCensusMetadata(
  name = "timeseries/eits/marts",
  type = "variables"
)

marts_total <- getCensus(
  name = "timeseries/eits/marts",
  key = CENSUS_API_KEY,
  vars = c("cell_value", 
           "data_type_code",
           "program_code"),
  data_type_code = "SM",
  category_code = "44X72",
  seasonally_adj = "yes",
  time = "from 1992",
  show_call = T,
  convert_variables = F
)

# TODO: 
# 1. Retool trailing average function and CSV write out function in 
# `bls_jolts_analysis` and `bls_ces_analysis` scripts to be more modular and
# copy over functions file to here.
# 2. Add more modularized data format to README in all three econ_analysis repos
# 3. Create MoM annualized and 3/6/12? month moving average change line graphs
# for each MARTS industry time series line graphs
# 4. Iteratively save data CSVs and graph PNGs for each MARTS industry
# 5. Add more comments to all three econ analysis `analysis.R` files




