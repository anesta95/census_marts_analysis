library(censusapi)


### Objects Needed ###
# API Keys
con <- file(description = ".api_keys/census.txt", open = "rt", blocking = F)
CENSUS_API_KEY <- readLines(con, n = 1)
close(con)

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
# 1. Import recession dates from FRED
# 2. Make a vector of all the MARTS category codes to loop over
# 3. Retool trailing average function and CSV write out function in 
# `bls_jolts_analysis` and `bls_ces_analysis` scripts to be more modular and
# copy over functions file to here.
# 4. Add more modularized data format to README in all three econ_analysis repos
# 5. Create MoM annualized and 3/6/12? month moving average change line graphs
# for each MARTS industry time series line graphs
# 6. Iteratively save data CSVs and graph PNGs for each MARTS industry
# 7. Add more comments to all three econ analysis `analysis.R` files




