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
  vars = c("category_code", 
           "cell_value", 
           "data_type_code",
           "program_code",
           "seasonally_adj"),
  data_type_code = "SM",
  category_code = "44X72",
  seasonally_adj = "yes",
  time = "from 1992",
  show_call = T,
  convert_variables = F
)