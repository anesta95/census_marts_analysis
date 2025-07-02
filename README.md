# U.S. Advance Monthly Sales for Retail and Food Services Analysis

This project contains code that downloads, analyzes, and visualizes data
from the [Advance Monthly Sales for Retail and Food Services Survey](https://www.census.gov/retail/sales.html) (MARTS) 
that is run monthly by the U.S. Department of Commerce's Census Bureau. 

Data analysis and visualization is executed by the [R](https://www.r-project.org/) 
code in the `analysis.R` and `functions.R` files in the top-level directory.
Project libraries and other resources needed to run the code can be managed through 
the [renv](https://rstudio.github.io/renv/) reproducible environment in the `renv`
folder and `renv.lock` and `.Rprofile` files in the top-level directory.

Details about data measures and data and graphics files are provided below.

## Data Measures

This project currently analyzes the following [data estimate](https://www.census.gov/retail/definitions.html) from MARTS:

* __Sales__: Sales include merchandise sold (for cash or credit at retail or wholesale) by establishments primarily engaged in retail trade. Services that are incidental to the sale of merchandise, and excise taxes that are paid by the manufacturer or wholesaler and passed along to the retailer are also included. Sales are net, after deductions, of refunds and allowances for merchandise returned by customers. Sales exclude sales taxes collected directly from customers and paid directly to a local, state, or federal tax agency. The estimates of sales measure the operations receipts rendered by stores that primarily sell at _retail_. The sales estimates represent total sales and receipts of all establishments primarily engaged in retail trade. They do **not** include sales at retail by manufacturers, wholesalers, service establishments, and others whose primary activity is _other than retail trade_. Because the retail establishment is the basic unit of measure, the published estimates of sales by type of retail store are not intended to measure the total sales for a given commodity or merchandise line.

The coverage of establishments included in the data inicludes retail and food service companies with one or more establishments that sell merchandise and associated services to final consumers ([NAICS](https://www.census.gov/naics/) Sector 44-45 & Sector 72, subsector 722).

Sales prior to the most recent month of data are inflation-adjusted with seasonally adjusted the [CPI-U Consumer Price Index for All Urban Consumers: All Items in U.S. City Average](https://fred.stlouisfed.org/series/CPIAUCSL) from the [Bureau of Labor Statistics](https://www.bls.gov/cpi/). All sales data are the [seasonally adjusted](https://www.census.gov/retail/marts/how_surveys_are_collected.html) figures from the Census Bureau that are adjusted for seasonal variation and holiday and trading-day differences using the Census Bureau's [X-13ARIMA-SEATS](https://www.census.gov/data/software/x13as.html) program using the X-11 filter-based adjustment procedure.


## Data Files and Graphics

### Data conventions
Every econ analysis data CSV file at minimum will have the following columns:
* `date`: The date associated with the data in the data row. The date will be in `YYYY-MM-DD` format regardless of the time period the date captures. All dates will be the first day of the time period. For example, data for April 2025 will be displayed as `2025-04-01`. Data for Q2 2025 will be `2025-04-01`. Data for the year 2025 will be `2025-01-01`. This will have a data type `double` with a class of `Date`.
* `date_period_text`: The time period that each row of the data captures. The most common formats are `Monthly`, `Quarterly`, and `Annually`. This will have a data type and class of `character`.
* `value`: The value that is being measured in the data. This will have a data type of `double` and a class of `numeric`.
* `data_element_text`: What the data in the `value` column describes. This will have a data type and class of `character`.
* `data_measure_text`: The mathematical expression the data in the `value` column is expressed as. The most common are `Level`, `Rate`, `Ratio`, `Percentage`, `Proportion`, and `Index`. This will have a data type and class of `character`.
* `date_measure_text`: The change in dates measured by the data in the `value` column. The most common are `Current`, `Year-over-year`, `Month-over-month` and `Quarter-over-quarter`. This will have a data type and class of `character`.
* `data_transform_text`: Any mathematical transformations applied to the data. The most common are `Raw`, `Percent change`, `Annualized`, `Trail N` where `N` is a number of periods in the `date_period_text` column. There can be multiple transformations for each row. Transformations are delimited by semi-colons `;` and are stated _in order of transformation_. For example, `Trail 3;Percent change` will be the percentage change between the trailing 3 period average of the current period — denoted in the `date` column — and the trailing 3 period average of the previous period which is deduced from the `date_measure_text`. Conversely, `Percent change;Trail 3` will be the trailing 3 period average applied to the percentage change between the current period and the previous period across the data series. This will have a data type and class of `character`.
* `geo_entity_type_text`: The geographic entity _type_ the data in the `value` column is covering. This will have a data type and class of `character`. If the region is in the United States there is a good chance it will be within the [Census Bureau Geographic Entity Hierarchy](https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf).
* `geo_entity_text`: The name(s) geographic entity/entities that are described by the data.
* `viz_type_text`: The type of visualization made by the data in the `value` column. The most common are `Time series line`, `Bar`, `Map`, and `Scatter`. This will have a data type and class of `character`.

### Naming conventions
All graphics are PNG files in the `charts` directory. Every data visualization 
has a corresponding CSV file that was used to create it in the `data` directory.
Both CSVs and PNGs are named with the following format where each aspect of the 
data is delimited with a dash `-` and spaces are replaced with underscores `_`.

Data and visualization files will be named in the following order:

1. `date`
2. `date_period_text`
3. `data_element_text`
4. `data_measure_text`
5. `date_measure_text`
6. `data_transform_text`
7. `geo_entity_type_text`
8. `geo_entity_text`
9. _Any other aspects of the data specific to the release that are needed to uniquely identify it._ Examples include `industry_text`, `size_class_text`, `seas_adj_text`, among others.
10. `viz_type_text`

#### Examples
* CSV files: 
  * `2025-05-01_2023-05-01-monthly-retail_trade-level-year-over-year-percent_change_trail_3-nation-us-seasonally_adjusted-time_series_line.csv`
  * `2025-05-01-monthly-5_data_element-level-month-over-month-percent_change-nation-us-seasonally_adjusted-bar.csv`
* PNG files: 
  * `2025-05-01_2023-05-01-monthly-retail_trade-level-year-over-year-percent_change_trail_3-nation-us-seasonally_adjusted-time_series_line.png`
  * `2025-05-01-monthly-5_data_element-level-month-over-month-percent_change-nation-us-seasonally_adjusted-bar.png`

Every column in the dataset with the `_text` suffix will be included in the filename, in addition to the `date` column. Data files will also include columns that have further information that is _not_ needed to uniquely identify the data series. Examples of this include the `value` column, variables with the `_code` suffix such as `industry_code`, `fips_code`,`preliminary_code`, as well as `moe`, and `moe_level`, among others. 


This specific repository will have data with the following variables:
### Included data

| Variable Name     | Variable Data Class | Variable Description                                                                                                                                                                                                                                                                                                                                       |
| ----------------- | ------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| date              | Date                | Date associated with data row. Will be in `YYYY-MM-DD` format. Monthly data will automatically be coded as the first day of said month, i.e. January 2025 is `2025-01-01`                                                                                                                                                                                  |
| date_period_text  | character           | The time period that each row of the data captures. Currently this will be `monthly`.                                                                                                                                                                                                                                                                                |
| value             | numeric             | The actual numerical value of data that is described by the columns with the `_text` suffix. *This figure is __not__ included in the data or chart filenames.*
| data_element_text | character           | The [data element](https://github.com/anesta95/census_marts_analysis/blob/main/reference_files/marts_category_codes_reference.csv) that is represented by the `value` column. Currently one of the two to four digit NAICS industry that establishments are part of for sales figures in a given row of data.                                                                                        |
| data_measure_text       | character           | The description of the what the numerical value the data in the `value` column is measuring. Currently just `Level`.                                                                                                                                                                                                                           |
| date_measure_text | character           | The change in dates measured by the data in the value column. Currently this will include `Year-over-year`, or `Month-over-month`.                                                                                                                                                                                 |
| data_transform_text       | character           | The description of what mathematical transformation(s) have been applied to the data in the `value` column. Multiple transformations delimited by semi-colons `;`. Currently can be `Percent change` or `Percent change;Trail 3`. If multiple data transformations are included, the chart filename will denote with `N_data_transform` where `N` is the number of data transformations in the data file.                                                                                                                                                                                                                           |
| geo_entity_type_text  | character           | The geographic entity type that is present in the `geo_entity_text` column. Currently will only be `Nation`.                                                                                                                                                                                                                                                    |
| geo_entity_text       | character           | The name(s) geographic entity/entities that are described by the data. These are defined by the [U.S. Census Bureau](https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf). If multiple geographic entities are included, the chart filename will denote with `N_geo_entity` where `N` is the number of geographic entities in the data file.                                                                                                                                                                                              |
| seas_adj_text     | character           | Text that will denote if the data in the `value` column is seasonally-adjusted or not.                                                                                                                                                                                                                                                                     |
| industry_code     | character           | The [NAICS code](https://www.census.gov/naics/?58967?yearbck=2017) that the data is associated with. This code matches the text in the `data_element_text`. Currently MARTS reports data from the 2017 NAICS codes. *This figure is __not__ included in the data or chart filenames.*                                                                                                                 |
| viz_type_text     | character           | The visualization type the data is used for. Currently o of `Time series line`, `Bar`, `Map`, or `Scatter` which stand for time series line chart, bar chart map and scatter plot.                                                                                                                                                                                          | 