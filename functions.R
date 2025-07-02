### FUNCTIONS ###
# TODO: Comment functions.R file like this https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html

## General functions to manipulate vectors, strings, and other data types ##
# Function to reverse %in% R Operator
`%nin%` <- function(x, y) !(x %in% y)


# Function that takes in data frame of columns with metadata on specific
# details of data or chart and returns a vector of lower-case values for each
#### TODO: Change this function to make sure it works with chart and CSV
#### write-out functions `econ_csv_write_out()` and `save_chart()`
make_metadata_vec <- function(df) {
  metadata_vec <- imap_chr(df, function(x, col_name){
    uniq_col_val <- str_replace_all(unique(x), ";", " ")
    
    if (length(uniq_col_val) > 1) {
      num_instances <- length(uniq_col_val)
      aspect_name <- str_remove(col_name, "_text")
      detail_val <- paste0(num_instances, "_", aspect_name)
    } else {
      detail_val <- uniq_col_val
    }
    
    detail_val_clean <- str_to_lower(detail_val)
    
    return(detail_val_clean)
    
  })
  
  return(metadata_vec)
}

#### TODO: Write a function that checks if data frame has at least the minimum
#### econanalyzr column names and are in the correct order. 

# Function to get average value from a vector of numeric data types after 
# filtering out data values.
#### TODO: Make sure this function uses proper dplyr programming for package environment
get_avg_col_val <- function(df, dts, val_col, filter_type) {
  
  val_col_name <- enquo(val_col)
  
  if (filter_type == "inclusive") {
    df_filtered <- df %>% 
      filter(date %in% dts)
  } else {
    df_filtered <- df %>% 
      filter(date %nin% dts)
  }
  
  avg <- df_filtered %>% 
    pull(!!val_col_name) %>% 
    mean(na.rm = T)
  
  return(avg)
}

# Function that add a moving (trailing) three-month average column to a data frame
# with a numeric column named `value`. 
#### TODO: Make this function more modular to take a variable time period and
#### uses dplyr functional programming to work inside of packages.
make_trail_avg_col <- function(df, trail_amount, additional_transformation = F) {
  
  trail_df <- df %>% 
    arrange(desc(date)) %>% 
    mutate(
      value = rollmean(value, {{ trail_amount }}, fill = NA, align = "left"),
      data_transform_text = ifelse(additional_transformation, 
                                   paste0(data_transform_text, ";", "Trail ", {{ trail_amount }}),
                                   paste("Trail", {{ trail_amount }})
      )
    )
  
  combo_df <- bind_rows(df, trail_df)
  
  return(combo_df)
}

# Function that will return all rows from the most recent date in the data frame
# until a specified time previous to the most recent date.
filter_recent_dates <- function(df, time_amount, time_measure) {
  latest_date <- max(df$date, na.rm = T)
  
  if (time_measure == "month") {
    start_date <- latest_date %m-% months(time_amount)
  } else if (time_measure == "year") {
    start_date <- latest_date - years(time_amount)
  } else if (time_measure == "day") {
    start_date <- latest_date - days(time_amount)
  }
  
  filtered_df <- df %>% 
    arrange(desc(date)) %>% 
    filter(date >= start_date)
  
  return(filtered_df)
}

# Function to make visualization title from either unique combination of 
# `dataelement_text` and `ratelevel_text` columns or use supplied title.
#### TODO: Make this function  conform to new econanalyzr
#### data syntax. Use dplyr functional programming to work inside of packages.
make_chart_title <- function(viz_df, viz_title) {
  if (is.null(viz_title)) {
    viz_title <- str_to_title(
      paste(
        unique(viz_df$data_element_text),
        unique(viz_df$data_measure_text)
      )
    )
  } 
  viz_title_wrapped <- str_wrap(viz_title, width = 40)
  return(viz_title_wrapped)
}

# Function to add 13% of numeric difference in vector of dates to latest date
# in vector for annotation for dashed line recession & non-recession averages.
get_x_annotation_val <- function(diff, dte) {
  x_ann <- base::as.Date(as.numeric(dte) + (.13 * diff))
  return(x_ann)
}

# Function to add on 10% on each side of the range of a numerical vector
get_data_range <- function(vec) {
  simple_range <- range(vec, na.rm = T)
  highest_extra <- simple_range[2] + (abs(simple_range[2]) * .1)
  lowest_extra <- simple_range[1] - (abs(simple_range[1]) * .1)
  extra_range <- c(lowest_extra, highest_extra)
  return(extra_range)
}

#### TODO: Write dynamic function that will annualize a numeric vector of 
#### changes depending on time periods. Use the econanalyzr data syntax and
#### dplyr functional programming to work inside of packages.

#### TODO: Write dynamic function that will create an index of a numeric vector of 
#### data depending on time periods. Use the econanalyzr data syntax and
#### dplyr functional programming to work inside of packages.

## Data gathering functions ##
# Function to retrieve data from FRED API
#### TODO: Make sure this function will be able to work inside packages 
get_fred_data <- function(series_id, api_key) {
  # API doc reference: https://fred.stlouisfed.org/docs/api/fred/series_observations.html
  fred_res <- GET(
    url = paste0(
      "https://api.stlouisfed.org/fred/series/observations?series_id=",
      series_id, "&api_key=", api_key, "&file_type=json"
    )
  )
  
  stop_for_status(fred_res)
  
  fred_content <- content(fred_res, 
                          as = "parsed", 
                          type = "application/json",
                          encoding = "UTF-8")
  
  fred_observations <- fred_content$observations
  
  fred_data <- bind_rows(fred_observations) %>% 
    mutate(across(matches("realtime|date"), base::as.Date),
           value = as.double(value))
  
  return(fred_data)
  
}

# Function to make HTTP GET requests with a provided email in the `user-agent`
# request header to the BLS database and parses returned data as TSVs.
#### TODO: Make sure this function will be able to work inside packages 
get_bls_data <- function(url, email) {
  bls_res <- GET(url = url, user_agent(email))
  stop_for_status(bls_res)
  
  bls_content <- content(bls_res, 
                         as = "parsed",
                         type = "text/tab-separated-values",
                         encoding = "UTF-8",
                         col_names = T,
                         col_types = cols(.default = col_character()),
                         trim_ws = T
  )
  return(bls_content)
  
}

# Function to get additional codes CSVs and return data frames without the 
# `display_level`, `selectable`, and `sort_sequence` columns
#### TODO: Make sure this function will be able to work inside packages 
get_bls_ref_code_table <- function(survey_abb, table_type, email) {
  ref_code_url <- paste0("https://download.bls.gov/pub/time.series/",
                         survey_abb,
                         "/",
                         survey_abb,
                         ".",
                         table_type)
  
  ref_code_df <- get_bls_data(ref_code_url, email = email)
  
  ref_code_df_trimmed <- ref_code_df %>% 
    select(-any_of(c("display_level", "selectable", "sort_sequence")))
  
  return(ref_code_df_trimmed)
  
}

## ggplot2 themes for each visualization type: lines, bars, maps, and scatter plots ##

ts_line_theme <- function() {
  theme_classic() +
    theme(
      panel.grid.major.y = element_line(color = "gray", linewidth = 0.3),
      plot.title = element_text(size = 36, face = "bold", color = "black"),
      plot.margin = margin(20, 100, 20, 20, "pt"),
      plot.subtitle = element_markdown(size = 24, color = "black"),
      plot.caption = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 18, face = "bold", color = "black"),
      axis.text.x = element_text(margin = margin(0, 0, 15, 0, "pt")),
      axis.ticks.y = element_blank(),
      axis.title = element_blank()
    )
  
}

ts_bar_theme <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(size = 36, face = "bold", color = "black"),
      plot.margin = margin(20, 100, 20, 20, "pt"),
      plot.subtitle = element_text(size = 24, color = "black"),
      plot.caption = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 14, color = "black", face = "bold", 
                               margin = margin(b = 15, t = 15, r = 5)),
      axis.line.x = element_blank(),
      axis.title = element_blank()
    )
}

bar_theme <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(size = 36, face = "bold", color = "black"),
      plot.margin = margin(20, 20, 20, 20, "pt"),
      plot.subtitle = element_text(size = 24, color = "black"),
      plot.caption = element_text(size = 10, color = "black"),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.text.y = element_text(size = 14, color = "black", face = "bold", 
                                 margin = margin(b = 15, t = 15, r = 5)),
      axis.ticks.x = element_blank(),
      axis.title = element_blank(),
      
    )
}

map_theme <- function() {
  theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          legend.position = "inside",
          legend.position.inside = c(.035, .66),
          legend.title = element_blank(),
          legend.key.width = unit(.07, "npc"),
          legend.key.height = unit(.05, "npc"),
          legend.ticks = element_line(color = "black"),
          legend.text = element_text(face = "bold", size = 18),
          plot.title = element_text(size = 36, face = "bold", color = "black"),
          plot.margin = margin(20, 20, 20, 20, "pt"),
          plot.subtitle = element_markdown(size = 24, color = "black"),
          plot.caption = element_text(size = 10, color = "black"))
}

scatter_theme <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(size = 36, face = "bold", color = "black"),
      plot.margin = margin(20, 20, 20, 20, "pt"),
      plot.subtitle = element_text(size = 24, color = "black"),
      plot.caption = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 16, color = "black", face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}

## Data Visualization Functions with ggplot2 ##
# Resource on how to adjust legend: https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-legends/
# Tidycensus state map section: https://walker-data.com/census-r/mapping-census-data-with-r.html
# ggplot2 color bar guide: https://ggplot2.tidyverse.org/reference/guide_colourbar.html
# ggplot2 resource: https://ggplot2-book.org/scales-colour

# Function to make the dual current and trailing three-month average
# line time-series plots.
make_ts_line_chart <- function(viz_df, x_col, y_col, rec_avg_line = NULL, 
                               non_rec_avg_line = NULL, y_data_type,
                               viz_title = NULL, viz_subtitle, viz_caption) {
  # TEMPORARY: Creating ad-hoc columns for line size and color
  viz_df <- viz_df %>% 
    mutate(linewidth = if_else(str_detect(data_transform_text, "Annualized"), 0.8, 2.75),
           color = if_else(str_detect(data_transform_text, "Annualized"), "#a6cee3", "#1f78b4"))
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_quo <- enquo(y_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  
  # Getting data range to use for annotation calculations
  data_range <- get_data_range(pull(viz_df, !!y_col_quo))
  
  latest_date_dte <- max(viz_df$date, na.rm = T)
  earliest_date_dte <- min(viz_df$date, na.rm = T)
  # Getting dashed recession/non-recession average line offset
  num_data_dte_range_diff <- diff(as.numeric(range(viz_df$date, na.rm = T)))
  x_ann <- get_x_annotation_val(num_data_dte_range_diff, latest_date_dte)
  
  latest_date_str <- format(latest_date_dte, "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date_str)
  
  
  # Base plt
  plt <- ggplot(viz_df, mapping = aes(x = !!x_col_quo, y = !!y_col_quo)) +
    coord_cartesian(
      xlim = c(earliest_date_dte, latest_date_dte),
      clip = "off") +
    geom_line(mapping = aes(linewidth = linewidth, color = color),
              lineend = "round",
              linejoin = "bevel") +
    scale_x_date(date_labels = "%b. '%y") +
    scale_linewidth_identity() +
    scale_color_identity() +
    guides(
      color = "none",
      linewidth = "none"
    ) +
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    ts_line_theme()
  
  if (!is.null(non_rec_avg_line)) {
    if (between(non_rec_avg_line, data_range[1], data_range[2])) {
      plt <- plt + geom_hline(yintercept = non_rec_avg_line,
                              color = "black",
                              linewidth = 0.75,
                              linetype = "dashed"
      ) + annotate("text",
                   x = x_ann,
                   y = non_rec_avg_line,
                   hjust = 0.5,
                   label = "Non-recession\navg.",
                   color = "black",
                   size = 3.5,
                   fontface = "bold")
    }
  }
  
  if (!is.null(rec_avg_line)) {
    if (between(rec_avg_line, data_range[1], data_range[2])) {
      plt <- plt + geom_hline(yintercept = rec_avg_line,
                              color = "red",
                              linewidth = 0.75,
                              linetype = "dashed"
      ) + annotate("text",
                   x = x_ann,
                   y = rec_avg_line,
                   hjust = 0.5,
                   label = "Recession\navg.",
                   color = "red",
                   size = 3.5,
                   fontface = "bold")
    }
  }
  
  if (y_data_type == "percentage") {
    plt <- plt + scale_y_continuous(labels = label_percent(scale = 100, 
                                                           suffix = "%", 
                                                           accuracy = 0.1))
  } else if (y_data_type == "dollar") {
    plt <- plt + scale_y_continuous(labels = label_currency(scale = 1, 
                                                            prefix = "$", 
                                                            scale_cut = cut_short_scale()))
  } else if (y_data_type == "number") {
    plt <- plt + scale_y_continuous(labels = label_number(scale = 1, 
                                                          scale_cut = cut_short_scale()))
  }
  
  return(plt)
  
}

# Function to make time series bar graph
make_ts_bar_chart <- function(viz_df, x_col, y_col_one, rec_avg_line = NULL, 
                              non_rec_avg_line = NULL, y_data_type,
                              viz_title = NULL, viz_subtitle, viz_caption) {
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_one_quo <- enquo(y_col_one)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  
  # Getting data range to use for annotation calculations
  data_range <- get_data_range(pull(viz_df, !!y_col_one_quo))
  
  latest_date_dte <- max(viz_df$date, na.rm = T)
  earliest_date_dte <- min(viz_df$date, na.rm = T)
  # Getting dashed recession/non-recession average line offset
  num_data_dte_range_diff <- diff(as.numeric(range(viz_df$date, na.rm = T)))
  x_ann <- get_x_annotation_val(num_data_dte_range_diff, latest_date_dte)
  
  latest_date_str <- format(latest_date_dte, "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date_str)
  
  # Base plt
  plt <- ggplot(viz_df, mapping = aes(x = !!x_col_quo, 
                                      y = !!y_col_one_quo,
                                      fill = !!y_col_one_quo)) +
    coord_cartesian(
      xlim = c(earliest_date_dte, latest_date_dte),
      clip = "off") +
    geom_col() +
    geom_text(aes(label = label_number(scale = 1, scale_cut = cut_short_scale())(!!y_col_one_quo), 
                  vjust = if_else(!!y_col_one_quo > 0, -.15, 1.05)), 
              color = "black", 
              size = 4) + 
    scale_fill_steps2(low = "#8c510a", 
                      mid = "#f5f5f5", 
                      high = "#01665e", midpoint = 0, guide = "none") +
    scale_x_date(date_labels = "%b. '%y") + # I think I can eventually make differing number of labels and ticks with this: https://teunbrand.github.io/ggh4x/ & https://stackoverflow.com/questions/14490071/adding-minor-tick-marks-to-the-x-axis-in-ggplot2-with-no-labels
    geom_hline(yintercept = 0, linewidth = 2) + 
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    ts_bar_theme()
  
  if (!is.null(non_rec_avg_line)) {
    if (between(non_rec_avg_line, data_range[1], data_range[2])) {
      plt <- plt + geom_hline(yintercept = non_rec_avg_line,
                              color = "black",
                              linewidth = 0.75,
                              linetype = "dashed",
                              alpha = 0.4
      ) + annotate("text",
                   x = x_ann,
                   y = non_rec_avg_line,
                   hjust = 0.5,
                   label = "Non-recession\navg.",
                   color = "black",
                   size = 3.5,
                   fontface = "bold")
    }
  }
  
  if (!is.null(rec_avg_line)) {
    if (between(rec_avg_line, data_range[1], data_range[2])) {
      plt <- plt + geom_hline(yintercept = rec_avg_line,
                              color = "red",
                              linewidth = 0.75,
                              linetype = "dashed",
                              alpha = 0.4
      ) + annotate("text",
                   x = x_ann,
                   y = rec_avg_line,
                   hjust = 0.5,
                   label = "Recession\navg.",
                   color = "red",
                   size = 3.5,
                   fontface = "bold")
    }
  }
  
  if (y_data_type == "percentage") {
    plt <- plt + scale_y_continuous(
      expand = expansion(mult = c(.15, .15)),
      labels = label_percent(scale = 100, suffix = "%", accuracy = 0.1)
    )
  } else if (y_data_type == "dollar") {
    plt <- plt + scale_y_continuous(
      expand = expansion(mult = c(.15, .15)),
      labels = label_currency(scale = 1, prefix = "$", scale_cut = cut_short_scale())
    )
  } else if (y_data_type == "number") {
    plt <- plt + scale_y_continuous(
      expand = expansion(mult = c(.03, .03)),
      labels = label_number(scale = 1, scale_cut = cut_short_scale())
    )
  }
  
  return(plt)
  
}

make_ts_faceted_line_chart <- function(viz_df, x_col, y_col_one, facet_col, y_data_type,
                                       viz_title = NULL, viz_subtitle, viz_caption) {
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_one_quo <- enquo(y_col_one)
  facet_col_quo <- enquo(facet_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  
  latest_date_dte <- max(viz_df$date, na.rm = T)
  earliest_date_dte <- min(viz_df$date, na.rm = T)
  
  unique_date_vec <- unique(viz_df$date) |> sort()
  
  latest_date_values <- viz_df %>% 
    arrange(desc(date), desc(!!y_col_one_quo)) %>% 
    filter(date == latest_date_dte) %>% 
    pull(!!y_col_one_quo)
  
  date_axis_breaks <- c(
    first(unique_date_vec),
    nth(unique_date_vec, n = (round(length(unique_date_vec) * .5))),
    last(unique_date_vec)
  )
  
  latest_date_str <- format(latest_date_dte, "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date_str)
  
  # Base plt
  plt <- ggplot(viz_df, mapping = aes(x = !!x_col_quo, 
                                      y = !!y_col_one_quo,
                                      color = !!y_col_one_quo)) +
    geom_line(mapping = aes(y = !!y_col_one_quo),
              linewidth = 1.5, 
              lineend = "round",
              linejoin = "bevel") +
    facet_wrap(vars(!!facet_col_quo), labeller = labeller(!!facet_col_quo := label_wrap_gen(23))) +
    scale_color_steps2(low = "#8c510a",
                       mid = "#f5f5f5",
                       high = "#01665e", 
                       midpoint = 0, 
                       guide = "none") +
    scale_x_date(date_labels = "%m/'%y", breaks = date_axis_breaks) + 
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    ts_line_theme() + 
    theme(strip.text = element_text(size = 14, face = "bold"),
          panel.spacing.x = unit(38, "pt"),
          axis.text.x = element_text(
            size = 12, face = "bold", color = "black",
            margin = margin(4, 0, 15, 0, "pt")
          ))
  
  if (y_data_type == "percentage") {
    plt <- plt + scale_y_continuous(
      expand = expansion(mult = c(.15, .15)),
      labels = label_percent(scale = 100, suffix = "%", accuracy = 1)
    )
  } else if (y_data_type == "dollar") {
    plt <- plt + scale_y_continuous(
      expand = expansion(mult = c(.15, .15)),
      labels = label_currency(scale = 1, prefix = "$", scale_cut = cut_short_scale())
    )
  } else if (y_data_type == "number") {
    plt <- plt + scale_y_continuous(
      expand = expansion(mult = c(.03, .03)),
      labels = label_number(scale = 1, scale_cut = cut_short_scale())
    )
  }
  
  return(plt)
  
  
  
}

make_bar_chart <- function(viz_df, x_col, y_col, viz_title = NULL, 
                           viz_subtitle, viz_caption) {
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_quo <- enquo(y_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  
  latest_date <- format(unique(viz_df$date), "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date)
  
  plt <- ggplot(viz_df, aes(x = !!x_col_quo, 
                            y = reorder(!!y_col_quo, !!x_col_quo),
                            fill = !!x_col_quo)) + 
    geom_col(width = 0.85) +
    scale_x_continuous(expand = expansion(mult = c(0, .15))) +
    scale_y_discrete(labels = label_wrap(23)) +
    geom_text(aes(label = label_number(accuracy = 0.1)(!!x_col_quo)), 
              color = "black", hjust = -0.3, size = 7) +
    scale_fill_steps(low = "#e5f5e0", high = "#31a354", guide = "none") +
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    bar_theme()
  
  return(plt)
}

make_pct_chg_bar_chart <- function(viz_df, x_col, y_col, viz_title = NULL, 
                                   viz_subtitle, viz_caption) {
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_quo <- enquo(y_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  latest_date <- format(unique(viz_df$date), "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date)
  
  ggplot(viz_df, aes(x = !!x_col_quo, 
                     y = reorder(!!y_col_quo, !!x_col_quo),
                     fill = !!x_col_quo)) + 
    geom_col(width = 0.85) +
    geom_vline(xintercept = 0, linewidth = 2) +
    scale_x_continuous(expand = expansion(mult = c(.2, .2))) +
    scale_y_discrete(labels = label_wrap(23)) +
    geom_text(aes(label = label_percent(scale = 100, 
                                        suffix = "%", 
                                        accuracy = 0.1)(!!x_col_quo), 
                  hjust = if_else(!!x_col_quo > 0, -.15, 1.05)), 
              color = "black", 
              size = 7) +
    scale_fill_steps2(low = "#8c510a", 
                      mid = "#f5f5f5", 
                      high = "#01665e", midpoint = 0, guide = "none") +
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    bar_theme()
}

make_cur_map <- function(viz_df, shp_df, fill_col, geo_col,
                         viz_title = NULL, viz_subtitle, 
                         viz_caption) {
  # Joining data tibble with sf tibble that has `geometry` column that can be mapped
  full_df <- inner_join(viz_df, shp_df)
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting fill and geometry variables:
  fill_col_quo <- enquo(fill_col)
  geo_col_quo <- enquo(geo_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  latest_date <- format(unique(full_df$date), "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date)
  
  plt <- ggplot(data = full_df, mapping = aes(geometry = !!geo_col_quo, 
                                              fill = !!fill_col_quo)) +
    geom_sf() +
    scale_fill_distiller(type = "seq",
                         palette = "Oranges",
                         direction = 1,
                         guide = "colourbar") +
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) + 
    map_theme()
  
  return(plt)
  
}

make_pct_chg_map <- function(viz_df, shp_df, fill_col, geo_col,
                             viz_title = NULL, viz_subtitle, 
                             viz_caption) {
  # Joining data tibble with sf tibble that has `geometry` column that can be mapped
  full_df <- inner_join(viz_df, shp_df)
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting fill and geometry variables:
  fill_col_quo <- enquo(fill_col)
  geo_col_quo <- enquo(geo_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  latest_date <- format(unique(full_df$date), "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date)
  
  plt <- ggplot(data = full_df, mapping = aes(geometry = !!geo_col_quo, 
                                              fill = !!fill_col_quo)) +
    geom_sf() +
    scale_fill_gradient2(
      labels = label_percent(scale = 100, suffix = "%", accuracy = 0.1),
      low = "#8c510a", 
      mid = "#f5f5f5", 
      high = "#01665e", midpoint = 0, guide = "colourbar" 
    ) +
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) + 
    map_theme()
  
  return(plt)
}

make_state_scatter <- function(viz_df, x_col, y_col, color_col,
                               label_col, x_intercept,
                               y_intercept, viz_title = NULL, 
                               viz_subtitle, viz_caption) {
  
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_quo <- enquo(y_col)
  color_col_quo <- enquo(color_col)
  label_col_quo <- enquo(label_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  latest_date <- format(unique(viz_df$date), "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date)
  
  plt <- ggplot(viz_df, aes(x = !!x_col_quo, 
                            y = !!y_col_quo)) +
    scale_x_continuous(expand = expansion(mult = c(.05, .05))) +
    scale_y_continuous(expand = expansion(mult = c(.1, .1))) +
    scale_color_discrete() + 
    geom_hline(yintercept = y_intercept,
               color = "black",
               linewidth = 0.25,
               linetype = "dashed"
    ) + 
    geom_vline(xintercept = x_intercept,
               color = "black",
               linewidth = 0.25,
               linetype = "dashed"
    ) + 
    geom_text_repel(aes(color = !!color_col_quo, 
                        label = !!label_col_quo),
                    size = 5,
                    max.time = 5,
                    max.iter = 100000,
                    max.overlaps = 15,
                    show.legend = F) +
    annotate("text", 
             x = min(pull(viz_df, !!x_col_quo)), 
             y = max(pull(viz_df, !!y_col_quo)), 
             label = "Lower\nLabor\nLeverage",
             color = "black",
             size = 4,
             fontface = "bold") + 
    annotate("text", 
             x = max(pull(viz_df, !!x_col_quo)), 
             y = min(pull(viz_df, !!y_col_quo)), 
             label = "Higher\nLabor\nLeverage",
             color = "black",
             size = 4,
             fontface = "bold") + 
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    scatter_theme()
}

## Write out functions. Functions that save data or graphics ##
# Function that writes out data frame as a CSV into specified folder. 
# Assumes the `df` dataframe has a `dataelement_text` column with the name of 
# the measure and a `date` column that has an associated date with the data.

# Function to check date range of data series to implement correct file name syntax
make_file_name_date <- function(date_vec) {
  if (length(unique(date_vec)) == 1) {
    data_date <- as.character(max(date_vec, na.rm = T))
  } else if (length(unique(date_vec)) > 1) {
    data_date <- paste(max(date_vec, na.rm = T), min(date_vec, na.rm = T), sep = "_")
  } else {
    stop("Invalid date column")
  }
}

#### TODO: Make sure this function conforms with the econanalyzr syntax
#### and uses dplyr programming to work inside of package.
econ_csv_write_out <- function(df, folder) {
  
  data_date <- make_file_name_date(df$date)
  
  data_details <- df %>% 
    select(all_of(matches("_text$"))) %>% 
    make_metadata_vec()
  
  name <- str_flatten(c(data_date, unname(data_details)), "-")
  
  name_clean <- str_replace_all(name, "\\s+", "_")
  
  filename <- paste0(folder, "/", name_clean, ".csv")
  
  write_csv(x = df, file = filename)
  
  on.exit(expr = message(paste("Writing out", filename)), add = T)
}

#### TODO: Make sure this function conforms with the econanalyzr syntax
#### and uses dplyr programming to work inside of package.
save_chart <- function(plt, folder) {
  
  bsky_width <- 600
  bsky_height <- 335
  
  plt_df <- plt$data
  
  data_date <- make_file_name_date(plt_df$date)
  
  data_details <- plt_df %>% 
    select(all_of(matches("_text$"))) %>% 
    make_metadata_vec()
  
  name <- str_flatten(c(data_date, unname(data_details)), "-")
  
  name_clean <- paste0(str_replace_all(name, "\\s+", "_"), ".png")
  
  message(paste0("Writing out ", name_clean, " chart..."))
  ggsave(
    filename = name_clean,
    plot = plt,
    device = "png",
    path = folder,
    width = bsky_width * 6, # The aspect ratio can be adjusted as necessary.
    height = bsky_height * 6,
    units = "px"
  )
}