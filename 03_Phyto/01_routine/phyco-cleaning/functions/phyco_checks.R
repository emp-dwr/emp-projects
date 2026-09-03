check_blank_rows <- function(df, data_cols){
  all_blank <- apply(df[data_cols], 1, function(x) all(is.na(x) | x == 0))
  
  df[all_blank, c('Month', 'Run', 'DateTime', data_cols)] %>%
    pivot_longer(cols = all_of(data_cols), names_to = 'Analyte', values_to = 'Value')
}

check_negative_values <- function(df, data_cols){
  df %>%
    select(Month, Run, DateTime, all_of(data_cols)) %>%
    pivot_longer(cols = all_of(data_cols), names_to = 'Analyte', values_to = 'Value') %>%
    filter(Value < 0)
}

check_duplicate_datetime <- function(df){
  df %>%
    group_by(Month, Run) %>%
    filter(duplicated(DateTime) | duplicated(DateTime, fromLast = TRUE)) %>%
    ungroup() %>%
    select(Month, Run, DateTime) %>%
    arrange(Month, Run, DateTime)
}

check_midnight <- function(df){
  df %>%
    filter(grepl('00:00:00$', DateTime)) %>%
    select(Month, Run, DateTime)
}

check_geo_bounds <- function(df){
  lat_n <- 38.369
  lat_s <- 37.678
  lon_e <- -121.262
  lon_w <- -122.786
  
  df %>%
    filter(Latitude < lat_s | Latitude > lat_n | Longitude < lon_w | Longitude > lon_e) %>%
    select(Month, Run, DateTime, Latitude, Longitude)
}

data_checks <- function(df, data_cols){
  blank <- check_blank_rows(df, data_cols)
  negative <- check_negative_values(df, data_cols)
  duplicates <- check_duplicate_datetime(df)
  midnight <- check_midnight(df)
  geo_bounds <- check_geo_bounds(df)
  
  message(glue::glue('{nrow(blank)} blank-row analyte value(s) flagged'))
  message(glue::glue('{nrow(negative)} negative analyte value(s) flagged'))
  message(glue::glue('{nrow(duplicates)} duplicate timestamp row(s) flagged'))
  message(glue::glue('{nrow(midnight)} midnight-timestamp row(s) flagged'))
  message(glue::glue('{nrow(geo_bounds)} out-of-bounds lat/lon row(s) flagged'))
  
  list(
    blank = blank,
    negative = negative,
    duplicates = duplicates,
    midnight = midnight,
    geo_bounds = geo_bounds
  )
}