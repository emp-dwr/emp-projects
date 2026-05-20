source('00_GlobalFunctions/functions.R')

check_analytes <- function(df, year = 'all', return_df = FALSE) {
  
  # read analyte reference list
  df_analytes <- read_csv('00_GlobalFunctions/dwq_analytes.csv', show_col_types = FALSE)
  
  # read active stations
  df_stations <- read_csv('00_GlobalFunctions/station_names.csv', show_col_types = FALSE) %>%
    filter(Status == 'active')
  
  valid_analytes <- unique(df_analytes$Analyte)
  valid_stations <- unique(df_stations$Station_new)
  
  # stations exempt from full-month presence check
  exempt_month_check <- c('LSZ2-SJR', 'LSZ6-SJR', 'NZ002', 'NZ004', 'NZ325')
  
  # remove analytes that should never be checked
  valid_analytes <- setdiff(valid_analytes, c('Filter Container ID', 'Churn Bucket ID'))
  
  # create month field
  df <- df %>%
    mutate(Month = month(Date))
  
  # optionally filter to one year
  if (!identical(year, 'all')) {
    df <- df %>%
      filter(str_detect(Date, paste0('^', year)))
  }
  
  # keep only valid stations
  df_valid <- df %>%
    filter(Station %in% valid_stations)
  
  # check for station-months with no data at all
  all_months <- df_valid %>%
    distinct(Month) %>%
    arrange(Month)
  
  missing_station_months <- expand_grid(
    Station = setdiff(valid_stations, exempt_month_check),
    all_months
  ) %>%
    anti_join(
      df_valid %>%
        distinct(Station, Month),
      by = c('Station', 'Month')
    ) %>%
    arrange(Month, Station)
  
  if (nrow(missing_station_months) > 0) {
    message(
      'Stations with no data for entire month(s):\n',
      missing_station_months %>%
        mutate(row_txt = paste0(Station, ' - ', Month)) %>%
        pull(row_txt) %>%
        paste(collapse = '\n')
    )
  } else {
    message('All active stations have at least some data in every month.')
  }
  
  # join SamplingType
  df_joined <- df_valid %>%
    left_join(
      df_stations %>% select(Station_new, SamplingType),
      by = c('Station' = 'Station_new')
    )
  
  # gather observed stations/dates
  station_dates <- df_joined %>%
    distinct(Station, Date)
  
  # create expected combos (station x analyte x date)
  expected <- expand_grid(
    station_dates,
    Analyte = valid_analytes
  )
  
  # RULES
  # 1. shore stations do not require Secchi
  shore_stations <- df_stations %>%
    filter(SamplingType == 'shore') %>%
    pull(Station_new)
  
  expected <- expected %>%
    filter(!(Station %in% shore_stations & Analyte == 'Secchi Depth'))
  
  # 2. non-LSZ stations do not need Lat/Long
  expected <- expected %>%
    filter(!(
      !grepl('LSZ', Station, ignore.case = TRUE) &
        Analyte %in% c('Latitude', 'Longitude')
    ))
  
  # 3. only D7, D16, and D19 have Dissolved Bromide
  expected <- expected %>%
    filter(!(Analyte == 'Dissolved Bromide' &
               !(Station %in% c('D7', 'D16', 'D19'))))
  
  # 4. C10A does not require any analyte containing '(Bottom)'
  expected <- expected %>%
    filter(!(Station == 'C10A' & str_detect(Analyte, fixed('(Bottom)'))))
  
  # observed combos
  observed <- df_joined %>%
    distinct(Station, Analyte, Date)
  
  # find missing combinations
  missing_combos <- anti_join(
    expected,
    observed,
    by = c('Station', 'Analyte', 'Date')
  ) %>%
    arrange(Date, Station, Analyte)
  
  # format message
  fmt_row <- function(x) {
    parts <- x[!is.na(x) & x != '']
    paste(parts, collapse = ' - ')
  }
  
  if (nrow(missing_combos) > 0) {
    message(
      'Missing ', nrow(missing_combos),
      ' station/analyte combinations:\n',
      paste0(apply(missing_combos, 1, fmt_row), collapse = '\n')
    )
  } else {
    message('All expected station/analyte combinations are represented.')
  }
  
  if (return_df) {
    return(missing_combos)
  }
}