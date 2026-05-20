# --- Helper Functions ---
# These functions are used later in the code by the "main" functions.

# Select only rows of the given benthic data type
base_data_filter <- function(df, key) {
  df %>%
    filter(str_detect(.data[['Lab: Specimen Name']], regex(key, ignore_case = TRUE)))
}

# Apply benthic data type filter (<key> = <filter function>)
## If a new filter is needed, add it to this function
apply_filter <- function(df, type) {
  switch(
    type,
    'Counts' = filter_counts(df),
    'Weights' = filter_weights(df),
    'Sediment' = filter_sediment(df),
    'Metadata: Species' = filter_meta_species(df),
    filter_counts(df)
  )
}

# Split the "Observed DateTime" column into Date and Time
add_date_time <- function(df) {
  df %>%
    mutate(
      Date = str_extract(.data[['Observed DateTime']], '^\\d{4}-\\d{2}-\\d{2}'),
      Time = str_extract(.data[['Observed DateTime']], '(?<=\\s)\\d{2}:\\d{2}:\\d{2}')
    )
}

# Pivot the results of the "Observed Property ID", "Result Value", and "Result Unit" (if applicable) column
## Column names are "Observed Property ID (Result Unit)"
## If a column doesn't have a unit, the name is just "Observed Property ID"
pivot_results <- function(df, id_cols) {
  df %>%
    mutate(
      # determine if units exist or not
      .unit_clean = case_when(
        is.na(.data[['Result Unit']]) ~ NA_character_,
        str_trim(as.character(.data[['Result Unit']])) == '' ~ NA_character_,
        str_to_lower(str_trim(as.character(.data[['Result Unit']]))) %in% c('na') ~ NA_character_,
        TRUE ~ str_trim(as.character(.data[['Result Unit']]))
      ),
      # create column name
      .col_name = if_else(
        is.na(.unit_clean),
        as.character(.data[['Observed Property ID']]),
        str_c(.data[['Observed Property ID']], ' (', .unit_clean, ')')
      )
    ) %>%
    select(-`Observed Property ID`, -`Result Unit`, -.unit_clean) %>%
    # pivot data wider
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = .col_name,
      values_from = `Result Value`
    )
}

# --- Filter Functions ---
## These functions define how data cleaning works
## Each "filter_<type>" defines the behavior for a different benthic data type
## If a new data type is added, add a new filter function
## Note: "Specimen ID" is needed as a pivot ID column when metadata info is stored in Observed Properties
## (eg. grab number or bin size)

# Filter function for count data
filter_counts <- function(df) {
  # define columns for pivot
  id_cols <- c('Sample ID', 'Specimen ID', 'Date', 'Time', 'Station', 'Latitude', 'Longitude', 'Species')
  
  df %>%
    # filter rows
    base_data_filter(key = 'count') %>% # based on unique "Lab: Specimen Name" key
    # remove "Taxon" rows (not needed)
    filter(!.data[['Observed Property ID']] %in% c('Taxon')) %>%
    # add date and time columns
    add_date_time() %>%
    # choose which columns to keep (and rename)
    transmute(
      `Sample ID` = .data[['Activity Name']],
      `Specimen ID` = .data[['Lab: Specimen Name']],
      Date = Date,
      Time = Time,
      `Observed Property ID` = .data[['Observed Property ID']],
      Station = .data[['Location ID']],
      Latitude = .data[['Latitude']],
      Longitude = .data[['Longitude']],
      Species = .data[['Specimen: Taxonomy Element']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    # pivot, select, and arrange data
    pivot_results(id_cols = id_cols) %>%
    select(`Sample ID`, Date, Time, Station, `Grab Number`, Species, TSN, `Count (individuals)`) %>%
    arrange(`Sample ID`, Date, Time, Station, `Grab Number`, Species)
}

# Filter function for clam weight data
filter_weights <- function(df) {
  # define columns for pivot
  id_cols <- c('Sample ID', 'Specimen ID', 'Date', 'Time', 'Station', 'Latitude', 'Longitude', 'Species')
  
  df %>%
    # filter rows
    base_data_filter(key = 'weight') %>% # based on unique "Lab: Specimen Name" key
    # remove "Taxon" rows (not needed)
    filter(!.data[['Observed Property ID']] %in% c('Taxon')) %>%
    # add date and time columns
    add_date_time() %>%
    # choose which columns to keep (and rename)
    transmute(
      `Sample ID` = .data[['Activity Name']],
      `Specimen ID` = .data[['Lab: Specimen Name']],
      Date = Date,
      Time = Time,
      `Observed Property ID` = .data[['Observed Property ID']],
      Station = .data[['Location ID']],
      Latitude = .data[['Latitude']],
      Longitude = .data[['Longitude']],
      Species = .data[['Specimen: Taxonomy Element']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    # pivot, select, and arrange data
    pivot_results(id_cols = id_cols) %>%
    select(
      `Sample ID`, Date, Time, Station, Latitude, Longitude, Species, TSN, `Size Bin`,
      `Wet Weight (g)`, `Dry Weight (g)`, `Clam Weight (g)`
    ) %>%
    arrange(`Sample ID`, Date, Time, Station, Latitude, Longitude, Species, `Size Bin`)
}

# Filter function for sediment data
filter_sediment <- function(df) {
  # define columns for pivot
  id_cols <- c('Sample ID', 'Date', 'Time', 'Station', 'Latitude', 'Longitude')
  
  df %>%
    # filter rows
    base_data_filter(key = 'sediment') %>% # based on unique "Lab: Specimen Name" key
    # add date and time columns
    add_date_time() %>%
    # choose which columns to keep (and rename)
    transmute(
      `Sample ID` = .data[['Activity Name']],
      Date = Date,
      Time = Time,
      `Observed Property ID` = .data[['Observed Property ID']],
      Station = .data[['Location ID']],
      Latitude = .data[['Latitude']],
      Longitude = .data[['Longitude']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    # pivot, select, and arrange data
    pivot_results(id_cols = id_cols) %>%
    select(`Sample ID`, Date, Time, Station, Latitude, Longitude, `Gravel (percent)`, `Sand (percent)`) %>%
    arrange(`Sample ID`, Date, Time, Station, Latitude, Longitude)
}

# Filter function for species metadata
filter_meta_species <- function(df) {
  # define columns for pivot
  id_cols <- c('Species')
  
  df %>%
    # filter rows
    base_data_filter(key = 'taxa') %>% # based on unique "Lab: Specimen Name" key
    # remove "Taxon" rows (not needed)
    filter(.data[['Observed Property ID']] != 'Taxon') %>%
    # choose which columns to keep (and rename)
    transmute(
      Species = .data[['Specimen: Taxonomy Element']],
      `Observed Property ID` = .data[['Observed Property ID']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    # pivot, select, and arrange data
    pivot_results(id_cols = id_cols) %>%
    select(Species, TSN, Habitat) %>%
    arrange(Species)
}
