# Benthic Output App
# Application to format exports from benthic's Aquarius database
# Questions: sarah.perry@water.ca.gov

# --- Structure ---
## Aquarius hosts multiple types of benthic data; these are distinguished by the "Lab: Specimen Name" column.
## This app allows for the selection of a benthic data type (via this column).
## The app filters the data to only those rows and formats them appropriately.
## Per benthic data type, there are "data sub types"; these are defined in the "Observed Property ID" column.
## Formatting is broadly done by pivoting rows
## based on the "Observed Property ID", "Result Value", and "Result Unit" columns.

# --- Load Packages ---
library(shiny)
library(tidyverse)
library(DT)

options(shiny.maxRequestSize = 50 * 1024^2) # set max file size limit

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

# --- Shiny App ---
## These functions create the app itself
## If new data type is created (or if ones removed), only sections with "TODO:" need to be edited

# Create the basic UI
ui <- fluidPage(
  titlePanel('Benthic Output Formatting'),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Upload CSV', accept = c('.csv', 'text/csv')),
      # TODO: below creates the data type dropdown list. Add/delete a choice if necessary.
      selectInput('type', 'Select Data', choices = c('Counts', 'Weights', 'Sediment', 'Metadata: Species')),
      uiOutput('download_ui')
    ),
    mainPanel(
      uiOutput('status_msg'),
      DTOutput('tbl')
    )
  )
)

# Setup the server
server <- function(input, output, session) {
  required_cols <- c(
    'Lab: Specimen Name', 'Observed DateTime', 'Activity Name', 'Location ID',
    'Latitude', 'Longitude', 'Observed Property ID', 'Result Value', 'Result Unit',
    'Specimen: Taxonomy Element'
  )
  
  # TODO: this maps the dropdown choices to the keys from the filter functions
  ## make sure "type" matches up with "choices" and that the "key" matches with the filter function key
  filter_catalog <- tibble(
    type = c('Counts', 'Weights', 'Sediment', 'Metadata: Species'),
    key = c('count',  'weight',  'sediment', 'taxa')
  )
  
  # read in data
  df_raw <- reactive({
    req(input$file)
    
    ext <- tolower(tools::file_ext(input$file$name))
    validate(need(ext == 'csv', 'Invalid file type. Please upload a .csv file.'))
    
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    missing <- setdiff(required_cols, names(df))
    validate(need(length(missing) == 0, paste('Missing columns:', paste(missing, collapse = ', '))))
    
    return(df)
  })
  
  # helper function: return vector of valid keys in the data
  valid_types <- reactive({
    req(input$file)
    df <- df_raw()
    
    lab <- df %>%
      pull(.data[['Lab: Specimen Name']]) %>%
      as.character()
    
    filter_catalog %>%
      mutate(has_key = map_lgl(key, ~ any(str_detect(lab, regex(.x, ignore_case = TRUE))))) %>%
      filter(has_key) %>%
      pull(type)
  })
  
  # create dropdown based on valid key types
  observeEvent(valid_types(), {
    choices <- valid_types()
    
    updateSelectInput(
      session,
      'type',
      choices = choices,
      selected = if (length(choices) > 0) choices[[1]] else character(0)
    )
  })
  
  # if no valid types, throw error
  output$status_msg <- renderUI({
    req(input$file)
    if (length(valid_types()) == 0) {
      tags$div(
        style = 'margin-bottom: 10px; font-weight: 600;',
        'No valid filters in data'
      )
    } else {
      NULL
    }
  })
  
  df_filtered <- reactive({
    req(input$file)
    validate(need(length(valid_types()) > 0, 'check import file'))
    req(input$type)
    
    apply_filter(df = df_raw(), type = input$type)
  })
  
  # output a table in the UI
  output$tbl <- renderDT({
    req(input$file)
    datatable(
      df_filtered(),
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })
  
  # create a download button
  output$download_ui <- renderUI({
    if (is.null(input$file) || length(valid_types()) == 0) {
      tags$button(
        'Download CSV',
        class = 'btn btn-default',
        disabled = 'disabled',
        style = 'opacity: 0.6; cursor: not-allowed;'
      )
    } else {
      downloadButton('download', 'Download CSV')
    }
  })
  
  # default download settings
  output$download <- downloadHandler(
    filename = function() paste0('filtered_', tolower(input$type), '.csv'),
    content = function(file) write_csv(df_filtered(), file)
  )
}

# Create app
shinyApp(ui, server)
