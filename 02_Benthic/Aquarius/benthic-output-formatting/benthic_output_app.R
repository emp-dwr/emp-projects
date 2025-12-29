library(shiny)
library(tidyverse)
library(DT)

options(shiny.maxRequestSize = 50 * 1024^2)

# --- Helper Functions ---

base_data_filter <- function(df, key) {
  df %>%
    filter(str_detect(.data[['Lab: Specimen Name']], regex(key, ignore_case = TRUE)))
}

add_date_time <- function(df) {
  df %>%
    mutate(
      Date = str_extract(.data[['Observed DateTime']], '^\\d{4}-\\d{2}-\\d{2}'),
      Time = str_extract(.data[['Observed DateTime']], '(?<=\\s)\\d{2}:\\d{2}:\\d{2}')
    )
}

pivot_results <- function(df, id_cols) {
  df %>%
    mutate(
      .unit_clean = case_when(
        is.na(.data[['Result Unit']]) ~ NA_character_,
        str_trim(as.character(.data[['Result Unit']])) == '' ~ NA_character_,
        str_to_lower(str_trim(as.character(.data[['Result Unit']]))) %in% c('na', 'n/a') ~ NA_character_,
        TRUE ~ str_trim(as.character(.data[['Result Unit']]))
      ),
      .col_name = if_else(
        is.na(.unit_clean),
        as.character(.data[['Observed Property ID']]),
        str_c(.data[['Observed Property ID']], ' (', .unit_clean, ')')
      )
    ) %>%
    select(-`Observed Property ID`, -`Result Unit`, -.unit_clean) %>%
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = .col_name,
      values_from = `Result Value`,
      values_fn = dplyr::first
    )
}

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

# --- Filter Functions ---

filter_counts <- function(df) {
  id_cols <- c('Activity Name', 'Date', 'Time', 'Station', 'Latitude', 'Longitude', 'Species')
  
  df %>%
    base_data_filter(key = 'count') %>%
    filter(!.data[['Observed Property ID']] %in% c('Taxon')) %>%
    add_date_time() %>%
    transmute(
      `Activity Name` = .data[['Activity Name']],
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
    pivot_results(id_cols = id_cols) %>%
    select(Date, Time, Station, `Grab Number`, Species, TSN, `Count (individuals)`) %>%
    arrange(Date, Time, Station, `Grab Number`, Species)
}

filter_weights <- function(df) {
  id_cols <- c('Activity Name', 'Date', 'Time', 'Station', 'Latitude', 'Longitude', 'Species')
  
  df %>%
    base_data_filter(key = 'weight') %>%
    filter(!.data[['Observed Property ID']] %in% c('Taxon')) %>%
    add_date_time() %>%
    transmute(
      `Activity Name` = .data[['Activity Name']],
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
    pivot_results(id_cols = id_cols) %>%
    select(
      Date, Time, Station, Latitude, Longitude, Species, TSN, `Size Bin`,
      `Wet Weight (g)`, `Dry Weight (g)`, `Clam Weight (g)`
    ) %>%
    arrange(Date, Time, Station, Latitude, Longitude, Species, `Size Bin`)
}

filter_sediment <- function(df) {
  id_cols <- c('Activity Name', 'Date', 'Time', 'Station', 'Latitude', 'Longitude')
  
  df %>%
    base_data_filter(key = 'sediment') %>%
    add_date_time() %>%
    transmute(
      `Activity Name` = .data[['Activity Name']],
      Date = Date,
      Time = Time,
      `Observed Property ID` = .data[['Observed Property ID']],
      Station = .data[['Location ID']],
      Latitude = .data[['Latitude']],
      Longitude = .data[['Longitude']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    pivot_results(id_cols = id_cols) %>%
    select(Date, Time, Station, Latitude, Longitude, `Gravel (percent)`, `Sand (percent)`) %>%
    arrange(Date, Time, Station, Latitude, Longitude)
}

filter_meta_species <- function(df) {
  id_cols <- c('Species')
  
  df %>%
    base_data_filter(key = 'taxa') %>%
    filter(.data[['Observed Property ID']] != 'Taxon') %>%
    transmute(
      Species = .data[['Specimen: Taxonomy Element']],
      `Observed Property ID` = .data[['Observed Property ID']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    pivot_results(id_cols = id_cols) %>%
    select(Species, TSN, Habitat) %>%
    arrange(Species)
}

# --- Shiny App ---

ui <- fluidPage(
  titlePanel('Benthic Output Formatting'),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Upload CSV', accept = c('.csv', 'text/csv')),
      selectInput('type', 'Select Data', choices = c('Counts', 'Weights', 'Sediment', 'Metadata: Species')),
      uiOutput('download_ui')
    ),
    mainPanel(
      uiOutput('status_msg'),
      DTOutput('tbl')
    )
  )
)

server <- function(input, output, session) {
  required_cols <- c(
    'Lab: Specimen Name', 'Observed DateTime', 'Activity Name', 'Location ID',
    'Latitude', 'Longitude', 'Observed Property ID', 'Result Value', 'Result Unit',
    'Specimen: Taxonomy Element'
  )
  
  filter_catalog <- tibble(
    type = c('Counts', 'Weights', 'Sediment', 'Metadata: Species'),
    key = c('count',  'weight',  'sediment', 'taxa')
  )
  
  df_raw <- reactive({
    req(input$file)
    
    ext <- tolower(tools::file_ext(input$file$name))
    validate(need(ext == 'csv', 'Invalid file type. Please upload a .csv file.'))
    
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    missing <- setdiff(required_cols, names(df))
    validate(need(length(missing) == 0, paste('Missing columns:', paste(missing, collapse = ', '))))
    
    return(df)
  })
  
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
  
  observeEvent(valid_types(), {
    choices <- valid_types()
    
    updateSelectInput(
      session,
      'type',
      choices = choices,
      selected = if (length(choices) > 0) choices[[1]] else character(0)
    )
  }, ignoreInit = TRUE)
  
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
  
  output$tbl <- renderDT({
    req(input$file)
    datatable(
      df_filtered(),
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })
  
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
  
  output$download <- downloadHandler(
    filename = function() paste0('filtered_', tolower(input$type), '.csv'),
    content = function(file) write_csv(df_filtered(), file)
  )
}

shinyApp(ui, server)
