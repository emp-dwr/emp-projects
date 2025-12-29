library(shiny)
library(tidyverse)
library(DT)

# --- Helper Functions ---
# functions to help with filtering functions

base_data_filter <- function(df, key) {
  df %>%
    filter(str_detect(.data[['Lab: Specimen Name']], key))
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
    mutate(.col_name = str_c(.data[['Observed Property ID']], ' (', .data[['Result Unit']], ')')) %>%
    select(-`Observed Property ID`, -`Result Unit`) %>%
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
    filter_counts(df)
  )
}

# --- Filter Functions ---
# these functions specify how data is filtered/manipulated for each data type

filter_counts <- function(df) {
  df %>%
    base_data_filter(key = 'specimen') %>%
    filter(.data[['Observed Property ID']] != 'Taxon') %>%
    add_date_time() %>%
    transmute(
      `Activity Name` = .data[['Activity Name']],
      Date = Date,
      Time = Time,
      Station = .data[['Location ID']],
      Latitude = .data[['Latitude']],
      Longitude = .data[['Longitude']],
      `Grab Number` = .data[['EA_Grab Number']],
      Species = .data[['Specimen: Taxonomy Element']],
      Count = .data[['Result Value']]
    ) %>%
    arrange(Date, Time, Station, `Grab Number`, Species)
}

filter_weights <- function(df) {
  id_cols <- c('Activity Name', 'Date', 'Time', 'Station', 'Latitude', 'Longitude', 'Species', 'Size Bin')
  
  df %>%
    base_data_filter(key = 'weight') %>%
    filter(.data[['Observed Property ID']] != 'Taxon') %>%
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
      `Size Bin` = .data[['EA_Size Bin']],
      `Result Value` = .data[['Result Value']],
      `Result Unit` = .data[['Result Unit']]
    ) %>%
    pivot_results(id_cols = id_cols) %>%
    arrange(
      Date, Time, Station, Latitude, Longitude, Species, `Size Bin`,
      `Wet Weight (g)`, `Dry Weight (g)`, `Clam Weight (g)`
    )
}

filter_sediment <- function(df) {
  id_cols <- c('Activity Name', 'Date', 'Time', 'Station', 'Latitude', 'Longitude')
  
  df %>%
    base_data_filter(key = 'Sediment') %>%
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
    arrange(Date, Time, Station, Latitude, Longitude)
}

# --- Shiny App ---

ui <- fluidPage(
  titlePanel('Benthic Output Formatting'),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Upload CSV', accept = c('.csv', 'text/csv')),
      selectInput('type', 'Select Data', choices = c('Counts', 'Weights', 'Sediment')),
      downloadButton('download', 'Download CSV')
    ),
    mainPanel(
      DTOutput('tbl')
    )
  )
)

server <- function(input, output, session) {
  df_raw <- reactive({
    req(input$file)
    read_csv(input$file$datapath, show_col_types = FALSE)
  })
  
  df_filtered <- reactive({
    apply_filter(df = df_raw(), type = input$type)
  })
  
  output$tbl <- renderDT({
    datatable(
      df_filtered(),
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })
  
  output$download <- downloadHandler(
    filename = function() paste0('filtered_', tolower(input$type), '.csv'),
    content = function(file) write_csv(df_filtered(), file)
  )
}

shinyApp(ui, server)
