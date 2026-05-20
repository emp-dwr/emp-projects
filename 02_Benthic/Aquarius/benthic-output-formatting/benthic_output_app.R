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
