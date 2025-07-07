# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(GeoLift) # Make sure this package is installed
library(plotly) # New: For interactive plots

# Define UI for Geo-Lift Study dashboard
ui <- dashboardPage(
  skin = "black", # Added skin = "black" here
  dashboardHeader(
    title = tagList(
      span(
        # Styling for the dashboard header title
        style = "color: black;", # Changed title font color back to white
        # Placeholder for the logo. You should place 'canvas_white.png' in a 'www' folder
        # within your app directory.
        tags$img(src = "canvas_white.png", height = "30px", style = "margin-right: 10px;"),
        "Geo-Lift" # Changed title text here
      )
    ),
    # Custom CSS for the header background to be black
    tags$li(class = "dropdown",
            tags$style(".main-header {background-color: black !important;}"))
  ),
  dashboardSidebar(
    fileInput("file_upload", "Upload CSV File", accept = c(".csv")),
    actionButton("upload_button", "Ingest Data", class = "btn-primary"),
    hr(),
    uiOutput("outcome_var_selector"),
    uiOutput("location_var_selector"),
    uiOutput("date_var_selector"),
    # Re-introducing Experiment Period Start and End selectors
    uiOutput("experiment_period_start_selector"),
    uiOutput("experiment_period_end_selector"),
    uiOutput("num_locations_selector"),
    uiOutput("excluded_locations_selector"),
    hr(),
    # Action button to trigger market selection after all inputs are chosen
    # Changed class to "btn-orange" for custom styling
    actionButton("run_market_selection", "Run Market Selection", class = "btn-orange")
  ),
  dashboardBody(
    # Custom CSS for the orange button and overall body background
    tags$head(
      tags$style(HTML("
        /* Set overall body background to black */
        body, html, .wrapper, .content-wrapper {
          background-color: black !important;
          color: white; /* Ensure default text color is white */
        }
        .btn-orange {
          background-color: pink; /* Changed to pink */
          color: white;
          border-color: pink; /* Changed to pink */
        }
        .btn-orange:hover {
          background-color: hotpink; /* Changed to a darker pink for hover */
          border-color: hotpink; /* Changed to a darker pink for hover */
        }
        /* Ensure text within tabs/panels is visible */
        .tab-pane h3, .tab-pane h4, .tab-pane table, .tab-pane pre {
          color: white;
        }
        /* Adjust table colors for readability on black background */
        .tab-pane table th, .tab-pane table td {
          color: white;
          border-color: #555; /* Lighter border for contrast */
        }
        /* Ensure inputs and selects are readable */
        .selectize-input, .selectize-dropdown, .form-control {
          background-color: #333 !important;
          color: white !important;
          border-color: #555 !important;
        }
        .selectize-input::placeholder { /* For placeholder text */
          color: #bbb !important;
        }
        .selectize-input.focus {
          border-color: #888 !important;
          box-shadow: 0 0 0 0.2rem rgba(255, 255, 255, 0.25) !important;
        }
        /* For numeric input arrows */
        .form-control::-webkit-inner-spin-button,
        .form-control::-webkit-outer-spin-button {
          -webkit-appearance: none;
          margin: 0;
        }
        .form-control[type=number] {
          -moz-appearance: textfield;
        }
        /* Fix for modal dialog text visibility */
        .modal-title {
          color: black !important; /* Ensure title is black on light background */
        }
        .modal-body {
          color: black !important; /* Ensure body text is black on light background */
        }
        .modal-content {
          background-color: white !important; /* Ensure modal background is white */
        }
      "))
    ),
    tabsetPanel(
      id = "tabs",
      tabPanel("Raw Data",
               h3("Top 20 Rows of Uploaded Data"),
               tableOutput("raw_data_table")
      ),
      tabPanel("Trend Plots by Location",
               h3("Trend of Outcome by Location"),
               # Change plotOutput to plotlyOutput for interactivity
               plotlyOutput("trend_plot", height = "600px")
      ),
      tabPanel("Market Selection",
               h3("GeoLift Market Selection Results"),
               h4("Best Markets:"),
               tableOutput("best_markets_table")
      ),
      tabPanel("Placeholder Tab",
               h3("This is a placeholder tab.")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # Observer to handle file upload and data ingestion
  observeEvent(input$upload_button, {
    req(input$file_upload) # Ensure a file is uploaded
    
    # Read the CSV file
    df <- tryCatch({
      read_csv(input$file_upload$datapath)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to read CSV file:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    })
    
    if (!is.null(df)) {
      uploaded_data(df)
      showModal(modalDialog(
        title = "Success",
        "Data ingested successfully!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Render dynamic UI elements based on uploaded data
  observeEvent(uploaded_data(), {
    req(uploaded_data())
    data_cols <- names(uploaded_data())
    
    # Outcome (Y) variable selector
    output$outcome_var_selector <- renderUI({
      selectInput("y_var", "Select Outcome (Y) Variable:", choices = data_cols)
    })
    
    # Location variable selector
    output$location_var_selector <- renderUI({
      selectInput("location_var", "Select Location Variable (DMA):", choices = data_cols)
    })
    
    # Date variable selector
    output$date_var_selector <- renderUI({
      selectInput("date_var", "Select Date Variable:", choices = data_cols)
    })
    
    # New: Experiment Period Start and End selectors
    output$experiment_period_start_selector <- renderUI({
      numericInput("experiment_period_start", "Experiment Duration:", value = 91, min = 1) # Label changed
    })
    output$experiment_period_end_selector <- renderUI({
      numericInput("experiment_period_end", "Second Experiment Duration (optional):", value = 105, min = 1) # Label changed
    })
    
    # Number of desired locations
    output$num_locations_selector <- renderUI({
      numericInput("num_locations", "Number of Desired Locations (N):", value = 3, min = 1)
    })
    
    # Desired excluded locations (multi-select)
    output$excluded_locations_selector <- renderUI({
      req(input$location_var) # Ensure location_var is selected first
      if (input$location_var %in% data_cols) {
        # Get unique locations from the selected location variable
        locations <- unique(uploaded_data()[[input$location_var]])
        selectizeInput("excluded_locations", "Select Desired Excluded Locations:",
                       choices = locations, multiple = TRUE, options = list(placeholder = 'Select locations to exclude'))
      } else {
        HTML("Please select a valid Location Variable first to populate excluded locations.")
      }
    })
  })
  
  # Tab 1: Display Raw Data
  output$raw_data_table <- renderTable({
    req(uploaded_data())
    head(uploaded_data(), 20)
  })
  
  # Tab 2: Trend Plots by Location
  # renderPlotly for interactivity
  output$trend_plot <- renderPlotly({
    req(uploaded_data(), input$y_var, input$location_var, input$date_var)
    
    df <- uploaded_data()
    
    # Ensure date variable is in Date format for plotting
    df[[input$date_var]] <- as.Date(df[[input$date_var]])
    
    # Plotting the trend by location
    p <- ggplot(df, aes_string(x = input$date_var, y = input$y_var, color = input$location_var, text = input$location_var)) +
      geom_line() +
      labs(title = paste("Trend of", input$y_var, "by", input$location_var),
           x = "Date",
           y = input$y_var,
           color = input$location_var) +
      theme_minimal() +
      theme(legend.position = "right")
    
    # Convert ggplot object to plotly object for interactivity
    ggplotly(p, tooltip = "text")
  })
  
  # Reactive value to store GeoLiftMarketSelection results
  market_selection_results <- reactiveVal(NULL)
  
  # Observer to run GeoLiftMarketSelection when the button is clicked
  observeEvent(input$run_market_selection, {
    req(uploaded_data(), input$y_var, input$location_var, input$date_var)
    # Ensure numeric inputs for experiment periods and num_locations are not NA and are finite
    req(is.numeric(input$experiment_period_start) && is.finite(input$experiment_period_start))
    req(is.numeric(input$experiment_period_end) && is.finite(input$experiment_period_end))
    req(is.numeric(input$num_locations) && is.finite(input$num_locations))
    
    
    df <- uploaded_data()
    
    withProgress(message = 'Running GeoLift Market Selection...', value = 0, {
      incProgress(0.2, detail = "Reading GeoLift Data")
      # Use GeoDataRead to prepare the data for GeoLift
      df_geolift <- tryCatch({
        GeoDataRead(
          data = df,
          date_id = input$date_var,
          location_id = input$location_var,
          Y_id = input$y_var
        )
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error: GeoDataRead Failed",
          paste("Failed to prepare data with GeoDataRead:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL) # Stop execution if GeoDataRead fails
      })
      
      req(df_geolift) # Ensure df_geolift was successfully created
      
      # Use experiment_period_start and experiment_period_end for treatment_periods
      treatment_periods_vector <- c(input$experiment_period_start, input$experiment_period_end)
      
      # Construct arguments for GeoLiftMarketSelection dynamically
      geolift_args <- list(
        data = df_geolift,
        Y_id = "Y", # These are the column names provided by GeoDataRead
        location_id = "location", # These are the column names provided by GeoDataRead
        time_id = "time", # These are the column names provided by GeoDataRead
        treatment_periods = treatment_periods_vector, # Updated to use start and end periods
        N = input$num_locations
      )
      
      # Add exclude_mkt argument only if locations are selected for exclusion
      if (!is.null(input$excluded_locations) && length(input$excluded_locations) > 0) {
        geolift_args$exclude_mkt <- input$excluded_locations
      }
      
      incProgress(0.5, detail = "Performing Market Selection")
      tryCatch({
        # Call GeoLiftMarketSelection using do.call to pass arguments from a list
        results <- do.call(GeoLiftMarketSelection, geolift_args)
        market_selection_results(results)
        incProgress(1, detail = "Completed!")
        # Add success message here
        showModal(modalDialog(
          title = "Success",
          "GeoLift Market Selection completed successfully!",
          easyClose = TRUE,
          footer = NULL
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Failed to run GeoLift Market Selection:", e$message), # More specific error message
          easyClose = TRUE,
          footer = NULL
        ))
        market_selection_results(NULL) # Clear results on error
      })
    })
  })
  
  # Tab 3: Display GeoLiftMarketSelection Results
  output$best_markets_table <- renderTable({
    req(market_selection_results())
    # GeoLiftMarketSelection returns a list. 'BestMarkets' is one of its components.
    market_selection_results()$BestMarkets
  })
}

# Run the application
shinyApp(ui = ui, server = server) 
