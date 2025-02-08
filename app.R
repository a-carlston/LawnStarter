library(shiny)
library(bs4Dash)
library(DT)
library(readxl)
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)





# Define lag_by_weekdays function ----
lag_by_weekdays <- function(df, column_name = "cleaned_volume", years_back = 2) {
  require(dplyr)
  require(lubridate)
  
  df <- df |> mutate(date = as.Date(date))
  
  for (i in 1:years_back) {
    current_year <- lubridate::year(Sys.Date()) - i
    lag_column_name <- paste0("Lag_", column_name, "_", current_year)
    
    df <- df %>%
      mutate(
        Lagged_Date = date - years(i),
        Lagged_Date = case_when(
          wday(date) == wday(Lagged_Date) ~ Lagged_Date,
          TRUE ~ Lagged_Date + (wday(date) - wday(Lagged_Date)) %% 7
        ),
        !!lag_column_name := .data[[column_name]][match(Lagged_Date, date)],
        !!lag_column_name := ifelse(
          is.na(.data[[lag_column_name]]),
          lag(.data[[lag_column_name]], 1),
          .data[[lag_column_name]]
        )
      ) %>%
      ungroup()
  }
  
  return(df)
}







# UI ----
ui <- dashboardPage(
  
  ## Header ---- 
  header = dashboardHeader(
    title = dashboardBrand(
      title = "WFM",
      image = "https://static.vecteezy.com/system/resources/previews/015/832/043/large_2x/wfm-letter-logo-design-on-white-background-wfm-creative-initials-circle-logo-concept-wfm-letter-design-vector.jpg"
    )
  ),
  
  ## Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("line-chart")),
      menuItem("Step One - Data Wrangle", tabName = "step_one", icon = icon("wrench"))
    )
  ),
  
  ## Control-bar (File Upload, Queue Selection, Log1p Toggle) ----
  controlbar = dashboardControlbar(
    fileInput("file_upload", "Upload Data File", accept = c(".csv", ".xlsx")),
    uiOutput("queue_select"),
    checkboxInput("apply_log1p", "Apply Log1p Transformation", value = FALSE)
  ),
  
  ## Footer ----
  footer = dashboardFooter(),
  
  ## Body ----
  body = dashboardBody(
    tabItems(
      # Dashboard Page
      tabItem(
        tabName = "dashboard",
        fluidRow(
          bs4Card(
            title = "Forecasted vs Actual Volume",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            collapsible = TRUE,
            plotlyOutput("time_series_plot")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Filtered Data",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            DTOutput("data_table")
          )
        )
      ),
      
      # Step One - Data Wrangle Page
      tabItem(
        tabName = "step_one",
        fluidRow(
          bs4Card(
            title = "Smoothing",
            solidHeader = TRUE,
            status = "warning",
            width = 12,
            collapsible = TRUE,
            plotlyOutput("wrangle_plot")  # New Wrangle Plot
          ),
          bs4Card(
            title = "Lag View",
            solidHeader = TRUE,
            status = "warning",
            width = 12,
            collapsible = TRUE,
            headerBorder = TRUE,
            dropdownMenu = boxDropdown(
              icon = "filter",
              boxDropdownItem(selectInput("lag_column", "Select Column", choices = NULL, selected = NULL)),
              boxDropdownItem(selectInput("lag_year", "Select Lag Year", choices = NULL, selected = NULL))
            ),
            plotlyOutput("lag_plot")  # Lag Plot
          )
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Reactive: Store uploaded data
  uploaded_data <- reactive({
    req(input$file_upload)
    file_ext <- tools::file_ext(input$file_upload$name)
    
    # Read file
    data <- if (file_ext == "csv") {
      read_csv(input$file_upload$datapath, show_col_types = FALSE)
    } else if (file_ext == "xlsx") {
      read_excel(input$file_upload$datapath)
    } else {
      return(NULL)
    }
    
    # Clean column names
    colnames(data) <- str_replace_all(colnames(data), " ", "_") %>% tolower()
    
    # Ensure numeric and date columns
    data <- data %>%
      mutate(
        forecasted_volume = suppressWarnings(as.numeric(str_replace_all(forecasted_volume, "[^0-9.]", ""))),
        actual_volume = suppressWarnings(as.numeric(str_replace_all(actual_volume, "[^0-9.]", ""))),
        date = parse_date_time(date, orders = c("mdy", "dmy", "ymd"), quiet = TRUE)
      ) %>%
      drop_na(date, forecasted_volume, actual_volume) %>%
      arrange(date)
    
    return(data)
  })
  
  # Populate Queue dropdown with default "Customer Phone"
  output$queue_select <- renderUI({
    req(uploaded_data())
    queues <- unique(uploaded_data()$queue)
    default_queue <- if ("Customer Phone" %in% queues) "Customer Phone" else queues[1]
    
    selectInput("queue", "Select Queue", choices = queues, selected = default_queue)
  })
  
  # Reactive: Processed Data with Log1p Option
  processed_data <- reactive({
    req(uploaded_data(), input$queue)
    
    data <- uploaded_data() %>%
      filter(queue == input$queue) %>%
      arrange(date)
    
    # Apply log1p transformation if checkbox is selected
    if (input$apply_log1p) {
      data <- data %>%
        mutate(
          forecasted_volume = log1p(forecasted_volume),
          actual_volume = log1p(actual_volume)
        )
    }
    
    if (nrow(data) == 0) return(NULL)  # Prevent errors if no data
    return(data)
  })
  
  # Time Series Chart: Forecasted vs Actual Volume
  output$time_series_plot <- renderPlotly({
    req(processed_data())
    
    if (is.null(processed_data()) || nrow(processed_data()) == 0) {
      return(NULL)
    }
    
    plot_data <- processed_data() %>%
      pivot_longer(cols = c(forecasted_volume, actual_volume), 
                   names_to = "Metric", values_to = "Value")
    
    plot_time_series(
      .data = plot_data,
      .date_var = date, 
      .value = Value, 
      .color_var = Metric, 
      .interactive = TRUE, 
      .smooth = FALSE
    ) |> 
      ggplotly()
  })
  
  # Display Data in a Table
  output$data_table <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Reactive: Apply `ts_clean_vec` on actual_volume, filtering out zero days (Step One - Data Wrangle)
  cleaned_data <- reactive({
    req(processed_data())
    
    data <- processed_data() %>%
      mutate(
        cleaned_volume = ifelse(actual_volume == 0, NA, actual_volume)  # Convert zeros to NA
      ) %>%
      mutate(cleaned_volume = ts_clean_vec(cleaned_volume, period = 7)) %>%
      select(date, actual_volume, cleaned_volume)
    
    return(data)
  })
  
  # Plot Actual vs Cleaned Volume (Step One - Data Wrangle)
  output$wrangle_plot <- renderPlotly({
    req(cleaned_data())
    
    plot_data <- cleaned_data() %>%
      pivot_longer(cols = c(actual_volume, cleaned_volume), 
                   names_to = "Metric", values_to = "Value")
    
    plot_time_series(
      .data = plot_data,
      .date_var = date, 
      .value = Value, 
      .color_var = Metric, 
      .interactive = TRUE, 
      .smooth = FALSE
    ) |> 
      ggplotly()
  })
  
  # Plot Lag (Step One - Data Wrangle)
  output$lag_plot <- renderPlotly({
    req(cleaned_data())
    
    # Function to get first day of the max year
    first_day_of_max_year <- function(df, date_col = "date") {
      max_year <- year(max(df[[date_col]], na.rm = TRUE))
      first_date <- make_date(year = max_year, month = 1, day = 1)
      return(first_date)
    }
    
    # Compute first day of the max year before filtering
    first_date <- first_day_of_max_year(cleaned_data(), "date")
    
    # Process data
    plot_lag <- cleaned_data() %>%
      select(date, cleaned_volume) |> 
      lag_by_weekdays() |> 
      pivot_longer(cols = c(cleaned_volume, contains("lag_")), 
                   names_to = "Metric", values_to = "Value") |> 
      filter(date >= first_date)  # Use precomputed first_date
    
    # Plot time series
    plot_time_series(
      .data = plot_lag,
      .date_var = date, 
      .value = Value, 
      .color_var = Metric, 
      .interactive = TRUE, 
      .smooth = FALSE
    ) |> 
      ggplotly()
  })
  
  
  
}

# Run the app ----
shinyApp(ui, server)
