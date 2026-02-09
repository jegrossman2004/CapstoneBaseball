library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #0a1628 0%, #1a2332 50%, #0f1922 100%);
        color: #e8edf2;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .well {
        background: rgba(30, 58, 95, 0.4);
        border: 1px solid rgba(255, 255, 255, 0.1);
        border-radius: 12px;
      }
      .header-box {
        background: linear-gradient(135deg, #1e3a5f 0%, #2d5a88 100%);
        padding: 30px;
        border-radius: 16px;
        margin-bottom: 20px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.4);
        border: 1px solid rgba(255, 255, 255, 0.1);
      }
      .header-title {
        font-size: 2.5rem;
        font-weight: 800;
        margin: 0 0 10px 0;
        background: linear-gradient(135deg, #ffffff 0%, #94c5f8 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      .header-subtitle {
        font-size: 1.1rem;
        color: #b8d4ed;
        margin: 0;
      }
      .stat-card {
        background: linear-gradient(135deg, rgba(30, 58, 95, 0.6) 0%, rgba(45, 90, 136, 0.4) 100%);
        padding: 20px;
        border-radius: 12px;
        border: 1px solid rgba(255, 255, 255, 0.1);
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.2);
        text-align: center;
        margin-bottom: 15px;
      }
      .stat-label {
        font-size: 0.85rem;
        color: #b8d4ed;
        margin-bottom: 8px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 600;
      }
      .stat-value {
        font-size: 2rem;
        font-weight: 800;
        color: #ffffff;
      }
      .stat-unit {
        font-size: 1.2rem;
        color: #94c5f8;
      }
      .sample-size-box {
        background: rgba(58, 123, 200, 0.2);
        border-radius: 8px;
        border: 1px solid rgba(148, 197, 248, 0.3);
        padding: 15px;
        margin-top: 15px;
      }
      .selectize-input {
        background: rgba(15, 25, 34, 0.8) !important;
        border: 1px solid rgba(148, 197, 248, 0.3) !important;
        color: #e8edf2 !important;
      }
      .selectize-dropdown {
        background: rgba(15, 25, 34, 0.95) !important;
        border: 1px solid rgba(148, 197, 248, 0.3) !important;
        color: #e8edf2 !important;
      }
      label {
        color: #b8d4ed !important;
        font-weight: 600 !important;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-size: 0.95rem !important;
      }
      .shiny-input-container {
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(class = "container-fluid", style = "max-width: 1400px; margin: 0 auto; padding: 20px;",
      # Header
      div(class = "header-box",
          div(class = "header-title", "Baseball Swing Analytics Dashboard"),
          div(class = "header-subtitle", "Analyze swing metrics by outcome, pitch type, and location")
      ),
      
      # File Input
      wellPanel(
        fileInput("datafile", "Upload CSV File",
                  accept = c("text/csv", ".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"),
        textOutput("file_status")
      ),
      
      # Main content (only shown when data is loaded)
      conditionalPanel(
        condition = "output.data_loaded",
        
        # Comparison View
        h2("Comparison View", style = "color: #94c5f8; font-weight: 700; margin-bottom: 20px;"),
        
        fluidRow(
          # Left Side - Comparison A
          column(6,
                 wellPanel(
                   h3("Comparison A", style = "color: #94c5f8; font-weight: 700; margin-top: 0;"),
                   fluidRow(
                     column(12,
                            selectInput("swing_outcome_a", "Swing Outcome",
                                        choices = c("All Swings" = "all",
                                                    "Swing & Miss" = "miss",
                                                    "Contact < 95 mph" = "contact_under_95",
                                                    "Contact ≥ 95 mph" = "contact_over_95"))
                     )
                   ),
                   fluidRow(
                     column(6,
                            selectInput("pitch_type_a", "Pitch Type",
                                        choices = c("All Pitch Types" = "all"))
                     ),
                     column(6,
                            selectInput("pitch_location_a", "Pitch Location",
                                        choices = c("All Locations" = "all",
                                                    "Strike Zone (1-9)" = "strike_zone",
                                                    "Zone 1 (High-Inside)" = "1",
                                                    "Zone 2 (High-Middle)" = "2",
                                                    "Zone 3 (High-Outside)" = "3",
                                                    "Zone 4 (Mid-Inside)" = "4",
                                                    "Zone 5 (Middle)" = "5",
                                                    "Zone 6 (Mid-Outside)" = "6",
                                                    "Zone 7 (Low-Inside)" = "7",
                                                    "Zone 8 (Low-Middle)" = "8",
                                                    "Zone 9 (Low-Outside)" = "9",
                                                    "Outside Zone (11-14)" = "outside_zone",
                                                    "Zone 11 (High)" = "11",
                                                    "Zone 12 (Low)" = "12",
                                                    "Zone 13 (Inside)" = "13",
                                                    "Zone 14 (Outside)" = "14"))
                     )
                   ),
                   div(class = "sample-size-box",
                       HTML("<strong style='color: #94c5f8;'>Sample Size:</strong> "),
                       textOutput("sample_size_a", inline = TRUE)
                   )
                 ),
                 
                 # Stats A
                 uiOutput("stat_attack_angle_a"),
                 uiOutput("stat_attack_direction_a"),
                 uiOutput("stat_swing_path_tilt_a"),
                 uiOutput("stat_bat_speed_a"),
                 uiOutput("stat_swing_length_a"),
                 uiOutput("stat_estimated_woba_a")
          ),
          
          # Right Side - Comparison B
          column(6,
                 wellPanel(
                   h3("Comparison B", style = "color: #94c5f8; font-weight: 700; margin-top: 0;"),
                   fluidRow(
                     column(12,
                            selectInput("swing_outcome_b", "Swing Outcome",
                                        choices = c("All Swings" = "all",
                                                    "Swing & Miss" = "miss",
                                                    "Contact < 95 mph" = "contact_under_95",
                                                    "Contact ≥ 95 mph" = "contact_over_95"),
                                        selected = "all")
                     )
                   ),
                   fluidRow(
                     column(6,
                            selectInput("pitch_type_b", "Pitch Type",
                                        choices = c("All Pitch Types" = "all"))
                     ),
                     column(6,
                            selectInput("pitch_location_b", "Pitch Location",
                                        choices = c("All Locations" = "all",
                                                    "Strike Zone (1-9)" = "strike_zone",
                                                    "Zone 1 (High-Inside)" = "1",
                                                    "Zone 2 (High-Middle)" = "2",
                                                    "Zone 3 (High-Outside)" = "3",
                                                    "Zone 4 (Mid-Inside)" = "4",
                                                    "Zone 5 (Middle)" = "5",
                                                    "Zone 6 (Mid-Outside)" = "6",
                                                    "Zone 7 (Low-Inside)" = "7",
                                                    "Zone 8 (Low-Middle)" = "8",
                                                    "Zone 9 (Low-Outside)" = "9",
                                                    "Outside Zone (11-14)" = "outside_zone",
                                                    "Zone 11 (High)" = "11",
                                                    "Zone 12 (Low)" = "12",
                                                    "Zone 13 (Inside)" = "13",
                                                    "Zone 14 (Outside)" = "14"))
                     )
                   ),
                   div(class = "sample-size-box",
                       HTML("<strong style='color: #94c5f8;'>Sample Size:</strong> "),
                       textOutput("sample_size_b", inline = TRUE)
                   )
                 ),
                 
                 # Stats B
                 uiOutput("stat_attack_angle_b"),
                 uiOutput("stat_attack_direction_b"),
                 uiOutput("stat_swing_path_tilt_b"),
                 uiOutput("stat_bat_speed_b"),
                 uiOutput("stat_swing_length_b"),
                 uiOutput("stat_estimated_woba_b")
          )
        ),
        
        # Data Tables
        wellPanel(
          h3("Detailed Data", style = "color: #94c5f8; font-weight: 700; margin-top: 0;"),
          tabsetPanel(
            tabPanel("Comparison A", 
                     br(),
                     DTOutput("data_table_a")),
            tabPanel("Comparison B", 
                     br(),
                     DTOutput("data_table_b"))
          )
        )
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Increase maximum file upload size to 100MB
  options(shiny.maxRequestSize = 100*1024^2)
  
  # Reactive value to store the data
  baseball_data <- reactiveVal(NULL)
  
  # Load data when file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)
    
    tryCatch({
      data <- read_csv(input$datafile$datapath, show_col_types = FALSE)
      baseball_data(data)
      
      # Update pitch type choices
      pitch_types <- data %>%
        filter(!is.na(pitch_name), pitch_name != "") %>%
        pull(pitch_name) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "pitch_type_a",
                        choices = c("All Pitch Types" = "all", setNames(pitch_types, pitch_types)))
      
      updateSelectInput(session, "pitch_type_b",
                        choices = c("All Pitch Types" = "all", setNames(pitch_types, pitch_types)))
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Check if data is loaded
  output$data_loaded <- reactive({
    !is.null(baseball_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # File status
  output$file_status <- renderText({
    if (!is.null(baseball_data())) {
      paste("✓ File loaded successfully:", nrow(baseball_data()), "rows")
    } else {
      ""
    }
  })
  
  # Categorize swing outcome
  categorize_swing <- function(description, launch_speed) {
    if (grepl("swinging_strike", description, ignore.case = TRUE)) {
      return("miss")
    }
    if (!is.na(launch_speed) && launch_speed > 0) {
      if (launch_speed < 95) {
        return("contact_under_95")
      } else {
        return("contact_over_95")
      }
    }
    return("other")
  }
  
  # Get pitch location category
  get_pitch_location <- function(zone) {
    if (is.na(zone) || zone == "") return("Unknown")
    z <- as.numeric(zone)
    if (is.na(z)) return("Unknown")
    if (z >= 1 && z <= 9) return("Strike Zone")
    if (z >= 11 && z <= 14) return("Outside Zone")
    return("Unknown")
  }
  
  # Filter data based on selections - Comparison A
  filtered_data_a <- reactive({
    req(baseball_data())
    
    data <- baseball_data() %>%
      filter(!is.na(bat_speed), bat_speed != "") %>%
      mutate(
        swing_outcome = map2_chr(description, launch_speed, categorize_swing),
        location_category = map_chr(zone, get_pitch_location)
      )
    
    # Filter by swing outcome
    if (input$swing_outcome_a != "all") {
      data <- data %>% filter(swing_outcome == input$swing_outcome_a)
    }
    
    # Filter by pitch type
    if (input$pitch_type_a != "all") {
      data <- data %>% filter(pitch_name == input$pitch_type_a)
    }
    
    # Filter by location
    if (input$pitch_location_a != "all") {
      if (input$pitch_location_a == "strike_zone") {
        data <- data %>% filter(zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_a == "outside_zone") {
        data <- data %>% filter(zone %in% c("11", "12", "13", "14"))
      } else {
        # Specific zone number
        data <- data %>% filter(zone == input$pitch_location_a)
      }
    }
    
    data
  })
  
  # Filter data based on selections - Comparison B
  filtered_data_b <- reactive({
    req(baseball_data())
    
    data <- baseball_data() %>%
      filter(!is.na(bat_speed), bat_speed != "") %>%
      mutate(
        swing_outcome = map2_chr(description, launch_speed, categorize_swing),
        location_category = map_chr(zone, get_pitch_location)
      )
    
    # Filter by swing outcome
    if (input$swing_outcome_b != "all") {
      data <- data %>% filter(swing_outcome == input$swing_outcome_b)
    }
    
    # Filter by pitch type
    if (input$pitch_type_b != "all") {
      data <- data %>% filter(pitch_name == input$pitch_type_b)
    }
    
    # Filter by location
    if (input$pitch_location_b != "all") {
      if (input$pitch_location_b == "strike_zone") {
        data <- data %>% filter(zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_b == "outside_zone") {
        data <- data %>% filter(zone %in% c("11", "12", "13", "14"))
      } else {
        # Specific zone number
        data <- data %>% filter(zone == input$pitch_location_b)
      }
    }
    
    data
  })
  
  # Calculate statistics - Comparison A
  stats_a <- reactive({
    data <- filtered_data_a()
    
    if (nrow(data) == 0) return(NULL)
    
    list(
      count = nrow(data),
      attack_angle = mean(data$attack_angle, na.rm = TRUE),
      attack_direction = mean(data$attack_direction, na.rm = TRUE),
      swing_path_tilt = mean(data$swing_path_tilt, na.rm = TRUE),
      bat_speed = mean(data$bat_speed, na.rm = TRUE),
      swing_length = mean(data$swing_length, na.rm = TRUE),
      estimated_woba = mean(data$estimated_woba_using_speedangle, na.rm = TRUE)
    )
  })
  
  # Calculate statistics - Comparison B
  stats_b <- reactive({
    data <- filtered_data_b()
    
    if (nrow(data) == 0) return(NULL)
    
    list(
      count = nrow(data),
      attack_angle = mean(data$attack_angle, na.rm = TRUE),
      attack_direction = mean(data$attack_direction, na.rm = TRUE),
      swing_path_tilt = mean(data$swing_path_tilt, na.rm = TRUE),
      bat_speed = mean(data$bat_speed, na.rm = TRUE),
      swing_length = mean(data$swing_length, na.rm = TRUE),
      estimated_woba = mean(data$estimated_woba_using_speedangle, na.rm = TRUE)
    )
  })
  
  # Sample size - Comparison A
  output$sample_size_a <- renderText({
    s <- stats_a()
    if (is.null(s)) return("0 swings")
    paste(format(s$count, big.mark = ","), "swings")
  })
  
  # Sample size - Comparison B
  output$sample_size_b <- renderText({
    s <- stats_b()
    if (is.null(s)) return("0 swings")
    paste(format(s$count, big.mark = ","), "swings")
  })
  
  # Individual stat cards - Comparison A
  output$stat_attack_angle_a <- renderUI({
    s <- stats_a()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Attack Angle"),
        div(class = "stat-value",
            sprintf("%.1f", s$attack_angle),
            span(class = "stat-unit", "°")
        )
    )
  })
  
  output$stat_attack_direction_a <- renderUI({
    s <- stats_a()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Attack Direction"),
        div(class = "stat-value",
            sprintf("%.1f", s$attack_direction),
            span(class = "stat-unit", "°")
        )
    )
  })
  
  output$stat_swing_path_tilt_a <- renderUI({
    s <- stats_a()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Swing Path Tilt"),
        div(class = "stat-value",
            sprintf("%.1f", s$swing_path_tilt),
            span(class = "stat-unit", "°")
        )
    )
  })
  
  output$stat_bat_speed_a <- renderUI({
    s <- stats_a()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Bat Speed"),
        div(class = "stat-value",
            sprintf("%.1f", s$bat_speed),
            span(class = "stat-unit", " mph")
        )
    )
  })
  
  output$stat_swing_length_a <- renderUI({
    s <- stats_a()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Swing Length"),
        div(class = "stat-value",
            sprintf("%.1f", s$swing_length),
            span(class = "stat-unit", " ft")
        )
    )
  })
  
  output$stat_estimated_woba_a <- renderUI({
    s <- stats_a()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Est. wOBA"),
        div(class = "stat-value",
            sprintf("%.3f", s$estimated_woba),
            span(class = "stat-unit", "")
        )
    )
  })
  # Individual stat cards - Comparison B
  output$stat_attack_angle_b <- renderUI({
    s <- stats_b()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Attack Angle"),
        div(class = "stat-value",
            sprintf("%.1f", s$attack_angle),
            span(class = "stat-unit", "°")
        )
    )
  })
  
  output$stat_attack_direction_b <- renderUI({
    s <- stats_b()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Attack Direction"),
        div(class = "stat-value",
            sprintf("%.1f", s$attack_direction),
            span(class = "stat-unit", "°")
        )
    )
  })
  
  output$stat_swing_path_tilt_b <- renderUI({
    s <- stats_b()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Swing Path Tilt"),
        div(class = "stat-value",
            sprintf("%.1f", s$swing_path_tilt),
            span(class = "stat-unit", "°")
        )
    )
  })
  
  output$stat_bat_speed_b <- renderUI({
    s <- stats_b()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Bat Speed"),
        div(class = "stat-value",
            sprintf("%.1f", s$bat_speed),
            span(class = "stat-unit", " mph")
        )
    )
  })
  
  output$stat_swing_length_b <- renderUI({
    s <- stats_b()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Swing Length"),
        div(class = "stat-value",
            sprintf("%.1f", s$swing_length),
            span(class = "stat-unit", " ft")
        )
    )
  })
  
  output$stat_estimated_woba_b <- renderUI({
    s <- stats_b()
    if (is.null(s)) return(NULL)
    div(class = "stat-card",
        div(class = "stat-label", "Est. wOBA"),
        div(class = "stat-value",
            sprintf("%.3f", s$estimated_woba),
            span(class = "stat-unit", "")
        )
    )
  })
  
  # Data table - Comparison A
  output$data_table_a <- renderDT({
    data <- filtered_data_a()
    req(data)
    
    data %>%
      select(player_name, pitch_name, description, 
             attack_angle, attack_direction, swing_path_tilt,
             bat_speed, swing_length, launch_speed, 
             estimated_woba_using_speedangle) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        style = 'bootstrap4'
      ) %>%
      formatRound(columns = c('attack_angle', 'attack_direction', 'swing_path_tilt',
                              'bat_speed', 'swing_length', 'launch_speed',
                              'estimated_woba_using_speedangle'), 
                  digits = 1)
  })
  
  # Data table - Comparison B
  output$data_table_b <- renderDT({
    data <- filtered_data_b()
    req(data)
    
    data %>%
      select(player_name, pitch_name, description, 
             attack_angle, attack_direction, swing_path_tilt,
             bat_speed, swing_length, launch_speed, 
             estimated_woba_using_speedangle) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        style = 'bootstrap4'
      ) %>%
      formatRound(columns = c('attack_angle', 'attack_direction', 'swing_path_tilt',
                              'bat_speed', 'swing_length', 'launch_speed',
                              'estimated_woba_using_speedangle'), 
                  digits = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

