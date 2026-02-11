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
                                                    "--- Vertical Locations ---" = "",
                                                    "High (Zones 1-3, 11)" = "vertical_high",
                                                    "Middle (Zones 4-6)" = "vertical_middle",
                                                    "Low (Zones 7-9, 12)" = "vertical_low",
                                                    "High - Strike Zone Only (1-3)" = "vertical_high_sz",
                                                    "Middle - Strike Zone Only (4-6)" = "vertical_middle_sz",
                                                    "Low - Strike Zone Only (7-9)" = "vertical_low_sz",
                                                    "--- Horizontal Locations ---" = "",
                                                    "Inside" = "horizontal_inside",
                                                    "Middle (Horizontal)" = "horizontal_middle",
                                                    "Outside" = "horizontal_outside",
                                                    "Inside - Strike Zone Only" = "horizontal_inside_sz",
                                                    "Middle - Strike Zone Only" = "horizontal_middle_sz",
                                                    "Outside - Strike Zone Only" = "horizontal_outside_sz",
                                                    "--- Zone Groups ---" = "",
                                                    "Strike Zone (1-9)" = "strike_zone",
                                                    "Outside Zone (11-14)" = "outside_zone",
                                                    "--- Specific Zones ---" = "",
                                                    "Zone 1 (High-Inside)" = "1",
                                                    "Zone 2 (High-Middle)" = "2",
                                                    "Zone 3 (High-Outside)" = "3",
                                                    "Zone 4 (Mid-Inside)" = "4",
                                                    "Zone 5 (Middle)" = "5",
                                                    "Zone 6 (Mid-Outside)" = "6",
                                                    "Zone 7 (Low-Inside)" = "7",
                                                    "Zone 8 (Low-Middle)" = "8",
                                                    "Zone 9 (Low-Outside)" = "9",
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
                                                    "--- Vertical Locations ---" = "",
                                                    "High (Zones 1-3, 11)" = "vertical_high",
                                                    "Middle (Zones 4-6)" = "vertical_middle",
                                                    "Low (Zones 7-9, 12)" = "vertical_low",
                                                    "High - Strike Zone Only (1-3)" = "vertical_high_sz",
                                                    "Middle - Strike Zone Only (4-6)" = "vertical_middle_sz",
                                                    "Low - Strike Zone Only (7-9)" = "vertical_low_sz",
                                                    "--- Horizontal Locations ---" = "",
                                                    "Inside" = "horizontal_inside",
                                                    "Middle (Horizontal)" = "horizontal_middle",
                                                    "Outside" = "horizontal_outside",
                                                    "Inside - Strike Zone Only" = "horizontal_inside_sz",
                                                    "Middle - Strike Zone Only" = "horizontal_middle_sz",
                                                    "Outside - Strike Zone Only" = "horizontal_outside_sz",
                                                    "--- Zone Groups ---" = "",
                                                    "Strike Zone (1-9)" = "strike_zone",
                                                    "Outside Zone (11-14)" = "outside_zone",
                                                    "--- Specific Zones ---" = "",
                                                    "Zone 1 (High-Inside)" = "1",
                                                    "Zone 2 (High-Middle)" = "2",
                                                    "Zone 3 (High-Outside)" = "3",
                                                    "Zone 4 (Mid-Inside)" = "4",
                                                    "Zone 5 (Middle)" = "5",
                                                    "Zone 6 (Mid-Outside)" = "6",
                                                    "Zone 7 (Low-Inside)" = "7",
                                                    "Zone 8 (Low-Middle)" = "8",
                                                    "Zone 9 (Low-Outside)" = "9",
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
        
        # Variance Analysis Section
        hr(style = "border-top: 2px solid rgba(148, 197, 248, 0.3); margin: 40px 0;"),
        
        h2("Player Variance Analysis", style = "color: #94c5f8; font-weight: 700; margin-bottom: 20px;"),
        p("Explore whether swing consistency or adaptability leads to better results. Variance is calculated across all swings for each player that meet the filter criteria.",
          style = "color: #b8d4ed; margin-bottom: 20px; font-size: 1rem;"),
        
        # Explanation box
        wellPanel(
          style = "background: rgba(58, 123, 200, 0.15); border: 1px solid rgba(148, 197, 248, 0.3);",
          h4("How Variance is Calculated", style = "color: #94c5f8; font-weight: 600; margin-top: 0;"),
          HTML("
          <p style='color: #b8d4ed; font-size: 0.9rem; margin-bottom: 10px;'>
            <strong>The Problem:</strong> Different metrics have very different scales. For example:
          </p>
          <ul style='color: #b8d4ed; font-size: 0.9rem; margin-bottom: 10px;'>
            <li>Attack angle variance: ~100-300 (degrees²)</li>
            <li>Bat speed variance: ~10-50 (mph²)</li>
            <li>Swing length variance: ~0.5-2 (feet²)</li>
          </ul>
          <p style='color: #b8d4ed; font-size: 0.9rem; margin-bottom: 10px;'>
            <strong>Without standardization:</strong> Attack angle would dominate the 'overall variance' calculation simply because its numbers are larger, not because it's actually more variable relative to its typical range.
          </p>
          <p style='color: #b8d4ed; font-size: 0.9rem; margin-bottom: 0;'>
            <strong>With standardization (recommended):</strong> Each metric is scaled to have the same mean and standard deviation before combining. This ensures equal weighting - a player who varies their attack angle a lot relative to other players counts the same as varying their bat speed a lot.
          </p>
        ")
        ),
        
        wellPanel(
          h3("Variance Filters", style = "color: #94c5f8; font-weight: 700; margin-top: 0;"),
          fluidRow(
            column(3,
                   selectInput("variance_swing_outcome", "Swing Outcome",
                               choices = c("All Swings" = "all",
                                           "Swing & Miss" = "miss",
                                           "Contact < 95 mph" = "contact_under_95",
                                           "Contact ≥ 95 mph" = "contact_over_95"))
            ),
            column(3,
                   selectInput("variance_pitch_type", "Pitch Type",
                               choices = c("All Pitch Types" = "all"))
            ),
            column(3,
                   selectInput("variance_pitch_location", "Pitch Location",
                               choices = c("All Locations" = "all",
                                           "--- Vertical Locations ---" = "",
                                           "High (Zones 1-3, 11)" = "vertical_high",
                                           "Middle (Zones 4-6)" = "vertical_middle",
                                           "Low (Zones 7-9, 12)" = "vertical_low",
                                           "High - Strike Zone Only (1-3)" = "vertical_high_sz",
                                           "Middle - Strike Zone Only (4-6)" = "vertical_middle_sz",
                                           "Low - Strike Zone Only (7-9)" = "vertical_low_sz",
                                           "--- Horizontal Locations ---" = "",
                                           "Inside" = "horizontal_inside",
                                           "Middle (Horizontal)" = "horizontal_middle",
                                           "Outside" = "horizontal_outside",
                                           "Inside - Strike Zone Only" = "horizontal_inside_sz",
                                           "Middle - Strike Zone Only" = "horizontal_middle_sz",
                                           "Outside - Strike Zone Only" = "horizontal_outside_sz",
                                           "--- Zone Groups ---" = "",
                                           "Strike Zone (1-9)" = "strike_zone",
                                           "Outside Zone (11-14)" = "outside_zone",
                                           "--- Specific Zones ---" = "",
                                           "Zone 1 (High-Inside)" = "1",
                                           "Zone 2 (High-Middle)" = "2",
                                           "Zone 3 (High-Outside)" = "3",
                                           "Zone 4 (Mid-Inside)" = "4",
                                           "Zone 5 (Middle)" = "5",
                                           "Zone 6 (Mid-Outside)" = "6",
                                           "Zone 7 (Low-Inside)" = "7",
                                           "Zone 8 (Low-Middle)" = "8",
                                           "Zone 9 (Low-Outside)" = "9",
                                           "Zone 11 (High)" = "11",
                                           "Zone 12 (Low)" = "12",
                                           "Zone 13 (Inside)" = "13",
                                           "Zone 14 (Outside)" = "14"))
            ),
            column(3,
                   numericInput("min_swings", "Minimum Swings per Player",
                                value = 10, min = 1, max = 100, step = 1)
            )
          ),
          fluidRow(
            column(4,
                   selectInput("variance_scale", "Variance Scale",
                               choices = c("Linear" = "linear",
                                           "Log Scale" = "log",
                                           "Standardized (Z-Score)" = "zscore"))
            ),
            column(4,
                   selectInput("point_size", "Point Size By",
                               choices = c("Fixed" = "fixed",
                                           "Number of Swings" = "swings"))
            ),
            column(4,
                   checkboxInput("show_outliers_only", "Highlight Outliers Only", value = FALSE)
            )
          ),
          fluidRow(
            column(12,
                   checkboxInput("standardize_variances", 
                                 "Standardize Variances Before Combining (Recommended)", 
                                 value = TRUE),
                   p("When checked, each metric's variance is scaled to the same range before calculating combined variances. This prevents metrics with larger scales from dominating the overall variance calculation.",
                     style = "color: #b8d4ed; font-size: 0.85rem; margin-top: 5px; margin-left: 20px;")
            )
          )
        ),
        
        # Variance scatter plots
        wellPanel(
          h4("Variance Quartile Summary", style = "color: #94c5f8; font-weight: 600; margin-top: 0;"),
          p("Players grouped by variance levels to show performance differences", 
            style = "color: #b8d4ed; font-size: 0.9rem; margin-bottom: 15px;"),
          uiOutput("variance_quartile_summary")
        ),
        
        fluidRow(
          column(6,
                 wellPanel(
                   h4("Attack Variance vs. Results", style = "color: #94c5f8; font-weight: 600; margin-top: 0;"),
                   p("Variance in Attack Angle, Attack Direction, and Swing Path Tilt", 
                     style = "color: #b8d4ed; font-size: 0.9rem; margin-bottom: 15px;"),
                   plotlyOutput("variance_plot_attack", height = "400px")
                 )
          ),
          column(6,
                 wellPanel(
                   h4("Speed/Length Variance vs. Results", style = "color: #94c5f8; font-weight: 600; margin-top: 0;"),
                   p("Variance in Bat Speed and Swing Length", 
                     style = "color: #b8d4ed; font-size: 0.9rem; margin-bottom: 15px;"),
                   plotlyOutput("variance_plot_speed", height = "400px")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 wellPanel(
                   h4("Overall Swing Variance vs. Results", style = "color: #94c5f8; font-weight: 600; margin-top: 0;"),
                   p("Combined variance across all swing metrics (Attack Angle, Attack Direction, Swing Path Tilt, Bat Speed, Swing Length)", 
                     style = "color: #b8d4ed; font-size: 0.9rem; margin-bottom: 15px;"),
                   plotlyOutput("variance_plot_overall", height = "400px")
                 )
          )
        ),
        
        # Individual metric variance plots
        h3("Individual Metric Variances", style = "color: #94c5f8; font-weight: 700; margin: 30px 0 20px 0;"),
        
        fluidRow(
          column(4,
                 wellPanel(
                   h4("Attack Angle Variance", style = "color: #94c5f8; font-weight: 600; margin-top: 0; font-size: 1rem;"),
                   plotlyOutput("variance_plot_attack_angle", height = "300px")
                 )
          ),
          column(4,
                 wellPanel(
                   h4("Attack Direction Variance", style = "color: #94c5f8; font-weight: 600; margin-top: 0; font-size: 1rem;"),
                   plotlyOutput("variance_plot_attack_direction", height = "300px")
                 )
          ),
          column(4,
                 wellPanel(
                   h4("Swing Path Tilt Variance", style = "color: #94c5f8; font-weight: 600; margin-top: 0; font-size: 1rem;"),
                   plotlyOutput("variance_plot_swing_tilt", height = "300px")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 wellPanel(
                   h4("Bat Speed Variance", style = "color: #94c5f8; font-weight: 600; margin-top: 0; font-size: 1rem;"),
                   plotlyOutput("variance_plot_bat_speed", height = "300px")
                 )
          ),
          column(6,
                 wellPanel(
                   h4("Swing Length Variance", style = "color: #94c5f8; font-weight: 600; margin-top: 0; font-size: 1rem;"),
                   plotlyOutput("variance_plot_swing_length", height = "300px")
                 )
          )
        ),
        
        # Variance data table
        wellPanel(
          h3("Player Variance Table", style = "color: #94c5f8; font-weight: 700; margin-top: 0;"),
          DTOutput("variance_table")
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
  
  # Increase maximum file upload size to 600MB
  options(shiny.maxRequestSize = 600*1024^2)
  
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
      
      pitch_choices <- c(
        "All Pitch Types" = "all",
        "--- Pitch Categories ---" = "",
        "Fastballs" = "category_fastball",
        "Breaking Balls" = "category_breaking",
        "Offspeed" = "category_offspeed",
        "--- Specific Pitches ---" = "",
        setNames(pitch_types, pitch_types)
      )
      
      updateSelectInput(session, "pitch_type_a", choices = pitch_choices)
      updateSelectInput(session, "pitch_type_b", choices = pitch_choices)
      updateSelectInput(session, "variance_pitch_type", choices = pitch_choices)
      
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
  
  # Get vertical location (high/middle/low)
  get_vertical_location <- function(zone) {
    if (is.na(zone) || zone == "") return("Unknown")
    z <- as.numeric(zone)
    if (is.na(z)) return("Unknown")
    # High: zones 1, 2, 3, 11
    if (z %in% c(1, 2, 3, 11)) return("High")
    # Middle: zones 4, 5, 6
    if (z %in% c(4, 5, 6)) return("Middle")
    # Low: zones 7, 8, 9, 12
    if (z %in% c(7, 8, 9, 12)) return("Low")
    return("Unknown")
  }
  
  # Get horizontal location (inside/middle/outside)
  get_horizontal_location <- function(zone, stand) {
    if (is.na(zone) || zone == "") return("Unknown")
    z <- as.numeric(zone)
    if (is.na(z)) return("Unknown")
    if (is.na(stand) || stand == "") return("Unknown")
    
    # For right-handed batters
    if (stand == "R") {
      # Inside: zones 1, 4, 7, 13
      if (z %in% c(1, 4, 7, 13)) return("Inside")
      # Middle: zones 2, 5, 8
      if (z %in% c(2, 5, 8)) return("Middle")
      # Outside: zones 3, 6, 9, 14
      if (z %in% c(3, 6, 9, 14)) return("Outside")
    } else if (stand == "L") {
      # For left-handed batters (mirror image)
      # Inside: zones 3, 6, 9, 14
      if (z %in% c(3, 6, 9, 14)) return("Inside")
      # Middle: zones 2, 5, 8
      if (z %in% c(2, 5, 8)) return("Middle")
      # Outside: zones 1, 4, 7, 13
      if (z %in% c(1, 4, 7, 13)) return("Outside")
    }
    
    # Zones 11 and 12 don't have inside/outside
    return("Unknown")
  }
  
  # Categorize pitch type into groups
  get_pitch_category <- function(pitch_name) {
    if (is.na(pitch_name) || pitch_name == "") return("Unknown")
    
    # Fastballs
    if (pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter", "2-Seam Fastball")) {
      return("Fastball")
    }
    # Breaking balls
    if (pitch_name %in% c("Curveball", "Slider", "Slurve", "Sweeper", "Knuckle Curve")) {
      return("Breaking")
    }
    # Offspeed
    if (pitch_name %in% c("Changeup", "Split-Finger", "Splitter", "Forkball", "Screwball")) {
      return("Offspeed")
    }
    
    return("Other")
  }
  
  # Filter data based on selections - Comparison A
  filtered_data_a <- reactive({
    req(baseball_data())
    
    data <- baseball_data() %>%
      filter(!is.na(bat_speed), bat_speed != "") %>%
      mutate(
        swing_outcome = map2_chr(description, launch_speed, categorize_swing),
        location_category = map_chr(zone, get_pitch_location),
        pitch_category = map_chr(pitch_name, get_pitch_category),
        vertical_location = map_chr(zone, get_vertical_location),
        horizontal_location = map2_chr(zone, stand, get_horizontal_location)
      )
    
    # Filter by swing outcome
    if (input$swing_outcome_a != "all") {
      data <- data %>% filter(swing_outcome == input$swing_outcome_a)
    }
    
    # Filter by pitch type or category
    if (input$pitch_type_a != "all" && input$pitch_type_a != "") {
      if (input$pitch_type_a == "category_fastball") {
        data <- data %>% filter(pitch_category == "Fastball")
      } else if (input$pitch_type_a == "category_breaking") {
        data <- data %>% filter(pitch_category == "Breaking")
      } else if (input$pitch_type_a == "category_offspeed") {
        data <- data %>% filter(pitch_category == "Offspeed")
      } else {
        # Specific pitch name
        data <- data %>% filter(pitch_name == input$pitch_type_a)
      }
    }
    
    # Filter by location
    if (input$pitch_location_a != "all" && input$pitch_location_a != "") {
      if (input$pitch_location_a == "strike_zone") {
        data <- data %>% filter(zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_a == "outside_zone") {
        data <- data %>% filter(zone %in% c("11", "12", "13", "14"))
      } else if (input$pitch_location_a == "vertical_high") {
        data <- data %>% filter(vertical_location == "High")
      } else if (input$pitch_location_a == "vertical_middle") {
        data <- data %>% filter(vertical_location == "Middle")
      } else if (input$pitch_location_a == "vertical_low") {
        data <- data %>% filter(vertical_location == "Low")
      } else if (input$pitch_location_a == "vertical_high_sz") {
        data <- data %>% filter(zone %in% c("1", "2", "3"))
      } else if (input$pitch_location_a == "vertical_middle_sz") {
        data <- data %>% filter(zone %in% c("4", "5", "6"))
      } else if (input$pitch_location_a == "vertical_low_sz") {
        data <- data %>% filter(zone %in% c("7", "8", "9"))
      } else if (input$pitch_location_a == "horizontal_inside") {
        data <- data %>% filter(horizontal_location == "Inside")
      } else if (input$pitch_location_a == "horizontal_middle") {
        data <- data %>% filter(horizontal_location == "Middle")
      } else if (input$pitch_location_a == "horizontal_outside") {
        data <- data %>% filter(horizontal_location == "Outside")
      } else if (input$pitch_location_a == "horizontal_inside_sz") {
        data <- data %>% filter(horizontal_location == "Inside", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_a == "horizontal_middle_sz") {
        data <- data %>% filter(horizontal_location == "Middle", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_a == "horizontal_outside_sz") {
        data <- data %>% filter(horizontal_location == "Outside", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
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
        location_category = map_chr(zone, get_pitch_location),
        pitch_category = map_chr(pitch_name, get_pitch_category),
        vertical_location = map_chr(zone, get_vertical_location),
        horizontal_location = map2_chr(zone, stand, get_horizontal_location)
      )
    
    # Filter by swing outcome
    if (input$swing_outcome_b != "all") {
      data <- data %>% filter(swing_outcome == input$swing_outcome_b)
    }
    
    # Filter by pitch type or category
    if (input$pitch_type_b != "all" && input$pitch_type_b != "") {
      if (input$pitch_type_b == "category_fastball") {
        data <- data %>% filter(pitch_category == "Fastball")
      } else if (input$pitch_type_b == "category_breaking") {
        data <- data %>% filter(pitch_category == "Breaking")
      } else if (input$pitch_type_b == "category_offspeed") {
        data <- data %>% filter(pitch_category == "Offspeed")
      } else {
        # Specific pitch name
        data <- data %>% filter(pitch_name == input$pitch_type_b)
      }
    }
    
    # Filter by location
    if (input$pitch_location_b != "all" && input$pitch_location_b != "") {
      if (input$pitch_location_b == "strike_zone") {
        data <- data %>% filter(zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_b == "outside_zone") {
        data <- data %>% filter(zone %in% c("11", "12", "13", "14"))
      } else if (input$pitch_location_b == "vertical_high") {
        data <- data %>% filter(vertical_location == "High")
      } else if (input$pitch_location_b == "vertical_middle") {
        data <- data %>% filter(vertical_location == "Middle")
      } else if (input$pitch_location_b == "vertical_low") {
        data <- data %>% filter(vertical_location == "Low")
      } else if (input$pitch_location_b == "vertical_high_sz") {
        data <- data %>% filter(zone %in% c("1", "2", "3"))
      } else if (input$pitch_location_b == "vertical_middle_sz") {
        data <- data %>% filter(zone %in% c("4", "5", "6"))
      } else if (input$pitch_location_b == "vertical_low_sz") {
        data <- data %>% filter(zone %in% c("7", "8", "9"))
      } else if (input$pitch_location_b == "horizontal_inside") {
        data <- data %>% filter(horizontal_location == "Inside")
      } else if (input$pitch_location_b == "horizontal_middle") {
        data <- data %>% filter(horizontal_location == "Middle")
      } else if (input$pitch_location_b == "horizontal_outside") {
        data <- data %>% filter(horizontal_location == "Outside")
      } else if (input$pitch_location_b == "horizontal_inside_sz") {
        data <- data %>% filter(horizontal_location == "Inside", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_b == "horizontal_middle_sz") {
        data <- data %>% filter(horizontal_location == "Middle", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$pitch_location_b == "horizontal_outside_sz") {
        data <- data %>% filter(horizontal_location == "Outside", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
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
  
  # Variance Analysis Section
  
  # Filter data for variance analysis
  filtered_data_variance <- reactive({
    req(baseball_data())
    
    data <- baseball_data() %>%
      filter(!is.na(bat_speed), bat_speed != "") %>%
      mutate(
        swing_outcome = map2_chr(description, launch_speed, categorize_swing),
        location_category = map_chr(zone, get_pitch_location),
        pitch_category = map_chr(pitch_name, get_pitch_category),
        vertical_location = map_chr(zone, get_vertical_location),
        horizontal_location = map2_chr(zone, stand, get_horizontal_location)
      )
    
    # Filter by swing outcome
    if (input$variance_swing_outcome != "all") {
      data <- data %>% filter(swing_outcome == input$variance_swing_outcome)
    }
    
    # Filter by pitch type or category
    if (input$variance_pitch_type != "all" && input$variance_pitch_type != "") {
      if (input$variance_pitch_type == "category_fastball") {
        data <- data %>% filter(pitch_category == "Fastball")
      } else if (input$variance_pitch_type == "category_breaking") {
        data <- data %>% filter(pitch_category == "Breaking")
      } else if (input$variance_pitch_type == "category_offspeed") {
        data <- data %>% filter(pitch_category == "Offspeed")
      } else {
        # Specific pitch name
        data <- data %>% filter(pitch_name == input$variance_pitch_type)
      }
    }
    
    # Filter by location
    if (input$variance_pitch_location != "all" && input$variance_pitch_location != "") {
      if (input$variance_pitch_location == "strike_zone") {
        data <- data %>% filter(zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$variance_pitch_location == "outside_zone") {
        data <- data %>% filter(zone %in% c("11", "12", "13", "14"))
      } else if (input$variance_pitch_location == "vertical_high") {
        data <- data %>% filter(vertical_location == "High")
      } else if (input$variance_pitch_location == "vertical_middle") {
        data <- data %>% filter(vertical_location == "Middle")
      } else if (input$variance_pitch_location == "vertical_low") {
        data <- data %>% filter(vertical_location == "Low")
      } else if (input$variance_pitch_location == "vertical_high_sz") {
        data <- data %>% filter(zone %in% c("1", "2", "3"))
      } else if (input$variance_pitch_location == "vertical_middle_sz") {
        data <- data %>% filter(zone %in% c("4", "5", "6"))
      } else if (input$variance_pitch_location == "vertical_low_sz") {
        data <- data %>% filter(zone %in% c("7", "8", "9"))
      } else if (input$variance_pitch_location == "horizontal_inside") {
        data <- data %>% filter(horizontal_location == "Inside")
      } else if (input$variance_pitch_location == "horizontal_middle") {
        data <- data %>% filter(horizontal_location == "Middle")
      } else if (input$variance_pitch_location == "horizontal_outside") {
        data <- data %>% filter(horizontal_location == "Outside")
      } else if (input$variance_pitch_location == "horizontal_inside_sz") {
        data <- data %>% filter(horizontal_location == "Inside", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$variance_pitch_location == "horizontal_middle_sz") {
        data <- data %>% filter(horizontal_location == "Middle", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else if (input$variance_pitch_location == "horizontal_outside_sz") {
        data <- data %>% filter(horizontal_location == "Outside", zone %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      } else {
        data <- data %>% filter(zone == input$variance_pitch_location)
      }
    }
    
    data
  })
  
  # Calculate player variance statistics
  player_variance <- reactive({
    data <- filtered_data_variance()
    req(data)
    req(nrow(data) > 0)
    
    # Calculate variance by player
    variance_stats <- data %>%
      group_by(player_name) %>%
      summarize(
        n_swings = n(),
        # Attack variance (angles)
        attack_angle_var = var(attack_angle, na.rm = TRUE),
        attack_direction_var = var(attack_direction, na.rm = TRUE),
        swing_path_tilt_var = var(swing_path_tilt, na.rm = TRUE),
        # Speed/length variance
        bat_speed_var = var(bat_speed, na.rm = TRUE),
        swing_length_var = var(swing_length, na.rm = TRUE),
        # Average wOBA
        avg_woba = mean(estimated_woba_using_speedangle, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(n_swings >= input$min_swings) %>%
      mutate(
        # Raw individual variances
        attack_angle_var_raw = attack_angle_var,
        attack_direction_var_raw = attack_direction_var,
        swing_path_tilt_var_raw = swing_path_tilt_var,
        bat_speed_var_raw = bat_speed_var,
        swing_length_var_raw = swing_length_var
      )
    
    # Standardize variances if requested (recommended to prevent scale dominance)
    if (input$standardize_variances) {
      variance_stats <- variance_stats %>%
        mutate(
          # Standardize each variance to 0-1 scale before combining
          attack_angle_var = scale(attack_angle_var_raw)[,1],
          attack_direction_var = scale(attack_direction_var_raw)[,1],
          swing_path_tilt_var = scale(swing_path_tilt_var_raw)[,1],
          bat_speed_var = scale(bat_speed_var_raw)[,1],
          swing_length_var = scale(swing_length_var_raw)[,1]
        )
    }
    
    # Calculate combined variances
    variance_stats <- variance_stats %>%
      mutate(
        # Combined attack variance (average of the three angle variances)
        attack_variance = (attack_angle_var + attack_direction_var + swing_path_tilt_var) / 3,
        # Combined speed/length variance
        speed_length_variance = (bat_speed_var + swing_length_var) / 2,
        # Overall variance (average of all five metrics)
        overall_variance = (attack_angle_var + attack_direction_var + swing_path_tilt_var + 
                              bat_speed_var + swing_length_var) / 5
      ) %>%
      filter(!is.na(attack_variance), !is.na(speed_length_variance), !is.na(overall_variance),
             !is.na(avg_woba))
    
    # Apply scaling based on user selection
    if (input$variance_scale == "log") {
      variance_stats <- variance_stats %>%
        mutate(
          attack_variance_scaled = log(attack_variance + 1),
          speed_length_variance_scaled = log(speed_length_variance + 1),
          overall_variance_scaled = log(overall_variance + 1),
          attack_angle_var_scaled = log(attack_angle_var + 1),
          attack_direction_var_scaled = log(attack_direction_var + 1),
          swing_path_tilt_var_scaled = log(swing_path_tilt_var + 1),
          bat_speed_var_scaled = log(bat_speed_var + 1),
          swing_length_var_scaled = log(swing_length_var + 1)
        )
    } else if (input$variance_scale == "zscore") {
      variance_stats <- variance_stats %>%
        mutate(
          attack_variance_scaled = scale(attack_variance)[,1],
          speed_length_variance_scaled = scale(speed_length_variance)[,1],
          overall_variance_scaled = scale(overall_variance)[,1],
          attack_angle_var_scaled = scale(attack_angle_var)[,1],
          attack_direction_var_scaled = scale(attack_direction_var)[,1],
          swing_path_tilt_var_scaled = scale(swing_path_tilt_var)[,1],
          bat_speed_var_scaled = scale(bat_speed_var)[,1],
          swing_length_var_scaled = scale(swing_length_var)[,1]
        )
    } else {
      # Linear - no transformation
      variance_stats <- variance_stats %>%
        mutate(
          attack_variance_scaled = attack_variance,
          speed_length_variance_scaled = speed_length_variance,
          overall_variance_scaled = overall_variance,
          attack_angle_var_scaled = attack_angle_var,
          attack_direction_var_scaled = attack_direction_var,
          swing_path_tilt_var_scaled = swing_path_tilt_var,
          bat_speed_var_scaled = bat_speed_var,
          swing_length_var_scaled = swing_length_var
        )
    }
    
    # Identify outliers if requested
    if (input$show_outliers_only) {
      # Calculate outliers based on wOBA (top/bottom 25%) or variance (top/bottom 25%)
      woba_q25 <- quantile(variance_stats$avg_woba, 0.25, na.rm = TRUE)
      woba_q75 <- quantile(variance_stats$avg_woba, 0.75, na.rm = TRUE)
      var_q25 <- quantile(variance_stats$overall_variance_scaled, 0.25, na.rm = TRUE)
      var_q75 <- quantile(variance_stats$overall_variance_scaled, 0.75, na.rm = TRUE)
      
      variance_stats <- variance_stats %>%
        mutate(is_outlier = avg_woba <= woba_q25 | avg_woba >= woba_q75 |
                 overall_variance_scaled <= var_q25 | overall_variance_scaled >= var_q75)
    } else {
      variance_stats <- variance_stats %>%
        mutate(is_outlier = TRUE)
    }
    
    variance_stats
  })
  
  # Quartile summary
  output$variance_quartile_summary <- renderUI({
    data <- player_variance()
    req(data)
    req(nrow(data) >= 4)
    
    # Calculate quartiles for overall variance
    quartile_summary <- data %>%
      mutate(
        overall_var_quartile = ntile(overall_variance, 4),
        attack_var_quartile = ntile(attack_variance, 4),
        speed_var_quartile = ntile(speed_length_variance, 4)
      ) %>%
      group_by(overall_var_quartile) %>%
      summarize(
        n_players = n(),
        avg_woba = mean(avg_woba, na.rm = TRUE),
        avg_overall_var = mean(overall_variance, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        quartile_label = case_when(
          overall_var_quartile == 1 ~ "Q1 (Most Consistent)",
          overall_var_quartile == 2 ~ "Q2 (Below Avg Variance)",
          overall_var_quartile == 3 ~ "Q3 (Above Avg Variance)",
          overall_var_quartile == 4 ~ "Q4 (Most Variable)"
        )
      )
    
    # Create HTML table
    table_html <- paste0(
      "<table style='width: 100%; color: #e8edf2; font-size: 0.95rem;'>",
      "<thead><tr style='background: rgba(58, 123, 200, 0.3); border-bottom: 2px solid rgba(148, 197, 248, 0.5);'>",
      "<th style='padding: 10px; text-align: left;'>Variance Group</th>",
      "<th style='padding: 10px; text-align: center;'>Players</th>",
      "<th style='padding: 10px; text-align: center;'>Avg Variance</th>",
      "<th style='padding: 10px; text-align: center;'>Avg wOBA</th>",
      "</tr></thead><tbody>"
    )
    
    for (i in 1:nrow(quartile_summary)) {
      row <- quartile_summary[i,]
      bg_color <- if (i %% 2 == 0) "rgba(30, 58, 95, 0.2)" else "rgba(30, 58, 95, 0.4)"
      table_html <- paste0(table_html,
                           "<tr style='background: ", bg_color, "; border-bottom: 1px solid rgba(148, 197, 248, 0.2);'>",
                           "<td style='padding: 10px;'>", row$quartile_label, "</td>",
                           "<td style='padding: 10px; text-align: center;'>", row$n_players, "</td>",
                           "<td style='padding: 10px; text-align: center;'>", sprintf("%.2f", row$avg_overall_var), "</td>",
                           "<td style='padding: 10px; text-align: center; font-weight: 700; color: #94c5f8;'>", sprintf("%.3f", row$avg_woba), "</td>",
                           "</tr>"
      )
    }
    
    table_html <- paste0(table_html, "</tbody></table>")
    
    HTML(table_html)
  })
  
  # Attack variance plot
  output$variance_plot_attack <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    
    # Filter to outliers if requested
    plot_data <- data %>% filter(is_outlier)
    
    # Calculate correlation on scaled data
    cor_val <- cor(plot_data$attack_variance_scaled, plot_data$avg_woba, use = "complete.obs")
    
    # Fit linear model for trend line
    fit <- lm(avg_woba ~ attack_variance_scaled, data = plot_data)
    plot_data$fitted <- predict(fit)
    
    # Determine point size
    point_sizes <- if (input$point_size == "swings") {
      scales::rescale(plot_data$n_swings, to = c(6, 16))
    } else {
      rep(10, nrow(plot_data))
    }
    
    # Create hover text showing raw values
    hover_text <- if (input$standardize_variances) {
      paste0(plot_data$player_name, "<br>",
             "Swings: ", plot_data$n_swings, "<br>",
             "Raw Attack Var: ", round(plot_data$attack_angle_var_raw, 2), ", ",
             round(plot_data$attack_direction_var_raw, 2), ", ",
             round(plot_data$swing_path_tilt_var_raw, 2), "<br>",
             "Combined (Standardized): ", round(plot_data$attack_variance, 2), "<br>",
             "Avg wOBA: ", round(plot_data$avg_woba, 3))
    } else {
      paste0(plot_data$player_name, "<br>",
             "Swings: ", plot_data$n_swings, "<br>",
             "Attack Var: ", round(plot_data$attack_variance, 2), "<br>",
             "Avg wOBA: ", round(plot_data$avg_woba, 3))
    }
    
    # Create axis label
    x_label <- paste0("Attack Variance",
                      if (input$variance_scale == "log") " (Log Scale)" 
                      else if (input$variance_scale == "zscore") " (Z-Score)"
                      else " (Angles)")
    
    plot_ly(plot_data, x = ~attack_variance_scaled, y = ~avg_woba, 
            type = 'scatter', mode = 'markers',
            text = hover_text,
            hovertemplate = '%{text}<extra></extra>',
            marker = list(
              size = point_sizes,
              color = ~avg_woba,
              colorscale = list(c(0, 'rgba(200, 100, 100, 0.8)'), 
                                c(0.5, 'rgba(148, 197, 248, 0.8)'),
                                c(1, 'rgba(100, 200, 100, 0.8)')),
              colorbar = list(title = "Avg wOBA"),
              line = list(color = 'rgba(255, 255, 255, 0.5)', width = 1)
            )) %>%
      add_lines(x = ~attack_variance_scaled, y = ~fitted, 
                line = list(color = 'rgba(255, 200, 100, 0.8)', width = 2, dash = 'dash'),
                name = 'Trend',
                hoverinfo = 'skip') %>%
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#e8edf2', family = 'Segoe UI'),
        xaxis = list(
          title = x_label,
          gridcolor = 'rgba(148, 197, 248, 0.2)',
          color = '#b8d4ed'
        ),
        yaxis = list(
          title = "Average wOBA",
          gridcolor = 'rgba(148, 197, 248, 0.2)',
          color = '#b8d4ed'
        ),
        annotations = list(
          list(x = 0.02, y = 0.98, xref = 'paper', yref = 'paper',
               text = paste0('Correlation: ', round(cor_val, 3)),
               showarrow = FALSE, xanchor = 'left', yanchor = 'top',
               font = list(size = 12, color = '#94c5f8'))
        ),
        showlegend = FALSE
      )
  })
  
  # Speed/Length variance plot
  output$variance_plot_speed <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    
    plot_data <- data %>% filter(is_outlier)
    cor_val <- cor(plot_data$speed_length_variance_scaled, plot_data$avg_woba, use = "complete.obs")
    fit <- lm(avg_woba ~ speed_length_variance_scaled, data = plot_data)
    plot_data$fitted <- predict(fit)
    
    point_sizes <- if (input$point_size == "swings") {
      scales::rescale(plot_data$n_swings, to = c(6, 16))
    } else {
      rep(10, nrow(plot_data))
    }
    
    x_label <- paste0("Speed/Length Variance",
                      if (input$variance_scale == "log") " (Log Scale)" 
                      else if (input$variance_scale == "zscore") " (Z-Score)" else "")
    
    plot_ly(plot_data, x = ~speed_length_variance_scaled, y = ~avg_woba, 
            type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>",
                           "Speed/Length Var: ", round(speed_length_variance, 2), "<br>",
                           "Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200, 100, 100, 0.8)'), 
                                            c(0.5, 'rgba(148, 197, 248, 0.8)'),
                                            c(1, 'rgba(100, 200, 100, 0.8)')),
                          colorbar = list(title = "Avg wOBA"),
                          line = list(color = 'rgba(255, 255, 255, 0.5)', width = 1))) %>%
      add_lines(x = ~speed_length_variance_scaled, y = ~fitted, 
                line = list(color = 'rgba(255, 200, 100, 0.8)', width = 2, dash = 'dash'),
                name = 'Trend', hoverinfo = 'skip') %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI'),
             xaxis = list(title = x_label, gridcolor = 'rgba(148, 197, 248, 0.2)', color = '#b8d4ed'),
             yaxis = list(title = "Average wOBA", gridcolor = 'rgba(148, 197, 248, 0.2)', color = '#b8d4ed'),
             annotations = list(list(x = 0.02, y = 0.98, xref = 'paper', yref = 'paper',
                                     text = paste0('Correlation: ', round(cor_val, 3)), showarrow = FALSE,
                                     xanchor = 'left', yanchor = 'top', font = list(size = 12, color = '#94c5f8'))),
             showlegend = FALSE)
  })
  
  # Overall variance plot
  output$variance_plot_overall <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    
    plot_data <- data %>% filter(is_outlier)
    cor_val <- cor(plot_data$overall_variance_scaled, plot_data$avg_woba, use = "complete.obs")
    fit <- lm(avg_woba ~ overall_variance_scaled, data = plot_data)
    plot_data$fitted <- predict(fit)
    
    point_sizes <- if (input$point_size == "swings") {
      scales::rescale(plot_data$n_swings, to = c(6, 16))
    } else {
      rep(10, nrow(plot_data))
    }
    
    x_label <- paste0("Overall Swing Variance",
                      if (input$variance_scale == "log") " (Log Scale)" 
                      else if (input$variance_scale == "zscore") " (Z-Score)" else "")
    
    plot_ly(plot_data, x = ~overall_variance_scaled, y = ~avg_woba, 
            type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>",
                           "Overall Var: ", round(overall_variance, 2), "<br>",
                           "Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200, 100, 100, 0.8)'), 
                                            c(0.5, 'rgba(148, 197, 248, 0.8)'),
                                            c(1, 'rgba(100, 200, 100, 0.8)')),
                          colorbar = list(title = "Avg wOBA"),
                          line = list(color = 'rgba(255, 255, 255, 0.5)', width = 1))) %>%
      add_lines(x = ~overall_variance_scaled, y = ~fitted, 
                line = list(color = 'rgba(255, 200, 100, 0.8)', width = 2, dash = 'dash'),
                name = 'Trend', hoverinfo = 'skip') %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI'),
             xaxis = list(title = x_label, gridcolor = 'rgba(148, 197, 248, 0.2)', color = '#b8d4ed'),
             yaxis = list(title = "Average wOBA", gridcolor = 'rgba(148, 197, 248, 0.2)', color = '#b8d4ed'),
             annotations = list(list(x = 0.02, y = 0.98, xref = 'paper', yref = 'paper',
                                     text = paste0('Correlation: ', round(cor_val, 3)), showarrow = FALSE,
                                     xanchor = 'left', yanchor = 'top', font = list(size = 12, color = '#94c5f8'))),
             showlegend = FALSE)
  })
  
  # Individual metric variance plots
  output$variance_plot_attack_angle <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    plot_data <- data %>% filter(is_outlier)
    point_sizes <- if (input$point_size == "swings") scales::rescale(plot_data$n_swings, to = c(6, 12)) else rep(8, nrow(plot_data))
    
    plot_ly(plot_data, x = ~attack_angle_var_scaled, y = ~avg_woba, type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>Variance: ", round(attack_angle_var, 2), "<br>Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200,100,100,0.8)'), c(0.5, 'rgba(148,197,248,0.8)'), c(1, 'rgba(100,200,100,0.8)')),
                          showscale = FALSE, line = list(color = 'rgba(255,255,255,0.5)', width = 1))) %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI', size = 10),
             xaxis = list(title = "Variance", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             yaxis = list(title = "Avg wOBA", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             margin = list(l = 50, r = 20, t = 20, b = 40))
  })
  
  output$variance_plot_attack_direction <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    plot_data <- data %>% filter(is_outlier)
    point_sizes <- if (input$point_size == "swings") scales::rescale(plot_data$n_swings, to = c(6, 12)) else rep(8, nrow(plot_data))
    
    plot_ly(plot_data, x = ~attack_direction_var_scaled, y = ~avg_woba, type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>Variance: ", round(attack_direction_var, 2), "<br>Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200,100,100,0.8)'), c(0.5, 'rgba(148,197,248,0.8)'), c(1, 'rgba(100,200,100,0.8)')),
                          showscale = FALSE, line = list(color = 'rgba(255,255,255,0.5)', width = 1))) %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI', size = 10),
             xaxis = list(title = "Variance", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             yaxis = list(title = "Avg wOBA", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             margin = list(l = 50, r = 20, t = 20, b = 40))
  })
  
  output$variance_plot_swing_tilt <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    plot_data <- data %>% filter(is_outlier)
    point_sizes <- if (input$point_size == "swings") scales::rescale(plot_data$n_swings, to = c(6, 12)) else rep(8, nrow(plot_data))
    
    plot_ly(plot_data, x = ~swing_path_tilt_var_scaled, y = ~avg_woba, type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>Variance: ", round(swing_path_tilt_var, 2), "<br>Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200,100,100,0.8)'), c(0.5, 'rgba(148,197,248,0.8)'), c(1, 'rgba(100,200,100,0.8)')),
                          showscale = FALSE, line = list(color = 'rgba(255,255,255,0.5)', width = 1))) %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI', size = 10),
             xaxis = list(title = "Variance", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             yaxis = list(title = "Avg wOBA", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             margin = list(l = 50, r = 20, t = 20, b = 40))
  })
  
  output$variance_plot_bat_speed <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    plot_data <- data %>% filter(is_outlier)
    point_sizes <- if (input$point_size == "swings") scales::rescale(plot_data$n_swings, to = c(6, 12)) else rep(8, nrow(plot_data))
    
    plot_ly(plot_data, x = ~bat_speed_var_scaled, y = ~avg_woba, type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>Variance: ", round(bat_speed_var, 2), "<br>Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200,100,100,0.8)'), c(0.5, 'rgba(148,197,248,0.8)'), c(1, 'rgba(100,200,100,0.8)')),
                          showscale = FALSE, line = list(color = 'rgba(255,255,255,0.5)', width = 1))) %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI', size = 10),
             xaxis = list(title = "Variance", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             yaxis = list(title = "Avg wOBA", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             margin = list(l = 50, r = 20, t = 20, b = 40))
  })
  
  output$variance_plot_swing_length <- renderPlotly({
    data <- player_variance()
    req(data)
    req(nrow(data) > 0)
    plot_data <- data %>% filter(is_outlier)
    point_sizes <- if (input$point_size == "swings") scales::rescale(plot_data$n_swings, to = c(6, 12)) else rep(8, nrow(plot_data))
    
    plot_ly(plot_data, x = ~swing_length_var_scaled, y = ~avg_woba, type = 'scatter', mode = 'markers',
            text = ~paste0(player_name, "<br>Swings: ", n_swings, "<br>Variance: ", round(swing_length_var, 2), "<br>Avg wOBA: ", round(avg_woba, 3)),
            hovertemplate = '%{text}<extra></extra>',
            marker = list(size = point_sizes, color = ~avg_woba,
                          colorscale = list(c(0, 'rgba(200,100,100,0.8)'), c(0.5, 'rgba(148,197,248,0.8)'), c(1, 'rgba(100,200,100,0.8)')),
                          showscale = FALSE, line = list(color = 'rgba(255,255,255,0.5)', width = 1))) %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#e8edf2', family = 'Segoe UI', size = 10),
             xaxis = list(title = "Variance", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             yaxis = list(title = "Avg wOBA", gridcolor = 'rgba(148,197,248,0.2)', color = '#b8d4ed', titlefont = list(size = 10)),
             margin = list(l = 50, r = 20, t = 20, b = 40))
  })
  
  # Variance table
  output$variance_table <- renderDT({
    data <- player_variance()
    req(data)
    
    data %>%
      select(player_name, n_swings, 
             attack_variance, speed_length_variance, overall_variance,
             attack_angle_var, attack_direction_var, swing_path_tilt_var,
             bat_speed_var, swing_length_var,
             avg_woba) %>%
      arrange(desc(avg_woba)) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        colnames = c('Player', 'Swings', 
                     'Attack Var', 'Speed/Length Var', 'Overall Var',
                     'Attack Angle Var', 'Attack Dir Var', 'Swing Tilt Var',
                     'Bat Speed Var', 'Swing Length Var',
                     'Avg wOBA'),
        class = 'cell-border stripe',
        style = 'bootstrap4'
      ) %>%
      formatRound(columns = c('attack_variance', 'speed_length_variance', 'overall_variance',
                              'attack_angle_var', 'attack_direction_var', 'swing_path_tilt_var',
                              'bat_speed_var', 'swing_length_var', 'avg_woba'), 
                  digits = 3)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

