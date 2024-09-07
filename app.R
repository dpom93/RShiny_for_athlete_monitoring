##################################################
# Learning R Shiny for Athlete Monitoring
##################################################
# rsconnect::showLogs()
# rm(list = ls()) # clear environment
# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyr)          # for pivot_wider
library(googlesheets4)
library(googledrive)
library(flexdashboard)  # For gauge chart
library(ggplot2)        # For plotting
library(DT)             # For ROM Table


################### GET GSHEETS TO WORK W/ SHINY ##########################
#rsconnect::setAccountInfo(name='dpomeroy', token='84531FE110FBC4D61E6BFFAC385308D1', secret='oqIlxOd4S1fPXycyVJSUoVQWudALJe9flanTdn2C')# designate project-specific cache
#options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
drive_auth()

# see your token file in the cache, if you like
list.files(".secrets/")

############################ READ IN DATA #################################

# Read in new assessment data from Google Sheets
new_assessment <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1tXBVGMXrCPWBsaq-mFqdj0yHRTx_smDG924V-rChEXo/edit?gid=0#gid=0",
  sheet = "Sheet1"
)

# Read in Historic Data
historic_data <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1JXtJeBE5z_fFERvof_DJruqthnNsNlkGQfaDeEwhzlk/edit?gid=0#gid=0",
  sheet = "Sheet1"
)

# Ensure column names are correct
colnames(new_assessment) <- toupper(colnames(new_assessment))  # Convert all to uppercase
colnames(historic_data) <- toupper(colnames(new_assessment))


############################# USER INTERFACE #############################

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("Player Assessment Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for Full Name
      selectInput("playerName", "Select Player Name:",
                  choices = unique(new_assessment$`FULL NAME`)),
      
      # Dropdown for Event ID based on selected player
      selectInput("eventId", "Select Event ID:",
                  choices = NULL),
      
      # Output for displaying the Training Program Suggestion
      h4("Training Program Suggestion"),
      #textOutput("p_power_score"),
      #textOutput("p_velo_score"),
      # Display Average Score and Ratio
      #textOutput("average_scores"),
      #textOutput("ratio_scores"),
      # Display Training Program Suggestion
      textOutput("training_program_suggestion"),
      
      # Player details section
      h4("Player Details"),
      
      textOutput("ageGroup"),
      textOutput("position"),
      textOutput("secPosition"),
      textOutput("batsThrows"),
      br(), # Add space between sections
      
      
      # Text & Gauge output for overall score with sidebarPanel
      fluidRow(
        column(width = 12,
               h4("Overall Score:"),
               br(),
               gaugeOutput("overallGauge")
        )
      ),
      
      # Display percentiles
      h4("Percentiles for Selected Metrics"),
      tableOutput("percentilesTable"),
    ),
    
    mainPanel(
      # Assessment Scores section
      fluidRow(
        column(width = 12,
               div(style = "background-color: black; padding: 10px; color: white; border-radius: 3px;",
                   h3("Assessment Scores")))),
      br(), # Add space between sections
      
      # Gauge Scores
      br(),
      fluidRow(
        column(width = 2),
        column(width = 2, 
               gaugeOutput("movementGauge"), 
               div(style = "text-align: center; position: relative; bottom: 80px; font-size: 10px; font-weight: bold;", textOutput("movementLabel"))),
        column(width = 2, 
               gaugeOutput("lowerBodyPowerGauge"), 
               div(style = "text-align: center; position: relative; bottom: 80px; font-size: 10px; font-weight: bold;", textOutput("lowerBodyPowerLabel"))),
        column(width = 2, 
               gaugeOutput("upperBodyPowerGauge"), 
               div(style = "text-align: center; position: relative; bottom: 80px; font-size: 10px; font-weight: bold;", textOutput("upperBodyPowerLabel"))),
        column(width = 2, 
               gaugeOutput("rotationalPowerGauge"), 
               div(style = "text-align: center; position: relative; bottom: 80px; font-size: 10px; font-weight: bold;", textOutput("rotationalPowerLabel"))),
        column(width = 2),
        br()
      ),
      
      # Categories Bar Chart
      plotOutput("categoriesBarChart"),
      br(),
      
      # ROM Data Table
      # black header
      # Header for norms table
      #fluidRow(
      #column(width = 12,
      #div(style = "background-color: black; padding: 10px; color: white; border-radius: 3px;",
      #h3("Range of Motion (ROM) Norms")))
      #),
      # Norms table
      #fluidRow(
      #column(width = 12,
      #tableOutput("normsTable"))
      #),
      
      # ROM Metrics Table
      fluidRow(
        column(width = 12,
               div(style = "background-color: black; padding: 10px; color: white; border-radius: 3px;",
                   h3("Range of Motion (ROM) Metrics")))
      ),
      fluidRow(
        column(width = 12,
               DTOutput("romMetricsTable")),  # Use DTOutput for rendering DT tables
      ),
      br(),
      
      ## LONGITUDINAL SECTION  ##  
      # Lower Body Power Section with black background
      fluidRow(
        column(width = 12,
               div(style = "background-color: black; padding: 10px; color: white; border-radius: 3px;",
                   h3("Lower Body Power Section")))
      ),
      # 10-5 header
      fluidRow(
        column(width = 12,
               div(style = "background-color: steelblue; padding: 1px; color: white; border-radius: 3px;",
                   h4("10-5 Test")))
      ),
      
      # 10-5 Test data table section
      h4("10-5 Test Metrics Table"),
      
      # data table
      fluidRow(
        column(width = 12,
               tableOutput("ten5MetricsTable"))
      ),
      
      # 10-5 graph header
      h4("10-5 Test Longitudinal Graph"),
      
      # 10-5 Dropdown
      fluidRow(
        column(width = 6,
               selectInput("selected10Metric", "Select 10-5 Test Metric:",
                           choices = NULL)) # Empty choices, to be populated in the server logic
      ),
      
      
      # 10-5 Graph
      fluidRow(
        column(width = 12,
               plotOutput("longitudinalBarGraph", height = "300px"))
      ),
      
      # CMJ Header
      br(),
      fluidRow(
        column(width = 12,
               div(style = "background-color: steelblue; padding: 1px; color: white; border-radius: 3px;",
                   h4("Bilateral CMJ")))
      ),
      
      # CMJ Test data table section
      h4("Bilateral CMJ Metrics Table"),
      
      # data table
      fluidRow(
        column(width = 12,
               tableOutput("cmjMetricsTable"))
      ),
      
      # 10-5 graph header
      h4("Bilateral CMJ Longitudinal Graph"),
      
      # CMJ Dropdown
      fluidRow(
        column(width = 6,
               selectInput("selectedCMJMetric", "Select CMJ Test Metric:",
                           choices = NULL)) # Empty choices, to be populated in the server logic
      ),
      # CMJ Graph
      fluidRow(
        column(width = 12,
               plotOutput("cmjBarGraph", height = "300px"))
      ),
      
      
      # Upper Body Power Section Header
      br(),
      fluidRow(
        column(width = 12,
               div(style = "background-color: black; padding: 10px; color: white; border-radius: 3px;",
                   h3("Upper Body Power Section")))
      ),
      
      # Upper Body Metric Header
      fluidRow(
        column(width = 12,
               div(style = "background-color: steelblue; padding: 1px; color: white; border-radius: 3px;",
                   h4("Upper Body Metrics")))
      ),
      
      # Upper Body Test data table section
      h4("Upper Body Metrics Table"),
      
      # data table
      fluidRow(
        column(width = 12,
               tableOutput("upperMetricsTable"))
      ),
      
      # Upper Body graph header
      h4("Upper Body Longitudinal Graph"),
      
      
      # Upper Body Test Dropdown
      fluidRow(
        column(width = 6,
               selectInput("selectedUpperMetric", "Select Upper Body Test Metric:",
                           choices = NULL)) # Empty choices, to be populated in the server logic
      ),
      # Upper Body Test Graph
      fluidRow(
        column(width = 12,
               plotOutput("upperBarGraph", height = "300px"))
      ),
      
      # Rotational Section Header
      br(),
      fluidRow(
        column(width = 12,
               div(style = "background-color: black; padding: 10px; color: white; border-radius: 3px;",
                   h3("Rotational Power Section")))
      ),
      
      # Rotational metric header
      fluidRow(
        column(width = 12,
               div(style = "background-color: steelblue; padding: 1px; color: white; border-radius: 3px;",
                   h4("Rotational Power Metrics")))
      ),
      
      
      # Rotational Power Test data table section
      h4("Rotational Power Metrics Table"),
      
      # data table
      fluidRow(
        column(width = 12,
               tableOutput("rotaMetricsTable"))
      ),
      
      # Rotational metric dropdown
      fluidRow(
        column(width = 6,
               selectInput("selectedRotaMetric", "Select Rotational Test Metric:",
                           choices = c("MED BALL - HITTER STYLE SHOT PUT PEAK VELOCITY (M/S)",
                                       "MED BALL - HITTER STYLE SHOT PUT PEAK POWER (W)",
                                       "MED BALL - HITTER STYLE SHOT PUT PEAK ACCELERATION (G)")) )
      ),
      # Rotational Test Graph
      fluidRow(
        column(width = 12,
               plotOutput("rotaBarGraph"))
      )
    )
  )
)


############################# SERVER CODE #############################

# Define server logic for Shiny app
server <- function(input, output, session) {
  
  # Reactive expression to gather selected metrics from longitudinal dropdowns
  selected_metrics <- reactive({
    c(input$selected10Metric, input$selectedCMJMetric, input$selectedUpperMetric, input$selectedRotaMetric)
  })
  
  # Render Event ID dropdown based on selected player
  observe({
    player_data <- filter(new_assessment, `FULL NAME` == input$playerName)
    event_choices <- unique(player_data$`EVENT ID`)
    
    # Extract numeric part of the event IDs for sorting
    event_numbers <- as.integer(gsub("\\D", "", event_choices)) # Remove non-digit characters and convert to integers
    
    # Handle cases where extraction might result in NA
    event_numbers[is.na(event_numbers)] <- 0
    
    # Find the max event number
    max_event <- event_choices[which.max(event_numbers)]
    
    updateSelectInput(session, "eventId",
                      choices = event_choices,
                      selected = max_event)
  })
  
  # Reactive expression to calculate percentiles
  percentiles_data <- reactive({
    req(input$playerName)
    
    player_data <- filter(new_assessment, `FULL NAME` == input$playerName)
    age_group <- unique(player_data$`AGE GROUP`)
    
    historic_data_filtered <- filter(historic_data, `AGE GROUP` == age_group)
    
    metrics <- selected_metrics()
    
    percentiles <- data.frame(Metric = character(),
                              `25th Percentile` = numeric(),
                              `75th Percentile` = numeric(),
                              stringsAsFactors = FALSE)
    
    for (metric in metrics) {
      if (metric %in% colnames(historic_data_filtered)) {
        p25 <- quantile(historic_data_filtered[[metric]], 0.25, na.rm = TRUE)
        p75 <- quantile(historic_data_filtered[[metric]], 0.75, na.rm = TRUE)
        percentiles <- rbind(percentiles, data.frame(Metric = metric, `25th Percentile` = p25, `75th Percentile` = p75))
      } else if (metric == input$selectedRotaMetric) {
        left_metric <- paste0(metric, " LEFT")
        right_metric <- paste0(metric, " RIGHT")
        
        if (left_metric %in% colnames(historic_data_filtered) && right_metric %in% colnames(historic_data_filtered)) {
          p25_left <- quantile(historic_data_filtered[[left_metric]], 0.25, na.rm = TRUE)
          p75_left <- quantile(historic_data_filtered[[left_metric]], 0.75, na.rm = TRUE)
          
          p25_right <- quantile(historic_data_filtered[[right_metric]], 0.25, na.rm = TRUE)
          p75_right <- quantile(historic_data_filtered[[right_metric]], 0.75, na.rm = TRUE)
          
          p25_avg <- mean(c(p25_left, p25_right), na.rm = TRUE)
          p75_avg <- mean(c(p75_left, p75_right), na.rm = TRUE)
          
          percentiles <- rbind(percentiles, data.frame(Metric = metric, `25th Percentile` = p25_avg, `75th Percentile` = p75_avg))
        }
      }
    }
    
    # Debugging output
    cat("Percentiles Data:\n")
    print(percentiles)
    
    percentiles
  })
  
  # Render the percentiles table
  output$percentilesTable <- renderTable({
    percentiles_data()
  }, rownames = FALSE)
  
  ###### TRAINING PROGRAM SUGGESTION ######
  # Reactive expression to extract program scores based on selected player and event
  program_scores <- reactive({
    req(input$playerName, input$eventId)
    
    # Filter the data to get scores for the selected player and event
    scores <- filter(new_assessment, `FULL NAME` == input$playerName & `EVENT ID` == input$eventId)
    
    # Check if scores exist
    if (nrow(scores) == 0) {
      return(list(p_power_score = NA, p_velo_score = NA))
    }
    
    # Return the scores
    list(
      p_power_score = scores$`P.POWER SCORE`,
      p_velo_score = scores$`P.VELO SCORE`,
      movement_score = scores$`MOVEMENT SCORE`
    )
  })
  
  # Calculate and display the average of P.POWER SCORE and P.VELO SCORE
  output$average_scores <- renderText({
    scores <- program_scores()
    
    if (!is.na(scores$p_power_score) && !is.na(scores$p_velo_score)) {
      p_power_score <- as.numeric(scores$p_power_score)
      p_velo_score <- as.numeric(scores$p_velo_score)
      
      average_score <- (p_power_score + p_velo_score) / 2
      paste("Average Score:", round(average_score, 2))
    } else {
      "Average Score: Not Available"
    }
  })
  
  # Calculate and display the ratio of P.POWER SCORE to P.VELO SCORE
  output$ratio_scores <- renderText({
    scores <- program_scores()
    
    if (!is.na(scores$p_power_score) && !is.na(scores$p_velo_score)) {
      p_power_score <- as.numeric(scores$p_power_score)
      p_velo_score <- as.numeric(scores$p_velo_score)
      
      # Calculate the ratio and convert to percentage
      if (p_velo_score != 0) {
        ratio <- (p_power_score / p_velo_score) / 100
        paste("Ratio (P.POWER SCORE / P.VELO SCORE / 100):", round(ratio, 2))
      } else {
        "Ratio: Cannot divide by zero"
      }
    } else {
      "Ratio: Not Available"
    }
  })
  
  # Display the P.POWER SCORE in the sidebar
  output$p_power_score <- renderText({
    if (!is.na(program_scores()$p_power_score)) {
      paste("P.POWER SCORE:", program_scores()$p_power_score)
    } else {
      "P.POWER SCORE: Not Available"
    }
  })
  
  # Display the P.VELO SCORE in the sidebar
  output$p_velo_score <- renderText({
    if (!is.na(program_scores()$p_velo_score)) {
      paste("P.VELO SCORE:", program_scores()$p_velo_score)
    } else {
      "P.VELO SCORE: Not Available"
    }
  })
  
  # Determine training program suggestion
  output$training_program_suggestion <- renderText({
    req(input$playerName, input$eventId)
    
    scores <- program_scores()
    
    if (!is.na(scores$p_power_score) && !is.na(scores$p_velo_score) && !is.na(scores$movement_score)) {
      p_power_score <- as.numeric(scores$p_power_score)
      p_velo_score <- as.numeric(scores$p_velo_score)
      movement_score <- as.numeric(scores$movement_score)
      
      average_score <- (p_power_score + p_velo_score) / 2
      ratio <- (p_power_score / p_velo_score) * 100
      
      suggestion <- "No suggestion"
      
      if (movement_score < 60) {
        suggestion <- "Movement"
      } else if (movement_score < average_score) {
        suggestion <- "Movement"
      } else if (ratio > 0) {
        suggestion <- "Strength"
      } else if (ratio < 0) {
        suggestion <- "Power"
      }
      
      paste("Suggestion:", suggestion)
    } else {
      "Suggestion: Not Available"
    }
  })
  
  ### ROM ##
  # Define norms data frame
  norms <- data.frame(
    Metric = c("ANKLE DORSIFLEXION LEFT", "ANKLE DORSIFLEXION RIGHT",
               "STRAIGHT LEG RAISE LEFT", "STRAIGHT LEG RAISE RIGHT",
               "SUPINE 90/90 HIP INTERNAL ROTATION RIGHT", "SUPINE 90/90 HIP INTERNAL ROTATION LEFT",
               "SUPINE 90/90 HIP EXTERNAL ROTATION RIGHT", "SUPINE 90/90 HIP EXTERNAL ROTATION LEFT",
               "TORSO ROTATION LEFT", "TORSO ROTATION RIGHT",
               "SHOULDER EXTERNAL ROTATION LEFT", "SHOULDER EXTERNAL ROTATION RIGHT"),
    Needs_Improvement = c(30, 30, 70, 70, 30, 30, 35, 35, 35, 35, 90, 90),
    Good = c(44.9, 44.9, 79.9, 79.9, 39.9, 39.9, 44.9, 44.9, 49.9, 49.9, 99.9, 99.9),
    Above_Average = c(45, 45, 80, 80, 40, 40, 45, 45, 50, 50, 100, 100)
  )
  
  # Render norms table
  output$normsTable <- renderTable({
    norms
  }, rownames = FALSE)
  
  # Render ROM data table
  output$romDataTable <- DT::renderDataTable({
    req(player_data_all_events())
    
    # Filter ROM data for the selected player across all events
    rom_data <- player_data_all_events() %>%
      select(
        `EVENT ID`, 
        `SHOULDER EXTERNAL ROTATION LEFT`, 
        `SHOULDER EXTERNAL ROTATION RIGHT`, 
        `STRAIGHT LEG RAISE LEFT`, 
        `STRAIGHT LEG RAISE RIGHT`, 
        `SUPINE 90/90 HIP EXTERNAL ROTATION LEFT`, 
        `SUPINE 90/90 HIP EXTERNAL ROTATION RIGHT`, 
        `SUPINE 90/90 HIP INTERNAL ROTATION RIGHT`, 
        `SUPINE 90/90 HIP INTERNAL ROTATION LEFT`, 
        `TORSO ROTATION LEFT`, 
        `TORSO ROTATION RIGHT`, 
        `ANKLE DORSIFLEXION LEFT`, 
        `ANKLE DORSIFLEXION RIGHT`
      )
    
    # Render as DataTable
    DT::datatable(rom_data, options = list(
      pageLength = 12,  # Show up to 12 rows per page
      autoWidth = TRUE,
      dom = 't',  # Only show the table (no search bar or pagination)
      ordering = FALSE  # Disable ordering
    ))
  })
  
  # Render ROM metrics table with color coding
  output$romMetricsTable <- DT::renderDataTable({
    req(player_data_all_events())
    
    # Extract and pivot the data to have metrics as rows and events as columns
    data <- player_data_all_events() %>%
      select(
        `EVENT ID`,
        `SHOULDER EXTERNAL ROTATION LEFT`, 
        `SHOULDER EXTERNAL ROTATION RIGHT`, 
        `STRAIGHT LEG RAISE LEFT`, 
        `STRAIGHT LEG RAISE RIGHT`, 
        `SUPINE 90/90 HIP EXTERNAL ROTATION LEFT`, 
        `SUPINE 90/90 HIP EXTERNAL ROTATION RIGHT`, 
        `SUPINE 90/90 HIP INTERNAL ROTATION RIGHT`, 
        `SUPINE 90/90 HIP INTERNAL ROTATION LEFT`, 
        `TORSO ROTATION LEFT`, 
        `TORSO ROTATION RIGHT`, 
        `ANKLE DORSIFLEXION LEFT`, 
        `ANKLE DORSIFLEXION RIGHT`
      ) %>%
      pivot_longer(cols = -`EVENT ID`, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = `EVENT ID`, values_from = Value) %>%
      arrange(Metric)
    
    ###### ROM COLOR TESTING ######
    
    # Add color based on norms
    data_colored <- data %>%
      pivot_longer(cols = -Metric, names_to = "Event ID", values_to = "Value") %>%
      left_join(norms, by = "Metric") %>%
      mutate(
        Color = case_when(
          Value < Needs_Improvement ~ "red",
          Value <= Good ~ "yellow",
          Value > Good ~ "green",
          TRUE ~ "transparent"
        )
      ) %>%
      pivot_wider(names_from = `Event ID`, values_from = c(Value, Color)) %>%
      arrange(Metric)
    
    # Dynamically update column names to remove underscores and add labels
    colnames(data_colored) <- gsub("_", " ", colnames(data_colored))  # Replace underscores with spaces
    colnames(data_colored) <- gsub("Value", " ", colnames(data_colored))  # Replace Value with spaces
    
    # Define the positions of the columns for styling borders
    border_after_cols <- c("Metric", "Above Average")
    
    # Render as DataTable with conditional formatting
    DT::datatable(data_colored, options = list(
      pageLength = 12,  # Show up to 12 rows per page
      dom = 't',  # Only show the table (no search bar or pagination)
      ordering = FALSE,  # Disable ordering
      columnDefs = list(
        list(className = 'dt-center', targets = "_all"),  # Center-align text for all columns
        list(className = 'dt-header-center', targets = "_all"),  # Center-align header text for all columns
        list(width = '250px', targets = which(colnames(data_colored) == "Metric")),  # Set width for 'Metric' column
        list(targets = which(grepl("^Color ", colnames(data_colored))),  # Apply styling to color columns
             createdCell = JS(
               "function(td, cellData, rowData, row, col) {",
               "  $(td).css({'border-right': '1px solid gray'});",  # Add gray border
               "}"
             ))
      ),
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().container()).css({'width': '100%'});",
        "}"
      )
    )) %>%
      formatStyle(
        columns = grep("^Color ", names(data_colored), value = TRUE),  # Style color columns as needed
        backgroundColor = styleEqual(c("red", "yellow", "green"), c("red", "yellow", "green")),
        color = "transparent"  # Ensure no text color is displayed
      ) %>%
      formatStyle(
        columns = which(colnames(data_colored) == "Metric"),  # Column index for 'Metric'
        borderRight = "1px solid gray"  # Add border after 'Metric'
      ) %>%
      formatStyle(
        columns = which(colnames(data_colored) == "Above Average"),  # Column index for 'Above Average'
        borderRight = "1px solid gray"  # Add border after 'Above Average'
      ) %>%
      formatStyle(
        columns = setdiff(names(data_colored), "Metric"),  # Apply to all columns except 'Metric'
        textAlign = "center"  # Center-align text
      ) %>%
      formatStyle(
        columns = names(data_colored),
        `text-align` = "center",
        `vertical-align` = "middle"
      )
  })
  
  ## LONGITUDINAL METRIC DROPDOWNS ##  
  # Update the dropdown choices for the metric selection based on the column names of new_assessment
  # 10-5 Test
  observe({
    metric_columns <- c("10-5 RSI", "10-5 AIR/CONTACT", 
                        "10-5 JUMP HEIGHT (INCH)", 
                        "10-5 FLIGHT TIME (S)", 
                        "10-5 CONTACT TIME (S)")
    updateSelectInput(session, "selected10Metric",
                      choices = metric_columns,
                      selected = metric_columns[1]) # Default to the first metric
  })
  
  # Bilateral CMJ
  observe({
    bilateral_columns <- c("BILATERAL CMJ JUMP HEIGHT (INCH)",
                           "BILATERAL CMJ FLIGHT TIME (S)",
                           "BILATERAL CMJ TAKEOFF VELOCITY (M/S)", 
                           "BILATERAL CMJ TOTAL WORK (J)")
    updateSelectInput(session, "selectedCMJMetric",
                      choices = bilateral_columns,
                      selected = bilateral_columns[1])
  })
  # Upper Body Power
  observe({
    upper_columns <- c("MED BALL - FREE MODE - DOWNWARD SLAM PEAK VELOCITY (M/S)",               
                       "MED BALL - FREE MODE - DOWNWARD SLAM PEAK POWER (W)",                    
                       "MED BALL - FREE MODE - DOWNWARD SLAM PEAK ACCELERATION (G)")
    updateSelectInput(session, "selectedUpperMetric",
                      choices = upper_columns,
                      selected = upper_columns[1])
  })
  
  ####**** NO ROTATIONAL TEST HERE BECAUSE WE WILL CREATE A L/R DATAFRAME LATER ON *****####
  
  
  # Reactive expression to filter data based on player and event selection
  player_data <- reactive({
    req(input$playerName, input$eventId)
    filter(new_assessment, `FULL NAME` == input$playerName & `EVENT ID` == input$eventId)
  })
  
  # Reactive expression to filter data based on player selection only
  player_data_all_events <- reactive({
    req(input$playerName)
    filter(new_assessment, `FULL NAME` == input$playerName)
  })
  
  # Output reactive elements based on selected player and event
  # player details section
  output$ageGroup <- renderText({
    req(player_data())
    paste("Age Group: ", player_data()$`AGE GROUP`)
  })
  
  output$position <- renderText({
    req(player_data())
    paste("Position: ", player_data()$`PRIMARY POSITION`)
  })
  
  output$secPosition <- renderText({
    req(player_data())
    paste("Secondary Position: ", player_data()$`SECONDARY POSITION`)
  })
  
  output$batsThrows <- renderText({
    req(player_data())
    paste("Bats/Throws: ", player_data()$`BATS/THROWS`)
  })
  
  output$movementScore <- renderText({
    req(player_data())
    paste("Movement Score: ", round(player_data()$`MOVEMENT SCORE`, 2))
  })
  
  output$lowerBodyPowerScore <- renderText({
    req(player_data())
    paste("Lower Body Power Score: ", round(player_data()$`LOWER BODY POWER SCORE`, 2))
  })
  
  output$upperBodyPowerScore <- renderText({
    req(player_data())
    paste("Upper Body Power Score: ", round(player_data()$`UPPER BODY POWER SCORE`, 2))
  })
  
  output$rotationalPowerScore <- renderText({
    req(player_data())
    paste("Rotational Power Score: ", round(player_data()$`ROTATIONAL POWER SCORE`, 2))
  })
  
  output$overallScore <- renderText({
    req(player_data())
    paste("Overall Score (CATS): ", round(player_data()$`OVERALL SCORE (CATS)`, 2))
  })
  
  # Render gauges for assessment scores
  output$movementGauge <- renderGauge({
    req(player_data())
    gauge(round(player_data()$`MOVEMENT SCORE`, 2), min = 0, max = 100, symbol = '%', 
          gaugeSectors(success = c(75, 100), warning = c(50, 74), danger = c(0, 49)))
  })
  
  output$lowerBodyPowerGauge <- renderGauge({
    req(player_data())
    gauge(round(player_data()$`LOWER BODY POWER SCORE`, 2), min = 0, max = 100, symbol = '%', 
          gaugeSectors(success = c(75, 100), warning = c(50, 74), danger = c(0, 49)))
  })
  
  output$upperBodyPowerGauge <- renderGauge({
    req(player_data())
    gauge(round(player_data()$`UPPER BODY POWER SCORE`, 2), min = 0, max = 100, symbol = '%', 
          gaugeSectors(success = c(75, 100), warning = c(50, 74), danger = c(0, 49)))
  })
  
  output$rotationalPowerGauge <- renderGauge({
    req(player_data())
    gauge(round(player_data()$`ROTATIONAL POWER SCORE`, 2), min = 0, max = 100, symbol = '%', 
          gaugeSectors(success = c(75, 100), warning = c(50, 74), danger = c(0, 49)))
  })
  
  output$overallGauge <- renderGauge({
    req(player_data())
    gauge(round(player_data()$`OVERALL SCORE (CATS)`, 2), min = 0, max = 100, symbol = '%', 
          gaugeSectors(success = c(75, 100), warning = c(50, 74), danger = c(0, 49)))
  })
  
  # Output labels for gauges
  output$movementLabel <- renderText({ "Movement Score" })
  output$lowerBodyPowerLabel <- renderText({ "Lower Body Power Score" })
  output$upperBodyPowerLabel <- renderText({ "Upper Body Power Score" })
  output$rotationalPowerLabel <- renderText({ "Rotational Power Score" })
  
  # Category scores Summary Bar Chart
  output$categoriesBarChart <- renderPlot({
    req(player_data())
    
    # Create a data frame for the bar chart
    categories_data <- data.frame(
      Category = factor(c("Movement Score", "Lower Body Power Score", "Upper Body Power Score", "Rotational Power Score", "Overall Score (CATS)"),
                        levels = c("Movement Score", "Lower Body Power Score", "Upper Body Power Score", "Rotational Power Score", "Overall Score (CATS)")),
      
      Score = c(
        round(player_data()$`MOVEMENT SCORE`, 2),
        round(player_data()$`LOWER BODY POWER SCORE`, 2),
        round(player_data()$`UPPER BODY POWER SCORE`, 2),
        round(player_data()$`ROTATIONAL POWER SCORE`, 2),
        round(player_data()$`OVERALL SCORE (CATS)`, 2)
      )
    )
    
    # Plot the categories bar chart
    ggplot(categories_data, aes(x = Category, y = Score, fill = Category)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label = Score), vjust = -0.5, color = "black", size = 3.5) + # add labels on top of bars
      ylim(0,100) + 
      labs(title = "Summary of Assessment Scores", x = "Category", y = "Score") + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10), 
            axis.title.x = element_text(vjust = 0.5), # adjusting the title position
            legend.position = "none") + 
      scale_fill_manual(
        values = c(
          "Movement Score" = "black",
          "Lower Body Power Score" = "black",
          "Upper Body Power Score" = "black",
          "Rotational Power Score" = "black",
          "Overall Score (CATS)" = "steelblue"
        )
      ) + 
      geom_hline(yintercept = 75, linetype = "solid", color = "darkgreen", size = 1) + 
      geom_hline(yintercept = 50, linetype = "dashed", color = "royalblue4", size = 1) + 
      geom_hline(yintercept = 25, linetype = "solid", color = "darkred", size = 1)
  })
  
  ## LONGITUDINAL BAR GRAPHS  
  # 10-5 data
  
  # Output for 10-5 metrics table
  output$ten5MetricsTable <- renderTable({
    req(player_data_all_events())
    
    # Define the 10-5 metrics you want to include
    ten_five_metrics <- c(
      "10-5 AIR/CONTACT",
      "10-5 CONTACT TIME (S)",
      "10-5 FLIGHT TIME (S)",
      "10-5 JUMP HEIGHT (INCH)",
      "10-5 RSI"
    )
    
    # Extract and filter data for the specified 10-5 metrics
    data <- player_data_all_events() %>%
      select(`EVENT ID`, all_of(ten_five_metrics)) %>%
      pivot_longer(cols = -`EVENT ID`, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = `EVENT ID`, values_from = Value) %>%
      arrange(Metric)
    
    return(data)
  })
  
  # Render 10-5 Bar Graph
  output$longitudinalBarGraph <- renderPlot({
    req(input$selected10Metric, input$playerName)
    
    # Filter the data based on the selected player
    player_data <- filter(new_assessment, `FULL NAME` == input$playerName)
    
    # Calculate percentiles
    percentiles <- percentiles_data()
    names(percentiles) <- gsub("X25th.Percentile", "25th Percentile", names(percentiles))
    names(percentiles) <- gsub("X75th.Percentile", "75th Percentile", names(percentiles))
    
    # Debug output
    cat("Percentiles Data:\n")
    cat(str(percentiles), "\n")
    cat("Selected Metric:\n")
    cat(input$selected10Metric, "\n")
    
    # Extract percentile values with correct column names
    p25 <- percentiles$`25th Percentile`[percentiles$Metric == input$selected10Metric]
    p75 <- percentiles$`75th Percentile`[percentiles$Metric == input$selected10Metric]
    
    # Debug percentile values
    cat("p25:\n")
    cat(p25, "\n")
    cat("p75:\n")
    cat(p75, "\n")
    
    # Check if percentile values are empty and provide defaults if necessary
    p25 <- ifelse(length(p25) > 0, as.numeric(p25), NA)
    p75 <- ifelse(length(p75) > 0, as.numeric(p75), NA)
    
    # Default values if percentiles are NA
    if (is.na(p25)) p25 <- min(player_data[[input$selected10Metric]], na.rm = TRUE)
    if (is.na(p75)) p75 <- max(player_data[[input$selected10Metric]], na.rm = TRUE)
    
    ggplot(player_data, aes(x = `EVENT ID`, y = !!sym(input$selected10Metric))) +
      geom_bar(stat = "identity", fill = "black") +
      geom_hline(yintercept = p25, color = "darkred", linetype = "dashed") +
      geom_hline(yintercept = p75, color = "darkgreen", linetype = "dashed") +
      labs(title = paste0(input$selected10Metric, " Longitudinal Graph"), x = "Event ID", y = input$selected10Metric) +
      theme_minimal()
  })
  
  
  # Bilateral CMJ Data
  # Output for CMJ metrics table
  output$cmjMetricsTable <- renderTable({
    req(player_data_all_events())
    
    # Define the 10-5 metrics you want to include
    cmj_metrics <- c(
      "BILATERAL CMJ JUMP HEIGHT (INCH)",
      "BILATERAL CMJ FLIGHT TIME (S)",
      "BILATERAL CMJ TAKEOFF VELOCITY (M/S)",
      "BILATERAL CMJ TOTAL WORK (J)"    
    )
    
    # Extract and filter data for the specified 10-5 metrics
    data <- player_data_all_events() %>%
      select(`EVENT ID`, all_of(cmj_metrics)) %>%
      pivot_longer(cols = -`EVENT ID`, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = `EVENT ID`, values_from = Value) %>%
      arrange(Metric)
    
    return(data)
  })
  
  # CMJ Bar graph output
  output$cmjBarGraph <- renderPlot({
    req(input$playerName, input$selectedCMJMetric)
    
    # Filter the data based on the selected player
    player_data <- filter(new_assessment, `FULL NAME` == input$playerName)
    
    # Calculate percentiles
    percentiles <- percentiles_data()
    names(percentiles) <- gsub("X25th.Percentile", "25th Percentile", names(percentiles))
    names(percentiles) <- gsub("X75th.Percentile", "75th Percentile", names(percentiles))
    
    # Debug output
    cat("Percentiles Data:\n")
    cat(str(percentiles), "\n")
    cat("Selected Metric:\n")
    cat(input$selected10Metric, "\n")
    
    # Extract percentile values with correct column names
    p25 <- percentiles$`25th Percentile`[percentiles$Metric == input$selectedCMJMetric]
    p75 <- percentiles$`75th Percentile`[percentiles$Metric == input$selectedCMJMetric]
    
    # Debug percentile values
    cat("p25:\n")
    cat(p25, "\n")
    cat("p75:\n")
    cat(p75, "\n")
    
    # Check if percentile values are empty and provide defaults if necessary
    p25 <- ifelse(length(p25) > 0, as.numeric(p25), NA)
    p75 <- ifelse(length(p75) > 0, as.numeric(p75), NA)
    
    # Default values if percentiles are NA
    if (is.na(p25)) p25 <- min(player_data[[input$selectedCMJMetric]], na.rm = TRUE)
    if (is.na(p75)) p75 <- max(player_data[[input$selectedCMJMetric]], na.rm = TRUE)
    
    ggplot(player_data, aes(x = `EVENT ID`, y = !!sym(input$selectedCMJMetric))) +
      geom_bar(stat = "identity", fill = "black") +
      geom_hline(yintercept = p25, color = "darkred", linetype = "dashed") +
      geom_hline(yintercept = p75, color = "darkgreen", linetype = "dashed") +
      labs(title = paste0(input$selected10Metric, " Longitudinal Graph"), x = "Event ID", y = input$selectedCMJMetric) +
      theme_minimal()
  })
  
  # Upper Body Test Data
  
  # Output for Upper Body metrics table
  output$upperMetricsTable <- renderTable({
    req(player_data_all_events())
    
    # Define the upper body metrics you want to include
    upper_metrics <- c(
      "MED BALL - FREE MODE - DOWNWARD SLAM PEAK VELOCITY (M/S)",
      "MED BALL - FREE MODE - DOWNWARD SLAM PEAK POWER (W)",                    
      "MED BALL - FREE MODE - DOWNWARD SLAM PEAK ACCELERATION (G)"    
    )
    
    # Extract and filter data for the specified 10-5 metrics
    data <- player_data_all_events() %>%
      select(`EVENT ID`, all_of(upper_metrics)) %>%
      pivot_longer(cols = -`EVENT ID`, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = `EVENT ID`, values_from = Value) %>%
      arrange(Metric)
    
    return(data)
  })
  
  # Graph output upper body
  output$upperBarGraph <- renderPlot({
    req(input$playerName, input$selectedUpperMetric)
    
    # Extract the values for selected metric
    player_data <- filter(new_assessment, `FULL NAME` == input$playerName)
    
    
    # Calculate percentiles
    percentiles <- percentiles_data()
    names(percentiles) <- gsub("X25th.Percentile", "25th Percentile", names(percentiles))
    names(percentiles) <- gsub("X75th.Percentile", "75th Percentile", names(percentiles))
    
    # Debug output
    cat("Percentiles Data:\n")
    cat(str(percentiles), "\n")
    cat("Selected Metric:\n")
    cat(input$selectedUpperMetric, "\n")
    
    # Extract percentile values with correct column names
    p25 <- percentiles$`25th Percentile`[percentiles$Metric == input$selectedUpperMetric]
    p75 <- percentiles$`75th Percentile`[percentiles$Metric == input$selectedUpperMetric]
    
    # Debug percentile values
    cat("p25:\n")
    cat(p25, "\n")
    cat("p75:\n")
    cat(p75, "\n")
    
    # Check if percentile values are empty and provide defaults if necessary
    p25 <- ifelse(length(p25) > 0, as.numeric(p25), NA)
    p75 <- ifelse(length(p75) > 0, as.numeric(p75), NA)
    
    # Default values if percentiles are NA
    if (is.na(p25)) p25 <- min(player_data[[input$selectedUpperMetric]], na.rm = TRUE)
    if (is.na(p75)) p75 <- max(player_data[[input$selectedUpperMetric]], na.rm = TRUE)
    
    ggplot(player_data, aes(x = `EVENT ID`, y = !!sym(input$selectedUpperMetric))) +
      geom_bar(stat = "identity", fill = "black") +
      geom_hline(yintercept = p25, color = "darkred", linetype = "dashed") +
      geom_hline(yintercept = p75, color = "darkgreen", linetype = "dashed") +
      labs(title = paste0(input$selectedUpperMetric, " Longitudinal Graph"), x = "Event ID", y = input$selectedUpperMetric) +
      theme_minimal()
  })
  
  # Rotational Test Data
  
  # Output for Rota metrics table
  output$rotaMetricsTable <- renderTable({
    req(player_data_all_events())
    
    # Define the rota metrics you want to include
    rota_metrics <- c(
      "MED BALL - HITTER STYLE SHOT PUT PEAK VELOCITY (M/S) LEFT",
      "MED BALL - HITTER STYLE SHOT PUT PEAK VELOCITY (M/S) RIGHT",
      "MED BALL - HITTER STYLE SHOT PUT PEAK POWER (W) LEFT",
      "MED BALL - HITTER STYLE SHOT PUT PEAK POWER (W) RIGHT",
      "MED BALL - HITTER STYLE SHOT PUT PEAK ACCELERATION (G) LEFT",            
      "MED BALL - HITTER STYLE SHOT PUT PEAK ACCELERATION (G) RIGHT"
    )
    
    # Extract and filter data for the specified rota metrics
    data <- player_data_all_events() %>%
      select(`EVENT ID`, all_of(rota_metrics)) %>%
      pivot_longer(cols = -`EVENT ID`, names_to = "Metric", values_to = "Value") %>%
      pivot_wider(names_from = `EVENT ID`, values_from = Value) %>%
      arrange(Metric)
    
    return(data)
  })
  
  # Rota Bar Graph
  output$rotaBarGraph <- renderPlot({
    req(player_data_all_events(), input$selectedRotaMetric)
    
    # Define the full metric names
    rota_left_metric <- paste0(input$selectedRotaMetric, " LEFT")
    rota_right_metric <- paste0(input$selectedRotaMetric, " RIGHT")
    
    # Check if the columns exist in the data
    if (!(rota_left_metric %in% colnames(new_assessment)) || !(rota_right_metric %in% colnames(new_assessment))) {
      stop("Selected columns for rotational metrics do not exist in the data.")
    }
    
    # Extract the values for the selected metric
    rota_metric_values_left <- player_data_all_events()[[rota_left_metric]]
    rota_metric_values_right <- player_data_all_events()[[rota_right_metric]]
    
    # Handle NA values
    rota_metric_values_left[is.na(rota_metric_values_left)] <- 0
    rota_metric_values_right[is.na(rota_metric_values_right)] <- 0
    
    # Create a combined df for plotting
    plot_data <- player_data_all_events() %>%
      select(`EVENT ID`) %>%
      mutate(
        `Left` = rota_metric_values_left,
        `Right` = rota_metric_values_right
      ) %>%
      pivot_longer(cols = c("Left", "Right"), names_to = "Side", values_to = "Value")
    
    # Calculate percentiles
    percentiles <- percentiles_data()
    names(percentiles) <- gsub("X25th.Percentile", "25th Percentile", names(percentiles))
    names(percentiles) <- gsub("X75th.Percentile", "75th Percentile", names(percentiles))
    
    p25 <- percentiles$`25th Percentile`[percentiles$Metric == paste0(input$selectedRotaMetric)]
    p75 <- percentiles$`75th Percentile`[percentiles$Metric == paste0(input$selectedRotaMetric)]
    
    # Debugging output
    cat("ROTA Percentiles Data:\n")
    cat(str(percentiles), "\n")
    cat("Selected Metric:\n")
    cat(input$selectedRotaMetric, "\n")
    cat("p25:\n")
    
    # Default values if percentiles are NA
    if (is.na(p25)) p25 <- min(c(rota_metric_values_left, rota_metric_values_right), na.rm = TRUE)
    if (is.na(p75)) p75 <- max(c(rota_metric_values_left, rota_metric_values_right), na.rm = TRUE)
    
    ggplot(plot_data, aes(x = `EVENT ID`, y = Value, fill = Side)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = Value), vjust = -0.5, color = "black", size = 3.5, position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = c("Left" = "black", "Right" = "steelblue")) +
      geom_hline(yintercept = p25, color = "darkred", linetype = "dashed") +
      geom_hline(yintercept = p75, color = "darkgreen", linetype = "dashed") +
      labs(x = "Event ID", y = input$selectedRotaMetric, title = paste("Longitudinal", input$selectedRotaMetric)) +
      theme_minimal()
  })
}

####### RUN THE APP #######
# Run the application 
shinyApp(ui = ui, server = server)
