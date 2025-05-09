# ========================================================================
# ALPB Analytics Platform (Count-based Widget)
# ========================================================================

# ----------------------------------------
# Library Imports
# ----------------------------------------
library(shiny)
library(shinyjs) 
library(dplyr)
library(ggplot2)
library(tidyr)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(RColorBrewer)
library(shinythemes)
library(knitr)
library(rmarkdown)
library(plotly)

# ----------------------------------------
# API Configuration
# ----------------------------------------
# Load configuration
if (file.exists("config.R")) {
  source("config.R")
} else {
  stop("Configuration file missing. Please create config.R based on config.dummy.R")
}

# ----------------------------------------
# Team Colors Lookup Table
# ----------------------------------------
team_colors <- list(
  "Lancaster Stormers" = "#EE1C25",
  "York Revolution" = "#C4122E",
  "Long Island Ducks" = "#EE3124",
  "Southern Maryland Blue Crabs" = "#1B3668",
  "High Point Rockers" = "#8B1F41",
  "Gastonia Honey Hunters" = "#D8B344",
  "Lexington Legends" = "#003366",
  "Charleston Dirty Birds" = "#000000",
  "Staten Island FerryHawks" = "#FF6600",
  "Hagerstown Flying Boxcars" = "#DD3333"
)

# ----------------------------------------
# Helper Functions
# ----------------------------------------

# Function to get count color for visualization
get_count_color <- function(count) {
  strikes <- as.numeric(strsplit(count, "-")[[1]][2])
  if(strikes == 0) return("#90EE90") # Light green
  if(strikes == 1) return("#FFD700") # Yellow
  if(strikes == 2) return("#FF6347") # Red
  return("#FFFFFF") # Default white
}

# ----------------------------------------
# API Data Retrieval Functions
# ---------------------------------------

# Function to fetch player details from API
fetch_player_details <- function(player_name) {
  response <- GET(
    paste0(API_BASE_URL, "/players"),
    add_headers("x-api-key" = API_KEY),
    query = list(player_name = player_name)
  )
  
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    if (length(content$data) > 0) {
      # Filter to keep only players who are either hitters or pitchers
      valid_players <- content$data %>% 
        filter(is_hitter | is_pitcher)
      
      return(valid_players)
    }
  }
  
  return(NULL)
}

# Function to fetch pitch data for players with date filtering
fetch_pitch_data <- function(player_ids = NULL, is_hitter = FALSE, is_pitcher = FALSE, 
                             start_date = NULL, end_date = NULL) {
  # Initialize empty dataframe for results
  all_pitch_data <- data.frame()
  
  # Handle case where we have multiple player IDs 
  if (is.character(player_ids) && length(player_ids) > 0) {
    # Fetch data for each player ID and combine
    for (player_id in player_ids) {
      query_params <- list(
        limit = 1000,
        page = 1
      )
      
      # Add date range parameters if provided
      if (!is.null(start_date)) {
        query_params$start_date <- format(start_date, "%Y-%m-%d")
      }
      if (!is.null(end_date)) {
        query_params$end_date <- format(end_date, "%Y-%m-%d")
      }
      
      # Adjust query based on player type
      if (is_hitter) {
        query_params$batter_id <- player_id
      } else if (is_pitcher) {
        query_params$pitcher_id <- player_id
      }
      
      response <- GET(
        paste0(API_BASE_URL, "/pitches"),
        add_headers("x-api-key" = API_KEY),
        query = query_params
      )
      
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content))
        if (length(content$data) > 0 && nrow(content$data) > 0) {
          if (!is.null(start_date) || !is.null(end_date)) {
            filtered_data <- content$data
            
            if (!is.null(start_date)) {
              filtered_data <- filtered_data[as.Date(filtered_data$date) >= start_date, ]
            }
            
            if (!is.null(end_date)) {
              filtered_data <- filtered_data[as.Date(filtered_data$date) <= end_date, ]
            }
            
            all_pitch_data <- rbind(all_pitch_data, filtered_data)
          } else {
            all_pitch_data <- rbind(all_pitch_data, content$data)
          }
        }
      }
    }
    
    if (nrow(all_pitch_data) > 0) {
      return(all_pitch_data)
    }
  }
  
  return(NULL)
}

# Function to validate ALPB tokens
validate_alpb_token <- function(token) {
  if (is.null(token) || token == "") {
    message("Token validation failed: Empty token")
    return(FALSE)
  }
  
  # Prepare request to validate token
  tryCatch({
    response <- POST(
      url = paste0(API_BASE_URL, "/validate-token"),
      add_headers("x-api-key" = API_KEY),
      body = list(
        alpb_token = token,
        widget_id = WIDGET_ID
      ),
      encode = "json",
      timeout(10)  
    )
    
    if (status_code(response) == 200) {
      content <- fromJSON(rawToChar(response$content))
      return(content$valid == TRUE)
    } else {
      message(paste("Token validation failed: HTTP status", status_code(response)))
      return(FALSE)
    }
  }, error = function(e) {
    message(paste("Token validation error:", e$message))
    return(FALSE)
  })
}

# ----------------------------------------
# Data Processing Functions
# ----------------------------------------

# Process raw pitch data for analysis
process_pitch_data <- function(df, is_hitter) {
  if (is.null(df)) return(NULL)
  if (nrow(df) == 0) return(NULL)
  
  df_filtered <- df %>%
    mutate(
      Count = paste0(balls, "-", strikes),
      PitchCall = coalesce(pitch_call, "Undefined"),
      PlayResult = coalesce(play_result, "Undefined"),
      k_or_bb = coalesce(k_or_bb, "Undefined"), 
      date = as.Date(date)  
    )
  
  if (is_hitter) {
    df_filtered <- df_filtered %>%
      select(
        BatterId = batter_id, 
        PitcherId = pitcher_id, 
        AutoPitchType = auto_pitch_type, 
        TaggedHitType = tagged_hit_type,
        Count, 
        PitchCall, 
        PlayResult,
        k_or_bb, 
        date    
      )
  } else {
    df_filtered <- df_filtered %>%
      select(
        PitcherId = pitcher_id, 
        AutoPitchType = auto_pitch_type, 
        Count, 
        PitchCall, 
        PlayResult,
        k_or_bb,  
        date      
      )
  }
  
  return(df_filtered)
}

# Helper function to group players with same name
group_players_by_name <- function(players) {
  if (is.null(players) || nrow(players) == 0) {
    return(NULL)
  }
  
  # Group players by name
  grouped_players <- players %>%
    group_by(player_name) %>%
    summarise(
      player_ids = list(player_id),
      player_pitching_handedness = first(player_pitching_handedness),
      player_batting_handedness = first(player_batting_handedness),
      is_hitter = any(is_hitter),
      is_pitcher = any(is_pitcher),
      team_name = paste(unique(team_name), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(
      display_name = paste0(
        player_name, 
        " (", 
        ifelse(is_hitter, "Hitter", ""), 
        ifelse(is_hitter & is_pitcher, "/", ""), 
        ifelse(is_pitcher, "Pitcher", ""),
        ")"
      )
    )
  
  return(grouped_players)
}

# ----------------------------------------
# Metric Calculation Functions - Hitting
# ----------------------------------------

# Calculate hitting metrics using k_or_bb field
calculate_hitting_metrics <- function(df, count = NULL) {
  # If count is specified, filter the data
  if (!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  # Calculate at-bats, plate appearances, and hits
  metrics <- df %>% summarise(
    # Plate appearances - count terminal events
    PA = sum(k_or_bb == "Strikeout" | k_or_bb == "Walk" | PitchCall == "InPlay" | PitchCall == "HitByPitch"),
    
    # At-bats - plate appearances minus walks and HBP
    AB = sum((k_or_bb == "Strikeout") | PitchCall == "InPlay"),
    
    # Hit outcomes
    Hits = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
    Singles = sum(PlayResult == "Single"),
    Doubles = sum(PlayResult == "Double"),
    Triples = sum(PlayResult == "Triple"),
    HomeRuns = sum(PlayResult == "HomeRun"),
    
    # On-base events
    Walks = sum(k_or_bb == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    
    # Calculate total bases
    TB = Singles + (2 * Doubles) + (3 * Triples) + (4 * HomeRuns)
  )
  
  # Calculate the metrics
  metrics <- metrics %>% mutate(
    BA = ifelse(AB > 0, round(Hits / AB, 3), 0),
    OBP = ifelse(PA > 0, round((Hits + Walks + HBP) / PA, 3), 0),
    SLG = ifelse(AB > 0, round(TB / AB, 3), 0),
    OPS = BA + SLG,
    
    # Calculate wOBA using standard weights
    wOBA = ifelse(PA > 0, 
                  round((0.72 * Walks + 0.75 * HBP + 
                           0.9 * Singles + 1.24 * Doubles + 
                           1.56 * Triples + 1.95 * HomeRuns) / PA, 3),
                  0)
  )
  
  return(metrics)
}

# Calculate metrics by date for trend analysis
calculate_metrics_by_date <- function(df) {
  # Group data by date
  df %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(
      TotalPitches = n(),
      ContactRate = sum(PitchCall == "InPlay") / TotalPitches * 100,
      StrikeRate = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallFieldable", "FoulBallNotFieldable")) / TotalPitches * 100,
      BallRate = sum(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch")) / TotalPitches * 100,
      .groups = 'drop'
    ) %>%
    arrange(date)
}

# Calculate contact outcome distribution
calculate_contact_outcomes <- function(df) {
  df %>%
    group_by(Count) %>%
    summarise(
      TotalPitches = n(),
      TotalInFieldContact = sum(PitchCall == "InPlay"),
      Single = sum(PlayResult == "Single"),
      Double = sum(PlayResult == "Double"),
      Triple = sum(PlayResult == "Triple"),
      HomeRun = sum(PlayResult == "HomeRun"),
      Sacrifice = sum(PlayResult == "Sacrifice"),
      Error = sum(PlayResult == "Error"),
      GroundOut = sum(PlayResult == "Out" & (TaggedHitType == "GroundBall")),
      FlyOut = sum(PlayResult == "Out" & (TaggedHitType == "FlyBall")),
      LineOut = sum(PlayResult == "Out" & (TaggedHitType == "LineDrive")),
      PopupOut = sum(PlayResult == "Out" & (TaggedHitType == "Popup"))
    ) %>%
    mutate(
      SingleRate = ifelse(TotalInFieldContact > 0, (Single / TotalInFieldContact) * 100, 0),
      DoubleRate = ifelse(TotalInFieldContact > 0, (Double / TotalInFieldContact) * 100, 0),
      TripleRate = ifelse(TotalInFieldContact > 0, (Triple / TotalInFieldContact) * 100, 0),
      HomeRunRate = ifelse(TotalInFieldContact > 0, (HomeRun / TotalInFieldContact) * 100, 0),
      SacrificeRate = ifelse(TotalInFieldContact > 0, (Sacrifice / TotalInFieldContact) * 100, 0),
      ErrorRate = ifelse(TotalInFieldContact > 0, (Error / TotalInFieldContact) * 100, 0),
      GroundOutRate = ifelse(TotalInFieldContact > 0, (GroundOut / TotalInFieldContact) * 100, 0),
      FlyOutRate = ifelse(TotalInFieldContact > 0, (FlyOut / TotalInFieldContact) * 100, 0),
      LineOutRate = ifelse(TotalInFieldContact > 0, (LineOut / TotalInFieldContact) * 100, 0),
      PopupOutRate = ifelse(TotalInFieldContact > 0, (PopupOut / TotalInFieldContact) * 100, 0), 
      OutRate = GroundOutRate + LineOutRate + LineOutRate + PopupOutRate,
    )
}

# ----------------------------------------
# Metric Calculation Functions - Pitching
# ----------------------------------------

# Calculate pitcher metrics across all counts or a specific count
calculate_pitcher_metrics <- function(df, count = NULL) {
  # If count is specified, filter the data
  if (!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  # Calculate pitcher stats
  metrics <- df %>% summarise(
    TotalPitches = n(),
    Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallFieldable","FoulBallNotFieldable")),
    Balls = sum(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch")),
    StrikePercentage = round(Strikes / TotalPitches * 100, 1),
    ContactPitches = sum(PitchCall == "InPlay"),
    InPlayRate = round(ContactPitches / TotalPitches * 100, 1),
    SwingingStrikes = sum(PitchCall == "StrikeSwinging"),
    SwingingStrikeRate = round(SwingingStrikes / TotalPitches * 100, 1),
    CalledStrikes = sum(PitchCall == "StrikeCalled"),
    CalledStrikeRate = round(CalledStrikes / TotalPitches * 100, 1),
    FoulBalls = sum(PitchCall %in% c("FoulBallFieldable","FoulBallNotFieldable")),
    FoulRate = round(FoulBalls / TotalPitches * 100, 1),
    BallPercentage = round(Balls / TotalPitches * 100, 1),
    BallsInPlay = sum(!is.na(PlayResult) & PlayResult != "Undefined"),
  )
  
  return(metrics)
}

# Calculate pitcher metrics by date for trend analysis
calculate_pitcher_metrics_by_date <- function(df) {
  # Group data by date
  df %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(
      TotalPitches = n(),
      StrikePercentage = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallFieldable", "FoulBallNotFieldable")) / TotalPitches * 100,
      BallPercentage = sum(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch")) / TotalPitches * 100,
      InPlayRate = sum(PitchCall == "InPlay") / TotalPitches * 100,  # Added this line
      SwingingStrikeRate = sum(PitchCall == "StrikeSwinging") / TotalPitches * 100,
      CalledStrikeRate = sum(PitchCall == "StrikeCalled") / TotalPitches * 100,
      .groups = 'drop'
    ) %>%
    arrange(date)
}

# ----------------------------------------
# Pitch Type Analysis Functions
# ----------------------------------------

# Calculate Pitch Type Performance for Pitchers
calculate_pitch_type_performance <- function(df) {
  df %>%
    group_by(AutoPitchType, PitchCall) %>%
    summarise(
      TotalPitches = n(),
      .groups = 'drop_last'
    ) %>%
    mutate(
      Percentage = TotalPitches / sum(TotalPitches) * 100
    ) %>%
    # Common pitch types
    filter(AutoPitchType %in% c("Four-Seam", "Curveball", "Slider", "Changeup", "Cutter", "Sinker")) %>%
    select(AutoPitchType, PitchCall, Percentage) %>%
    pivot_wider(
      names_from = PitchCall, 
      values_from = Percentage,
      values_fill = 0
    )
}

# Calculate pitch type distribution across all counts or a specific count
calculate_pitch_type_distribution <- function(df, count = NULL) {
  # If count is specified, filter the data
  if (!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  df %>%
    group_by(AutoPitchType) %>%
    summarise(
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      Percentage = round(Count / sum(Count) * 100, 1)
    ) %>%
    filter(!is.na(AutoPitchType) & AutoPitchType != "")
}


# ========================================================================
# UI Definition
# ========================================================================

# UI Definition
ui <- function(request) {
  # Extract token from URL query parameters
  query <- parseQueryString(request$QUERY_STRING)
  token <- query$alpb_token
  
  # Add debug logging
  message("URL query string: ", request$QUERY_STRING)
  message("Extracted token: ", token)
  
  # Developer mode flag - set to TRUE during development, FALSE in production
  developer_mode <- TRUE
  
  # Validate token (or bypass if in developer mode)
  is_valid <- developer_mode || validate_alpb_token(token)
  message("Token validation result: ", is_valid)
  
  if (!is_valid) {
    # Show unauthorized UI
    fluidPage(
      shinyjs::useShinyjs(),
      theme = shinythemes::shinytheme("flatly"),
      tags$head(
        tags$style(HTML("
          @import url('https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700&display=swap');
          
          body {
            font-family: 'Nunito', sans-serif;
            background-color: #f8f9fa;
            display: flex;
            justify-content: center;
            align-items: center;
            height: 100vh;
          }
          
          .unauthorized-container {
            text-align: center;
            padding: 30px;
            max-width: 600px;
            background: white;
            border-radius: 8px;
            box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          }
          
          .title {
            color: #003366;
            margin-bottom: 20px;
          }
          
          .error-icon {
            font-size: 64px;
            color: #DC143C;
            margin-bottom: 20px;
          }
          
          .message {
            margin-bottom: 20px;
            font-size: 18px;
            color: #555;
          }
          
          .alpb-link {
            display: inline-block;
            margin-top: 20px;
            padding: 10px 20px;
            background-color: #003366;
            color: white;
            text-decoration: none;
            border-radius: 5px;
            font-weight: bold;
          }
          
          .alpb-link:hover {
            background-color: #004080;
          }
        "))
      ),
      
      div(class = "unauthorized-container",
          div(class = "error-icon", icon("lock")),
          h2(class = "title", "Access Restricted"),
          div(class = "message", "This application can only be accessed through the ALPB Analytics platform."),
          div(class = "message", "Please log in to ALPB Analytics to use this widget."),
          a(href = "https://alpb-analytics.com", class = "alpb-link", "Go to ALPB Analytics")
      )
    )
  } else {
    # Show the authorized UI - this is your current UI with the auth_panel removed
    fluidPage(
      # Apply theme and add custom CSS
      theme = shinythemes::shinytheme("flatly"),
      tags$head(
        tags$style(HTML("
          @import url('https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700&display=swap');
          
          body {
            font-family: 'Nunito', sans-serif;
            background-color: #f8f9fa;
          }
          
          .title {
            background-color: #003366;
            color: white;
            padding: 15px;
            margin-bottom: 20px;
            border-radius: 5px;
          }
          
          .section-header {
            background: linear-gradient(to right, #DC143C, #FFA500);
            color: white;
            padding: 10px 15px;
            border-radius: 5px;
            margin-bottom: 15px;
            font-family: 'Nunito', sans-serif;
            font-weight: 700;
          }
          
          .search-button, .fetch-button {
            margin-top: 20px;
            margin-bottom: 20px;
            width: 100%;
            height: 45px;
            font-weight: bold;
            border-radius: 5px;
          }
          
          .search-button {
            background-color: #007bff;
            color: white;
          }
          
          .fetch-button {
            background-color: #28a745;
            color: white;
          }
          
          .count-button {
            margin-top: 10px;
            border-radius: 5px;
            font-weight: bold;
            color: black;
          }
          
          .table-styled {
            width: 100%;
            border-collapse: collapse;
            margin-bottom: 20px;
            border: 2px solid #ddd;
          }
          
          .table-styled th {
            background-color: #000;
            color: #fff;
            padding: 10px;
            text-align: center;
            font-family: 'Nunito', sans-serif;
            border: 1px solid #ddd;
          }
          
          .table-styled td {
            padding: 8px;
            text-align: center;
            background-color: #fff;
            color: #555;
            font-family: 'Nunito', sans-serif;
            border: 1px solid #ddd;
          }
          
          .player-info-card {
            border: 1px solid #ddd;
            border-radius: 5px;
            padding: 15px;
            margin-bottom: 20px;
            background-color: white;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          
          .player-info-label {
            font-weight: bold;
            margin-right: 5px;
          }
          
          .handedness-right {
            color: #DC143C;
            font-weight: bold;
          }
          
          .handedness-left {
            color: #1B3668;
            font-weight: bold;
          }
          
          .handedness-na {
            color: #000;
            font-weight: normal;
          }
        "))
      ),
      
      div(class = "title", 
          titlePanel("ALPB Analytics Platform")),
      
      # Main layout
      sidebarLayout(
        sidebarPanel(
          textInput("player_name", "Search Player Name", width = "100%", 
                    placeholder = "Type name"),
          tags$script('
            $(document).ready(function() {
              $("#player_name").on("keyup", function(e) {
                if(e.keyCode == 13) {
                  $("#search").click();
                }
              });
            });
          '),
          # Make the search button distinct
          div(
            actionButton("search", "Search Player", class = "search-button")
          ),
          
          # Only show these after player is selected
          uiOutput("player_selection_ui"),
          
          uiOutput("count_selector"),
          
          # Add option to view all counts data - now for both hitters and pitchers
          uiOutput("view_all_counts_checkbox"),
          
          # Date filter UI will now be rendered after pitch data is fetched
          uiOutput("date_filter_ui")
        ),
        
        mainPanel(
          # Show player info
          uiOutput("player_info_ui"),
          
          # Only show download buttons after player is searched and selected
          uiOutput("download_buttons_ui"),
          
          # Show tabs based on player type
          uiOutput("player_tabs_ui")
        )
      )
    )
  }
}

# ========================================================================
# Server Logic
# ========================================================================

# ----------------------------------------
# Server Function Definition
# ----------------------------------------
server <- function(input, output, session) {
  # JavaScript integration for Enter key press
  shinyjs::runjs('
    $(document).on("keyup", "#player_name", function(e) {
      if(e.keyCode == 13){
        Shiny.setInputValue("keyPressed", 13);
      }
    });
  ')
  
  # ----------------------------------------
  # Key Press Handling
  # ----------------------------------------
  # Observer to trigger search when Enter key is pressed
  observeEvent(input$keyPressed, {
    message("Key pressed: ", input$keyPressed)
    if (input$keyPressed == 13 && !is.null(input$player_name) && input$player_name != "") {
      message("Enter key detected, triggering search for: ", input$player_name)
      shinyjs::click("search")
    }
  }, ignoreNULL = TRUE)
  
  # ----------------------------------------
  # Reactive Values
  # ----------------------------------------
  player_details <- reactiveVal(NULL)
  grouped_players <- reactiveVal(NULL)
  pitch_data <- reactiveVal(NULL)
  selected_player <- reactiveVal(NULL)
  player_date_ranges <- reactiveVal(list())
  original_pitch_data <- reactiveVal(NULL)
  data_date_range <- reactiveVal(c(Sys.Date() - 90, Sys.Date())) # Default initial values
  
  # ----------------------------------------
  # Player Search Logic
  # ----------------------------------------
  # Search for player when button is clicked
  observeEvent(input$search, {
    req(input$player_name)
    
    withProgress(message = 'Searching for player...', {
      details <- fetch_player_details(input$player_name)
      
      if (!is.null(details) && nrow(details) > 0) {
        player_details(details)
        
        # Group players with the same name
        grouped <- group_players_by_name(details)
        
        # Validate critical columns exist and are not null
        if (is.null(grouped$player_ids) || is.null(grouped$is_pitcher)) {
          showNotification("Error: Player information incomplete", type = "error")
          return()
        }
        grouped_players(grouped)
        
        # Select first player by default
        selected_player(grouped[1, ])
        
        # Now immediately fetch data for the first player
        incProgress(0.5, message = "Fetching player data...")
        
        player <- grouped[1, ]
        player_ids <- unlist(player$player_ids)
        
        # IMPORTANT: Always pass NULL for date range to get ALL data
        raw_data <- fetch_pitch_data(player_ids, player$is_hitter, player$is_pitcher, NULL, NULL)
        
        if (!is.null(raw_data) && nrow(raw_data) > 0) {
          # Extract date range from raw data
          min_date <- as.Date(min(raw_data$date, na.rm = TRUE))
          max_date <- as.Date(max(raw_data$date, na.rm = TRUE))
          data_date_range(c(min_date, max_date))
          
          # Process the data
          processed_data <- process_pitch_data(raw_data, player$is_hitter)
          
          # Store both the filtered and original data
          original_pitch_data(processed_data)
          pitch_data(processed_data)
          
          # Update date input to reflect the actual data range
          updateDateRangeInput(session, "date_range",
                               start = min_date,
                               end = max_date,
                               min = min_date,
                               max = max_date)
        } else {
          showNotification("No pitch data found for this player", type = "warning")
          original_pitch_data(NULL)
          pitch_data(NULL)
        }
      } else {
        showNotification("No players found with that name", type = "error")
        player_details(NULL)
        grouped_players(NULL)
        selected_player(NULL)
        original_pitch_data(NULL)
        pitch_data(NULL)
      }
    })
  })
  
  # ----------------------------------------
  # Player Selection Handling
  # ----------------------------------------
  # Player selection UI (only shown if multiple players found)
  output$player_selection_ui <- renderUI({
    req(grouped_players())
    
    players <- grouped_players()
    if (nrow(players) > 1) {
      selectInput("player_select", "Select Player:", 
                  choices = setNames(seq_len(nrow(players)), players$display_name))
    }
  })
  
  # Update selected player when selection changes
  observeEvent(input$player_select, {
    req(grouped_players(), input$player_select)
    player_index <- as.numeric(input$player_select)
    player <- grouped_players()[player_index, ]
    
    # Update the selected player
    selected_player(player)
    
    # Fetch data for the newly selected player
    withProgress(message = 'Fetching player data...', {
      player_ids <- unlist(player$player_ids)
      
      # Always pass NULL for date range to get ALL data
      raw_data <- fetch_pitch_data(player_ids, player$is_hitter, player$is_pitcher, NULL, NULL)
      
      if (!is.null(raw_data) && nrow(raw_data) > 0) {
        # Extract date range from raw data
        min_date <- as.Date(min(raw_data$date, na.rm = TRUE))
        max_date <- as.Date(max(raw_data$date, na.rm = TRUE))
        data_date_range(c(min_date, max_date))
        
        # Process the data
        processed_data <- process_pitch_data(raw_data, player$is_hitter)
        
        # Store both the filtered and original data
        original_pitch_data(processed_data)
        pitch_data(processed_data)
        
        # Update date input to reflect the actual data range
        updateDateRangeInput(session, "date_range",
                             start = min_date,
                             end = max_date,
                             min = min_date,
                             max = max_date)
      } else {
        showNotification("No pitch data found for this player", type = "warning")
        original_pitch_data(NULL)
        pitch_data(NULL)
      }
    })
  })
  
  # ----------------------------------------
  # Count Selection UI
  # ----------------------------------------
  output$count_selector <- renderUI({
    req(pitch_data())
    counts <- unique(pitch_data()$Count)
    
    # Sort counts in the desired order
    count_order <- c("0-0", "1-0", "2-0", "3-0", "0-1", "1-1", "2-1", "0-2", "1-2", "2-2", "3-2")
    valid_counts <- intersect(counts, count_order)
    valid_counts <- valid_counts[order(match(valid_counts, count_order))]
    
    if(length(valid_counts) == 0) {
      return(div("No count data available"))
    }
    
    # Create the select input
    tagList(
      div(class = "section-header", "Select Count:"),
      selectInput("count", NULL, choices = valid_counts, selected = valid_counts[1]),
      tags$style(HTML("
        /* Style for 0 strikes - light green */
        #count option[value$='-0'] { background-color: #90EE90; }
        
        /* Style for 1 strike - yellow */
        #count option[value$='-1'] { background-color: #FFD700; }
        
        /* Style for 2 strikes - red */
        #count option[value$='-2'] { background-color: #FF6347; }
      "))
    )
  })
  
  # ----------------------------------------
  # View All Counts Option
  # ----------------------------------------
  # Add "View All Counts" checkbox for both hitters and pitchers
  output$view_all_counts_checkbox <- renderUI({
    req(selected_player())
    player <- selected_player()
    
    # Now enabled for both hitters and pitchers
    checkboxInput("view_all_counts", "View Metrics Across All Counts", value = FALSE)
  })
  
  # ----------------------------------------
  # Player Information Display
  # ----------------------------------------
  # Player info display with enhanced styling
  output$player_info_ui <- renderUI({
    req(selected_player())
    player <- selected_player()
    
    # Format the player IDs as a string
    player_ids_str <- paste(unlist(player$player_ids), collapse = ", ")
    
    # Determine team color
    team_name <- player$team_name
    team_color <- "#003366"  # Default blue
    for (name in names(team_colors)) {
      if (grepl(name, team_name)) {
        team_color <- team_colors[[name]]
        break
      }
    }
    
    # Handle batting hand coloring
    batting_hand_class <- "handedness-na"
    if (!is.na(player$player_batting_handedness)) {
      if (player$player_batting_handedness == "Right") {
        batting_hand_class <- "handedness-right"
      } else if (player$player_batting_handedness == "Left") {
        batting_hand_class <- "handedness-left"
      }
    }
    
    # Handle pitching hand coloring
    pitching_hand_class <- "handedness-na"
    if (!is.na(player$player_pitching_handedness)) {
      if (player$player_pitching_handedness == "Right") {
        pitching_hand_class <- "handedness-right"
      } else if (player$player_pitching_handedness == "Left") {
        pitching_hand_class <- "handedness-left"
      }
    }
    
    # Format date range if available
    date_info <- NULL
    if (!is.null(input$date_range) && !is.null(pitch_data())) {
      start_date <- format(input$date_range[1], "%B %d, %Y")
      end_date <- format(input$date_range[2], "%B %d, %Y")
      date_info <- p(span(class = "player-info-label", "Date Range:"), 
                     paste(start_date, "to", end_date))
    }
    
    div(class = "player-info-card",
        h3("Player Information", class = "section-header"),
        p(span(class = "player-info-label", "Name:"), player$player_name),
        p(span(class = "player-info-label", "Type:"), 
          ifelse(player$is_hitter, "Hitter", ""),
          ifelse(player$is_hitter & player$is_pitcher, "/", ""),
          ifelse(player$is_pitcher, "Pitcher", "")),
        p(span(class = "player-info-label", "Team:"), 
          span(style = paste0("color: ", team_color, "; font-weight: bold;"), team_name)),
        p(span(class = "player-info-label", "Batting Hand:"), 
          span(class = batting_hand_class, player$player_batting_handedness)),
        p(span(class = "player-info-label", "Pitching Hand:"), 
          span(class = pitching_hand_class, player$player_pitching_handedness)),
        p(span(class = "player-info-label", "Player IDs:"), player_ids_str),
        
        # Add date range info if available
        date_info
    )
  })
  
  # ----------------------------------------
  # Player Tabs UI
  # ----------------------------------------
  # Main tabs display
  output$player_tabs_ui <- renderUI({
    req(selected_player(), pitch_data())
    player <- selected_player()
    
    if (player$is_hitter) {
      tabsetPanel(
        tabPanel("Hitting Metrics",
                 h3("Batting Statistics", class = "section-header"),
                 div(tableOutput("hitting_metrics_table")),
                 h3("Metrics Across All Counts", class = "section-header"),
                 plotlyOutput("counts_comparison_plot")
        ),
        tabPanel("Pitch Outcomes",
                 h3("Pitch Outcomes", class = "section-header"),
                 div(tableOutput("hitter_pitch_outcomes")),
                 h3("Contact Outcomes", class = "section-header"),
                 div(tableOutput("contact_outcomes_table")),
                 plotlyOutput("contact_rate_plot"),
                 plotlyOutput("contact_outcome_plot")
        ),
        tabPanel("Date Analysis",
                 h3("Performance Over Time", class = "section-header"),
                 plotlyOutput("date_metrics_plot", height = "500px")
        )
      )
    } else {
      # Pitcher tabs - updated to handle "all counts" view
      tabsetPanel(
        tabPanel("Pitch Outcomes",
                 h3("Performance by Count", class = "section-header"),
                 div(tableOutput("pitcher_stats_table")),
                 h3("Metrics Across All Counts", class = "section-header"),
                 plotlyOutput("pitcher_counts_comparison_plot")
        ),
        tabPanel("Pitch Type Analysis",
                 h3("Pitch Type Selection", class = "section-header"),
                 div(tableOutput("pitch_type_table")),
                 plotlyOutput("pitch_type_plot"),
                 h3("Pitch Type Performance", class = "section-header"),
                 div(tableOutput("pitch_type_performance_table")),
                 plotlyOutput("pitch_type_performance_plot")
        ),
        tabPanel("Date Analysis",
                 h3("Performance Over Time", class = "section-header"),
                 plotlyOutput("pitcher_date_metrics_plot", height = "500px")
        )
      )
    }
  })
  
  # ----------------------------------------
  # Hitting Metrics Table
  # ----------------------------------------
  # Hitting metrics table - for the selected count or all counts
  output$hitting_metrics_table <- renderTable({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Use selected count or all counts based on checkbox
    if (input$view_all_counts) {
      metrics <- calculate_hitting_metrics(pitch_data())
      metrics_display <- metrics %>%
        select(PA, AB, Hits, BA, OBP, SLG, OPS, wOBA)
    } else {
      req(input$count)
      metrics <- calculate_hitting_metrics(pitch_data(), input$count)
      metrics_display <- metrics %>%
        select(PA, AB, Hits, BA, OBP, SLG, OPS, wOBA)
    }
    
    metrics_display
  }, 
  striped = TRUE, 
  bordered = TRUE,
  width = "100%",
  class = "table-styled")
  
  # ----------------------------------------
  # Date Filter UI and Handling
  # ----------------------------------------
  # Date filter UI - appears after player is selected
  output$date_filter_ui <- renderUI({
    # Only show date filter after pitch data has been fetched
    req(selected_player(), pitch_data())
    
    # Find earliest and latest game dates from the data
    dates <- as.Date(pitch_data()$date)
    earliest_date <- min(dates, na.rm = TRUE)
    latest_date <- max(dates, na.rm = TRUE)
    
    # Set default date range based on actual data
    tagList(
      div(class = "section-header", "Filter by Date:"),
      dateRangeInput("date_range", NULL, 
                     start = earliest_date,
                     end = latest_date,
                     min = earliest_date,
                     max = latest_date,
                     separator = " to ",
                     format = "MM/dd/yyyy"),
      
      # Rename quick select buttons to reflect "last few games"
      div(style = "display: flex; justify-content: space-between; margin-top: 5px;",
          actionButton("last_few_games", "Last 5 Games", 
                       style = "font-size: 12px; padding: 5px;"),
          actionButton("more_games", "Last 10 Games", 
                       style = "font-size: 12px; padding: 5px;"),
          actionButton("all_games", "All Games", 
                       style = "font-size: 12px; padding: 5px;"),
          actionButton("apply_date", "Apply Filter", 
                       style = "font-size: 12px; padding: 5px; background-color: #28a745; color: white;")
      )
    )
  })
  
  # ----------------------------------------
  # Date Filter Button Handlers
  # ----------------------------------------
  # Observer for "All Dates" button 
  observeEvent(input$all_dates, {
    date_range <- data_date_range()
    updateDateRangeInput(session, "date_range",
                         start = date_range[1],
                         end = date_range[2])
  })
  
  # Observer for "Last 5 Games" button
  observeEvent(input$last_few_games, {
    req(original_pitch_data())
    
    # Get all unique game dates from the original data
    game_dates <- as.Date(unique(original_pitch_data()$date))
    game_dates <- sort(game_dates, decreasing = TRUE)
    
    # Set to last 5 games or all games if fewer than 5
    num_games <- min(5, length(game_dates))
    
    if (num_games > 0) {
      latest_date <- max(game_dates)
      # Find the date for the nth most recent game
      if (num_games <= length(game_dates)) {
        earliest_date <- game_dates[num_games]
      } else {
        earliest_date <- min(game_dates)
      }
      
      # Update date range input
      updateDateRangeInput(session, "date_range",
                           start = earliest_date,
                           end = latest_date)
      
      # Apply the filter immediately
      filtered_data <- original_pitch_data() %>%
        filter(date >= earliest_date & date <= latest_date)
      
      if (nrow(filtered_data) > 0) {
        pitch_data(filtered_data)
      }
    }
  })
  
  # Observer for "Last 10 Games" button
  observeEvent(input$more_games, {
    req(original_pitch_data())
    
    # Get all unique game dates from the original data
    game_dates <- as.Date(unique(original_pitch_data()$date))
    game_dates <- sort(game_dates, decreasing = TRUE)
    
    # Set to last 10 games or all games if fewer than 10
    num_games <- min(10, length(game_dates))
    
    if (num_games > 0) {
      latest_date <- max(game_dates)
      # Find the date for the nth most recent game
      if (num_games <= length(game_dates)) {
        earliest_date <- game_dates[num_games]
      } else {
        earliest_date <- min(game_dates)
      }
      
      # Update date range input
      updateDateRangeInput(session, "date_range",
                           start = earliest_date,
                           end = latest_date)
      
      # Apply the filter immediately
      filtered_data <- original_pitch_data() %>%
        filter(date >= earliest_date & date <= latest_date)
      
      if (nrow(filtered_data) > 0) {
        pitch_data(filtered_data)
      }
    }
  })
  
  # Observer for "All Games" button
  observeEvent(input$all_games, {
    req(original_pitch_data())
    
    # Get all unique game dates from the original data
    game_dates <- as.Date(original_pitch_data()$date)
    
    if (length(game_dates) > 0) {
      min_date <- min(game_dates, na.rm = TRUE)
      max_date <- max(game_dates, na.rm = TRUE)
      
      # Update date range input
      updateDateRangeInput(session, "date_range",
                           start = min_date,
                           end = max_date)
      
      # Also apply the change immediately - restore full dataset
      pitch_data(original_pitch_data())
    }
  })
  
  # Apply date filter when button is clicked
  observeEvent(input$apply_date, {
    req(selected_player(), input$date_range, original_pitch_data())
    
    withProgress(message = 'Applying date filter...', {
      # Get date range values
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]
      
      # Get the original data and filter it by date
      filtered_data <- original_pitch_data() %>%
        filter(date >= start_date & date <= end_date)
      
      if (nrow(filtered_data) > 0) {
        # Update pitch_data with the filtered data
        pitch_data(filtered_data)
      } else {
        showNotification("No pitch data found for this player within the selected date range", 
                         type = "warning")
      }
    })
  })


  # ----------------------------------------
  # Hitting Visualization Outputs
  # ----------------------------------------
  
  # Hitting metrics comparison plot across counts
  output$counts_comparison_plot <- renderPlotly({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Get all unique counts
    all_counts <- unique(pitch_data()$Count)
    
    # Calculate metrics for each count
    count_metrics <- data.frame()
    for (cnt in all_counts) {
      metrics <- calculate_hitting_metrics(pitch_data(), cnt)
      metrics$Count <- cnt
      count_metrics <- rbind(count_metrics, metrics)
    }
    
    # Add "All Counts" row
    all_metrics <- calculate_hitting_metrics(pitch_data())
    all_metrics$Count <- "All Counts"
    count_metrics <- rbind(count_metrics, all_metrics)
    
    # Prepare data for plotting
    plot_data <- count_metrics %>%
      select(Count, BA, OBP, wOBA) %>%
      pivot_longer(cols = c(BA, OBP, wOBA), names_to = "Metric", values_to = "Value")
    
    # Create the ggplot object with custom tooltip text
    p <- ggplot(plot_data, aes(x = Count, y = Value, fill = Metric,
                               text = paste0(Metric, ": ", round(Value, 3)))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Hitting Metrics by Count",
           x = "Count", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert to plotly for interactive hover
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(
        bordercolor = "transparent",
        font = list(color = "white", family = "Nunito", size = 12)
      ))
  })
  
  # Hitter pitch outcomes table
  output$hitter_pitch_outcomes <- renderTable({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Use all data or filter by count
    if (input$view_all_counts) {
      data <- pitch_data() %>% 
        group_by(PitchCall) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
        arrange(desc(Count))
    } else {
      req(input$count)
      data <- pitch_data() %>% 
        filter(Count == input$count) %>%
        group_by(PitchCall) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
        arrange(desc(Count))
    }
    
    data
  }, 
  striped = TRUE, 
  bordered = TRUE,
  width = "100%",
  class = "table-styled")
  
  # Contact outcomes table
  output$contact_outcomes_table <- renderTable({
    req(pitch_data(), selected_player(), input$count)
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Use selected count or all counts based on checkbox
    if (input$view_all_counts) {
      outcomes <- calculate_contact_outcomes(pitch_data())
    } else {
      filtered_data <- pitch_data() %>% filter(Count == input$count)
      outcomes <- calculate_contact_outcomes(filtered_data)
    }
    
    # Format the display table
    outcomes %>%
      select(Count, TotalInFieldContact, 
             SingleRate, DoubleRate, TripleRate, HomeRunRate, 
             GroundOutRate, FlyOutRate, LineOutRate, PopupOutRate) %>%
      arrange(Count)
  }, 
  striped = TRUE, 
  bordered = TRUE,
  width = "100%",
  class = "table-styled")
  
  # Contact rate visualization by count
  output$contact_rate_plot <- renderPlotly({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Calculate contact rate by count
    contact_rates <- pitch_data() %>%
      group_by(Count) %>%
      summarise(
        TotalPitches = n(),
        ContactPitches = sum(PitchCall == "InPlay"),
        InPlayRate = ContactPitches / TotalPitches * 100,
        .groups = 'drop'
      ) %>%
      arrange(Count)
    
    # Define count types for coloring
    contact_rates <- contact_rates %>%
      mutate(CountType = case_when(
        Count %in% c("0-0", "1-0", "2-0", "3-0") ~ "0 Strike",
        Count %in% c("0-1", "1-1", "2-1", "3-1") ~ "1 Strike",
        Count %in% c("0-2", "1-2", "2-2", "3-2") ~ "2 Strikes",
        TRUE ~ "Other"
      ))
    
    # Define the custom color palette
    count_colors <- c(
      "0 Strike" = "#90EE90",  # Light green for hitter counts
      "1 Strike" = "#FFFF99",  # Yellow for neutral counts
      "2 Strikes" = "#FF9999"  # Red for pitcher counts
    )
    
    # Create the ggplot with custom tooltip text
    p <- ggplot(contact_rates, 
                aes(x = Count, y = InPlayRate, fill = CountType,
                    text = paste0("Count: ", Count, "<br>",
                                  "Contact Rate: ", round(InPlayRate, 1), "%<br>",
                                  "Pitches: ", ContactPitches, "/", TotalPitches))) +
      geom_bar(stat = "identity") +
      labs(title = "Contact Rate by Count",
           x = "Count", y = "Contact Rate (%)") +
      scale_fill_manual(values = count_colors, name = "Count Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert to plotly for interactive tooltips
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(
        bordercolor = "transparent",
        font = list(color = "black", family = "Nunito", size = 12)
      ))
  })
  
  # Contact outcome visualization 
  output$contact_outcome_plot <- renderPlotly({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Calculate outcome data using our helper function
    outcome_data <- calculate_contact_outcomes(pitch_data())
    
    # Prepare data for plotting - select only the outcome rates we want to show
    plot_data <- outcome_data %>%
      select(Count, OutRate, SingleRate, DoubleRate, TripleRate, HomeRunRate) %>%
      pivot_longer(cols = c(OutRate, SingleRate, DoubleRate, TripleRate, HomeRunRate),
                   names_to = "OutcomeType", values_to = "Rate") %>%
      # Clean up the outcome type names for display
      mutate(OutcomeType = gsub("Rate", "", OutcomeType))
    
    # Create the plot with custom tooltip text
    p <- ggplot(plot_data, aes(x = Count, y = Rate, fill = OutcomeType,
                               text = paste0("Count: ", Count, "<br>",
                                             OutcomeType, ": ", round(Rate, 1), "%"))) +
      geom_bar(stat = "identity") +
      labs(title = "Contact Outcome by Count",
           x = "Count", y = "Rate (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("Out" = "#4682B4", 
                                   "Single" = "#77DD77", 
                                   "Double" = "#FF7F00", 
                                   "Triple" = "#FFFF00", 
                                   "HomeRun" = "#FF0000"))
    
    # Convert to plotly for interactive hover
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(
        bordercolor = "transparent",
        font = list(color = "black", family = "Nunito", size = 12)
      ))
  })
  
  # Hitter metrics over time visualization
  output$date_metrics_plot <- renderPlotly({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_hitter) return(NULL)
    
    # Calculate metrics by date
    date_metrics <- calculate_metrics_by_date(pitch_data())
    
    # If we have too few dates, show a message
    if(nrow(date_metrics) < 2) {
      return(plot_ly() %>% 
               add_annotations(
                 text = "Not enough data to display date-based metrics",
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    }
    
    # Make sure dates are properly sorted and unique
    date_metrics <- date_metrics %>%
      mutate(date = as.Date(date)) %>%
      arrange(date) %>%
      # Create a display date format that's guaranteed to be unique
      mutate(display_date = paste0(format(date, "%b %d"), "_", row_number()))
    
    # Create plot with multiple metrics
    plot_ly() %>%
      add_trace(
        data = date_metrics,
        x = ~date,  # Use actual date for proper spacing
        y = ~ContactRate,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Contact Rate',
        text = ~paste0("Date: ", format(date, "%b %d, %Y"), "<br>",
                       "Contact Rate: ", round(ContactRate, 2), "%"),
        hoverinfo = 'text',
        line = list(color = '#32CD32', width = 2),
        marker = list(color = '#32CD32', size = 8)
      ) %>%
      add_trace(
        data = date_metrics,
        x = ~date,  # Use actual date for proper spacing
        y = ~StrikeRate,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Strike Rate',
        text = ~paste0("Date: ", format(date, "%b %d, %Y"), "<br>",
                       "Strike Rate: ", round(StrikeRate, 2), "%"),
        hoverinfo = 'text',
        line = list(color = '#DC143C', width = 2),
        marker = list(color = '#DC143C', size = 8)
      ) %>%
      add_trace(
        data = date_metrics,
        x = ~date,  # Use actual date for proper spacing
        y = ~BallRate,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Ball Rate',
        text = ~paste0("Date: ", format(date, "%b %d, %Y"), "<br>",
                       "Ball Rate: ", round(BallRate, 2), "%"),
        hoverinfo = 'text',
        line = list(color = '#1E90FF', width = 2),
        marker = list(color = '#1E90FF', size = 8)
      ) %>%
      layout(
        title = list(
          text = "Batter Performance Metrics Over Time",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Game Date",
          type = "date",        # Keep as date type
          tickformat = "%b %d", # Format like "May 24"
          dtick = 86400000,     # Set tick spacing to 1 day (in milliseconds)
          gridcolor = "#EEEEEE"
        ),
        yaxis = list(
          title = "Percentage of Pitches (%)",
          range = c(0, 100),
          gridcolor = "#EEEEEE"
        ),
        hovermode = "closest",
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        ),
        legend = list(orientation = "h", y = -0.2),
        plot_bgcolor = "#FFFFFF"
      )
  })
  
  # ----------------------------------------
  # Pitching Visualization Outputs
  # ----------------------------------------
  
  # Pitcher stats table
  output$pitcher_stats_table <- renderTable({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Use selected count or all counts based on checkbox
    if (input$view_all_counts) {
      metrics <- calculate_pitcher_metrics(pitch_data())
    } else {
      req(input$count)
      metrics <- calculate_pitcher_metrics(pitch_data(), input$count)
    }
    
    # Format for display
    metrics_display <- metrics %>%
      select(TotalPitches, Strikes, Balls, StrikePercentage, BallPercentage,
             InPlayRate, SwingingStrikeRate, CalledStrikeRate, FoulRate)
    
    metrics_display
  }, 
  striped = TRUE, 
  bordered = TRUE,
  width = "100%",
  class = "table-styled")
  
  # Pitcher stats plot
  output$pitcher_stats_plot <- renderPlot({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Calculate metrics by count
    count_metrics <- data.frame()
    for (cnt in unique(pitch_data()$Count)) {
      metrics <- calculate_pitcher_metrics(pitch_data(), cnt)
      metrics$Count <- cnt
      count_metrics <- rbind(count_metrics, metrics)
    }
    
    # Add "All Counts" row
    all_metrics <- calculate_pitcher_metrics(pitch_data())
    all_metrics$Count <- "All Counts"
    count_metrics <- rbind(count_metrics, all_metrics)
    
    # Prepare data for plotting
    plot_data <- count_metrics %>%
      select(Count, StrikePercentage, InPlayRate, BallPercentage) %>%
      pivot_longer(cols = c(StrikePercentage, BallPercentage, InPlayRate),
                   names_to = "Metric", values_to = "Value") %>%
      arrange(Count)
    
    ggplot(plot_data, aes(x = Count, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Pitch Metrics by Count",
           x = "Count", y = "Value (%)") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Pitcher metrics by count comparison plot
  output$pitcher_counts_comparison_plot <- renderPlotly({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Get all unique counts
    all_counts <- unique(pitch_data()$Count)
    
    # Calculate metrics for each count
    count_metrics <- data.frame()
    for (cnt in all_counts) {
      metrics <- calculate_pitcher_metrics(pitch_data(), cnt)
      metrics$Count <- cnt
      count_metrics <- rbind(count_metrics, metrics)
    }
    
    # Add "All Counts" row
    all_metrics <- calculate_pitcher_metrics(pitch_data())
    all_metrics$Count <- "All Counts"
    count_metrics <- rbind(count_metrics, all_metrics)
    
    # Prepare data for plotting
    plot_data <- count_metrics %>%
      select(Count, BallPercentage, InPlayRate, StrikePercentage) %>%
      pivot_longer(cols = c(BallPercentage, InPlayRate, StrikePercentage), 
                   names_to = "Metric", values_to = "Percentage")
    
    # Create the plot with custom tooltip text
    p <- ggplot(plot_data, aes(x = Count, y = Percentage, fill = Metric,
                               text = paste0("Count: ", Count, "<br>",
                                             Metric, ": ", round(Percentage, 1), "%"))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Outcome Metrics by Count",
           x = "Count", y = "Percentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("BallPercentage" = "#4682B4", 
                                   "InPlayRate" = "#90EE90", 
                                   "StrikePercentage" = "#DC143C"))
    
    # Convert to plotly for interactive hover
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(
        bordercolor = "transparent",
        font = list(color = "white", family = "Nunito", size = 12)
      ))
  })
  
  # ----------------------------------------
  # Pitch Type Analysis Outputs
  # ----------------------------------------
  
  # Pitch type distribution table
  output$pitch_type_table <- renderTable({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Use selected count or all counts
    if (input$view_all_counts) {
      distribution <- calculate_pitch_type_distribution(pitch_data())
    } else {
      req(input$count)
      distribution <- calculate_pitch_type_distribution(pitch_data(), input$count)
    }
    
    distribution %>% arrange(desc(Percentage))
  }, 
  striped = TRUE, 
  bordered = TRUE,
  width = "100%",
  class = "table-styled")
  
  # Pitch type distribution visualization
  output$pitch_type_plot <- renderPlotly({
    # Ensure required packages are loaded
    require(plotly)
    require(dplyr)
    
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Use selected count or all counts
    if (input$view_all_counts) {
      distribution <- calculate_pitch_type_distribution(pitch_data())
    } else {
      req(input$count)
      distribution <- calculate_pitch_type_distribution(pitch_data(), input$count)
    }
    
    # Filter to show only common pitch types and "Other"
    common_types <- c("Four-Seam", "Curveball", "Slider", "Changeup", "Cutter", "Sinker")
    processed_distribution <- distribution %>%
      mutate(PitchType = ifelse(AutoPitchType %in% common_types, 
                                AutoPitchType, "Other")) %>%
      group_by(PitchType) %>%
      summarise(Count = sum(Count),
                Percentage = sum(Percentage),
                .groups = 'drop')
    
    # Create a direct plotly pie chart instead of converting from ggplot
    plot_ly(processed_distribution, 
            labels = ~PitchType, 
            values = ~Percentage,
            type = 'pie',
            textinfo = 'none',
            hoverinfo = 'text',
            text = ~paste0("Pitch Type: ", PitchType, "<br>",
                           "Percentage: ", round(Percentage, 2), "%<br>",
                           "Count: ", Count),
            marker = list(colors = RColorBrewer::brewer.pal(length(unique(processed_distribution$PitchType)), "Set1"))) %>%
      layout(
        title = "Pitch Type Distribution",
        legend = list(orientation = "h"),
        hoverlabel = list(
          bordercolor = "transparent",
          font = list(color = "black", family = "Nunito")
        )
      )
  })
  
  # Pitch type performance table
  output$pitch_type_performance_table <- renderTable({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Calculate performance by pitch type
    performance <- calculate_pitch_type_performance(pitch_data())
    
    # Format for display
    performance
  }, 
  striped = TRUE, 
  bordered = TRUE,
  width = "100%",
  class = "table-styled")
  
  # Pitch type performance visualization
  output$pitch_type_performance_plot <- renderPlotly({
    # Ensure required packages are loaded
    require(plotly)
    require(dplyr)
    
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Get performance by pitch type
    df <- pitch_data() %>%
      filter(!is.na(AutoPitchType) & AutoPitchType != "") %>%
      group_by(AutoPitchType, PitchCall) %>%
      summarise(Count = n(), .groups = 'drop_last') %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      filter(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "BallCalled", "FoulBallFieldable","FoulBallNotFieldable", "InPlay"))
    
    # Keep only common pitch types and "Other"
    common_types <- c("Four-Seam", "Curveball", "Slider", "Changeup", "Cutter", "Sinker")
    df <- df %>%
      mutate(PitchType = ifelse(AutoPitchType %in% common_types, 
                                AutoPitchType, "Other"))
    
    # Create the ggplot with custom tooltip text
    p <- ggplot(df, aes(x = PitchType, y = Percentage, fill = PitchCall,
                        text = paste0("Pitch Type: ", PitchType, "<br>",
                                      "Result: ", PitchCall, "<br>",
                                      "Percentage: ", round(Percentage, 2), "%<br>",
                                      "Count: ", Count))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Pitch Outcome by Pitch Type",
           x = "Pitch Type", y = "Percentage (%)") +
      scale_fill_brewer(palette = "Set1") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert to plotly for interactive tooltips
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(
        bordercolor = "transparent",
        font = list(color = "white", family = "Nunito", size = 12)
      ))
  })
  
  # Pitcher performance over time visualization
  output$pitcher_date_metrics_plot <- renderPlotly({
    req(pitch_data(), selected_player())
    player <- selected_player()
    if (!player$is_pitcher) return(NULL)
    
    # Calculate metrics by date
    date_metrics <- calculate_pitcher_metrics_by_date(pitch_data())
    
    # If we have too few dates, show a message
    if(nrow(date_metrics) < 2) {
      return(plot_ly() %>% 
               add_annotations(
                 text = "Not enough data to display date-based metrics",
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    }
    
    # Make sure dates are properly sorted
    date_metrics <- date_metrics %>%
      mutate(date = as.Date(date)) %>%
      arrange(date)
    
    # Create plot with multiple metrics
    plot_ly() %>%
      add_trace(
        data = date_metrics,
        x = ~date,  # Use actual date object
        y = ~StrikePercentage,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Strike %',
        text = ~paste0("Date: ", format(date, "%b %d, %Y"), "<br>",
                       "Strike %: ", round(StrikePercentage, 2), "%"),
        hoverinfo = 'text',
        line = list(color = '#DC143C', width = 2),
        marker = list(color = '#DC143C', size = 8)
      ) %>%
      add_trace(
        data = date_metrics,
        x = ~date,  # Use actual date object
        y = ~BallPercentage,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Ball %',
        text = ~paste0("Date: ", format(date, "%b %d, %Y"), "<br>",
                       "Ball %: ", round(BallPercentage, 2), "%"),
        hoverinfo = 'text',
        line = list(color = '#1E90FF', width = 2),
        marker = list(color = '#1E90FF', size = 8)
      ) %>%
      add_trace(
        data = date_metrics,
        x = ~date,  # Use actual date object
        y = ~InPlayRate,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'InPlay %',
        text = ~paste0("Date: ", format(date, "%b %d, %Y"), "<br>",
                       "InPlay %: ", round(InPlayRate, 2), "%"),
        hoverinfo = 'text',
        line = list(color = '#32CD32', width = 2),
        marker = list(color = '#32CD32', size = 8)
      ) %>%
      layout(
        title = list(
          text = "Pitcher Performance Over Time",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Game Date",
          type = "date",        # Important: keep as date type
          tickformat = "%b %d", # Format like "May 24"
          dtick = "D1",         # Set tick interval to 1 day
          gridcolor = "#EEEEEE",
          autorange = TRUE      # Let plotly handle date range
        ),
        yaxis = list(
          title = "Percentage (%)",
          range = c(0, 100),
          gridcolor = "#EEEEEE"
        ),
        hovermode = "closest",
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        ),
        legend = list(orientation = "h", y = -0.2),
        plot_bgcolor = "#FFFFFF"
      )
  })
  
  # ----------------------------------------
  # Report Generation Functions
  # ----------------------------------------
  
  # Create temp file for the report
  report_file <- reactive({
    # Create a temporary directory
    temp_dir <- tempdir()
    file.path(temp_dir, "player_report.Rmd")
  })
  
  # Generate the Rmd content for the report
  generate_report_content <- function() {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      # Use an alternative approach without gridExtra
      grid_code <- "# Plot them individually since gridExtra is not available
par(mfrow = c(1, 2))
print(p1)
print(p2)"
    } else {
      grid_code <- "# Plot them in a grid
gridExtra::grid.arrange(p1, p2, ncol = 2)"
    }
    req(selected_player(), pitch_data())
    player <- selected_player()
    
    # Get the selected count (or use '0-0' as default if not available)
    selected_count <- isolate(input$count)
    if(is.null(selected_count)) selected_count <- "0-0"
    
    # Basic report header with compact margins and smaller font
    header <- paste0("---
title: \"Player Report: ", player$player_name, "\"
date: \"", format(Sys.Date(), '%B %d, %Y'), "\"
output: 
  pdf_document:
    toc: false
    toc_depth: 2
    latex_engine: pdflatex
    fig_width: 6.5
    fig_height: 4
    fig_caption: true
    keep_tex: false
    dev: pdf
    highlight: tango
mainfont: Helvetica
header-includes:
  - \\usepackage{titlesec}
  - \\titlespacing*{\\section}{0pt}{*0.5}{*0.25}
  - \\titlespacing*{\\subsection}{0pt}{*0.5}{*0.25}
  - \\usepackage{setspace}
  - \\setstretch{0.8}
  - \\usepackage{geometry}
  - \\geometry{top=0.5in, bottom=0.5in}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                     fig.width = 7, fig.height = 3.5, dpi = 100, out.width = '100%',
                     fig.align = 'center')
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(RColorBrewer)
library(gridExtra)

# Make the processed data available to all chunks
pitch_data <- function() {
  return(", paste0(deparse(pitch_data()), collapse = "\n"), ")
}

# Selected count
selected_count <- \"", selected_count, "\"

# Add the calculation functions needed for the report
calculate_hitting_metrics <- function(df, count = NULL) {
  # If count is specified, filter the data
  if (!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  # Calculate at-bats, plate appearances, and hits
  metrics <- df %>% summarise(
    # Plate appearances - all pitch sequences that end in a terminal event
    PA = sum(k_or_bb == \"Strikeout\" | k_or_bb == \"Walk\" | PitchCall == \"InPlay\" | PitchCall == \"HitByPitch\"),
    
    # At-bats - plate appearances minus walks and HBP
    AB = sum((k_or_bb == \"Strikeout\") | PitchCall == \"InPlay\"),
    
    # Hit outcomes
    Hits = sum(PlayResult %in% c(\"Single\", \"Double\", \"Triple\", \"HomeRun\")),
    Singles = sum(PlayResult == \"Single\"),
    Doubles = sum(PlayResult == \"Double\"),
    Triples = sum(PlayResult == \"Triple\"),
    HomeRuns = sum(PlayResult == \"HomeRun\"),
    
    # On-base events
    Walks = sum(k_or_bb == \"Walk\"),
    HBP = sum(PitchCall == \"HitByPitch\"),
    
    # Calculate total bases
    TB = Singles + (2 * Doubles) + (3 * Triples) + (4 * HomeRuns)
  )
  
  # Calculate the metrics
  metrics <- metrics %>% mutate(
    BA = ifelse(AB > 0, round(Hits / AB, 3), 0),
    OBP = ifelse(PA > 0, round((Hits + Walks + HBP) / PA, 3), 0),
    SLG = ifelse(AB > 0, round(TB / AB, 3), 0),
    OPS = BA + SLG,
    
    # Calculate wOBP using standard weights
    wOBA = ifelse(PA > 0, 
                round((0.72 * Walks + 0.75 * HBP + 
                         0.9 * Singles + 1.24 * Doubles + 
                         1.56 * Triples + 1.95 * HomeRuns) / PA, 3),
                0)
  )
  
  return(metrics)
}

calculate_contact_outcomes <- function(df) {
  df %>%
    group_by(Count) %>%
    summarise(
      TotalInFieldContact = sum(PitchCall == \"InPlay\"),
      Single = sum(PlayResult == \"Single\"),
      Double = sum(PlayResult == \"Double\"),
      Triple = sum(PlayResult == \"Triple\"),
      HomeRun = sum(PlayResult == \"HomeRun\"),
      GroundOut = sum(PlayResult == \"Out\" & (TaggedHitType == \"GroundBall\")),
      FlyOut = sum(PlayResult == \"Out\" & (TaggedHitType == \"FlyBall\")),
      LineOut = sum(PlayResult == \"Out\" & (TaggedHitType == \"LineDrive\")),
      PopupOut = sum(PlayResult == \"Out\" & (TaggedHitType == \"Popup\"))
    ) %>%
    mutate(
SingleRate = ifelse(TotalInFieldContact > 0, round((Single / TotalInFieldContact) * 100, 3), 0),
DoubleRate = ifelse(TotalInFieldContact > 0, round((Double / TotalInFieldContact) * 100, 3), 0),
TripleRate = ifelse(TotalInFieldContact > 0, round((Triple / TotalInFieldContact) * 100, 3), 0),
HomeRunRate = ifelse(TotalInFieldContact > 0, round((HomeRun / TotalInFieldContact) * 100, 3), 0),
GroundOutRate = ifelse(TotalInFieldContact > 0, round((GroundOut / TotalInFieldContact) * 100, 3), 0),
FlyOutRate = ifelse(TotalInFieldContact > 0, round((FlyOut / TotalInFieldContact) * 100, 3), 0),
LineOutRate = ifelse(TotalInFieldContact > 0, round((LineOut / TotalInFieldContact) * 100, 3), 0),
PopupOutRate = ifelse(TotalInFieldContact > 0, round((PopupOut / TotalInFieldContact) * 100, 3), 0), 
OutRate = round(GroundOutRate + LineOutRate + LineOutRate + PopupOutRate, 3)
    )
}

calculate_pitcher_metrics <- function(df, count = NULL) {
  # If count is specified, filter the data
  if (!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  # Calculate pitcher stats
  metrics <- df %>% summarise(
    TotalPitches = n(),
    Strikes = sum(PitchCall %in% c(\"StrikeCalled\", \"StrikeSwinging\", \"FoulBallFieldable\",\"FoulBallNotFieldable\")),
    Balls = sum(PitchCall %in% c(\"BallCalled\", \"BallinDirt\", \"HitByPitch\")),
    StrikePercentage = round(Strikes / TotalPitches * 100, 1),
    ContactPitches = sum(PitchCall == \"InPlay\"),
    InPlayRate = round(ContactPitches / TotalPitches * 100, 1),
    SwingingStrikes = sum(PitchCall == \"StrikeSwinging\"),
    SwingingStrikeRate = round(SwingingStrikes / TotalPitches * 100, 1),
    CalledStrikes = sum(PitchCall == \"StrikeCalled\"),
    CalledStrikeRate = round(CalledStrikes / TotalPitches * 100, 1),
    FoulBalls = sum(PitchCall %in% c(\"FoulBallFieldable\",\"FoulBallNotFieldable\")),
    FoulRate = round(FoulBalls / TotalPitches * 100, 1),
    BallPercentage = round(Balls / TotalPitches * 100, 1),
    BallsInPlay = sum(!is.na(PlayResult) & PlayResult != \"Undefined\"),
  )
  
  return(metrics)
}

calculate_pitch_type_distribution <- function(df, count = NULL) {
  # If count is specified, filter the data
  if (!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  df %>%
    group_by(AutoPitchType) %>%
    summarise(
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      Percentage = round(Count / sum(Count) * 100, 1)
    ) %>%
    filter(!is.na(AutoPitchType) & AutoPitchType != \"\")
}

# Function to create performance summary table by pitch type and count
create_pitch_performance_table <- function(df, count = NULL) {
  # Filter by count if specified
  if(!is.null(count)) {
    df <- df %>% filter(Count == count)
  }
  
  # Get performance metrics by pitch type
  performance <- df %>%
    filter(!is.na(AutoPitchType) & AutoPitchType != '') %>%
    group_by(AutoPitchType) %>%
    summarise(
      TotalPitches = n(),
      StrikePercentage = round(sum(PitchCall %in% c('StrikeCalled', 'StrikeSwinging', 
                                                  'FoulBallFieldable', 'FoulBallNotFieldable')) / 
                              TotalPitches * 100, 1),
      SwingingStrikePercentage = round(sum(PitchCall == 'StrikeSwinging') / 
                                      TotalPitches * 100, 1),
      BallPercentage = round(sum(PitchCall %in% c('BallCalled', 'BallinDirt', 'HitByPitch')) / 
                            TotalPitches * 100, 1),
      InPlayPercentage = round(sum(PitchCall == 'InPlay') / 
                              TotalPitches * 100, 1),
      .groups = 'drop'
    ) %>%
    arrange(desc(TotalPitches))
  
  return(performance)
}
```

")
    
    # Add appropriate analysis based on player type
    if (player$is_hitter) {
      # Overall hitting metrics
      all_metrics <- calculate_hitting_metrics(pitch_data())
      metrics_table <- capture.output(kable(
        all_metrics %>% select(PA, AB, Hits, BA, OBP, SLG, OPS, wOBA),
        format = "markdown",
        caption = "Overall Hitting Metrics"
      ))
      
      # Selected count metrics
      count_metrics <- calculate_hitting_metrics(pitch_data(), selected_count)
      count_metrics_table <- capture.output(kable(
        count_metrics %>% select(PA, AB, Hits, BA, OBP, SLG, OPS, wOBA),
        format = "markdown",
        caption = paste("Hitting Metrics for Count:", selected_count)
      ))
      
      # Table for all counts - combine with the overall plot for space efficiency
      all_counts_metrics <- data.frame()
      for (cnt in unique(pitch_data()$Count)) {
        metrics <- calculate_hitting_metrics(pitch_data(), cnt)
        metrics$Count <- cnt
        all_counts_metrics <- rbind(all_counts_metrics, metrics)
      }
      
      # Add "All Counts" row
      all_counts_metrics <- rbind(
        all_counts_metrics,
        data.frame(all_metrics, Count = "All Counts")
      )
      
      all_counts_table <- capture.output(kable(
        all_counts_metrics %>% select(Count, PA, AB, Hits, BA, OBP, SLG),
        format = "markdown",
        caption = "Hitting Metrics by Count"
      ))
      
      hitting_section <- paste0("
# Hitting Analysis

## Hitting Metrics Overview
", paste(metrics_table, collapse = "\n"), "

Selected Count Performance (", selected_count, "): ", paste(count_metrics_table, collapse = "\n"), "

```{r fig.width=6.5, fig.height=5}
# Get all unique counts
all_counts <- unique(pitch_data()$Count)

# Calculate metrics for each count
count_metrics <- data.frame()
for (cnt in all_counts) {
  metrics <- calculate_hitting_metrics(pitch_data(), cnt)
  metrics$Count <- cnt
  count_metrics <- rbind(count_metrics, metrics)
}

# Add 'All Counts' row
all_metrics <- calculate_hitting_metrics(pitch_data())
all_metrics$Count <- 'All Counts'
count_metrics <- rbind(count_metrics, all_metrics)

# Prepare data for plotting - just focus on key metrics
plot_data <- count_metrics %>%
  select(Count, BA, OBP, wOBA) %>%
  pivot_longer(cols = c(BA, OBP, wOBA), names_to = 'Metric', values_to = 'Value')

# Highlight the selected count
plot_data$Selected <- ifelse(plot_data$Count == selected_count, 'Selected', 'Not Selected')

# Create the compact plot
ggplot(plot_data, aes(x = Count, y = Value, fill = Metric)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5) +
  labs(title = 'Hitting Metrics by Count',
       x = 'Count', y = 'Value') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 10),
        legend.position = 'top',
        legend.title = element_blank()) +
  geom_vline(xintercept = which(unique(plot_data$Count) == selected_count), 
             linetype = 'dashed', color = 'black', size = 0.5)
```

## Contact Outcomes

```{r fig.width=6.5, fig.height=5.5}
# Calculate contact outcomes
outcomes <- calculate_contact_outcomes(pitch_data())

# Only show the most important outcomes for space efficiency
outcomes_compact <- outcomes %>%
  select(Count, TotalInFieldContact, SingleRate, DoubleRate, TripleRate, HomeRunRate, OutRate)

# Create a formatted table with only key metrics
kable(
  outcomes_compact,
  format = 'markdown',
  caption = 'Contact Outcomes by Count (%)',
  col.names = c(\"Count\", \"Total Contact\", \"1B%\", \"2B%\", \"3B%\", \"HR%\", \"Out%\")
)

# Prepare data for plotting - focus on the most important outcomes
plot_data <- outcomes %>%
  select(Count, SingleRate, DoubleRate, TripleRate, HomeRunRate, OutRate) %>%
  pivot_longer(cols = c(SingleRate, DoubleRate, TripleRate, HomeRunRate, OutRate),
               names_to = 'OutcomeType', values_to = 'Rate')

# Create a more compact visualization
ggplot(plot_data, aes(x = Count, y = Rate, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(title = 'Contact Outcome by Count',
       x = 'Count', y = 'Rate (%)') +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 8),
        legend.position = 'top',
        legend.title = element_blank()) +
  geom_vline(xintercept = which(unique(plot_data$Count) == selected_count), 
             linetype = 'dashed', color = 'black', size = 0.3)
```
")

report_content <- paste0(header, hitting_section)

    } else if (player$is_pitcher) {
      # Overall pitching metrics
      all_metrics <- calculate_pitcher_metrics(pitch_data())
      metrics_table <- capture.output(kable(
        all_metrics %>% 
          select(TotalPitches, StrikePercentage, BallPercentage,
                 InPlayRate, SwingingStrikeRate, CalledStrikeRate) %>%
          rename(
            Pitches = TotalPitches,
            StrPct = StrikePercentage,
            BallPct = BallPercentage,
            InPlayPct = InPlayRate,
            SwStrPct = SwingingStrikeRate,
            CalledStrPct = CalledStrikeRate
          ),
        format = "markdown",
        caption = "Overall Pitching Metrics"
      ))
      
      # Selected count metrics
      count_metrics <- calculate_pitcher_metrics(pitch_data(), selected_count)
      count_metrics_table <- capture.output(kable(
        count_metrics %>% select(TotalPitches, StrikePercentage, BallPercentage,
                                 InPlayRate, SwingingStrikeRate, CalledStrikeRate) %>%
          rename(
            Pitches = TotalPitches,
            StrPct = StrikePercentage,
            BallPct = BallPercentage,
            InPlayPct = InPlayRate,
            SwStrPct = SwingingStrikeRate,
            CalledStrPct = CalledStrikeRate
          ),
        format = "markdown",
        caption = paste("Pitching Metrics for Count:", selected_count)
      ))
      
      # Table for all counts - integrate with the visualization
      all_counts_metrics <- data.frame()
      for (cnt in unique(pitch_data()$Count)) {
        metrics <- calculate_pitcher_metrics(pitch_data(), cnt)
        metrics$Count <- cnt
        all_counts_metrics <- rbind(all_counts_metrics, metrics)
      }
      
      # Add "All Counts" row
      all_counts_metrics <- rbind(
        all_counts_metrics,
        data.frame(all_metrics, Count = "All Counts")
      )
      
      pitching_section <- paste0("
# Pitching Analysis

## Pitching Metrics Overview
", paste(metrics_table, collapse = "\n"), "

Selected Count Performance (", selected_count, "): ", paste(count_metrics_table, collapse = "\n"), "

```{r fig.width=6.5, fig.height=5}
# Calculate metrics by count
count_metrics <- data.frame()
for (cnt in unique(pitch_data()$Count)) {
  metrics <- calculate_pitcher_metrics(pitch_data(), cnt)
  metrics$Count <- cnt
  count_metrics <- rbind(count_metrics, metrics)
}

# Add 'All Counts' row
all_metrics <- calculate_pitcher_metrics(pitch_data())
all_metrics$Count <- 'All Counts'
count_metrics <- rbind(count_metrics, all_metrics)

# Prepare data for plotting - focus on key metrics
plot_data <- count_metrics %>%
  select(Count, StrikePercentage, BallPercentage, SwingingStrikeRate) %>%
  pivot_longer(cols = c(StrikePercentage, BallPercentage, SwingingStrikeRate),
               names_to = 'Metric', values_to = 'Value') %>%
  arrange(Count)

# Create a more compact visualization
ggplot(plot_data, aes(x = Count, y = Value, fill = Metric)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5) +
  labs(title = 'Outcome Metrics by Count',
       x = 'Count', y = 'Percentage (%)') +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 8),
        legend.position = 'top',
        legend.title = element_blank()) +
  geom_vline(xintercept = which(unique(plot_data$Count) == selected_count), 
             linetype = 'dashed', color = 'black', size = 0.3)
```

## Pitch Type Analysis

```{r fig.width=6.5, fig.height=3.0}
# Calculate pitch type distribution
distribution <- calculate_pitch_type_distribution(pitch_data())
count_distribution <- calculate_pitch_type_distribution(pitch_data(), selected_count)

# Keep only common pitch types and 'Other' for clarity
common_types <- c('Four-Seam', 'Curveball', 'Slider', 'Changeup', 'Cutter', 'Sinker')

# Overall distribution plot
plot_distribution <- distribution %>%
  mutate(PitchType = ifelse(AutoPitchType %in% common_types, 
                            AutoPitchType, 'Other')) %>%
  group_by(PitchType) %>%
  summarise(Count = sum(Count),
            Percentage = sum(Percentage),
            .groups = 'drop')

# Count-specific distribution plot
count_plot_distribution <- count_distribution %>%
  mutate(PitchType = ifelse(AutoPitchType %in% common_types, 
                            AutoPitchType, 'Other')) %>%
  group_by(PitchType) %>%
  summarise(Count = sum(Count),
            Percentage = sum(Percentage),
            .groups = 'drop')

# Create the plots using a horizontal layout with gridExtra
p1 <- ggplot(plot_distribution, aes(x = '', y = Percentage, fill = PitchType)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  labs(title = 'Overall Pitch Types') +
  scale_fill_brewer(palette = 'Set1') +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 10),
        legend.position = 'bottom',
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.spacing.x = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.key.size = unit(0.3, 'lines'))

p2 <- ggplot(count_plot_distribution, aes(x = '', y = Percentage, fill = PitchType)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  labs(title = paste('Pitch Types:', selected_count)) +
  scale_fill_brewer(palette = 'Set1') +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 10),
        legend.position = 'bottom',
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'lines'))

# Use appropriate code for gridExtra availability
", grid_code, "

# Get performance for all pitches and selected count
all_performance <- create_pitch_performance_table(pitch_data()) %>% 
  filter(TotalPitches >= 5) %>%
  select(AutoPitchType, TotalPitches, StrikePercentage, SwingingStrikePercentage, BallPercentage)

# Performance table
kable(all_performance,
      format = 'markdown',
      caption = 'Pitch Type Performance Summary',
      col.names = c('Pitch Type', 'Total', 'Strike%', 'SwStr%', 'Ball%'))

# Get performance by pitch type for selected count
count_df <- pitch_data() %>%
  filter(!is.na(AutoPitchType) & AutoPitchType != '' & Count == selected_count) %>%
  group_by(AutoPitchType, PitchCall) %>%
  summarise(Count = n(), .groups = 'drop_last') %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  filter(PitchCall %in% c('StrikeCalled', 'StrikeSwinging', 'BallCalled', 'InPlay'))

# Keep only common pitch types and limit height
count_df <- count_df %>%
  mutate(PitchType = ifelse(AutoPitchType %in% common_types, 
                            AutoPitchType, 'Other'))

# Create a more compact outcome by pitch type plot for selected count
if(nrow(count_df) > 0) {
  ggplot(count_df, aes(x = PitchType, y = Percentage, fill = PitchCall)) +
    geom_bar(stat = 'identity', position = 'stack') +
    labs(title = paste('Pitch Outcome by Type for Count:', selected_count),
         x = NULL, y = 'Percentage (%)') +
    scale_fill_brewer(palette = 'Set1') + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
          plot.title = element_text(size = 10),
          axis.text.y = element_text(size = 5),
          axis.title.y = element_text(size = 6),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, 'lines'),
          legend.margin = margin(0, 0, 0, 0))
}
```
")
      
      report_content <- paste0(header, pitching_section)
    }

return(report_content)
  }

# ----------------------------------------
# Download Handlers
# ----------------------------------------

# PDF Download handler
output$downloadPDF <- downloadHandler(
  filename = function() {
    player_name <- gsub(" ", "_", selected_player()$player_name)
    paste0(player_name, "_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    # Create the temporary Rmd file
    rmd_content <- generate_report_content()
    writeLines(rmd_content, report_file())
    
    # Render the PDF
    tryCatch({
      rmarkdown::render(report_file(), output_file = file, quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error generating PDF:", e$message), type = "error")
    })
  }
)

# Download buttons UI
output$download_buttons_ui <- renderUI({
  # Only show download buttons if a player has been selected and pitch data exists
  req(selected_player(), pitch_data())
  
  div(style = "margin-bottom: 15px;",
      dropdown(
        inputId = "downloadOptions",
        label = "Download Report",
        icon = icon("download"), 
        status = "primary",
        size = "sm",
        style = "unite",
        width = "300px",
        downloadButton(
          "downloadPDF", 
          "Download PDF Report",
          style = "width: 100%; background-color: #003366; color: white; font-weight: bold;"
        )
      )
  )
})
}

# ----------------------------------------
# Run the Application
# ----------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
