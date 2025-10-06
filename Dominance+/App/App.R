# Libraries ####
library(tidyverse)
library(DT)
library(shiny)

# Data ####
dominance_pitch <- read_csv("pitch_dominance.csv") %>% 
  select(-xwOBA:-yr_avg, -ab, -put_away, -`gDominance+`) %>% 
  rename(Name = player_name,
         Season = game_year,
         Pitch = pitch_type,
         Pitches = pitches,
         "Whiff" = whiff,
         "IZ Whiff" = iz_whiff,
         "Chase %" = chase_rate,
         "Hard Hit" = hard_hit,
         "Paint %" = paint_rate,
         "IZ Take" = iz_take) %>% 
  mutate(Pitch = case_when(Pitch == "CH" ~ "Changeup",
                           Pitch == "FS" ~ "Splitter",
                           Pitch == "FC" ~ "Cutter",
                           Pitch == "CU" ~ "Curveball",
                           Pitch == "SL" ~ "Slider",
                           Pitch == "ST" ~ "Sweeper",
                           Pitch == "FF" ~ "4-Seam Fastball",
                           Pitch == "FO" ~ "Forkball",
                           Pitch == "SI" ~ "Sinker",
                           Pitch == "SV" ~ "Slurve",
                           Pitch == "KN" ~ "Knuckleball",
                           Pitch == "SC" ~ "Screwball"))

dominance_pitcher <- read_csv("pitcher_dominance.csv") %>% 
  select(player_name, game_year, IP, pitches, ERA, FIP, xERA, `Stuff+`, WAR, `Dominance+`) %>% 
  rename(Name = player_name, 
         Season = game_year,
         Pitches = pitches)

# ---- UI ----
ui <- fluidPage(
  # Apply Times New Roman globally
  tags$head(
    tags$style(HTML("
      * {
        font-family: 'Times New Roman', Times, serif;
      }
      table.dataTable th, table.dataTable td {
        text-align: center !important;
      }
      .dataTables_filter, .dataTables_length, .dataTables_info, .dataTables_paginate {
        font-family: 'Times New Roman', Times, serif;
      }
    "))
  ),
  
  titlePanel("Dominance+ Leaderboard"),
  
  tabsetPanel(
    # ---- TAB 1 ----
    tabPanel("Pitcher Dominance+",
             br(),
             fluidRow(
               column(12, align = "center",
                      checkboxInput("show_traditional", "Show Stats", TRUE)
               )
             ),
             br(),
             fluidRow(
               column(12,
                      DTOutput("pitcher_table")
               )
             )
    ),
    
    # ---- TAB 2 ----
    tabPanel("Pitch-Level Dominance+",
             br(),
             fluidRow(
               column(12, align = "center",
                      checkboxGroupInput("pitch_class", "",
                                         choices = c("Fastballs" = "fastball",
                                                     "Breaking Balls" = "breaking",
                                                     "Changeups" = "changeup"),
                                         selected = c("fastball", "breaking", "changeup"),
                                         inline = TRUE)
               )
             ),
             br(),
             fluidRow(
               column(12,
                      DTOutput("pitch_table")
               )
             )),
    # ---- TAB 3 ----
    tabPanel("About",
             br(),
             h4("Dominance+ is a statistic that combines whiff rate, in-zone whiff rate, chase rate, hard hit rate, paint rate, and in zone take rate to predict xwOBA using an XGBoost model. The methods and background are detailed in this blog post: ", 
                a("link", href = "https://mlb.com"))
             
    )
  )
)


# ---- SERVER ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- dominance_pitcher
    
    if (input$show_traditional) {
      data <- data %>% select(Name, Season, IP, Pitches, ERA, FIP, xERA, WAR, `Stuff+`, `Dominance+`)
    } else {
      data <- data %>% select(Name, Season, IP, Pitches, `Dominance+`)
    }
    
    data$Season <- as.integer(data$Season)
    data
  })
  
  output$pitcher_table <- renderDT({
    data_to_display <- filtered_data()
    season_col_index <- which(names(data_to_display) == "Season") - 1
    
    dt <- datatable(
      data_to_display,
      options = list(
        pageLength = 20,
        autoWidth = FALSE,
        dom = "tpl",
        columnDefs = list(list(targets = season_col_index, type = "string"))
      ),
      filter = "top",
      rownames = FALSE
    )
    
    if (input$show_traditional) {
      dt <- dt %>%
        formatRound(columns = c("ERA", "FIP", "xERA", "Dominance+"), digits = 2) %>%
        formatRound(columns = "WAR", digits = 1)
    } else {
      dt <- dt %>%
        formatRound(columns = "Dominance+", digits = 2)
    }
    dt
  })
  
  # ---- Pitch Filter Reactive ----
  filtered_pitch_data <- reactive({
    data <- dominance_pitch
    
    if (!is.null(input$pitch_class) && length(input$pitch_class) > 0) {
      
      fastballs <- c("4-Seam Fastball", "Sinker", "Cutter")
      breaking  <- c("Slider", "Sweeper", "Curveball", "Slurve", "Screwball", "Knuckleball")
      changeups <- c("Changeup", "Splitter", "Forkball")
      
      # Pitch Type Vector
      selected_pitches <- c()
      if ("fastball" %in% input$pitch_class)
        selected_pitches <- c(selected_pitches, fastballs)
      if ("breaking" %in% input$pitch_class)
        selected_pitches <- c(selected_pitches, breaking)
      if ("changeup" %in% input$pitch_class)
        selected_pitches <- c(selected_pitches, changeups)
      
      # Filter Selected Pitches
      data <- data %>% filter(Pitch %in% selected_pitches)
    }
    data
  })
  
  output$pitch_table <- renderDT({
    filtered <- filtered_pitch_data()
    season_col_index2 <- which(names(filtered) == "Season") - 1
    
    dt <- datatable(
      filtered,
      options = list(
        pageLength = 20,
        autoWidth = FALSE,
        dom = "tpl",
        columnDefs = list(list(targets = season_col_index2, type = "string"))
      ),
      filter = "top",
      rownames = FALSE
    ) %>%
      formatPercentage(columns = c("Whiff", "IZ Whiff", "Chase %", "Hard Hit", "Paint %", "IZ Take"), digits = 1) %>%
      formatRound(columns = "Dominance+", digits = 2)
    
    dt
  })
}

# ---- RUN APP ----
shinyApp(ui, server)
