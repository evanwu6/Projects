library(tidyverse)
library(shiny)
library(shinyjs)

# Main Data
data <- read_csv("AppData.csv") %>% 
  select(-...1) %>% 
  mutate(pitch_name = case_when(pitch_type == "FF" ~ "4-Seam",
                                pitch_type == "FC" ~ "Cutter",
                                pitch_type == "SI" ~ "Sinker",
                                pitch_type == "SL" ~ "Slider",
                                pitch_type == "CU" ~ "Curveball",
                                pitch_type == "CH" ~ "Change Up",
                                pitch_type == "ST" ~ "Sweeper",
                                pitch_type == "FS" ~ "Splitter",
                                pitch_type == "SV" ~ "Slurve")) %>% 
  mutate(p_hand = ifelse(handedness == "R_R" | handedness == "R_L", "Right", "Left"),
         b_hand = ifelse(handedness == "R_R" | handedness == "L_R", "Right", "Left")) %>% 
  mutate(zone = as.factor(zone)) %>% 
  mutate(F_Runs_Col = case_when(F_Runs <= 5 & F_Runs >= -5 ~ F_Runs,
                                F_Runs > 5 ~ 5,
                                F_Runs < 5~ -5))

# Zone Location Info
zone_data <- data.frame(
  zone = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  x = c(-0.7083, 0, 0.7083, -0.7083, 0, 0.7083, -0.7083, 0, 0.7083),
  y = c(-0.7083, -0.7083, -0.7083, 0, 0, 0, 0.7083, 0.7083, 0.7083)) %>% 
  mutate(zone = as.factor(zone))

extra_zones <- data.frame(
  zone = c(11, 12, 13, 14),
  x = c(-0.7083, 0.7083, -0.7083, 0.7083),
  y = c(0.7083, 0.7083, -0.7083, -0.7083)
) %>% 
  mutate(zone = as.factor(zone))

full_zone_data <- rbind(zone_data, extra_zones)



ui <- fluidPage(
  
  titlePanel("Diamond Dollars 2024 - FRADER"),
  
  tabPanel("F Runs",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectizeInput(inputId = "pitch_type",
                                label = "Pitch Type",
                   choices = c("4-Seam", "Cutter", "Sinker", "Slider", "Curveball", 
                               "Change Up", "Sweeper", "Splitter", "Slurve"),
                   multiple = FALSE,
                   selected = c("4-Seam")
                 ),
                 
                 selectizeInput(inputId = "pitcher_hand",
                                label = "Pitcher Hand",
                                choices = c("Right", "Left"),
                                multiple = FALSE,
                                selected = c("Right")
                 ),
                
                 selectizeInput(inputId = "batter_hand",
                                label = "Batter Hand",
                                choices = c("Right", "Left"),
                                multiple = FALSE,
                                selected = c("Right")
                 ),
                 
                 numericInput(inputId = "pitch_speed",
                              label = "Pitch Speed",
                              min = 0,
                              max = 104,
                              value = 94,
                              step = 1),
                 textOutput("names")
               ),
               
               mainPanel(
                 textOutput("error_text"),
                 textOutput("error_suggestion"),
                 plotOutput("zone"),
               )
             )
    ),
  
  
) # ui Fluid end


server <- function(input, output) {
  

output$error_text <- renderText({

  loc_g <- data %>% 
    filter(pitch_name == input$pitch_type) %>% 
    filter(pitch_speed == input$pitch_speed) %>% 
    filter(p_hand == input$pitcher_hand) %>% 
    filter(b_hand == input$batter_hand) %>% 
    left_join(full_zone_data, join_by(zone))
  
  message <- ""
  
  if(nrow(loc_g) < 13){
    
message <- "Selected inputs do not have enough observations in the 2023 season."
    
  }
  
})



output$error_suggestion <- renderText({
  
  loc_g <- data %>% 
    filter(pitch_name == input$pitch_type) %>% 
    filter(pitch_speed == input$pitch_speed) %>% 
    filter(p_hand == input$pitcher_hand) %>% 
    filter(b_hand == input$batter_hand) %>% 
    left_join(full_zone_data, join_by(zone))
  
limits <- data %>% 
  group_by(pitch_name, p_hand, b_hand, pitch_speed) %>% 
  summarize(obs = n()) %>% 
  filter(obs == 13) %>% 
  ungroup() %>% 
  filter(pitch_name == input$pitch_type) %>% 
  filter(p_hand == input$pitcher_hand) %>% 
  filter(b_hand == input$batter_hand) %>% 
  summarize(min = min(pitch_speed), max = max(pitch_speed))
  
  message <- ""
  
  if(nrow(loc_g) < 13){
    
    message <- paste0("For ", input$pitcher_hand, "-on-", input$batter_hand,
                      " ", input$pitch_type, "s, try a speed between ", limits$min,
                      " and ", limits$max, " mph.")
    
  }
  
})
  
  
output$zone <- renderPlot({
  
  # Zone Graph
  
  loc_g <- data %>% 
    filter(pitch_name == input$pitch_type) %>% 
    filter(pitch_speed == input$pitch_speed) %>% 
    filter(p_hand == input$pitcher_hand) %>% 
    filter(b_hand == input$batter_hand) %>% 
    left_join(full_zone_data, join_by(zone))

  loc_g_extra <- loc_g %>% 
    filter(zone %in% 11:14) %>% 
    mutate(x = ifelse(x > 0, x + 0.5, x - 0.5),
           y = ifelse(y > 0, y + 0.5, y - 0.5))
  
  if(count(loc_g) < 13) {
    loc_g <- data.frame(
      pitch_type = rep(input$pitch_type, 13),
      pitch_speed = rep(input$pitch_speed, 13),
      zone = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14),
      x = c(-0.7083, 0, 0.7083, -0.7083, 0, 0.7083, -0.7083, 0, 0.7083, -0.7083, 0.7083, -0.7083, 0.7083),
      y = c(-0.7083, -0.7083, -0.7083, 0, 0, 0, 0.7083, 0.7083, 0.7083, 0.7083, 0.7083, -0.7083, -0.7083))
  }
  
  ggplot() +
    geom_rect(data = loc_g %>% filter(zone  %in% 11:14),
              aes(xmin = x - 0.75, xmax = x + 0.75, ymin = y - 0.75, ymax = y + 0.75,
                  fill = F_Runs_Col), color = "black", size = 0.5, show.legend = FALSE) +
    geom_text(data = loc_g_extra,
              aes(x = x, y = y, label = round(F_Runs, 2)),
              color = "black", size = 5, show.legend = FALSE,
              family = "Times", fontface = "bold") +
    geom_tile(data = loc_g %>% filter(zone %in% 1:9),
              aes(x = x, y = y, fill = F_Runs_Col),
              color = "black", size = 0.25, show.legend = FALSE) +
    geom_text(data = loc_g %>% filter(zone %in% 1:9),
              aes(x = x, y = y, label = round(F_Runs, 2)),
              color = "black", size = 5, show.legend = FALSE,
              family = "Times", fontface = "bold") +
    theme_void() +  # Remove axis and background
    geom_polygon(data = data.frame(x = c(-0.708, 0.7083, 0.7083, 0, -0.7083),
                                   y = c(-2, -2, -2.25, -2.5, -2.25)), 
                 aes(x = x, y = y), fill = "white", color = "black", size = 0.5) +  # Home plate
    scale_fill_gradient2(low = "cornflowerblue", midpoint = 0, high = "firebrick",
                         limits = c(-5, 5)) +
    coord_fixed() +
    theme(strip.text = element_text(size = 5, hjust = 0.5)) +
    labs(title = "F Runs by Zone",
         subtitle = paste(input$pitch_speed, " MPH ", input$pitch_type),
         caption = "+ F Runs = Good Foul Ball For Hitter 
         
         - F Runs = Good Foul Ball For Pitcher") +
    theme(plot.title = element_text(hjust = 0.5, 
                                    family = "Times", face = "bold", size = 25),
          plot.subtitle = element_text(hjust = 0.5, 
                                    family = "Times", face = "italic", size = 20)) +
    NULL
  
})



output$names <- renderText({
  
  "Created by Elon SABR: Tobias Coker, Tristan Hiestand, George Lyche, Alex Wigder and Evan Wu"
  
})

  

}

shinyApp(ui = ui, server = server)