# Libraries ####
library(tidyverse)
library(patchwork)
library(knitr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(kableExtra)
library(magick)
library(gridExtra)
library(grid)
library(ggforce)
library(sportyR)
library(GeomMLBStadiums)

# Last Updated

update_date <- "04_29"    # Will run this date and beyond

update_date <- update_date %>% str_replace("_", "/") %>% paste0("2025/", .) %>% as.Date()

# Creating Functions ####

# Colors
palettes <- list(
  "Normal" = c("#000000", "#E69F00", "#56B4E9", "#0072B2", 
               "#F0E442", "#009E73", "#D55E00", "#CC79A7"),
  "Set1" = brewer.pal(9, "Set1"),
  "Set2" = brewer.pal(8, "Set2"),
  "Set3" = brewer.pal(12, "Set3"))

# ggplot Functions
geom_zone <- function(top = 3.4, bottom = 1.5, linecolor = "black", size = 0.75){
  geom_rect(xmin = -.7083, xmax = .7083, ymin = bottom, ymax = top,
            alpha = 0, color = linecolor, linewidth = size)
}



geom_plate <- function(pov = "pitcher", size = 1){
  df <- case_when(
    pov == "pitcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, .25, .5, .25)),
    pov == "catcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, -.25, -.5, -.25)))
  g <- geom_polygon(data = df, aes(x = x, y = y), fill = "white", color = "black", linewidth = size)
  g
}


scale_color_trackman <- function(colors = "normal"){
  scale_color_manual(values = c("Fastball" = "black", "Cutter" = "purple", 
                                "Sinker" = "red", "Slider" = "blue",
                                "ChangeUp" = "chartreuse3", "Curveball" = "darkorange",
                                "Splitter" = "darkturquoise"))
}

scale_fill_trackman <- function(colors = "normal"){
  scale_fill_manual(values = c("Fastball" = "black", "Cutter" = "purple", 
                               "Sinker" = "red", "Slider" = "blue",
                               "ChangeUp" = "chartreuse3", "Curveball" = "darkorange",
                               "Splitter" = "darkturquoise"))
}

# Heat Map Colors
heat_colors <- c(
  "#F7FBFF",
  "#F7FBFF",
  "#C6DBEF",
  "#9ECAE1",
  "#6BAED6",
  "#4292C6",
  "#2171B5",
  "#CB181D",
  "#A50F15",
  "#4D004A" 
)

# Tilt Function
S2HM <- function(seconds) {
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  minutes_str <- sprintf("%02d", minutes)  # Add leading zero if needed
  return(paste0(hours, ":", minutes_str))
}

# Radians
rad <- function(degrees) {
  radians <- degrees * (pi / 180)
  return(radians)
}

# Name Fixer
swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}

find_percentile_xwOBA <- function(data, value) {
  if (value <= min(data$xwOBA)) {
    return(0)
  } else if (value >= max(data$xwOBA)) {
    return(100)
  } else {
    lower <- max(data[data$xwOBA <= value, "Percentile"])
    upper <- min(data[data$xwOBA > value, "Percentile"])
    
    lower_value <- data[data$Percentile == lower, "xwOBA"]
    upper_value <- data[data$Percentile == upper, "xwOBA"]
    
    # Linear interpolation
    percentile <- lower + (value - lower_value) / (upper_value - lower_value) * (upper - lower)
    
    return(percentile)
  }
}


convert_date_format <- function(date_string) {
  date_string <- gsub("_", "/", date_string)
  date_string <- paste(date_string, format(Sys.Date(), "%Y"), sep = "/")
  date <- mdy(date_string)
  month_name <- toupper(month(date, label = TRUE, abbr = TRUE))
  day <- day(date)
  day_suffix <- ifelse(day %in% c(11, 12, 13), "TH",
                       ifelse(day %% 10 == 1, "ST",
                              ifelse(day %% 10 == 2, "ND",
                                     ifelse(day %% 10 == 3, "RD", "TH"))))
  formatted_date <- paste0(month_name, " ", day, day_suffix)
  return(formatted_date)
}

is.barrel <- function(LA, EV){
  EV <- EV + 5
  upper <- 1.11*EV - 78.89
  lower <- -EV + 124
  outcome <- (LA >= lower) & (LA <= upper) & (EV >= 98) & (LA >= 8) & (LA <= 50)
  outcome <- replace_na(outcome, FALSE)
  outcome
}


# Pitch Order
pitch_order <- c("Fastball", "Sinker", "Cutter", "Slider", "Curveball", "ChangeUp", "Splitter")

# Data ####
data <- read_csv(paste0("Example Data.csv")) %>%
  mutate(date = substr(GameID, 1, 8),
         date = as.character(date),
         date = as.Date(date, format = "%Y%m%d"))

# Barrel / Swing
data <- data %>% 
  mutate(Barrel = ifelse(Angle >= 10 & Angle <= 30 & ExitSpeed >= 95, 1, 0)) %>% 
  mutate(swing = ifelse(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable",
                                         "InPlay", "StrikeSwinging"), 1, 0))

# Pitch Categories
data <- data %>% 
  mutate(pitch = case_when(TaggedPitchType  %in% c("Fastball", "Sinker", "FourSeamFastBall",
                                                 "TwoSeamFastball", "OneSeamFastBall") ~ "Fastball",
                         TaggedPitchType %in% c("Slider", "Curveball", "Cutter", "Sweeper") ~ "Breaking",
                         TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "Offspeed",
                         TRUE ~ "Other"))

# Uniform Batted Balls
data <- data %>% 
  mutate(TaggedHitType = case_when(TaggedHitType == "Undefined" ~ "Undefined",
                                   Angle < 6 ~ "GroundBall",
                                   Angle < 25 ~ "LineDrive",
                                   Angle < 50 ~ "FlyBall",
                                   Angle >= 50 ~ "Popup"))

# xwOBA
xwOBA_Value <- read_csv("xwOBA.csv") %>% 
  rename("rExitSpeed" = ExitSpeed,
         "rAngle" = Angle)

data <- data %>% 
  mutate(rExitSpeed = round(ExitSpeed, 0),
         rAngle = round(Angle, 0)) %>% 
  left_join(xwOBA_Value, by = c("rAngle", "rExitSpeed")) %>% 
  mutate(xwOBA = case_when(PitchCall == "InPlay" ~ xwOBA,
                           KorBB == "Walk" ~ 0.69,
                           KorBB == "Strikeout" ~ 0,
                           PitchCall == "HitByPitch" ~ 0.72,
                           TRUE ~ NA))




# Edge / Heart
data <- data %>% 
  mutate(in_edge = ifelse(PlateLocSide >= (-0.7083 - 0.1208) & 
                            PlateLocSide <= (0.7083 + 0.1208) &
                            PlateLocHeight >= (1.5 - 0.1208) & 
                            PlateLocHeight <= (3.4 + 0.1208),
                          1, 0)) %>% 
  mutate(in_heart = ifelse(PlateLocSide >= (-0.7083 + 0.2416667) & 
                            PlateLocSide <= (0.7083 - 0.2416667) &
                            PlateLocHeight >= (1.5 + 0.2416667) & 
                            PlateLocHeight <= (3.4 - 0.2416667),
                          1, 0)) %>% 
  mutate(in_edge = ifelse(in_heart == 1, 0, in_edge))


data <- data %>% 
  mutate(Zone = case_when(
    PlateLocSide <= (-17/72) & PlateLocSide >= (-39/48) & PlateLocHeight >= 2.77 & PlateLocHeight <= 3.4 ~ 3,
    PlateLocSide >= (-17/72) & PlateLocSide < (17/72) & PlateLocHeight >= 2.77 & PlateLocHeight <= 3.4 ~ 2,
    PlateLocSide >= (17/72) & PlateLocSide <= (39/48) & PlateLocHeight >= 2.77 & PlateLocHeight <= 3.4 ~ 1,
    
    PlateLocSide <= (-17/72) & PlateLocSide >= (-39/48) & PlateLocHeight >= 2.133 & PlateLocHeight <= 2.77 ~ 6,
    PlateLocSide >= (-17/72) & PlateLocSide < (17/72) & PlateLocHeight >= 2.133 & PlateLocHeight <= 2.77 ~ 5,
    PlateLocSide >= (17/72) & PlateLocSide <= (39/48) & PlateLocHeight >= 2.133 & PlateLocHeight <= 2.77 ~ 4,
    
    PlateLocSide <= (-17/72) & PlateLocSide >= (-39/48) & PlateLocHeight <= 2.133 & PlateLocHeight >= 1.5 ~ 9,
    PlateLocSide >= (-17/72) & PlateLocSide < (17/72) & PlateLocHeight <= 2.133 & PlateLocHeight >= 1.5 ~ 8,
    PlateLocSide >= (17/72) & PlateLocSide <= (39/48) & PlateLocHeight <= 2.133 & PlateLocHeight >= 1.5 ~ 7,
    
    PlateLocSide >= 0 & PlateLocSide <= (121/48) & PlateLocHeight >= 2.5 & PlateLocHeight <= (50/12) ~ 11,
    PlateLocSide <= 0 & PlateLocSide >= (-121/48) & PlateLocHeight >= 2.5 & PlateLocHeight <= (50/12) ~ 12,
    PlateLocSide >= 0 & PlateLocSide <= (121/48) & PlateLocHeight <= 2.5 & PlateLocHeight >= (10/12) ~ 13,
    PlateLocSide <= 0 & PlateLocSide >= (-121/48) & PlateLocHeight <= 2.5 & PlateLocHeight >= (10/12) ~ 14,
    TRUE ~ 15)) %>% 
  mutate(Zone = as.factor(Zone))

# Zone Graph Placement Data
zone_data <- data.frame(
  Zone = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  x = c(-0.7083, 0, 0.7083, -0.7083, 0, 0.7083, -0.7083, 0, 0.7083),
  y = c(-0.7083, -0.7083, -0.7083, 0, 0, 0, 0.7083, 0.7083, 0.7083)
) %>% 
  mutate(Zone = as.factor(Zone))

extra_zones <- data.frame(
  Zone = c(11, 12, 13, 14),
  x = c(-0.7083, 0.7083, -0.7083, 0.7083),
  y = c(0.7083, 0.7083, -0.7083, -0.7083)
) %>% 
  mutate(Zone = as.factor(Zone))

full_zone_data <- rbind(zone_data, extra_zones)


# First Pitch
data <- data %>% 
  mutate(First = case_when(
    Balls == 0 & Strikes == 0 ~ 1,
    TRUE ~ 0
  ))

# Preparing Loop ####


# Table Themes
custom_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = "white"),
    fg_params = list(fontsize = 12, fontfamily = "Times New Roman"),
    hline = gpar(col = "black", lwd = 1),
    vline = gpar(col = "black", lwd = 1)),
  colhead = list(
    fg_params = list(fontsize = 13, fontface = "bold", fontfamily = "Times New Roman"),
    hline = gpar(col = "black", lwd = 1)))

# Creating Reference Database
summary_data <- data %>% 
  filter(BatterTeam == "ELO_PHO",
         date >= update_date) %>% 
  distinct(Batter) %>% 
  mutate(TitleName = sapply(Batter, swap_names),
         FileName = Batter,
         FileName = str_replace(FileName, ", ", "")) %>% 
  rename(ProperName = Batter)

# Loop ####
# FOR LOOP STARTS HERE #

for (i in 1:nrow(summary_data)) {
# for (i in 17:nrow(summary_data)) {
# for (i in c(1)) {

name_loop <- subset(summary_data[i, 1])
name_loop <- name_loop$ProperName[1]

# Creating Titles
name_title <- summary_data %>% filter(ProperName == name_loop)
name_title <- name_title$TitleName[1]
name_file <- summary_data %>% 
  filter(ProperName == name_loop) %>% 
  mutate(ProperName = ifelse(ProperName == "TIffan, Trey", "TIffan, Trey", ProperName))
name_file <- name_file$FileName[1]

# Creating Graphics ####

batter_data <- data %>% 
  filter(Batter == name_loop)


# Location Heat Map
graph_location <- batter_data %>%
  mutate(pitch = factor(pitch, levels = c("Fastball", "Breaking", "Offspeed"))) %>% 
  ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         bins = 10,
                         alpha = 0.8,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "catcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  theme_bw() +
  facet_wrap(~ pitch) +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(family = "Times New Roman", size = 20),
        strip.background = element_blank())

file_graph_location <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_location.png")
ggsave(filename = file_graph_location, plot = graph_location, 
       width = 9, height = 4.1, dpi = 300)


# Damage
barrel_count <- filter(batter_data, Barrel == 1) %>% nrow()
if(barrel_count > 1) {
graph_barrels <- batter_data %>%
  filter(Barrel == 1, PlateLocSide >= -2.25, PlateLocSide <= 2.25) %>% 
  ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "catcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
}

if(barrel_count <= 1 | is.na(barrel_count)) {
  graph_barrels <- batter_data %>%
    filter(Barrel == 1) %>% 
    ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
    geom_zone() +
    geom_point(color = "#4D004A", size = 2) +
    geom_plate(pov = "catcher") +
    xlim(-2, 2) +
    ylim(-0.5, 5) +
    coord_fixed() +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

file_graph_barrels <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_barrels.png")
ggsave(filename = file_graph_barrels, plot = graph_barrels, 
       width = 3.25, height = 4.15, dpi = 300) 


# Swing
swing_count <- filter(batter_data, swing == 1) %>% nrow()

if(swing_count > 1) {
graph_swings <- batter_data %>%
  filter(swing == 1) %>% 
  ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "catcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
}

if(swing_count <= 1 | is.na(swing_count)) {
  graph_swings <- batter_data %>%
    filter(swing == 1) %>% 
    ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
    geom_zone() +
    geom_point(color = "#4D004A", size = 2) +
    geom_plate(pov = "catcher") +
    xlim(-2, 2) +
    ylim(-0.5, 5) +
    coord_fixed() +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

file_graph_swings <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_swings.png")
ggsave(filename = file_graph_swings, plot = graph_swings, 
       width = 3.25, height = 4.15, dpi = 300) 

  
# Whiffs
whiff_count <- filter(batter_data, PitchCall == "StrikeSwinging") %>% nrow()
if(whiff_count > 1) {
graph_whiffs <- batter_data %>%
  filter(PitchCall == "StrikeSwinging") %>%
  ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "catcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
}

if(whiff_count <= 1 | is.na(whiff_count)) {
    graph_whiffs <- batter_data %>%
      filter(PitchCall == "StrikeSwinging") %>%
      ggplot(aes(x = -PlateLocSide, y = PlateLocHeight)) +
      geom_zone() +
      geom_point(color = "#4D004A", size = 2) +
      geom_plate(pov = "catcher") +
      xlim(-2, 2) +
      ylim(-0.5, 5) +
      coord_fixed() +
      theme_bw() +
      theme(text = element_text(family = "Times New Roman"),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  }

file_graph_whiffs <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_whiffs.png")
ggsave(filename = file_graph_whiffs, plot = graph_whiffs, 
       width = 3.25, height = 4.15, dpi = 300) 


# Hard Hit Spray Chart
graph_sprayhh <- batter_data %>% 
  mutate(xwOBA = ifelse(xwOBA > 1, 1, xwOBA)) %>% 
  filter(ExitSpeed >= 95) %>% 
  mutate(hc_x = sin(rad(Bearing))*Distance,
         hc_y = cos(rad(Bearing))*Distance) %>%
  ggplot(aes(x = hc_x, y = hc_y,
             color = xwOBA)) +
  geom_mlb_stadium(stadium_ids = "braves", 
                   stadium_transform_coords = TRUE,
                   stadium_segments = c("home_plate", "infield_inner",
                                        "infield_outer", "outfield_inner")) + 
  coord_fixed() +
  geom_point() +
  scale_color_gradient2(low = "gray90", high = "red",
                       limits = c(0, 1)) +
  # geom_segment(aes(xend = 0, yend = 0), alpha = 0.5, show.legend = FALSE) +
  theme_void() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

file_graph_sprayhh <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_sprayhh.png")
ggsave(filename = file_graph_sprayhh, plot = graph_sprayhh, 
       width = 7, height = 6, dpi = 300) 


# Decisions Table
# Heart
table_heart <- batter_data %>% 
  mutate(whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0)) %>%
  filter(in_heart == 1) %>% 
  summarize("Hrt Swg" = mean(swing, na.rm = TRUE),
            "Hrt Whf" = sum(whiff, na.rm = TRUE)/sum(swing, na.rm = TRUE))

# Edge
table_edge <- batter_data %>% 
  mutate(whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0)) %>%
  filter(in_edge == 1) %>% 
  summarize("Edge Swg" = mean(swing, na.rm = TRUE),
            "Edge Whf" = sum(whiff, na.rm = TRUE)/sum(swing, na.rm = TRUE))

# Chase & 2K Chase
table_chase <- batter_data %>% 
  filter(in_edge == 0, in_heart == 0) %>% 
  summarize("Chase" = mean(swing, na.rm = TRUE),
            "2K Chase" = sum(swing == 1 & Strikes == 2, na.rm = TRUE)/sum(Strikes == 2, na.rm = TRUE))

table_decision <- cbind(table_heart, table_edge, table_chase)
table_decision[is.na(table_decision)] <- 0


# Contact Table
# Contact %
table_contact1 <- batter_data %>% 
  filter(swing == 1) %>% 
  summarize(Contact = mean(PitchCall == "InPlay", na.rm = TRUE))

# FB In-Zone Miss
table_contact2 <- batter_data %>% 
  mutate(whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0)) %>%
  filter(in_edge == 1 | in_heart == 1) %>% # in zone
  filter(pitch == "Fastball") %>% 
  summarize("FB IZM" = mean(whiff, na.rm = TRUE))

# Out of Zone Miss
table_contact3 <- batter_data %>% 
  mutate(whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0)) %>%
  filter(in_edge == 1 | in_heart == 1) %>% # in zone
  filter(pitch %in% c("Breaking", "Offspeed")) %>% 
  summarize("OS IZM" = mean(whiff, na.rm = TRUE))

# Heart In Play
table_contact4 <- batter_data %>% 
  filter(in_heart == 1) %>%
  summarize("Hrt BIP" = mean(PitchCall == "InPlay", na.rm = TRUE))

table_contact <- cbind(table_contact1, table_contact2, table_contact3, table_contact4)
table_contact[is.na(table_contact)] <- 0


# Ball Flight
table_ball_flight <- batter_data %>% 
  filter(PitchCall == "InPlay") %>% 
  summarize(GB = mean(TaggedHitType == "GroundBall", na.rm = TRUE),
            LD = mean(TaggedHitType == "LineDrive", na.rm = TRUE),
            FB = mean(TaggedHitType == "FlyBall", na.rm = TRUE),
            POP = mean(TaggedHitType == "Popup", na.rm = TRUE),
            "LA 10-30" = mean(Angle >= 10 & Angle <= 30, na.rm = TRUE))

table_ball_flight[is.na(table_ball_flight)] <- 0


# Exit Velo
hh_count <- filter(batter_data, ExitSpeed >= 95) %>% nrow()
table_ev <- batter_data %>% 
  summarize("HH LA" = mean(if_else(ExitSpeed >= 95, Angle, NA_real_), na.rm = TRUE),
            "HH/Swg" = sum(ExitSpeed >= 95, na.rm = TRUE)/sum(swing, na.rm = TRUE),
            "HH%" = sum(ExitSpeed >= 95, na.rm = TRUE)/sum(PitchCall == "InPlay", na.rm = TRUE))

table_ev[is.na(table_ev)] <- 0

table_ev <- table_ev %>% 
  mutate(`HH LA` = ifelse(hh_count > 0, `HH LA`, NA))


# Table Contact & Ball Flight
table1 <- cbind(table_contact, table_ball_flight) %>% 
  mutate(across(everything(), percent)) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table1 <- ggplot() + 
  annotation_custom(table1) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank())

file_table1 <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "table1.png")
ggsave(filename = file_table1, plot = table1, 
       width = 7.5, height = 0.75, dpi = 300) 


# Table Decision & Exit Velocity
table2 <- cbind(table_decision, table_ev) %>% 
  mutate(across(-`HH LA`, percent),
         `HH LA` = paste0(as.character(round(`HH LA`, 1)), "\u00B0")) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table2 <- ggplot() + 
  annotation_custom(table2) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank())

file_table2 <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "table2.png")
ggsave(filename = file_table2, plot = table2, 
       width = 7.5, height = 0.75, dpi = 300) 


# Exit Velocity Spray Chart
graph_sprayev <- batter_data %>% 
  mutate(is_hr = ifelse(PlayResult == "HomeRun", "HR", "No HR"),
         ExitSpeed = case_when(ExitSpeed > 105 ~ 105,
                               ExitSpeed < 40 ~ 40,
                               TRUE ~ ExitSpeed)) %>% 
  filter(PitchCall == "InPlay") %>% 
  mutate(hc_x = sin(rad(Bearing))*Distance,
         hc_y = cos(rad(Bearing))*Distance) %>%
  ggplot(aes(x = hc_x, y = hc_y,
             color = ExitSpeed,
             shape = is_hr,
             size = is_hr)) +
  geom_mlb_stadium(stadium_ids = "braves", 
                   stadium_transform_coords = TRUE,
                   stadium_segments = c("home_plate", "infield_inner",
                                        "infield_outer", "outfield_inner")) + 
  coord_fixed() +
  geom_point() +
  scale_color_gradient2(low = "gray90", high = "red", limits = c(40, 105)) +
  scale_shape_manual(values = c("No HR" = 20, "HR" = 18), guide = "none") +
  scale_size_manual(values = c("No HR" = 2, "HR" = 2.75), guide = "none") +
  labs(color = "Exit Velo") +
  theme_void() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

file_graph_sprayev <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_sprayev.png")
ggsave(filename = file_graph_sprayev, plot = graph_sprayev, 
       width = 7, height = 6, dpi = 300) 

# Table Batter Splits
table_splits <- batter_data %>% 
  filter(!is.na(PitcherThrows)) %>% 
  mutate(PA = case_when(PitchCall == "InPlay" ~ 1,
                           KorBB == "Walk" ~ 1,
                           KorBB == "Strikeout" ~ 1,
                           PitchCall == "HitByPitch" ~ 1,
                           TRUE ~ 0),
         AVG = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         SLG = case_when(PlayResult == "Single" ~ 1,
                         PlayResult == "Double" ~ 2,
                         PlayResult == "Triple" ~ 3,
                         PlayResult == "HomeRun" ~ 4,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         OBP = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                         KorBB == "Walk" ~ 1,
                         PitchCall == "HitByPitch" ~ 1,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         swing = ifelse(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable",
                                                "InPlay", "StrikeSwinging"), 1, 0)) %>% 
  group_by(PitcherThrows) %>% 
  summarize(PA = sum(PA, na.rm = TRUE),
            AVG = mean(AVG, na.rm = TRUE),
            OBP = mean(OBP, na.rm = TRUE),
            SLG = mean(SLG, na.rm = TRUE),
            OPS = mean(OBP, na.rm = TRUE) + mean(SLG, na.rm = TRUE),
            xwOBA = mean(xwOBA, na.rm = TRUE),
            `Whiff%` = sum(PitchCall == "StrikeSwinging", na.rm = TRUE) / sum(swing, na.rm = TRUE),
            `HH%` = sum(ExitSpeed >= 95, na.rm = TRUE) / sum(PitchCall == "InPlay"),
            `Barrel%` = sum(Barrel, na.rm = TRUE) / sum(PitchCall == "InPlay")) %>% 
  rename(Pitcher = PitcherThrows) %>% 
  mutate(AVG = round(AVG, 3),
         AVG = formatC(AVG, format = "f", digits = 3),
         OBP = round(OBP, 3),
         OBP = formatC(OBP, format = "f", digits = 3),
         SLG = round(SLG, 3),
         SLG = formatC(SLG, format = "f", digits = 3),
         xwOBA = round(xwOBA, 3),
         OPS = formatC(OPS, format = "f", digits = 3),
         `Whiff%` = percent(`Whiff%`, accuracy = 0.1),
         `HH%` = percent(`HH%`, accuracy = 0.1),
         `Barrel%` = percent(`Barrel%`, accuracy = 0.1),
         order = case_when(Pitcher == "Right" ~ 1,
                           Pitcher == "Left" ~ 2)) %>%
  arrange(order) %>% select(-order)

table_splits1 <- table_splits %>% 
  select(Pitcher, PA:xwOBA) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_splits2 <- table_splits %>% 
  select(Pitcher, `Whiff%`:`Barrel%`) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_splits1 <- ggplot() + 
  annotation_custom(table_splits1) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank())

table_splits2 <- ggplot() + 
  annotation_custom(table_splits2) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank())

file_table_splits1 <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "table_splits1.png")
ggsave(filename = file_table_splits1, plot = table_splits1, 
       width = 4, height = 0.75, dpi = 300) 

file_table_splits2 <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "table_splits2.png")
ggsave(filename = file_table_splits2, plot = table_splits2, 
       width = 3, height = 0.75, dpi = 300) 


# Table Batter Pitch Type RHP
table_pitch_r <- batter_data %>% 
  filter(PitcherThrows == "Right",
         !is.na(TaggedPitchType)) %>% 
  mutate(pitch = case_when(TaggedPitchType  %in% c("Fastball", "Sinker", "FourSeamFastBall",
                                                   "TwoSeamFastball", "OneSeamFastBall") ~ "Fastball",
                           TaggedPitchType %in% c("Slider", "Curveball", "Cutter", "Sweeper") ~ "Breaking",
                           TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "Offspeed",
                           TRUE ~ "Other")) %>% 
  mutate(PA = case_when(PitchCall == "InPlay" ~ 1,
                        KorBB == "Walk" ~ 1,
                        KorBB == "Strikeout" ~ 1,
                        PitchCall == "HitByPitch" ~ 1,
                        TRUE ~ 0),
         AVG = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         SLG = case_when(PlayResult == "Single" ~ 1,
                         PlayResult == "Double" ~ 2,
                         PlayResult == "Triple" ~ 3,
                         PlayResult == "HomeRun" ~ 4,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         OBP = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                         KorBB == "Walk" ~ 1,
                         PitchCall == "HitByPitch" ~ 1,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         swing = ifelse(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable",
                                         "InPlay", "StrikeSwinging"), 1, 0)) %>% 
  group_by(pitch) %>% 
  summarize(PA = sum(PA, na.rm = TRUE),
            AVG = mean(AVG, na.rm = TRUE),
            OBP = mean(OBP, na.rm = TRUE),
            SLG = mean(SLG, na.rm = TRUE),
            OPS = mean(OBP, na.rm = TRUE) + mean(SLG, na.rm = TRUE),
            xwOBA = mean(xwOBA, na.rm = TRUE),
            `Whiff%` = sum(PitchCall == "StrikeSwinging", na.rm = TRUE) / sum(swing, na.rm = TRUE),
            `HH%` = sum(ExitSpeed >= 95, na.rm = TRUE) / sum(PitchCall == "InPlay"),
            `Barrel%` = sum(Barrel, na.rm = TRUE) / sum(PitchCall == "InPlay"),
            "Avg EV" = round(mean(ExitSpeed, na.rm = TRUE), 1),
            "EV Range" = paste0(round(min(ExitSpeed, na.rm = TRUE)), " - ", round(max(ExitSpeed, na.rm = TRUE)))) %>% 
  rename(Pitch = pitch) %>% 
  mutate(AVG = round(AVG, 3),
         AVG = formatC(AVG, format = "f", digits = 3),
         OBP = round(OBP, 3),
         OBP = formatC(OBP, format = "f", digits = 3),
         SLG = round(SLG, 3),
         SLG = formatC(SLG, format = "f", digits = 3),
         xwOBA = round(xwOBA, 3),
         OPS = formatC(OPS, format = "f", digits = 3),
         `Whiff%` = percent(`Whiff%`, accuracy = 0.1),
         `HH%` = percent(`HH%`, accuracy = 0.1),
         `Barrel%` = percent(`Barrel%`, accuracy = 0.1),
         order = case_when(Pitch == "Fastball" ~ 1,
                           Pitch == "Breaking" ~ 2,
                           Pitch == "Offspeed" ~ 3)) %>%
  arrange(order) %>% select(-order) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_pitch_r <- ggplot() + 
  annotation_custom(table_pitch_r) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank())

file_table_pitch_r <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "table_pitch_r.png")
ggsave(filename = file_table_pitch_r, plot = table_pitch_r, 
       width = 8, height = 1.25, dpi = 300) 


# Table Batter Pitch Type RHP
table_pitch_l <- batter_data %>% 
  filter(PitcherThrows == "Left",
         !is.na(TaggedPitchType)) %>% 
  mutate(pitch = case_when(TaggedPitchType  %in% c("Fastball", "Sinker", "FourSeamFastBall",
                                                   "TwoSeamFastball", "OneSeamFastBall") ~ "Fastball",
                           TaggedPitchType %in% c("Slider", "Curveball", "Cutter", "Sweeper") ~ "Breaking",
                           TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "Offspeed",
                           TRUE ~ "Other")) %>% 
  mutate(PA = case_when(PitchCall == "InPlay" ~ 1,
                        KorBB == "Walk" ~ 1,
                        KorBB == "Strikeout" ~ 1,
                        PitchCall == "HitByPitch" ~ 1,
                        TRUE ~ 0),
         AVG = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         SLG = case_when(PlayResult == "Single" ~ 1,
                         PlayResult == "Double" ~ 2,
                         PlayResult == "Triple" ~ 3,
                         PlayResult == "HomeRun" ~ 4,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         OBP = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                         KorBB == "Walk" ~ 1,
                         PitchCall == "HitByPitch" ~ 1,
                         PlayResult == "StolenBase" ~ NA,
                         PlayResult != "Undefined" ~ 0,
                         KorBB == "Strikeout" ~ 0,
                         TRUE ~ NA),
         swing = ifelse(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable",
                                         "InPlay", "StrikeSwinging"), 1, 0)) %>% 
  group_by(pitch) %>% 
  summarize(PA = sum(PA, na.rm = TRUE),
            AVG = mean(AVG, na.rm = TRUE),
            OBP = mean(OBP, na.rm = TRUE),
            SLG = mean(SLG, na.rm = TRUE),
            OPS = mean(OBP, na.rm = TRUE) + mean(SLG, na.rm = TRUE),
            xwOBA = mean(xwOBA, na.rm = TRUE),
            `Whiff%` = sum(PitchCall == "StrikeSwinging", na.rm = TRUE) / sum(swing, na.rm = TRUE),
            `HH%` = sum(ExitSpeed >= 95, na.rm = TRUE) / sum(PitchCall == "InPlay"),
            `Barrel%` = sum(Barrel, na.rm = TRUE) / sum(PitchCall == "InPlay"),
            "Avg EV" = round(mean(ExitSpeed, na.rm = TRUE), 1),
            "EV Range" = paste0(round(min(ExitSpeed, na.rm = TRUE)), " - ", round(max(ExitSpeed, na.rm = TRUE)))) %>% 
  rename(Pitch = pitch) %>% 
  mutate(AVG = round(AVG, 3),
         AVG = formatC(AVG, format = "f", digits = 3),
         OBP = round(OBP, 3),
         OBP = formatC(OBP, format = "f", digits = 3),
         SLG = round(SLG, 3),
         SLG = formatC(SLG, format = "f", digits = 3),
         xwOBA = round(xwOBA, 3),
         OPS = formatC(OPS, format = "f", digits = 3),
         `Whiff%` = percent(`Whiff%`, accuracy = 0.1),
         `HH%` = percent(`HH%`, accuracy = 0.1),
         `Barrel%` = percent(`Barrel%`, accuracy = 0.1),
         order = case_when(Pitch == "Fastball" ~ 1,
                           Pitch == "Breaking" ~ 2,
                           Pitch == "Offspeed" ~ 3)) %>%
  arrange(order) %>% select(-order) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_pitch_l <- ggplot() + 
  annotation_custom(table_pitch_l) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank())

file_table_pitch_l <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "table_pitch_l.png")
ggsave(filename = file_table_pitch_l, plot = table_pitch_l, 
       width = 8, height = 1.25, dpi = 300) 


# Batting Average Zones

loc <- batter_data %>% 
  mutate(AVG = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                  PlayResult == "StolenBase" ~ NA,
                  PlayResult != "Undefined" ~ 0,
                  KorBB == "Strikeout" ~ 0,
                  TRUE ~ NA)) %>% 
  group_by(PitcherThrows, Zone) %>% 
  summarize(AVG = mean(AVG, na.rm = TRUE)) %>% 
  mutate(AVG = ifelse(is.na(AVG) | is.nan(AVG), 0, AVG))

loc_g <- loc %>% 
  left_join(full_zone_data, join_by(Zone)) %>%
  filter(Zone  %in% seq(1, 14, 1)) %>% 
  mutate(AVG_label = sub("^0", "", sprintf("%.3f", AVG)))

loc_g <- loc_g %>% 
  mutate(AVG_color = ifelse(AVG > 0.5, 0.5, AVG)) %>% 
  mutate(PitcherThrows = factor(PitcherThrows, levels = c("Left", "Right")))


graph_avgs <- ggplot() +
  geom_tile(data = loc_g %>% filter(Zone %in% 1:9),
            aes(x = x, y = y, fill = AVG_color),
            color = "black", size = 0.25, show.legend = FALSE) +
  geom_text(data = loc_g %>% filter(Zone %in% 1:9),
            aes(x = x, y = y, label = AVG_label), family = "Times",
            color = "black", size = 5, show.legend = FALSE) +
  theme_void() +  # Remove axis and background
  geom_polygon(data = data.frame(x = c(-0.708, 0.7083, 0.7083, 0, -0.7083),
                                 y = c(-1.5, -1.5, -1.75, -2, -1.75)), 
               aes(x = x, y = y), fill = "white", color = "black", size = 0.5) +  # Home plate
  geom_text(
    data = loc_g %>% distinct(PitcherThrows) %>% mutate(x = 0, y = -1.675),
    aes(x = x, y = y, label = PitcherThrows),
    size = 5, fontface = "bold", family = "Times",
    inherit.aes = FALSE) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "#BF0A30",
                       midpoint = 0.3) +
  coord_fixed(ratio = 1.34) +
  facet_wrap(~ PitcherThrows, ncol = 2) +
  theme(text = element_text(family = "Times"),
        strip.text = element_blank(),
        panel.spacing = unit(3, "lines"))


file_graph_avgs <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_avgs.png")
ggsave(filename = file_graph_avgs, plot = graph_avgs, 
       width = 4.5, height = 3.5, dpi = 300) 



# Exit Velocity Zones

locev <- batter_data %>% 
  filter(Bearing >= -45 | Bearing <= 45) %>%
  filter(PitchCall == "InPlay",
         !is.na(ExitSpeed), !is.nan(ExitSpeed)) %>% 
  group_by(PitcherThrows, Zone) %>% 
  summarize(EV = mean(ExitSpeed)) %>% 
  mutate(EV = ifelse(is.na(EV) | is.nan(EV), 0, EV))

loc_evg <- locev %>% 
  left_join(full_zone_data, join_by(Zone)) %>%
  filter(Zone  %in% seq(1, 14, 1)) %>% 
  mutate(PitcherThrows = factor(PitcherThrows, levels = c("Left", "Right"))) %>% 
  mutate(EV = round(EV, 1))


graph_ev <- ggplot() +
  geom_tile(data = loc_evg %>% filter(Zone %in% 1:9),
            aes(x = x, y = y, fill = EV),
            color = "black", size = 0.25, show.legend = FALSE) +
  geom_text(data = loc_evg %>% filter(Zone %in% 1:9),
            aes(x = x, y = y, label = EV), family = "Times",
            color = "black", size = 5, show.legend = FALSE) +
  theme_void() +  # Remove axis and background
  geom_polygon(data = data.frame(x = c(-0.708, 0.7083, 0.7083, 0, -0.7083),
                                 y = c(-1.5, -1.5, -1.75, -2, -1.75)), 
               aes(x = x, y = y), fill = "white", color = "black", size = 0.5) +  # Home plate
  geom_text(
    data = loc_evg %>% distinct(PitcherThrows) %>% mutate(x = 0, y = -1.675),
    aes(x = x, y = y, label = PitcherThrows),
    size = 5, fontface = "bold", family = "Times",
    inherit.aes = FALSE) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "#BF0A30",
                       midpoint = 70) +
  coord_fixed(ratio = 1.34) +
  facet_wrap(~ PitcherThrows, ncol = 2) +
  theme(text = element_text(family = "Times"),
        strip.text = element_blank(),
        panel.spacing = unit(3, "lines"))


file_graph_ev <- paste0("Reports/Hitter Season Reports/Graphics/", name_file, ".", "graph_ev.png")
ggsave(filename = file_graph_ev, plot = graph_ev, 
       width = 4.5, height = 3.5, dpi = 300) 






# Writing PDFs ####

# Reading In All Templates
Rep1 <- image_read_pdf("Report Template Pages/Hitter Reports 1.pdf")
Rep2 <- image_read_pdf("Report Template Pages/Hitter Reports 2.pdf")
Rep3 <- image_read_pdf("Report Template Pages/Hitter Reports 3.pdf")


### REPORT PAGE 1 ###

Rep1 <- image_annotate(Rep1, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep1 <- image_annotate(Rep1, toupper("2025 Season"), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")
Rep1 <- image_composite(Rep1, image_resize(image_read(file_table1), "2350x235"), offset = "+3455+3055")
Rep1 <- image_composite(Rep1, image_resize(image_read(file_table2), "2350x235"), offset = "+3455+2650")
Rep1 <- image_composite(Rep1, image_read(file_graph_sprayhh), offset = "+3600+525")
Rep1 <- image_composite(Rep1, image_read(file_graph_location), offset = "+300+550")
Rep1 <- image_composite(Rep1, image_read(file_graph_barrels), offset = "+70+2050")
Rep1 <- image_composite(Rep1, image_read(file_graph_swings), offset = "+1150+2050")
Rep1 <- image_composite(Rep1, image_read(file_graph_whiffs), offset = "+2230+2050")
Rep1 <- image_composite(Rep1, image_resize(image_read("Report Template Pages/Hitter Reports Header.png"), "2400x600"), offset = "+3440+2525")

Rep2 <- image_annotate(Rep2, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep2 <- image_annotate(Rep2, toupper("2025 Season"), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")
Rep2 <- image_composite(Rep2, image_read(file_graph_sprayev), offset = "+3600+525")
Rep2 <- image_composite(Rep2, image_read(file_graph_ev), offset = "+150+600")
Rep2 <- image_composite(Rep2, image_read(file_graph_avgs), offset = "+1775+600")
Rep2 <- image_composite(Rep2, image_resize(image_read(file_table_splits1), "1600x300"), offset = "+3825+2575")
Rep2 <- image_composite(Rep2, image_resize(image_read(file_table_splits2), "1200x340"), offset = "+4050+2950")
Rep2 <- image_composite(Rep2, image_resize(image_read(file_table_pitch_r), "3100x500"), offset = "+95+1950")
Rep2 <- image_composite(Rep2, image_resize(image_read(file_table_pitch_l), "3100x500"), offset = "+95+2750")

Rep3 <- image_annotate(Rep3, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep3 <- image_annotate(Rep3, toupper("2025 Season"), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")


Full_Rep <- image_join(Rep1, Rep2, Rep3)
image_write(Full_Rep, path=paste0("Reports/Hitter Season Reports/", name_file, ".pdf"), format="pdf", quality = 100, density = 300)


print(paste0(name_title, " Completed ", "(",i, " of ", nrow(summary_data), ")"))

if(i == nrow(summary_data)){
print("Loop Complete")
}

}


