library(tidyverse)
library(mgcv)
library(scales)


# Functions ####
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

swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}


scale_color_pitch <- function() {
  scale_color_manual(
    values = c(
      "FF" = "#E41A1C",
      "CH"        = "#377EB8",
      "CU"       = "#4DAF4A",
      "FC"          = "#984EA3",
      "SL"          = "#FF7F00",
      "ST"         = "#A65628",
      "SI"          = "#F781BF",
      "FS"    = "#999999",
      "SV"          = "#FFD92F",
      "KC"   = "#8DA0CB",
      "CS"      = "#FC8D62",
      "FO"        = "#A6D854",
      "KN"     = "#E78AC3",
      "SC"       = "#40B0A6"
    ),
  )
}

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





# Data ####
data <- read_csv("all_data.csv") %>% 
  filter(!(pitch_name %in% c("NA", "Pitch Out", "Eephus", "Other")), !is.na(pitch_name))

default <- data %>% 
  filter(zone == 5) %>% 
  filter(pitch_type == "FF")

# Attack Angle (swing up/down/flat)
# Tilt

mm <- default %>% 
  group_by(batter) %>% 
  filter(!is.na(swing_path_tilt)) %>%
  summarize(dTilt = mean(swing_path_tilt, na.rm = TRUE),
            dAngle = mean(attack_angle, na.rm = TRUE)) %>% 
  ungroup()

expected <- data %>% 
  filter(!is.na(swing_path_tilt)) %>%
  left_join(mm, by = "batter") %>% 
  mutate(delta_tilt = swing_path_tilt - dTilt,
         delta_angle = attack_angle - dAngle)


# Main Data

load("xHeightGAM.rda")

tunnel_data <- read_csv("full_data_predicted.csv")

# Graphics for Post ####

# Swing Tilt (Aaron Judge)
tunnel_data %>% 
  filter(batter == 592450) %>% 
  ggplot(aes(x = plate_x, y = plate_z, color = delta_tilt)) +
  geom_zone() + geom_plate(pov = "catcher") +
  geom_point(size = 0.6) +
  scale_color_gradient(low = "yellow", high = "red") +
  xlim(-1.85, 1.85) + ylim(-0.5, 4.25) + coord_fixed() +
  labs(title = "Delta Tilt by Location",
       subtitle = "Aaron Judge",
       color = "Δ Swing Tilt") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank())
  
# Attack Angle (Aaron Judge)
tunnel_data %>% 
    filter(batter == 592450) %>% 
    ggplot(aes(x = plate_x, y = plate_z, color = delta_angle)) +
    geom_zone() + geom_plate(pov = "catcher") +
    geom_point(size = 0.6) +
    scale_color_gradient(low = "yellow", high = "red") +
    xlim(-1.85, 1.85) + ylim(-0.5, 4.25) + coord_fixed() +
    labs(title = "Delta Attack Angle by Location",
         subtitle = "Aaron Judge",
         color = "Δ Attack Angle") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank())


# Whiffs on Expected Fastballs
tunnel_data %>%
  filter(xPitch == "FF") %>% 
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "SI", "SL"),
         is_whiff == 1,
         p_throws == "R") %>% 
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_zone() + geom_plate(pov = "catcher") +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  xlim(-2, 2) + ylim(-0.6, 5) +
  labs(title = "Whiffs on Expected Fastballs",
       subtitle = "For Right Handed Pitchers") +
  facet_wrap(~ pitch_type) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank())


# Whiffs on Sliders by Expected Pitch
tunnel_data %>%
  group_by(pitch_type, pitch_name) %>% 
  summarize(N = n())

tunnel_data %>%
  filter(pitch_type == "SL") %>% 
  filter(xPitch %in% c("CH", "CU", "FC", "FF", "SI", "SL"),
         is_whiff == 1,
         p_throws == "R") %>% 
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_zone() + geom_plate(pov = "catcher") +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  xlim(-2, 2) + ylim(-0.6, 5) +
  labs(title = "Whiffs on Sliders",
       subtitle = "For Right Handed Pitchers") +
  facet_wrap(~ xPitch) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank())

# Whiffs on Fastballs by Expected Pitch
tunnel_data %>%
  group_by(pitch_type, pitch_name) %>% 
  summarize(N = n())

tunnel_data %>%
  filter(pitch_type == "FF") %>% 
  filter(xPitch %in% c("CH", "CU", "FC", "FF", "SI", "SL"),
         is_whiff == 1,
         p_throws == "R") %>% 
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_zone() + geom_plate(pov = "catcher") +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  xlim(-2, 2) + ylim(-0.6, 5) +
  labs(title = "Whiffs on Fastballs",
       subtitle = "For Right Handed Pitchers") +
  facet_wrap(~ xPitch) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank())

# Expected Pitch by Count
tunnel_data %>% 
  mutate(pitch_class = case_when(pitch_type %in% c("FF", "SI") ~ "Fastball",
                                 pitch_type %in% c("CS", "CU", "FC", "KC", "SL", "ST", "SV") ~ "Breaking Ball",
                                 pitch_type %in% c("CH", "FO", "FS") ~ "Offspeed",
                                 TRUE ~ "Other"),
         pitch_class = factor(pitch_class, levels = c("Fastball", "Breaking Ball", "Offspeed"))) %>% 
  mutate(Count = paste(balls, strikes, sep = "-")) %>% 
  filter(Count != "4-2", pitch_class != "Other") %>% 
  group_by(Count, pitch_class) %>%
  summarize(N = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = Count, y = N, fill = pitch_class)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Fastball" = "orange", 
                               "Breaking Ball" = "purple", 
                               "Offspeed" = "green")) +
  labs(
    title = "Pitch Types by Count",
    x = "Count",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

# Expected Pitch by Count
tunnel_data %>% 
  mutate(matchup = ifelse(stand == p_throws, 0, 1)) %>% 
  filter(matchup == 1) %>% 
  mutate(xClass = case_when(xPitch %in% c("FF", "SI") ~ "Fastball",
                            xPitch %in% c("CS", "CU", "FC", "KC", "SL", "ST", "SV") ~ "Breaking Ball",
                            xPitch %in% c("CH", "FO", "FS") ~ "Offspeed",
                            TRUE ~ "Other"),
         xClass = factor(xClass, levels = c("Fastball", "Breaking Ball", "Offspeed"))) %>% 
  mutate(Count = paste(balls, strikes, sep = "-")) %>% 
  filter(Count != "4-2", xClass != "Other") %>% 
  group_by(Count, xClass) %>%
  summarize(N = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = Count, y = N, fill = xClass)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Fastball" = "orange", 
                               "Breaking Ball" = "purple", 
                               "Offspeed" = "green")) +
  labs(
    title = "Expected Pitch Types by Count",
    subtitle = "Handedness Advantage",
    x = "Count",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

tunnel_data %>% 
  mutate(matchup = ifelse(stand == p_throws, 0, 1)) %>% 
  filter(matchup == 0) %>% 
  mutate(xClass = case_when(xPitch %in% c("FF", "SI") ~ "Fastball",
                            xPitch %in% c("CS", "CU", "FC", "KC", "SL", "ST", "SV") ~ "Breaking Ball",
                            xPitch %in% c("CH", "FO", "FS") ~ "Offspeed",
                            TRUE ~ "Other"),
         xClass = factor(xClass, levels = c("Fastball", "Breaking Ball", "Offspeed"))) %>% 
  mutate(Count = paste(balls, strikes, sep = "-")) %>% 
  filter(Count != "4-2", xClass != "Other") %>% 
  group_by(Count, xClass) %>%
  summarize(N = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = Count, y = N, fill = xClass)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Fastball" = "orange", 
                               "Breaking Ball" = "purple", 
                               "Offspeed" = "green")) +
  labs(
    title = "Expected Pitch Types by Count",
    subtitle = "Handedness Disadvantage",
    x = "Count",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

# Pitch Usage and Expected Usage
pitch_usage <- data %>% 
  group_by(player_name, pitch_type) %>%
  summarize(N = n(), .groups = "drop_last") %>%
  mutate(Usage = N / sum(N)) %>%
  ungroup() %>% 
  rename("Total Usage" = Usage)

pitch_usage_swing <- tunnel_data %>% 
  group_by(player_name, pitch_type) %>%
  summarize(Usage = n(), .groups = "drop_last") %>%
  mutate(Usage = Usage / sum(Usage)) %>%
  ungroup() %>% 
  rename("Swing Usage" = Usage)

pitch_usage_swing_x <- tunnel_data %>% 
  group_by(player_name, xPitch) %>%
  summarize(Usage = n(), .groups = "drop_last") %>%
  mutate(Usage = Usage / sum(Usage)) %>%
  ungroup() %>% 
  rename("Expected Usage" = Usage)

arsenal_usage <- pitch_usage %>% 
  full_join(pitch_usage_swing, by = c("player_name", "pitch_type" = "pitch_type")) %>% 
  full_join(pitch_usage_swing_x, by = c("player_name", "pitch_type" = "xPitch"))

# Expectation vs. Reality
tunnel_data %>% 
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "SI", "SL", "ST")) %>% 
  filter(xPitch %in% c("CH", "CU", "FC", "FF", "SI", "SL", "ST")) %>% 
  mutate(xPitch = case_when(xPitch == "CH" ~ "Change Up",
                            xPitch == "CU" ~ "Curveball",
                            xPitch == "FC" ~ "Cutter",
                            xPitch == "FF" ~ "4-Seam",
                            xPitch == "SI" ~ "Sinker",
                            xPitch == "SL" ~ "Slider",
                            xPitch == "ST" ~ "Sweeper")) %>% 
  group_by(xPitch, pitch_type) %>% 
  summarize(N = n(), Whiff = mean(is_whiff, na.rm = TRUE)) %>% 
  arrange(xPitch, desc(Whiff)) %>% 
  ungroup() %>% 
  ggplot(aes(x = pitch_type, y = Whiff, fill = pitch_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~ xPitch) +
  labs(title = "Whiff Rate: Expected Pitch vs. Actual",
       fill = "Actual Pitch Type",
       x = "") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank())


# Deception Leaderboard
stuff <- read_csv("Stuff.csv") %>% 
  select(Name, `Stuff+`, `Location+`, `Pitching+`)

leaderboard <- tunnel_data %>% 
  mutate(is_match = ifelse(xPitch == pitch_type, 1, 0)) %>% 
  group_by(player_name) %>% 
  summarize(N = n(),
            Deception = 1 - mean(is_match, na.rm = TRUE),
            RE_Added = -sum(delta_run_exp, na.rm = TRUE)) %>% 
  arrange(desc(Deception)) %>% 
  filter(N > 100) %>% 
  mutate(RE100 = RE_Added/N*100)

leaderboard %>% 
  filter(N >= 1000) %>% 
  head(20) %>% 
  select(-N, -RE_Added, - RE100) %>% 
  mutate(player_name = sapply(player_name, swap_names)) %>% 
  left_join(stuff, by = c("player_name" = "Name")) %>% 
  mutate(Deception = percent(Deception, accuracy = 0.01)) %>% 
  rename(Name = player_name) %>% 
  View

# Pitcher Example
skubal <- tunnel_data %>% 
  filter(player_name == "Skubal, Tarik")

