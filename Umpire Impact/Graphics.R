library(tidyverse)
library(lubridate)
library(Metrics)
library(ggforce)

options(scipen = 999)
set.seed(3630)

geom_plate <- function(pov = "pitcher", size = 1){
  df <- case_when(
    pov == "pitcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, .25, .5, .25)),
    pov == "catcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, -.25, -.5, -.25)))
  g <- geom_polygon(data = df, aes(x = x, y = y), fill = "white", color = "black", linewidth = size)
  g
}

ump_full <- read.csv("full_ump_data.csv")

data_final <- read.csv("full_pitch_data.csv")

data <- read_csv("all_data2.csv")


# Accuracy
accuracy_best <- ump_full %>% 
  filter(Calls >= 1000) %>% 
  arrange(desc(Accuracy)) %>% 
  head(5)

accuracy_worst <- ump_full %>% 
  filter(Calls >= 1000) %>% 
  arrange(desc(Accuracy)) %>% 
  tail(5)

accuracy <- rbind(accuracy_best, accuracy_worst) %>% 
  select(ump, Calls, Accuracy, RE100, WP100)

# Delta RE100
re_best <- ump_full %>% 
  filter(Calls >= 1000) %>% 
  arrange(RE100) %>% 
  head(5)

re_worst <- ump_full %>% 
  filter(Calls >= 1000) %>% 
  arrange(RE100) %>% 
  tail(5)

re <- rbind(re_best, re_worst) %>% 
  select(ump, Calls, Accuracy, RE100, WP100)


# Delta WP100
wp_best <- ump_full %>% 
  filter(Calls >= 1000) %>% 
  arrange(WP100) %>% 
  head(5)

wp_worst <- ump_full %>% 
  filter(Calls >= 1000) %>% 
  arrange(WP100) %>% 
  tail(5)

wp <- rbind(wp_best, wp_worst) %>% 
  select(ump, Calls, Accuracy, RE100, WP100)


# Best Game
wp_game_best <- data_final %>% 
  group_by(game_pk, game_date, ump) %>% 
  summarize(Wrong = n(),
            RE = sum(abs(xfld_delta_re - fld_delta_re), na.rm = TRUE),
            WP = sum(abs(xfld_delta_wp - fld_delta_wp), na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(WP) %>% 
  head(5)

data_best <- data %>% 
  filter(game_pk == 745188,
         description %in% c("called_strike", "ball")) %>%
  mutate(is_strike = ifelse(zone %in% 1:9, 1, 0),
         called_strike = ifelse(description == "called_strike", 1, 0),
         is_correct = ifelse(is_strike == called_strike, 1, 0)) %>% 
  filter(is_correct == 0)

miss_1 <- data_best %>% 
  head(1) %>% 
  ggplot() +
  geom_rect(xmin = -.7083, xmax = .7083, aes(ymin = sz_bot, ymax = sz_top),
            alpha = 0, color = "black", linewidth = 1) +
  geom_circle(aes(x0 = plate_x, y0 = plate_z, r = 0.12083), color = "red", fill = "red", alpha = 1) +
  geom_plate(pov = "catcher") +
  xlim(-2, 2) +
  ylim(-0.5, 4) +
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
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(family = "Times New Roman", size = 20),
        strip.background = element_blank())

miss_1



  

# Worst Game
wp_game_worst <- data_final %>% 
  group_by(game_pk, game_date, ump) %>% 
  summarize(Wrong = n(),
            RE = sum(abs(xfld_delta_re - fld_delta_re), na.rm = TRUE),
            WP = sum(abs(xfld_delta_wp - fld_delta_wp), na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(WP) %>% 
  tail(5)

