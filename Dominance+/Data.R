# Libraries ####
library(tidyverse)
library(lubridate)

set.seed(2880)

# Data ####
data23 <- read_csv("Raw Data/Full2023.csv") %>% 
  mutate(estimated_woba_using_speedangle = case_when(events %in% c("strikeout", "strikeout_double_play") ~ 0,
                                                     events == "hit_by_pitch" ~ 0.72,
                                                     events == "walk" ~ 0.69,
                                                     TRUE ~ estimated_woba_using_speedangle))
data2425 <- read_csv("Raw Data/all_data3.csv")

raw_data <- bind_rows(data23, data2425)


sz_database <- raw_data %>% 
  group_by(batter, game_year) %>% 
  summarize(xsz_top = mean(sz_top, na.rm = TRUE),
            xsz_bot = mean(sz_bot, na.rm = TRUE),
            .groups = "drop")

barrier <- 2.9/12/2

data <- raw_data %>%
  left_join(sz_database, by = c("batter", "game_year")) %>% 
  mutate(sz_top = coalesce(sz_top, xsz_top),
         sz_bot = coalesce(sz_bot, xsz_bot)) %>%
  select(-xsz_top, -xsz_bot) %>%
  mutate(
    is_paint = as.integer(plate_z <= sz_top + barrier & 
                            plate_z >= sz_bot - barrier &
                            plate_x >= -(8.5/12 + barrier) &
                            plate_x <=  (8.5/12 + barrier) &
        (plate_z >= sz_top - barrier | 
           plate_z <= sz_bot + barrier | 
           plate_x >= (8.5/12 - barrier) | 
           plate_x <= -(8.5/12 - barrier)))) %>% 
  filter(!is.na(pitch_type), 
         !(pitch_type %in% c("PO", "EP", "FA")))

data <- data %>% 
  mutate(pitch_type = case_when(pitch_type %in% c("CS", "KC") ~ "CU",
                                TRUE ~ pitch_type))


# data %>% 
#   filter(description == "hit_into_play") %>% 
#   mutate(EV = round(launch_speed/5, 0)*5) %>% 
#   group_by(EV) %>% 
#   summarize(N = n(),
#             BABIP_24 = mean(babip_value[game_year == 2024], na.rm = TRUE),
#             BABIP_25 = mean(babip_value[game_year == 2025], na.rm = TRUE),
#             wOBA = mean(woba_value, na.rm = TRUE),
#             .groups = "drop") %>% 
#   View


# Pitcher Stats

pitcher_stats <- data %>% 
  mutate(in_zone = ifelse(zone %in% 1:9, 1, 0),
         is_swing = ifelse(description %in% c("hit_into_play", "foul", "swinging_strike",
                                              "swinging_strike_blocked", "foul_tip",
                                              "foul_bunt", "missed_bunt", "bunt_foul_tip"), 
                           1, 0),
         is_whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"),
                           1, 0),
         is_hh = ifelse(launch_speed >= 95, 1, 0),
         is_k = ifelse(events %in% c("strikeout", "strikeout_double_play"), 1, 0)) %>% 
  group_by(player_name, game_year) %>% 
  summarize(pitches = n(),
            ab = sum(woba_denom, na.rm = TRUE),
            whiff = mean(is_whiff[is_swing == 1], na.rm = TRUE),
            iz_whiff = mean(is_whiff[is_swing == 1 & in_zone == 1], na.rm = TRUE),
            chase_rate = mean(is_swing[in_zone == 0], na.rm = TRUE),
            hard_hit = mean(is_hh[description == "hit_into_play"], na.rm = TRUE),
            paint_rate = mean(is_paint, na.rm = TRUE),
            iz_take = 1 - mean(is_swing[in_zone == 1], na.rm = TRUE),
            put_away = mean(is_k[strikes == 2], na.rm = TRUE),
            
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
            
            .groups = "drop")

# Pitch Stats
pitch_stats <- data %>% 
  mutate(in_zone = ifelse(zone %in% 1:9, 1, 0),
         is_swing = ifelse(description %in% c("hit_into_play", "foul", "swinging_strike",
                                              "swinging_strike_blocked", "foul_tip",
                                              "foul_bunt", "missed_bunt", "bunt_foul_tip"), 
                           1, 0),
         is_whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"),
                           1, 0),
         is_hh = ifelse(launch_speed >= 95, 1, 0),
         is_k = ifelse(events %in% c("strikeout", "strikeout_double_play"), 1, 0)) %>% 
  group_by(player_name, game_year, pitch_type) %>% 
  summarize(pitches = n(),
            ab = sum(woba_denom, na.rm = TRUE),
            whiff = mean(is_whiff[is_swing == 1], na.rm = TRUE),
            iz_whiff = mean(is_whiff[is_swing == 1 & in_zone == 1], na.rm = TRUE),
            chase_rate = mean(is_swing[in_zone == 0], na.rm = TRUE),
            hard_hit = mean(is_hh[description == "hit_into_play"], na.rm = TRUE),
            paint_rate = mean(is_paint, na.rm = TRUE),
            iz_take = 1 - mean(is_swing[in_zone == 1], na.rm = TRUE),
            put_away = mean(is_k[strikes == 2], na.rm = TRUE),
            
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
            
            .groups = "drop")

# Save CSVs
write.csv(pitch_stats, "Data/pitch_stats.csv", row.names = FALSE)
write.csv(pitcher_stats, "Data/pitcher_stats.csv", row.names = FALSE)
