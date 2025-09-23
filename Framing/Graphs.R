# Libraries ####
library(tidyverse)
library(lubridate)
library(caret)
library(xgboost)
library(knitr)

set.seed(2880)

# GG Objects ####
geom_zone <- function(top = 3.42, bottom = 1.60, linecolor = "black", size = 0.75){
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
raw_data <- read_csv("all_data3.csv")
data <- read_csv("edge_data.csv")
model_data <- read_csv("model_data.csv")
model24 <- xgb.load("strike24.model")
model25 <- xgb.load("strike25.model")
catcher_strikes <- read_csv("catcher_sv.csv")

people <- read_csv("player_ids.csv") %>% select(Name, MLBAMID)
people2 <- read_csv("player_ids2.csv") %>% select(PLAYERNAME, MLBID) %>% rename(Name = PLAYERNAME, MLBAMID = MLBID)
people <- bind_rows(people, people2) %>% distinct(MLBAMID, .keep_all = TRUE)


# Model Implementation

features <- model_data %>% 
  select(-description, -is_strike, -train, -catcher, -game_year) %>% 
  colnames()

pred_data24 <- model_data %>%
  filter(game_year == 2024) %>% 
  select(all_of(features)) %>%
  as.matrix() %>%
  xgb.DMatrix()

pred_data25 <- model_data %>%
  filter(game_year == 2025) %>% 
  select(all_of(features)) %>%
  as.matrix() %>%
  xgb.DMatrix()

catcher_data <- model_data %>%
  mutate(strike_pred = ifelse(game_year == 2024,
                              predict(model24, pred_data24),
                              predict(model25, pred_data25)),
         delta_strikes = is_strike - strike_pred)

# Catcher Good/Bad Designation
catcher_good_bad <- catcher_strikes %>% 
  mutate(positive_strike = ifelse(strikes_gained > 0, 1, 0)) %>% 
  select(catcher, game_year, positive_strike)

# Catcher Leaders by SG100
catcher_strikes %>% 
  left_join(people, by = c("catcher" = "MLBAMID")) %>% 
  filter(close_pitches >= 1000) %>% 
  select(Name, game_year, SG100, close_pitches, strikes_gained, catcher) %>% 
  rename(ID = catcher,
         Catcher = Name,
         Season = game_year,
         "Edge Pitches" = close_pitches,
         "Strikes Gained" = strikes_gained) %>% 
  arrange(desc(SG100)) %>% 
  View

# Swing Rates by Catcher, In Zone
data %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(positive_strike = ifelse(positive_strike == 1, "Good Framer", "Bad Framer"),
         in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  group_by(in_zone, positive_strike) %>% 
  summarize("swing rate" = mean(is_swing, na.rm = TRUE)) %>% 
  kable

# Swing Rates by Catcher, In Zone, and Season
data %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(positive_strike = ifelse(positive_strike == 1, "Good Framer", "Bad Framer"),
         in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  group_by(game_year, in_zone, positive_strike) %>% 
  summarize("swing rate" = mean(is_swing, na.rm = TRUE)) %>% 
  kable()

# Swing Rates by Catcher, In Zone with 2 Strikes
data %>% 
  filter(strikes == 2) %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(positive_strike = ifelse(positive_strike == 1, "Good Framer", "Bad Framer"),
         in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  group_by(in_zone, positive_strike) %>% 
  summarize("swing rate" = mean(is_swing, na.rm = TRUE)) %>% 
  kable

# Locational Swing Rates by Catcher Ability
swing_delta <- data %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(positive_strike = ifelse(positive_strike == 1, "Good Framer", "Bad Framer"),
         in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  mutate(loc_x = round(plate_x*5, 0)/5,
         loc_z = round(plate_z*5, 0)/5) %>% 
  group_by(loc_x, loc_z) %>% 
  summarize(N = n(),
            SR = mean(is_swing),
            SRG = mean(is_swing[positive_strike == "Good Framer"]),
            SRB = mean(is_swing[positive_strike == "Bad Framer"])) %>% 
  ungroup() %>% 
  mutate(Swing_Delta = SRG - SRB,
         Relative_SD = (SRG - SRB)/SR)

# Swing Rate Heat Map by Catcher Ability
swing_delta %>%   
  filter(N >= 30) %>% 
  ggplot(aes(x = loc_x, y = loc_z, color = Relative_SD)) +
  geom_point(shape = 15, size = 3.5) +
  scale_color_gradient2(low = "skyblue", mid = "white", high = "firebrick2", 
                       midpoint = 0,
                       labels = scales::percent) +
  geom_zone() +
  geom_plate(pov = "catcher") +
  coord_fixed() +
  labs(title = "Swing Likelihood Difference",
       color = "") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Hitter Swing Rates by Catcher Ability
swing_delta_hitters <- data %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  group_by(batter) %>% 
  summarize(SRG = mean(is_swing[positive_strike == 1], na.rm = TRUE),
            SRB = mean(is_swing[positive_strike == 0], na.rm = TRUE),
            SRG_iz = mean(is_swing[positive_strike == 1 & in_zone == 1], na.rm = TRUE),
            SRB_iz = mean(is_swing[positive_strike == 0 & in_zone == 1], na.rm = TRUE),
            SRG_oz = mean(is_swing[positive_strike == 1 & in_zone == 0], na.rm = TRUE),
            SRB_oz = mean(is_swing[positive_strike == 0 & in_zone == 0], na.rm = TRUE),
            N = n()) %>% 
  ungroup() %>% 
  mutate(SR_Delta = SRG - SRB,
         SR_Delta_iz = SRG_iz - SRB_iz,
         SR_Delta_oz = SRG_oz - SRB_oz) %>% 
  filter(N >= 100) %>% 
  arrange(desc(SR_Delta)) %>% 
  left_join(people, by = c("batter" = "MLBAMID")) %>% 
  select(Name, batter, N, SRG:SRB_oz, SR_Delta:SR_Delta_oz)

stats <- read_csv("stats.csv") %>% 
  select(player_id, xwoba)

# Do Better Hitters Change Their Swing Rates?
swing_delta_hitters %>% 
  left_join(stats, by = c("batter" = "player_id")) %>% 
  ggplot(aes(x = SR_Delta, y = xwoba)) +
  geom_point()

# Catchers by Swing Differential
swing_delta_catchers <- data %>% 
  left_join(catcher_strikes, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  group_by(fielder_2, game_year, close_pitches, strikes_gained, SG100) %>% 
  summarize(SR = mean(is_swing, na.rm = TRUE),
            SR_iz = mean(is_swing[in_zone == 1], na.rm = TRUE),
            SR_oz = mean(is_swing[in_zone == 0], na.rm = TRUE),
            N = n()) %>% 
  ungroup() %>% 
  filter(N >= 500) %>% 
  left_join(people, by = c("fielder_2" = "MLBAMID")) %>% 
  rename(id = fielder_2,
         CloseN = close_pitches,
         Season = game_year) %>% 
  select(Name, id, Season, N, SR, SR_iz, SR_oz, CloseN, strikes_gained, SG100)

# Swing Rate Correlations
cor_sr <- cor(swing_delta_catchers$SG100, swing_delta_catchers$SR)
cor_sriz <- cor(swing_delta_catchers$SG100, swing_delta_catchers$SR_iz)
cor_sroz <- cor(swing_delta_catchers$SG100, swing_delta_catchers$SR_oz)

# Swing Rate by Catcher Framing
swing_delta_catchers %>% 
  ggplot(aes(x = SG100, y = SR)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Swing Rate by Catcher Framing",
       x = "Strikes Gained per 100 Pitches",
       y = "Swing Rate",
       caption = paste("Correlation:", round(cor_sr, 4))) +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5))

# Swing Rate in Zone by Catcher Framing
swing_delta_catchers %>% 
  ggplot(aes(x = SG100, y = SR_iz)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Swing Rate In Zone by Catcher Framing",
       x = "Strikes Gained per 100 Pitches",
       y = "Swing Rate",
       caption = paste("Correlation:", round(cor_sriz, 4))) +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5))

# Swing Rate Out of Zone by Catcher Framing
swing_delta_catchers %>% 
  ggplot(aes(x = SG100, y = SR_oz)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Swing Rate Out of Zone by Catcher Framing",
       x = "Strikes Gained per 100 Pitches",
       y = "Swing Rate",
       caption = paste("Correlation:", round(cor_sroz, 4))) +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5))

# Pitch Location Map by Catcher Ability
data %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(loc_x = round(plate_x*5, 0)/5,
         loc_z = round(plate_z*5, 0)/5) %>% 
  group_by(loc_x, loc_z, positive_strike) %>% 
  summarize(N = n()) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = positive_strike,
    values_from = N,
    values_fill = 0) %>% 
  rename("Bad" = "0",
         "Good" = "1") %>% 
  mutate(N = Bad + Good,
         Good_Ratio = Good/Bad - 1) %>% 
  filter(N >= 30) %>% 
  ggplot(aes(x = loc_x, y = loc_z, color = Good_Ratio)) +
  geom_point(shape = 15, size = 3.5) +
  scale_color_gradient2(low = "skyblue", mid = "white", high = "firebrick2", 
                        midpoint = 0,
                        labels = scales::percent) +
  geom_zone() +
  geom_plate(pov = "catcher") +
  coord_fixed() +
  labs(title = "Pitch Likelihood Difference",
       color = "") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

 
 