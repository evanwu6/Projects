library(tidyverse)
library(lubridate)
library(scales)

options(scipen = 999)

# Data
arsenal <- read_csv("arsenal_data.csv")
usage <- read_csv("usage.csv")
xpitch <- read_csv("full_data_predicted.csv")


true <- xpitch %>% 
  mutate(year = year(game_date)) %>% 
  group_by(player_name, year, pitch_type) %>% 
  summarize(N = n()) %>% 
  ungroup() %>% 
  group_by(player_name, year) %>% 
  mutate(SeasonPC = sum(N)) %>% 
  ungroup() %>% 
  mutate(Real_Usage = round(N/SeasonPC, 4)) %>% 
  filter(N >= 30) %>% 
  select(-N, -SeasonPC)

expected <- xpitch %>% 
  mutate(year = year(game_date)) %>% 
  group_by(player_name, year, xPitch) %>% 
  summarize(N = n()) %>% 
  ungroup() %>% 
  group_by(player_name, year) %>% 
  mutate(SeasonPC = sum(N)) %>% 
  ungroup() %>% 
  mutate(Expected_Usage = round(N/SeasonPC, 4)) %>% 
  select(-N, -SeasonPC)

eval <- true %>% 
  full_join(expected, by = c("player_name", "year", "pitch_type" = "xPitch")) %>% 
  rename(Pitch = pitch_type)


# Batter Prediction by Pitch
eval %>% 
  mutate(Diff = Expected_Usage - Real_Usage) %>% 
  ggplot(aes(x = Real_Usage, y = Expected_Usage, color = Diff)) +
  geom_point(size = 1.25, shape = 18,
             alpha = 0.75) +
  geom_smooth(se = FALSE, color = "black", linetype = 2) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_color_gradient2(low = "red", mid = "gray80", high = "green",
                        midpoint = 0) +
  labs(title = "Batter Prediction Rate by Pitch Usage",
       x = "Real Pitch Usage",
       y = "Batter Prediction Percentage") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")


# Match by Arsenal Size
arsenal_size <- xpitch %>% 
  mutate(year = year(game_date)) %>% 
  group_by(player_name, year, pitch_type) %>% 
  summarize(N = n()) %>% 
  ungroup() %>% 
  group_by(player_name, year) %>% 
  mutate(SeasonPC = sum(N)) %>% 
  ungroup() %>% 
  mutate(Usage = round(N/SeasonPC, 4),
         main = ifelse(Usage >= 0.10, 1, 0)) %>% 
  group_by(player_name, year) %>% 
  summarize(all_pitches = n(),
            main_pitches = sum(main, na.rm = TRUE)) %>% 
  ungroup()

guess_rate <- xpitch %>% 
  mutate(year = year(game_date)) %>% 
  group_by(player_name, year) %>% 
  summarize(match = mean(is_match, na.rm = TRUE)) %>% 
  ungroup()

arsenal_guess <- arsenal_size %>% 
  left_join(guess_rate, by = c("player_name", "year"))

# Batter Prediction Accuracy by Arsenal Size
arsenal_guess %>% 
  mutate(all_pitches = as.factor(all_pitches)) %>% 
  ggplot(aes(x = all_pitches, y = match, color = all_pitches)) +
  geom_boxplot() +
  scale_y_continuous(labels = percent) +
  labs(title = "Batter Prediction Accuracy by Arsenal Size",
       x = "Pitch Arsenal Size",
       y = "Batter Prediction Accuracy") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

# Batter Prediction Accuracy by Functional Arsenal Size
arsenal_guess %>% 
  mutate(main_pitches = as.factor(main_pitches)) %>% 
  ggplot(aes(x = main_pitches, y = match, color = main_pitches)) +
  geom_boxplot() +
  scale_y_continuous(labels = percent) +
  labs(title = "Batter Prediction Accuracy by Functional Arsenal Size",
       x = "Pitch Arsenal Size (at least 10% Usage)",
       y = "Batter Prediction Accuracy") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

arsenal_guess %>% 
  filter(main_pitches == 3) %>% 
  summarize(median(match))

arsenal_guess %>% 
  filter(all_pitches == 4) %>% 
  summarize(median(match))


  



# Pitch Match by Arsenal Size
pitch_usage <- xpitch %>% 
  mutate(year = year(game_date)) %>% 
  group_by(player_name, year, pitch_type) %>% 
  summarize(N = n()) %>% 
  ungroup() %>% 
  group_by(player_name, year) %>% 
  mutate(SeasonPC = sum(N)) %>% 
  ungroup() %>% 
  mutate(Usage = round(N/SeasonPC, 4)) %>% 
  filter(N >= 30) %>% 
  select(-N, -SeasonPC)

pitch_guess_rate <- xpitch %>% 
  mutate(year = year(game_date)) %>% 
  group_by(player_name, year, pitch_type) %>% 
  summarize(match = mean(is_match, na.rm = TRUE)) %>% 
  ungroup()

pitch_guess <- pitch_usage %>% 
  left_join(pitch_guess_rate, by = c("player_name", "year", "pitch_type"))

# Batter Prediction Accuracy by Pitch Usage
pitch_guess %>% 
  ggplot(aes(x = Usage, y = match, color = match)) +
  geom_point(size = 1.25, shape = 18,
             alpha = 0.75) +
  geom_smooth(se = FALSE, color = "black", linetype = 2) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_color_gradient2(low = "green", mid = "gray80", high = "red",
                        midpoint = 0) +
  labs(title = "Batter Prediction Accuracy by Pitch Usage",
       x = "Pitch Usage",
       y = "Batter Prediction Accuracy") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")



