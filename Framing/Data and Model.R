# Libraries ####
library(tidyverse)
library(lubridate)
library(caret)
library(xgboost)

set.seed(2880)

# GG Objects ####
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

ball <- 2.9/12
barrier <- ball*1.5

# Adding missing zone data
sz_database <- raw_data %>% 
  group_by(batter, game_year) %>% 
  summarize(xsz_top = mean(sz_top, na.rm = TRUE),
            xsz_bot = mean(sz_bot, na.rm = TRUE)) %>% 
  ungroup()

# Data Wrangling ####
data <- raw_data %>%
  # Replacing NA values with batter season averages
  left_join(sz_database, by = c("batter", "game_year")) %>% 
  mutate(sz_top = ifelse(!is.na(sz_top), sz_top, xsz_top),
         sz_bot = ifelse(!is.na(sz_bot), sz_bot, xsz_bot)) %>% 
  select(-xsz_top, -xsz_bot) %>% 
  # Removing Outer Pitcher
  filter(plate_z <= sz_top + barrier,
         plate_z >= sz_bot - barrier,
         plate_x >= -(8.5/12 + barrier),
         plate_x <= (8.5/12 + barrier)) %>% 
  # Removing Inner Pitches
  filter(plate_z >= sz_top - barrier | 
           plate_z <= sz_bot + barrier |
           plate_x >= (8.5/12 - barrier) | 
           plate_x <= -(8.5/12 - barrier))


model_data <- data %>% 
  rename(catcher = fielder_2) %>% 
  select(release_pos_x, release_pos_z, release_extension,
         release_speed, pfx_x, pfx_z, 
         plate_x, plate_z, sz_top, sz_bot,
         description, catcher, game_year) %>% 
  filter(description %in% c("ball", "called_strike", "blocked_ball")) %>% 
  mutate(is_strike = ifelse(description == "called_strike", 1, 0)) %>% 
  group_by(game_year, is_strike) %>%
  mutate(train = rbinom(n(), size = 1, prob = 0.8)) %>%
  ungroup() %>% 
  mutate(is_strike = as.factor(is_strike))

# XGBoost Model ####

features <- model_data %>% 
  select(-description, -is_strike, -train, -catcher, game_year) %>% 
  colnames()

model_data %>% 
  select(-description, -is_strike, -train, -catcher) %>% 
  ggplot(aes(x = sz_bot)) +
  geom_histogram(binwidth = 0.05, color = "white") +
  facet_wrap(~ game_year,
             ncol = 1)

model_data %>% 
  group_by(game_year) %>% 
  summarize(N = n(),
            Strikes = sum(description == "called_strike")) %>% 
  ungroup() %>% 
  mutate(rate = Strikes/N)

# Grid Search
grid_data <- model_data %>% 
  group_by(is_strike) %>% 
  slice_sample(prop = 0.05) %>% 
  ungroup()
  

x <- as.matrix(grid_data[, features])
y <- grid_data$is_strike

ctrl <- trainControl(method = "cv", number = 5)

grid <- expand.grid(
  nrounds = c(100, 250, 500),
  max_depth = c(3, 6, 9),
  eta = c(0.05, 0.1, 0.3),
  gamma = c(0, 0.5, 1),
  colsample_bytree = c(0.5, 1),
  min_child_weight = c(1, 10),
  subsample = c(0.5, 1)
)

tune_model <- train(
  x = x,
  y = y,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid
)

tune_model$bestTune


# Training Model
params <- list(
  objective = "binary:logistic",  
  eval_metric = "logloss"    
)

# 2024
train_data24 <- model_data %>% 
  filter(train == 1,
         game_year == 2024)

model_x24 <- as.matrix(train_data24[, features])
model_y24 <- as.numeric(as.character(train_data24$is_strike))

dtrain <- xgb.DMatrix(data = model_x24, label = model_y24)

model24 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  max_depth = 9,
  eta = 0.05,
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 10,
  subsample = 0.5
)

xgb.save(model24, "strike24.model")

# 2025
train_data25 <- model_data %>% 
  filter(train == 1,
         game_year == 2025)

model_x25 <- as.matrix(train_data25[, features])
model_y25 <- as.numeric(as.character(train_data25$is_strike))

dtrain <- xgb.DMatrix(data = model_x25, label = model_y25)

model25 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  max_depth = 9,
  eta = 0.05,
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 10,
  subsample = 0.5
)

xgb.save(model25, "strike25.model")


# Catcher Strikes Data

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
                          predict(model25, pred_data25)))

catcher_strikes <- catcher_data %>% 
  mutate(is_strike = as.numeric(is_strike) - 1,
         delta_strikes = is_strike - strike_pred) %>% 
  group_by(catcher, game_year) %>% 
  summarize(close_pitches = n(),
            strikes_gained = sum(delta_strikes, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SG100 = strikes_gained/close_pitches*100) %>% 
  arrange(desc(SG100))

catcher_data %>% 
  mutate(is_strike = as.numeric(is_strike) - 1,
         strike_pred = round(strike_pred, 2)) %>%
  group_by(strike_pred) %>% 
  summarize(strike_rate = mean(is_strike, na.rm = TRUE),
            N = n()) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = strike_pred, y = strike_rate))


catcher_good_bad <- catcher_strikes %>% 
  mutate(positive_strike = ifelse(strikes_gained > 0, 1, 0)) %>% 
  select(catcher, game_year, positive_strike)

data %>% 
  left_join(catcher_good_bad, by = c("fielder_2" = "catcher", "game_year")) %>% 
  mutate(is_swing = ifelse(description %in% c("ball", "called_strike", "blocked_ball",
                                              "hit_by_pitch", "pitchout"), 0, 1)) %>% 
  mutate(positive_strike = ifelse(positive_strike == 1, "Good Framer", "Bad Framer"),
         in_zone = ifelse(zone %in% 1:9, 1, 0)) %>% 
  group_by(in_zone, positive_strike) %>% 
  summarize("swing rate" = mean(is_swing, na.rm = TRUE))

write.csv(data, "edge_data.csv", row.names = FALSE)
write.csv(model_data, "model_data.csv", row.names = FALSE)
write.csv(catcher_strikes, "catcher_sv.csv", row.names = FALSE)
