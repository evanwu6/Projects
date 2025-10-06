# Libraries ####
library(tidyverse)
library(lubridate)
library(caret)
library(xgboost)

set.seed(3371)


# Data ####
pitch_stats <- read_csv("Data/pitch_stats.csv")


pitch_model_data <- pitch_stats %>% 
  filter(pitches >= 100) %>% 
  group_by(pitch_type, game_year) %>%
  mutate(train = rbinom(n(), size = 1, prob = 0.8)) %>%
  ungroup()

features <- pitch_model_data %>% 
  select(-player_name, -game_year, -pitch_type, -pitches, -ab, -xwOBA, -train, -put_away) %>% 
  colnames()



# Grid Search
pitch_grid_data <- pitch_model_data %>% 
  filter(train == 1)
  

grid_x <- as.matrix(pitch_grid_data[, features])
grid_y <- pitch_grid_data$xwOBA

ctrl <- trainControl(method = "cv", number = 5)

grid <- expand.grid(
  nrounds = c(100, 250, 500),
  max_depth = c(2, 4, 6, 8),
  eta = c(0.05, 0.1, 0.3),
  gamma = c(0, 0.5, 1),
  colsample_bytree = c(0.5, 1),
  min_child_weight = c(1, 10),
  subsample = c(0.5, 1)
)

# tune_model <- train(
#   x = grid_x,
#   y = grid_y,
#   method = "xgbTree",
#   trControl = ctrl,
#   tuneGrid = grid
# )

tune_model$bestTune


# Training Model
params <- list(
  objective = "reg:squarederror",  
  eval_metric = "rmse"    
)

pitch_model_data2 <- pitch_model_data %>% 
  filter(train == 1)

model_x <- as.matrix(pitch_model_data2[, features])
model_y <- pitch_model_data2$xwOBA

dtrain2 <- xgb.DMatrix(data = model_x, label = model_y)

pitch_model_test <- xgb.train(
  params = params,
  data = dtrain2,
  nrounds = 100,
  max_depth = 4,
  eta = 0.05,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 10,
  subsample = 0.5
)

pitch_pred_data <- pitch_model_data %>%
  select(all_of(features)) %>%
  as.matrix() %>%
  xgb.DMatrix()

pitch_preds <- pitch_model_data %>%
  mutate(pred = predict(pitch_model_test, pitch_pred_data))

pitch_preds %>% 
  ggplot(aes(x = xwOBA, y = pred, color = as.character(train))) +
  xlim(0.2, 0.45) + ylim(0.2, 0.45) +
  geom_point(size = 0.5, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0)


# Final Model

# Rounding Pitch Model Data
pitch_model_data <- pitch_model_data
  


x <- as.matrix(pitch_model_data[, features])
y <- pitch_model_data$xwOBA

dtrain <- xgb.DMatrix(data = x, label = y)

pitch_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  max_depth = 4,
  eta = 0.05,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 10,
  subsample = 1
)

xgb.save(pitch_model, "dominance_pitch.model")

