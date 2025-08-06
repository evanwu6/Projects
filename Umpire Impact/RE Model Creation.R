library(tidyverse)
library(lubridate)
library(Metrics)
library(xgboost)
library(caret)

options(scipen = 999)
set.seed(3630)


raw_data <- read_csv("umpire_data2.csv")

data <- raw_data %>% 
  mutate(Indicator = rbinom(n = n(), size = 1, prob = 0.8)) %>% 
  # Setting Up Data Structure - Making XGBoost Friendly
  mutate(on_1b = ifelse(is.na(on_1b), 0, 1),
         on_2b = ifelse(is.na(on_2b), 0, 1),
         on_3b = ifelse(is.na(on_3b), 0, 1)) %>% 
  mutate(inning_topbot = ifelse(inning_topbot == "Top", 1, 0),
         truth = ifelse(truth == "strike", 1, 0),
         call = ifelse(call == "strike", 1, 0)) %>%
  rename(inning_top = inning_topbot,
         is_strike = truth,
         called_strike = call)


train <- data %>% 
  filter(Indicator == 1)

test <- data %>% 
  filter(Indicator == 0)

train <- train %>% 
  filter(if_all(everything(), ~ !is.na(.)))

test <- test %>% 
  filter(if_all(everything(), ~ !is.na(.)))



# Modeling
x <- as.matrix(train[, c("inning", "inning_top", "balls", "strikes", "outs_when_up",
                         "on_1b", "on_2b", "on_3b", "fld_lead", "called_strike")])
y <- train$fld_delta_re


dtrain <- xgb.DMatrix(data = x, label = y)

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# Hyperparameter Tuning
ctrl <- trainControl(method = "cv", number = 5)

grid <- expand.grid(
  nrounds = c(10, 15, 25, 50, 100),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_model <- train(
  x = x,
  y = y,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid
)

tune_model$bestTune


# Train the model
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  max_depth = 9,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Save
xgb.save(model, "bsre.model")

features <- c("inning", "inning_top", "balls", "strikes", "outs_when_up",
              "on_1b", "on_2b", "on_3b", "fld_lead", "called_strike")

preds <- test %>%
  select(all_of(features)) %>%
  as.matrix() %>%
  xgb.DMatrix()

# Predict and join with meanpred
result <- test %>%
  mutate(pred = predict(model, preds))



