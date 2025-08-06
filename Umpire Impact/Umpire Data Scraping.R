library(tidyverse)
library(lubridate)
library(Metrics)
library(xgboost)
library(jsonlite)

options(scipen = 999)
set.seed(3630)


game_list <- read_csv("umpire_data.csv") %>% 
  distinct(game_pk) %>% 
  mutate(ump = as.character(NA))






for(i in 1:nrow(game_list)) {
  
  game_pk <- game_list[i, 1]

url <- paste0("https://statsapi.mlb.com/api/v1/game/", game_pk, "/boxscore")

# Find Ump
ump <- fromJSON(url)$officials %>% 
  filter(officialType == "Home Plate") %>% 
  pull(official) %>% 
  pluck("fullName")

game_list[i, 2] <- ump

print(paste("progress:", i, "of", nrow(game_list)))

}

write.csv(game_list, "ump_names.csv", row.names = FALSE)

# game_list <- read_csv("ump_names.csv")

data <- read_csv("umpire_data.csv")

data %>% 
  select(-umpire) %>% 
  left_join(game_list, by = "game_pk") %>% 
  write.csv("umpire_data2.csv", row.names = FALSE)
