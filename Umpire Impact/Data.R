library(tidyverse)
library(lubridate)


raw_data <- read_csv("all_data2.csv")

# Swinging Strike vs. Called Strike Impact
raw_data %>% 
  filter(description %in% c("called_strike", "swinging_strike")) %>% 
  mutate(count = paste(balls, strikes, sep = "-")) %>% 
  group_by(count, description) %>% 
  summarize(n = n(),
            re = mean(delta_pitcher_run_exp, na.rm = TRUE))


data <- raw_data %>% 
  mutate(fld_lead = -bat_score_diff) %>%
  mutate(call = case_when(description %in% c("called_strike", "swinging_strike", "swinging_strike_blocked", "foul_tip") ~ "strike",
                        description %in% c("blocked_ball", "ball", "pitchout") ~ "ball",
                        TRUE ~ "other")) %>% 
  mutate(description = ifelse(description %in% c("swinging_strike_blocked", "foul_tip"), "swinging_strike", description)) %>% 
  mutate(truth = ifelse(zone %in% 1:9, "strike", "ball")) %>% 
  mutate(fld_delta_wp = ifelse(inning_topbot == "Top" ,home_win_exp, -home_win_exp)) %>% 
  select(game_date, game_pk, zone, inning, inning_topbot, 
         truth, call, description,
         balls, strikes, outs_when_up, on_1b, on_2b, on_3b,
         fld_lead, delta_pitcher_run_exp, fld_delta_wp,
         umpire) %>% 
  rename(fld_delta_re = delta_pitcher_run_exp) %>% 
  filter(call != "other")

write.csv(data, "umpire_data.csv", row.names = FALSE)

