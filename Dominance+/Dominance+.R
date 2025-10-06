# Libraries ####
library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)


# Data
pitcher_stats <- read_csv("Data/pitcher_stats.csv")
pitch_stats <- read_csv("Data/pitch_stats.csv")

fangraphs <- read_csv("fangraphs_pitching.csv")

# Functions
swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}


# Models
pitcher_model <- xgb.load("dominance_pitcher.model")
pitch_model <- xgb.load("dominance_pitch.model")

features <- c("whiff", 'iz_whiff', "chase_rate", "hard_hit", "paint_rate",
              "iz_take")


# Model Predictions

# Pitcher
pitcher_pred_data <- pitcher_stats %>%
  select(all_of(features)) %>%
  as.matrix() %>%
  xgb.DMatrix()

pitcher_data <- pitcher_stats %>%
  mutate(pred_xwoba = predict(pitcher_model, pitcher_pred_data))

# Pitch
pitch_pred_data <- pitch_stats %>%
  select(all_of(features)) %>%
  as.matrix() %>%
  xgb.DMatrix()

pitch_data <- pitch_stats %>%
  mutate(pred_xwoba = predict(pitch_model, pitch_pred_data))

# League Averages
pitch_avgs <- pitch_data %>%
  mutate(pitch_class = case_when(pitch_type %in% c("FO", "FS") ~ "CH",
                                 pitch_type %in% c("SV") ~ "CU",
                                 TRUE ~ pitch_type)) %>% 
  group_by(pitch_class, game_year) %>% 
  summarize(lg_avg = mean(pred_xwoba, na.rm = TRUE),
            .groups = "drop")

year_avgs <- pitch_data %>%
  group_by(game_year) %>% 
  summarize(yr_avg = mean(pred_xwoba, na.rm = TRUE),
            .groups = "drop")


# Dominance: Pitch
pitch_dominance <- pitch_data %>%
  mutate(pitch_class = case_when(pitch_type %in% c("FO", "FS") ~ "CH",
                                 pitch_type %in% c("SV") ~ "CU",
                                 TRUE ~ pitch_type)) %>% 
  left_join(pitch_avgs, by = c("game_year", "pitch_class")) %>% 
  left_join(year_avgs, by = "game_year") %>% 
  mutate("Dominance+" = round(lg_avg/pred_xwoba, 3)*100,
         "gDominance+" = round(yr_avg/pred_xwoba, 3)*100) %>% 
  filter(pitches >= 100) %>% 
  arrange(desc(`Dominance+`)) %>% 
  mutate(player_name = sapply(player_name, swap_names))

pitch_dominance %>% 
  ggplot(aes(x = `Dominance+`, y = `gDominance+`, color = pitch_class)) +
  geom_line() +
  coord_fixed()


# Dominance: Pitcher
pitcher_dominance <- pitcher_data %>%
  left_join(year_avgs, by = "game_year") %>% 
  mutate("Dominance+" = round(yr_avg/pred_xwoba, 3)*100) %>% 
  filter(pitches >= 500) %>% 
  arrange(desc(`Dominance+`)) %>% 
  mutate(player_name = sapply(player_name, swap_names))

# 100 Average
pitcher_mean <- pitcher_dominance$`Dominance+` %>% mean
pitch_mean <- pitch_dominance$`Dominance+` %>% mean
gpitch_mean <- pitch_dominance$`gDominance+` %>% mean

pitcher_dominance <- pitcher_dominance %>% 
  mutate(`Dominance+` = `Dominance+` / pitcher_mean*100)

pitch_dominance <- pitch_dominance %>% 
  mutate(`Dominance+` = `Dominance+` / pitch_mean*100,
         `gDominance+` = `gDominance+` / gpitch_mean*100)

# Model Graphics
importance_pitcher <- xgb.importance(feature_names = features, model = pitcher_model) %>% 
  mutate(Model = "Pitcher")

importance_pitch <- xgb.importance(feature_names = features, model = pitch_model) %>% 
  mutate(Model = "Pitch")

importance_data <- bind_rows(importance_pitcher, importance_pitch) %>% 
  arrange(Feature, Model) %>% 
  mutate(Feature_Label = case_when(Feature == "hard_hit" ~ "Hard Hit Rate",
                                   Feature == "chase_rate" ~ "Chase Rate",
                                   Feature == "iz_whiff" ~ "In-Zone Whiff",
                                   Feature == "iz_take" ~ "In-Zone Take",
                                   Feature == "paint_rate" ~ "Paint Rate",
                                   Feature == "whiff" ~ "Whiff Rate"))

importance_pitcher <- importance_data %>% 
  filter(Model == "Pitcher") %>% 
  mutate(Feature = fct_reorder(Feature, Gain))

importance_pitch <- importance_data %>% 
  filter(Model == "Pitch") %>% 
  mutate(Feature = fct_reorder(Feature, Gain))

# Pitcher Importance Plot

custom_colors <- c(
  "hard_hit" = "purple",
  "chase_rate" = "darkorange",
  "iz_whiff" = "hotpink",
  "iz_take" = "royalblue",
  "paint_rate" = "gold",
  "whiff" = "red"
)

pitcher_importance_plot <- importance_pitcher %>% 
  ggplot(aes(x = Gain, y = Feature, fill = Feature)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(labels = scales::percent) +
  geom_label(aes(label = Feature_Label, x = Gain / 2), 
             hjust = 0.5, 
             family = "Times", 
             color = "black", 
             size = 3.25,
             fill = "white", 
             alpha = 0.75,
             label.padding = unit(0.1, "lines"),
             label.size = 0) +
  theme_minimal() + 
  labs(title = "Dominance+ Feature Importance",
       subtitle = "Pitcher Dominance",
       x = "Importance") +
  theme(text = element_text(family = "Times"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        axis.text.x = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())


# Pitch Importance Plot

pitch_importance_plot <- importance_pitch %>% 
  ggplot(aes(x = Gain, y = Feature, fill = Feature)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(labels = scales::percent) +
  geom_label(data = filter(importance_pitch, Gain > 0.1), 
             aes(label = Feature_Label, x = Gain / 2), 
             hjust = 0.5, 
             family = "Times", 
             color = "black", 
             size = 3.25,
             fill = "white",
             alpha = 0.75,
             label.padding = unit(0.1, "lines"),
             label.size = 0) + 
  geom_label(data = filter(importance_pitch, Gain <= 0.1), 
             aes(label = Feature_Label, x = Gain), 
             hjust = -0.15, 
             family = "Times", 
             color = "black", 
             size = 3.25,
             fill = "white",
             alpha = 0.75,
             label.padding = unit(0.1, "lines"),
             label.size = 0) +
  theme_minimal() + 
  labs(title = "Dominance+ Feature Importance",
       subtitle = "Pitch Dominance",
       x = "Importance") +
  theme(text = element_text(family = "Times"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        axis.text.x = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

pitch_importance_plot

# Saving Graphs
ggsave(pitcher_importance_plot, height = 5, width = 7,
       filename = "Graphs/pitcher_importance.png")

ggsave(pitch_importance_plot, height = 5, width = 7,
       filename = "Graphs/pitch_importance.png")


# Dominance vs Stats
dominance_fangraphs <- pitcher_dominance %>% 
  left_join(fangraphs, by = c("player_name" = "Name", "game_year" = "Season"))

# Starter Data
dominance_fangraphs %>% 
  filter(IP >= 100) %>% 
  View

pitch_dominance %>% 
  filter(pitches >= 200) %>% 
  View

pitch_dominance %>% 
  filter(pitches >= 500) %>% 
  View


# Dominance Data Export
dominance_fangraphs %>% 
  # select(player_name, game_year, G, IP, pitches, `Dominance+`, xwOBA, WAR, `Stuff+`) %>% 
  write.csv("pitcher_dominance.csv", row.names = FALSE)

write.csv(pitch_dominance, "pitch_dominance.csv", row.names = FALSE)


# Dominance Predictivity

compare_stat_years <- function(data,
                               player_col = "player_name",
                               year_col = "game_year",
                               stat = "Dominance+",
                               stat_prior  = "Dominance+",
                               offset = 1) {
  player_sym <- sym(player_col)
  year_sym <- sym(year_col)
  stat_sym <- sym(stat)
  stat_prior_sym <- sym(stat_prior)
  
  # Column Names
  season_col <- paste0("season_", stat)
  next_col <- paste0("prior_", stat_prior)
  
  # Current
  prior <- data %>%
    select(!!player_sym, !!year_sym, !!stat_sym) %>%
    rename(!!season_col := !!stat_sym)
  
  # Prior Data
  next_year_data <- data %>%
    select(!!player_sym, !!year_sym, !!stat_prior_sym) %>%
    rename(!!next_col := !!stat_prior_sym) %>%
    mutate(!!year_sym := !!year_sym + offset)
  

  paired <- prior %>%
    left_join(next_year_data, by = c(player_col, year_col)) %>%
    select(!!player_sym, !!year_sym, !!sym(season_col), !!sym(next_col))
  
  paired
}


comp_dom_dom <- compare_stat_years(dominance_fangraphs,
                   stat = "Dominance+",
                   stat_prior = "Dominance+",
                   offset = 1)

cor_dom_dom <- comp_dom_dom %>% 
  filter(!is.na(`prior_Dominance+`)) %>% 
  with(cor(`prior_Dominance+`, `season_Dominance+`))

comp_dom_dom %>% 
  mutate(stat = ifelse(`season_Dominance+` >= 100, 1, 0),
         stat_prior = ifelse(`prior_Dominance+` >= 100, 1, 0)) %>% 
  group_by(stat_prior) %>% 
    summarize(mean = mean(stat, na.rm = TRUE))

comp_dom_dom %>%   
  ggplot(aes(x = `prior_Dominance+`, y = `season_Dominance+`)) +
  geom_abline(slope = 1, intercept = 0, 
              color = "green", linetype = 44, size = 1.25) +
  geom_point() +
  geom_smooth(size = 1.5, alpha = 0) +
  xlim(80, 140) + ylim(80, 140) +
  labs(title = "Year-to Year Predictivity",
       subtitle = "Predicting Dominance+ Using Prior Dominance+",
       x = "Prior Dominance+",
       y = "Dominance+",
       caption = paste("Correlation:", round(cor_dom_dom, 4))) +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



# Predictivity
cor_all_combinations <- function(data, stats, offset = 1) {
  results <- data.frame(
    stat_prior = character(),
    stat_season = character(),
    correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (prior_stat in stats) {
    for (season_stat in stats) {
      comp <- compare_stat_years(
        data,
        stat = season_stat,
        stat_prior = prior_stat,
        offset = offset
      )
      
      comp <- comp %>% filter(!is.na(!!sym(paste0("prior_", prior_stat))))
      
      corr_val <- cor(comp[[paste0("prior_", prior_stat)]], comp[[paste0("season_", season_stat)]])
      
      results <- rbind(results, data.frame(
        stat_prior = prior_stat,
        stat_season = season_stat,
        correlation = corr_val,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

# Example usage:
stats <- c("ERA", "FIP", "Dominance+", "xERA", "WAR", "Stuff+")

dominance_fangraphs %>% 
  cor_all_combinations(stats) %>% 
  mutate(abs_cor = abs(correlation)) %>% 
  arrange(stat_season, desc(abs_cor)) %>% 
  filter(stat_prior == stat_season | stat_prior == "Dominance+" | stat_season == "Dominance+") %>% 
  View


# Dominance+ vs ERA
dominance_fangraphs %>% 
  compare_stat_years(stat = "ERA",
                   stat_prior = "Dominance+",
                   offset = 1) %>%   
  mutate(sERA = case_when(season_ERA >= 6 ~ 6,
                          season_ERA <= 2.25 ~ 2.25,
                          TRUE ~ season_ERA)) %>% 
  ggplot(aes(x = `prior_Dominance+`, y = season_ERA,
             color = sERA)) +
  geom_point(shape = 18, size = 2) +
  geom_smooth(size = 1.5, alpha = 0, color = "black") +
  labs(title = "Predicting ERA Using Prior Dominance+",
       x = "Prior Dominance+",
       y = "ERA") +
  xlim(80, 140) + 
  scale_y_continuous(breaks = seq(0, 8, 1), limits = c(0, 7),
                     labels = label_number(accuracy = 0.01)) +
  scale_color_gradient2(low = "red", mid = "gray70", high = "blue",
                        midpoint = 4) +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")


# Dominance+ vs ERA - Starters
dominance_fangraphs %>% 
  filter(IP >= 100) %>%
  compare_stat_years(stat = "ERA",
                     stat_prior = "Dominance+",
                     offset = 1) %>%  View 
  mutate(sERA = case_when(season_ERA >= 6 ~ 6,
                          season_ERA <= 2.25 ~ 2.25,
                          TRUE ~ season_ERA)) %>% 
  ggplot(aes(x = `prior_Dominance+`, y = season_ERA,
             color = sERA, fill = sERA)) +
  geom_point(shape = 25, size = 2.5) +
  geom_smooth(size = 1.5, alpha = 0, color = "black") +
  labs(title = "Predicting ERA Using Prior Dominance+",
       subtitle = "Minimum 100 IP in each season",
       x = "Prior Dominance+",
       y = "ERA") +
  scale_x_continuous(breaks = seq(80, 120, 5), limits = c(87, 117)) +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(0, 7),
                     labels = label_number(accuracy = 0.01)) +
  scale_color_gradient2(low = "red", mid = "gray70", high = "blue",
                        midpoint = 4) +
  scale_fill_gradient2(low = "red", mid = "gray70", high = "blue",
                        midpoint = 4) +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
                       