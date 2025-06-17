library(tidyverse)
library(lubridate)



# Functions ####
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

swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}


scale_color_pitch <- function() {
  scale_color_manual(
    values = c(
      "FF" = "#E41A1C",
      "CH"        = "#377EB8",
      "CU"       = "#4DAF4A",
      "FC"          = "#984EA3",
      "SL"          = "#FF7F00",
      "ST"         = "#A65628",
      "SI"          = "#F781BF",
      "FS"    = "#999999",
      "SV"          = "#FFD92F",
      "KC"   = "#8DA0CB",
      "CS"      = "#FC8D62",
      "FO"        = "#A6D854",
      "KN"     = "#E78AC3",
      "SC"       = "#40B0A6"
    ),
  )
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
full_data <- read_csv("all_data.csv") %>% 
  filter(!(pitch_name %in% c("NA", "Pitch Out", "Eephus", "Other", "Knuckleball")), 
         !is.na(pitch_name))

stuff <- read_csv("Stuff.csv") %>% 
  select(Name, Season, `Stuff+`)

stuff_pitch <- read_csv("Stuff.csv") %>% 
  select(Name, Season, FF:FO) %>% 
  pivot_longer(cols = FF:FO,
               names_to = "Pitch",
               values_to = "Stuff") %>% 
  filter(!is.na(Stuff)) %>% 
  arrange(Name, Season) %>% 
  mutate(Name = str_to_lower(Name),
         Name = str_replace_all(Name, "[^a-z0-9]", "")) %>% 
  distinct(Name, Season, Pitch, .keep_all = TRUE)


# Joining Stuff
join <- full_data %>% 
  mutate(player_name = sapply(player_name, swap_names),
         year = year(game_date)) %>% 
  mutate(Name = str_to_lower(player_name),
         Name = str_replace_all(Name, "[^a-z0-9]", ""),
         Pitch = case_when(pitch_type %in% c("SV", "CS", "SC") ~ "CU",
                           pitch_type == "ST" ~ "SL",
                           TRUE ~ pitch_type)) %>% 
  left_join(stuff_pitch, by = c("Name", "year" = "Season", "Pitch")) %>% 
  select(-Name, -Pitch)

# Adding Missing 2024 pitches using 2025 Stuff+

join1 <- join %>% 
  filter(!is.na(Stuff))

join2 <- join %>% 
  filter(is.na(Stuff)) %>% 
  select(-Stuff) %>% 
  mutate(player_name = sapply(player_name, swap_names),
         year = year(game_date)) %>% 
  mutate(Name = str_to_lower(player_name),
         Name = str_replace_all(Name, "[^a-z0-9]", ""),
         Pitch = case_when(pitch_type %in% c("SV", "CS", "SC", "KC") ~ "CU",
                           pitch_type == "ST" ~ "SL",
                           TRUE ~ pitch_type)) %>% 
  left_join(stuff_pitch, by = c("Name", "Pitch")) %>% 
  select(-Name, -Pitch, -Season)


data <- bind_rows(join1, join2) %>% 
  filter(!is.na(Stuff))


pitch_data <- data %>% 
  mutate(pitch_type = ifelse(pitch_type %in% c("SV", "CS", "SC", "KC"), "CU", pitch_type)) %>% 
  group_by(player_name, year, pitch_type) %>% 
  summarize(N = n(),
            Speed = mean(release_speed, na.rm = TRUE),
            Spin = mean(release_spin_rate, na.rm = TRUE),
            IVB = mean(pfx_z, na.rm = TRUE),
            HB = mean(pfx_x, na.rm = TRUE),
            Stuff = mean(Stuff, na.rm = TRUE),
            RE = sum(delta_pitcher_run_exp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(player_name, year) %>%
  mutate(SeasonPC = sum(N),
         Usage = N / SeasonPC,
         RE100 = RE / N * 100) %>%
  ungroup() %>% 
  rename(Name = player_name,
         Season = year,
         Pitch = pitch_type) %>% 
  select(Name, Season, Pitch, N, Usage,
         Stuff, Speed, Spin, IVB, HB,
         RE100)


arsenal_re <- data %>%
  group_by(player_name, year) %>% 
  summarize(RE = sum(delta_pitcher_run_exp, na.rm = TRUE),
            N = n()) %>% 
  ungroup() %>% 
  mutate(RE100 = RE/N*100) %>% 
  select(-RE, -N)

arsenal <- pitch_data %>% 
  select(Name, Season, Pitch, N, Stuff) %>% 
  pivot_wider(names_from = Pitch,
              values_from = c(N, Stuff)) %>% 
  left_join(arsenal_re, by = c("Name" = "player_name", "Season" = "year")) %>% 
  mutate(Pitches = rowSums(select(., N_CH:N_FO), na.rm = TRUE),
         across(N_CH:N_FO, ~ round(.x / Pitches, 3))) %>% 
  filter(Pitches >= 100) %>% 
  rename_with(~ str_replace(.x, "^N_", "Usage_"), starts_with("N_"))

write.csv(arsenal, "arsenal_data.csv", row.names = FALSE)
