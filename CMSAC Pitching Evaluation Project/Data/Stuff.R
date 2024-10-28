library(tidyverse)
library(readxl)
library(stringi)
library(patchwork)
library(Lahman)


# Function to remove accent marks from a string ####
remove_accents <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

remove_pattern <- function(data, col, pattern) {
  data %>%
    mutate(!!col := gsub(pattern, "", !!sym(col)))
}

# New Function Cleaning ####
separate_name_into_first_last <- function(data, full_name) {
  data %>% 
    separate(full_name, into = c("first_name", "last_name"), sep = " ")
}
rename_col <- function(data, first, last) {
  data %>% 
    rename(first_name = first, last_name = last)
}
combine_names <- function(data) {
  data %>% mutate(full_name = paste(first_name, last_name, sep = " "))
}
clean_names <- function(data) {
  remove_accents <- function(x) {
    stri_trans_general(x, "Latin-ASCII")
  }
  
  remove_pattern <- function(data, col, pattern) {
    data %>%
      mutate(!!col := gsub(pattern, "", !!sym(col)))
  }
  
  # Remove accents
  data <- data %>%
    mutate_at(vars("first_name"), remove_accents)
  
  data <- data %>%
    mutate_at(vars("last_name"), remove_accents)
  
  # Remove "."
  data <- data %>% 
    remove_pattern("first_name", "\\.")
  
  data <- data %>% 
    remove_pattern("last_name", "\\.")
  
  data <- data %>% 
    remove_pattern("last_name", "\\ Jr")
  
  data <- data %>% 
    remove_pattern("first_name", "\\ ")
  
}

# Ethan's Data ####
e_stuff <- read_excel("Stuff Plus.xlsx", sheet = "Data") %>% 
  separate_name_into_first_last("Name") %>% 
  clean_names() %>% 
  combine_names()

e_movement <- read_excel("Stuff Plus.xlsx", sheet = "Movement") %>% 
  filter(year > 2019) %>% 
  clean_names() %>% 
  combine_names()

e_people_data <- Lahman::People %>%
  rename_col("nameFirst", "nameLast") %>% 
  select(first_name, last_name, playerID) %>% 
  clean_names() %>% 
  combine_names() %>% 
  select(full_name, playerID) %>% 
  rename("Name" = "full_name")

# Importing Data ####
stuff <- read_excel("Stuff Plus.xlsx", sheet = "Data")

stuff_explore <- read_excel("Stuff Plus.xlsx", sheet = "Extended")

movement_dirty <- read_excel("Stuff Plus.xlsx", sheet = "Movement")
movement <- movement_dirty %>% 
  filter(year > 2019)

pitches <- read_excel("Stuff Plus.xlsx", sheet = "Results_Pitch")

# Reformatting Movement (Statcast) ####
movement <- movement %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU"),
         pitch_type = str_replace(pitch_type, "ST", "SL"),
         pitch_type_name = str_replace(pitch_type_name, "Sweeper", "Slider"),
         pitch_type = str_replace(pitch_type, "SV", "SL"),
         pitch_type_name = str_replace(pitch_type_name, "Sluve", "Slider"))

movement <- movement %>% 
  mutate("Name" = paste(first_name, last_name)) %>% 
  select(-first_name, -last_name, -team_name, -pitcher_id, -diff_z, -diff_x)

movement <- movement %>% 
  mutate_at(vars("Name"), remove_accents)

movement <- movement %>%
  mutate(ID = paste0(Name, year, "_", pitch_type))

# Reformatting Stuff (Fangraphs) ####
stuff <- stuff %>% 
  rename(`Stf CUR` = `Stf CU`)

stuff <- stuff %>% 
  mutate(`Stf CU` = case_when(
    `Stf CUR` > -300 ~ `Stf CUR`,
    `Stf KC` > -300 ~ `Stf KC`,
    TRUE ~ NA))

stuff <- stuff %>% 
  select(-`Stf CUR`, -`Stf KC`)

stuff <- stuff %>% 
  rename(FF = `Stf FA`,
         FC = `Stf FC`,
         SI = `Stf SI`,
         SL = `Stf SL`,
         CU = `Stf CU`,
         CH = `Stf CH`,
         FS = `Stf FS`)

stuff <- stuff %>% 
  pivot_longer(c(FF:CH, CU))

stuff <- stuff %>% 
  rename(pitch_type = name, `Stf+ Pitch` = value)

stuff <- stuff %>% 
  mutate(ID = paste0(Name, Season, "_",pitch_type))

stuff <- stuff %>% 
  mutate(Team = str_replace(Team, "ARI", "AZ"),
         Team = str_replace(Team, "CHW", "CWS"),
         Team = str_replace(Team, "KCR", "KC"),
         Team = str_replace(Team, "SDP", "SD"),
         Team = str_replace(Team, "SFG", "SF"),
         Team = str_replace(Team, "TBR", "TB"),
         Team = str_replace(Team, "WSN", "WSH")
         )

stuff <- stuff %>% 
  mutate(ID = paste0(Name, Season, "_",pitch_type))

# Note: for statcast, slurve, sweeper are now categorized as sliders too
# Note: for fangraphs, knuckle and regular curve (priority) are integrated

stuff_join <- stuff %>% 
  select(IP:`Pitching`, `Stf+ Pitch`, ID)

# Aggregation ####
data_init <- movement %>% 
  left_join(stuff_join, by = "ID")

data_init1 <- data_init %>% 
  filter(Name != "Luis Garcia")


stuff_init1 <- stuff %>% 
  filter(Name == "Luis Garcia") %>%
  mutate(ID = paste0(Name, Season, Team, "_",pitch_type)) %>% 
  select(IP:`Pitching`, `Stf+ Pitch`, ID)

movement_init1 <- movement %>%
  filter(Name == "Luis Garcia") %>%
  mutate(ID = paste0(Name, year, team_name_abbrev, "_", pitch_type))

data_init2 <- movement_init1 %>% 
  left_join(stuff_init1, by = "ID")


data1 <- rbind(
  data_init1, data_init2)  

data1 <- data1 %>% 
  select(Name, year:pitch_hand, pitch_type:pitch_type_name, 
         IP:`Stf+ Pitch`, pitcher_break_z:percent_rank_diff_x,
         avg_speed:pitch_per) %>% 
  filter(!is.na(IP))

Data <- data1 %>% 
  left_join(e_people_data, by = c("Name" = "Name"))

Data <- Data %>% 
  select(-`new name`) %>% 
  filter(!is.na(IP)) %>% 
  filter(!is.na(playerID)) %>% 
  filter(Name != "Luis Garcia")


# Graphs

# Slider ####
movement_slider_r <- data %>% 
  filter(pitch_type == "SL") %>% 
  filter(pitch_hand == "R") %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(aes(x = pitcher_break_x, y = pitcher_break_z,
                 color = `Stuff+`), alpha = 0.7) +
  scale_y_reverse() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Horizontal Break",
       y = "Vertical Break",
       title = "Sliders (R)") +
  xlim(40, -40) + ylim(60, -60) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect("white"),
        plot.title = element_text(hjust = 0.5)) + 
  coord_fixed(ratio = 1)
  
movement_slider_l <- data %>% 
    filter(pitch_type == "SL") %>% 
    filter(pitch_hand == "L") %>% 
  ggplot() +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(aes(x = pitcher_break_x, y = pitcher_break_z,
                   color = `Stuff+`), alpha = 0.7) +
    scale_y_reverse() +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Horizontal Break",
         y = "Vertical Break",
         title = "Sliders (L)") +
    xlim(-40, 40) + ylim(60, -60) +
    theme(axis.title = element_blank(), axis.ticks = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect("white"),
          plot.title = element_text(hjust = 0.5)) + 
    coord_fixed(ratio = 1)

movement_slider <- movement_slider_l + movement_slider_r
movement_slider
ggsave("slider.png",movement_slider)


# 4-Seam Fastball ####
movement_fourseam_r <- data %>% 
  filter(pitch_type == "FF") %>% 
  filter(pitch_hand == "R") %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(aes(x = pitcher_break_x, y = pitcher_break_z,
                 color = `Stuff+`), alpha = 0.7) +
  scale_y_reverse() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Horizontal Break",
       y = "Vertical Break",
       title = "4-Seam (R)") +
  xlim(-30, 30) + ylim(-30, 30) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect("white"),
        plot.title = element_text(hjust = 0.5)) + 
  coord_fixed(ratio = 1)

movement_fourseam_l <- data %>% 
  filter(pitch_type == "FF") %>% 
  filter(pitch_hand == "L") %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(aes(x = pitcher_break_x, y = pitcher_break_z,
                 color = `Stuff+`), alpha = 0.7) +
  scale_y_reverse() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Horizontal Break",
       y = "Vertical Break",
       title = "Four-Seam (L)") +
  xlim(30, -30) + ylim(-30, 30) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect("white"),
        plot.title = element_text(hjust = 0.5)) + 
  coord_fixed(ratio = 1)

movement_fourseam <- movement_fourseam_l + movement_fourseam_r
movement_fourseam
ggsave("fourseam.png",movement_fourseam)



data %>% 
  filter(pitch_type == "FF") %>% 
  filter(pitch_hand == "R") %>% 
  filter(avg_speed >= 94) %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(aes(x = pitcher_break_x, y = pitcher_break_z,
                 color = avg_speed), alpha = 0.75) +
  scale_y_reverse() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Horizontal Break",
       y = "Vertical Break",
       title = "4-Seam (R)",
       color = "MPH") +
  xlim(-30, 30) + ylim(-30, 30) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect("white"),
        plot.title = element_text(hjust = 0.5)) + 
  coord_fixed(ratio = 1)

# Exploratory Data Analysis of Pitch Results Data ####
p_1 <- pitches %>% 
  select(result:release_extension) %>% 
  filter(pitches > 9)

# Andrew Heaney FF Demo ####
heaney <- p_1 %>% 
  filter(player_name == "Heaney, Andrew") %>% 
  select(result:pitches, player_name:pitch_percent, woba, launch_speed:velocity)

heaney_fast <- heaney %>% 
  filter(pitch_type == "FF")

heaney_fast %>% 
  ggplot(aes(x = result, y = woba, fill = pitches)) +
  geom_col() +
  scale_fill_continuous(low = "#FFE338", high = "forestgreen") +
  labs(y = "wOBA",
       x = "Result",
       fill = "# of Pitches",
       title = "Andrew Heaney Batted Balls on 4-Seam Fastballs") +
  theme_classic()

