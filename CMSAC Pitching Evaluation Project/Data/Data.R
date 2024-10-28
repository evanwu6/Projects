# Libraries ####
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

metrics <- read_excel("Stuff Plus.xlsx", sheet = "Metrics")

pitches <- read_excel("Stuff Plus.xlsx", sheet = "Results_Pitch") %>% 
  select(-whiffs, -takes)

spin <- read_excel("Stuff Plus.xlsx", sheet = "Spin")

seq2022 <- read_csv("2022_seq.csv")

# Reformatting Movement (Statcast) ####

movement <- movement %>% 
  mutate("Name" = paste(first_name, last_name)) %>% 
  select(-first_name, -last_name, -team_name, -diff_z, -diff_x)

movement <- movement %>% 
  mutate_at(vars("Name"), remove_accents)

movement <- movement %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU"),
         pitch_type = str_replace(pitch_type, "ST", "SL"),
         pitch_type_name = str_replace(pitch_type_name, "Sweeper", "Slider"),
         pitch_type = str_replace(pitch_type, "SV", "SL"),
         pitch_type_name = str_replace(pitch_type_name, "Slurve", "Slider"))

movement <- movement %>%
  mutate(ID = paste0(Name, year, "_", pitch_type))


# Reformatting Metrics (Stuff+) ####

metrics <- metrics %>% 
  mutate("Name" = paste(first_name, last_name)) %>% 
  select(-first_name, -last_name)

metrics <- metrics %>% 
  mutate_at(vars("Name"), remove_accents)

metrics <- metrics %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU"),
         pitch_type = str_replace(pitch_type, "ST", "SL"),
         pitch_name = str_replace(pitch_name, "Sweeper", "Slider"),
         pitch_type = str_replace(pitch_type, "SV", "SL"),
         pitch_name = str_replace(pitch_name, "Slurve", "Slider"))

metrics <- metrics %>%
  mutate(ID = paste0(Name, Season, "_", pitch_type)) %>% 
  select(run_value, run_value_per_100, pa:hard_hit_percent, ID)

# Reformatting spin ####

spin <- spin %>% 
  mutate("Name" = paste(first_name, last_name))

spin <- spin %>% 
  pivot_longer(c(FF:FS))

spin <- spin %>% 
  rename(pitch_type = name, spin_rate = value)

spin <- spin %>% 
  filter(!is.na(spin_rate))

spin <- spin %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU"),
         pitch_type = str_replace(pitch_type, "ST", "SL"),
         pitch_type = str_replace(pitch_type, "SV", "SL"))

spin <- spin %>% 
  mutate(ID = paste0(Name, Season, "_", pitch_type))

spin <- spin %>% 
  select(ID, spin_rate)


# Combining movement, metrics, spin ####

movement <- movement %>% 
  left_join(metrics, by = "ID")

movement <- movement %>% 
  left_join(spin, by = "ID")



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

# Adding back spin data ####
spin <- read_excel("Stuff Plus.xlsx", sheet = "Spin") %>% 
  mutate("Name" = paste(first_name, last_name)) %>% 
  pivot_longer(c(FF:CUKC)) %>% 
  rename(pitch_type = name, spin_rate = value) %>% 
  filter(!is.na(spin_rate)) %>% 
  select(Season, Name, player_id, pitch_type, spin_rate)

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
         avg_speed:pitch_per, spin_rate, run_value:hard_hit_percent) %>% 
  filter(!is.na(IP))

Data <- data1 %>% 
  left_join(e_people_data, by = c("Name" = "Name"))

Data <- Data %>% 
  filter(playerID != "danisty01",
         playerID != "rogerty01")


Data <- Data %>% 
  mutate(pitch_type_name = str_replace(pitch_type_name, "Slurve", "Slider"))

Data <- Data %>% 
  select(-`new name`) %>% 
  filter(!is.na(IP)) %>% 
  filter(!is.na(playerID)) %>% 
  filter(Name != "Luis Garcia")

# Fastball Differential Data ####

Data <- Data %>% 
  mutate(id = paste0(playerID, "_", year))

FB <- Data %>% 
  filter(pitch_type == "FF" | pitch_type == "FC" | pitch_type == "SI") %>% 
  select(id, pitch_type, `Stf+ Pitch`, avg_speed, pitches_thrown) %>% 
  rename(fb_type = pitch_type, fb_stuff = `Stf+ Pitch`, 
         fb_speed = avg_speed, fb_thrown = pitches_thrown) %>% 
  arrange(desc(fb_thrown)) %>% 
  distinct(id, .keep_all = TRUE)

Data2 <- Data %>% 
  left_join(FB, by = "id")

Data2 <- Data2 %>% 
  mutate(speed_diff = avg_speed - fb_speed)

rm(FB)

Data <- Data %>% 
  select(-id)

Data2 <- Data2 %>% 
  select(-id)

Data2 <- Data2 %>% 
  rename(rv100 = run_value_per_100)

# Inter-Pitch Data ####

ip <- function(url, pitch, season){
read_csv(url) %>% 
  mutate(pitch_type = pitch, year = season) %>% 
  select(pitch_type, year, player_name:FC_release_pos_z_diff)}

# sl_2020 <- ip("Ethan-Data/Inter-Pitch/2020/2020_SL_inter_pitch.csv", "SL", 2020)


# Environment Cleaning ####
rm(data_init)
rm(data_init1)
rm(data_init2)
rm(data1)
rm(e_movement)
rm(e_people_data)
rm(e_stuff)
rm(movement)
rm(movement_dirty)
rm(movement_init1)
rm(stuff)
rm(stuff_init1)
rm(stuff_join)
rm(clean_names)
rm(combine_names)
rm(remove_accents)
rm(remove_pattern)
rm(rename_col)
rm(separate_name_into_first_last)

# Notes ####
# Removed Luis Garcia, Tyler Danish
# Categorized Sweeper and Slurve as Sliders, Knuckle Curves as Curveballs

