library(tidyverse)
library(ranger)
library(mgcv)


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
data <- read_csv("all_data.csv") %>% 
  filter(!(pitch_name %in% c("NA", "Pitch Out", "Eephus", "Other")), !is.na(pitch_name))

default <- data %>% 
  filter(zone == 5) %>% 
  filter(pitch_type == "FF")

# Attack Angle (swing up/down/flat)
# Tilt

mm <- default %>% 
  group_by(batter) %>% 
  filter(!is.na(swing_path_tilt)) %>%
  summarize(dTilt = mean(swing_path_tilt, na.rm = TRUE),
            dAngle = mean(attack_angle, na.rm = TRUE)) %>% 
  ungroup()

expected <- data %>% 
  filter(!is.na(swing_path_tilt)) %>%
  left_join(mm, by = "batter") %>% 
  mutate(delta_tilt = swing_path_tilt - dTilt,
         delta_angle = attack_angle - dAngle)


  
# Predicting vertical location using vertical bat angle and swing tilt.

model_data <- expected %>% 
  filter(!is.na(swing_path_tilt), !is.na(dTilt), !is.na(delta_tilt),
         !is.na(attack_angle), !is.na(dAngle), !is.na(delta_angle))

model <- gam(plate_z ~ s(delta_tilt) + s(delta_angle),
                data = model_data)
# Removed sz_top because it wasn't that useful (+ 0.0324 R2)

preds <- expected %>% 
  mutate(prediction = predict(model, expected)) %>% 
  filter(!is.na(prediction))
  

cor(preds$prediction, preds$plate_z)^2


# Tunnels ####
# Pitch Tunnels

tunnels <- data %>% 
  group_by(player_name, pitch_type) %>% 
  summarize(IVB = mean(pfx_z, na.rm = TRUE)) %>% 
  ungroup()

tunnels <- tunnels %>% 
  pivot_wider(names_from = "pitch_type",
              values_from = "IVB")

# Computing Pitch Tunnels
tunnel_data <- expected %>% 
  left_join(tunnels, by = "player_name") %>% 
  mutate(tunnel_sl = plate_z - pfx_z + SL,
         tunnel_cu = plate_z - pfx_z + CU,
         tunnel_fc = plate_z - pfx_z + FC,
         tunnel_ff = plate_z - pfx_z + FF,
         tunnel_fs = plate_z - pfx_z + FS,
         tunnel_si = plate_z - pfx_z + SI,
         tunnel_ch = plate_z - pfx_z + CH,
         tunnel_st = plate_z - pfx_z + ST,
         tunnel_fa = plate_z - pfx_z + FA,
         tunnel_kc = plate_z - pfx_z + KC,
         tunnel_sv = plate_z - pfx_z + SV,
         tunnel_cs = plate_z - pfx_z + CS,
         tunnel_kn = plate_z - pfx_z + KN,
         tunnel_sc = plate_z - pfx_z + SC,
         tunnel_fo = plate_z - pfx_z + FO)

# Predicting Height
tunnel_data <- tunnel_data %>% 
  mutate(xHeight = predict(model, tunnel_data))


# Location Differences
diffs <- tunnel_data %>% 
  filter(!is.na(xHeight)) %>% 
  select(tunnel_sl:tunnel_fo, xHeight) %>% 
  filter(!if_all(starts_with("tunnel_"), is.na)) %>% 
  mutate(across(.cols = starts_with("tunnel_"),
                .fns = ~ abs(xHeight - .),
                .names = "diff_{.col}")) %>% 
  select(-xHeight, -tunnel_sl:-tunnel_fo) %>% 
  rowwise() %>% 
  mutate(xPitch = names(.)[which.min(c_across(everything()))]) %>%
  ungroup() %>% 
  select(xPitch) %>% 
  mutate(xPitch = str_remove(xPitch, "diff_tunnel_"),
         xPitch = toupper(xPitch))




tunnel_data <- tunnel_data %>% 
  filter(!is.na(xHeight)) %>% 
  bind_cols(diffs) %>% 
  mutate(is_match = ifelse(xPitch == pitch_type, 1, 0),
         is_whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked", "missed_bunt"),
                           1, 0))

tunnel_data %>%
  group_by(xPitch, pitch_type) %>% 
  summarize(N = n(),
            whiff_rate = mean(is_whiff, na.rm = TRUE)) %>% 
  filter(N > 200) %>% 
  arrange(xPitch, desc(whiff_rate)) %>% 
  View

# Saving Models & Data
save(model, file = "xHeightGAM.rda")
# load("xHeightGAM.rda")

write.csv(tunnel_data, "full_data_predicted.csv", row.names = FALSE)
# tunnel_data <- read_csv("full_data_predicted.csv")

# Pivot Example
crochet <- tunnel_data %>% 
  filter(player_name == "Crochet, Garrett") %>% 
  head(10) %>% 
  bind_cols(1:10) %>% 
  rename(pitch_num = ...154) %>% 
  select(-tunnel_fa:-tunnel_fo, -tunnel_fs, -tunnel_sl:-tunnel_cu, -CH:-SC) %>% 
  pivot_longer(
    cols = starts_with("tunnel_"),
               names_to = "tunnel") %>% 
  mutate(tunnel = toupper(str_replace(tunnel, "tunnel_", ""))) %>% 
  rename(tunnel_height = value)


crochet %>%
  ggplot(aes(x = 1, y = tunnel_height)) +
  geom_hline(aes(yintercept = xHeight)) +
  geom_point(aes(color = tunnel)) +
  geom_label(aes(x = 1, y = 1, label = pitch_type), nudge_y = -0.05, size = 3) +
  facet_wrap(~ pitch_num)

  t %>%
  ggplot(aes(x = x, y = z, color = tunnel)) +
  geom_zone() + geom_plate(pov = "catcher") +
  geom_point() +
  scale_color_pitch() +
  xlim(-1.85, 1.85) + ylim(-0.5, 4.25) + coord_fixed() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom")
  
  