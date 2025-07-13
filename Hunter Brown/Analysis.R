library(tidyverse)
library(lubridate)
library(Metrics)
library(RColorBrewer)


options(scipen = 999)


# Data

data <- read_csv("Brown, Hunter.csv")


# GG Objects
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


scale_fill_pitch <- function(colors = "normal"){
  scale_fill_manual(values = c("Fastball" = "gray35", "Cutter" = "gray42", 
                                "Sinker" = "green", "Sweeper" = "gray60",
                                "Knuckle Curve" = "gray75", "Slider" = "gray85",
                                "Changeup" = "gray90"))
}


# Data Wrangling
data <- data %>% 
  mutate(game_month = month(game_date))

# Sinker Marker
data <- data %>% 
  mutate(sink = ifelse(game_date >= as.Date("2024-05-05"), 1, 0))

# Pitch Usage
usage <- data %>% 
  group_by(game_year, game_month, pitch_name) %>% 
  summarize(N = n(),
            Velo = mean(release_speed, na.rm = TRUE),
            IVB = mean(pfx_z, na.rm = TRUE)*12,
            HB = mean(pfx_x, na.rm = TRUE)*12,
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
            EV = mean(launch_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(game_year, game_month) %>% 
  mutate(Usage = N / sum(N)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(paste(game_year, game_month, "01", sep = "-")),
         date = as.factor(date)) %>% 
  left_join(month_order, by = c("game_year", "game_month"))

month_order <- usage %>%
  distinct(game_year, game_month) %>%
  arrange(game_year, game_month) %>%
  mutate(month_id = row_number())


usage %>% 
  mutate(date = as.Date(paste(game_year, game_month, "01", sep = "-"))) %>% 
  ggplot(aes(x = date, y = Usage, color = pitch_name)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 0.5, linetype = 44,
              show.legend = FALSE) +
  labs(color = "", x = "") +
  theme_classic() +
  theme(legend.position = "bottom")


usage %>% 
  mutate(pitch_name = factor(pitch_name, levels = c(
    "Sinker", "4-Seam Fastball", "Cutter", "Changeup", "Knuckle Curve", "Slider", "Sweeper"))) %>% 
  ggplot() +
  geom_col(aes(x = date, y = Usage, fill = pitch_name),
           show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_pitch() +
  labs(fill = "") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



data %>% 
  group_by(game_date, pitch_name) %>% 
  summarize(N = n(),
            Velo = mean(release_speed, na.rm = TRUE),
            IVB = mean(pfx_z, na.rm = TRUE)*12,
            HB = mean(pfx_x, na.rm = TRUE)*12,
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
            EV = mean(launch_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(game_date) %>% 
  mutate(Usage = N / sum(N)) %>% 
  ungroup() %>% 
  View


# Sinker Locations
data %>% 
  mutate(stand = ifelse(stand == "R", "Right", "Left")) %>% 
  filter(pitch_name == "Sinker") %>% 
  ggplot(aes(x = -plate_x, y = plate_z)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "pitcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  facet_wrap(~ stand) +
  labs(title = "Sinker Location") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text.y = element_blank())


# Fastball Locations
data %>% 
  filter(stand == "R",
         pitch_name %in% c("Sinker", "4-Seam Fastball", "Cutter")) %>% 
  mutate(sink = ifelse(sink == 1, "Sinker", "No Sinker")) %>% 
  ggplot(aes(x = -plate_x, y = plate_z)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "pitcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  facet_grid(rows = vars(sink),
             cols = vars(pitch_name)) +
  labs(title = "Pitch Location",
       subtitle = "Fastballs to Right-Handed Hitters") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Batter Performance
arsenal <- data %>% 
  group_by(sink, pitch_name) %>% 
  summarize(N = n(),
            Velo = mean(release_speed, na.rm = TRUE),
            IVB = mean(pfx_z, na.rm = TRUE)*12,
            HB = mean(pfx_x, na.rm = TRUE)*12,
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
            EV = mean(launch_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(sink) %>% 
  mutate(Usage = N / sum(N)) %>% 
  ungroup() %>% 
  filter(N > 1)


arsenal %>%
  filter(pitch_name != "Sinker", pitch_name != "Sweeper") %>% 
  ggplot(aes(y = pitch_name, x = xwOBA, fill = factor(sink))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("gray80", "green"), labels = c("No Sinker", "Sinker")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.001)) +
  labs(title = "Quality of Contact by Pitch", 
       x = "xwOBA", y = "", fill = "") +
  theme_classic() +
  theme(text = element_text(family = "Times"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


# Take Rates

zone_data <- data %>% 
  filter(pitch_name %in% c("Sinker", "4-Seam Fastball", "Cutter")) %>%
  filter(stand == "R") %>% 
  mutate(sink = ifelse(sink == 1, "Sinker", "No Sinker")) %>% 
  mutate(swing = ifelse(description %in% c("called_strike", "ball", "blocked_ball", "hit_by_pitch"), 0, 1)) %>% 
  mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 1, 0)) %>% 
  group_by(sink, zone) %>% 
  summarize(rate = mean(swing, na.rm = TRUE)) %>%
  # summarize(rate = sum(whiff, na.rm = TRUE)/sum(swing, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rate_display = scales::percent(rate, accuracy = 1))

# Zone Graph Placement Data
zone_data_p <- data.frame(
  zone = c(9, 8, 7, 6, 5, 4, 3, 2, 1),
  x = c(-0.7083, 0, 0.7083, -0.7083, 0, 0.7083, -0.7083, 0, 0.7083),
  y = c(-0.7083, -0.7083, -0.7083, 0, 0, 0, 0.7083, 0.7083, 0.7083)
)

extra_zones_p <- data.frame(
  zone = c(12, 11, 14, 13),
  x = c(-0.7083, 0.7083, -0.7083, 0.7083),
  y = c(0.7083, 0.7083, -0.7083, -0.7083)
)

full_zone_data_p <- rbind(zone_data_p, extra_zones_p)

# Making Zone Locations
z1 <- zone_data %>% 
  filter(zone %in% 1:9) %>% 
  left_join(zone_data_p, by = "zone")

z1_g <- zone_data %>% 
  filter(zone %in% 11:14) %>% 
  left_join(extra_zones_p, by = "zone")

z1_g_extra <- z1_g %>% 
  filter(zone %in% 11:14) %>% 
  mutate(x = ifelse(x > 0, x + 0.5, x - 0.5),
         y = ifelse(y > 0, y + 0.5, y - 0.5))

# Graph
ggplot() +
  geom_rect(data = z1_g %>% filter(zone  %in% 11:14),
            aes(xmin = x - 0.75, xmax = x + 0.75, ymin = y - 0.75, ymax = y + 0.75,
                fill = rate), color = "black", size = 0.5, show.legend = FALSE) +
  geom_text(data = z1_g_extra,
            aes(x = x, y = y, label = rate_display),
            color = "black", size = 2.5, family = "Times",
            show.legend = FALSE) +
  geom_tile(data = z1 %>% filter(zone %in% 1:9),
            aes(x = x, y = y, fill = rate),
            color = "black", size = 0.25, show.legend = FALSE) +
  geom_text(data = z1 %>% filter(zone %in% 1:9),
            aes(x = x, y = y, label = rate_display),
            color = "black", size = 3, family = "Times",
            show.legend = FALSE) +
  theme_void() +  # Remove axis and background
  geom_polygon(data = data.frame(x = c(-0.708, 0.7083, 0.7083, 0, -0.7083),
                                 y = c(-2.5, -2.5, -2.25, -2, -2.25)), 
               aes(x = x, y = y), fill = "white", color = "black", size = 0.5) +  # Home plate
  scale_fill_gradient2(low = "skyblue", mid = "gray90", high = "red",
                       midpoint = 0.50) +
  coord_fixed() +
  labs(title = "Swing Rate on Fastballs",
       subtitle = "Right-Handed Hitters") +
  facet_wrap(~ sink) +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.text = element_text(size = 12))


# xwOBA Zones

zone_data2 <- data %>%
  filter(stand == "R") %>% 
  mutate(sink = ifelse(sink == 1, "Sinker", "No Sinker")) %>% 
  mutate(swing = ifelse(description %in% c("called_strike", "ball", "blocked_ball", "hit_by_pitch"), 0, 1)) %>% 
  mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 1, 0)) %>% 
  group_by(sink, zone) %>% 
  summarize(xwOBA = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 3)) %>%
  ungroup()

# Making Zone Locations
zo1 <- zone_data2 %>% 
  filter(zone %in% 1:9) %>% 
  left_join(zone_data_p, by = "zone")

zo1_g <- zone_data2 %>% 
  filter(zone %in% 11:14) %>% 
  left_join(extra_zones_p, by = "zone")

zo1_g_extra <- zo1_g %>% 
  filter(zone %in% 11:14) %>% 
  mutate(x = ifelse(x > 0, x + 0.5, x - 0.5),
         y = ifelse(y > 0, y + 0.5, y - 0.5))

# Graph
ggplot() +
  geom_rect(data = zo1_g %>% filter(zone  %in% 11:14),
            aes(xmin = x - 0.75, xmax = x + 0.75, ymin = y - 0.75, ymax = y + 0.75,
                fill = xwOBA), color = "black", size = 0.5, show.legend = FALSE) +
  geom_text(data = zo1_g_extra,
            aes(x = x, y = y, label = xwOBA),
            color = "black", size = 2.5, family = "Times",
            show.legend = FALSE) +
  geom_tile(data = zo1 %>% filter(zone %in% 1:9),
            aes(x = x, y = y, fill = xwOBA),
            color = "black", size = 0.25, show.legend = FALSE) +
  geom_text(data = zo1 %>% filter(zone %in% 1:9),
            aes(x = x, y = y, label = xwOBA),
            color = "black", size = 3, family = "Times",
            show.legend = FALSE) +
  theme_void() +  # Remove axis and background
  geom_polygon(data = data.frame(x = c(-0.708, 0.7083, 0.7083, 0, -0.7083),
                                 y = c(-2.5, -2.5, -2.25, -2, -2.25)), 
               aes(x = x, y = y), fill = "white", color = "black", size = 0.5) +  # Home plate
  scale_fill_gradient2(low = "skyblue", mid = "gray90", high = "red",
                       midpoint = 0.314) +
  coord_fixed() +
  labs(title = "xwOBA on Fastballs",
       subtitle = "Right-Handed Hitters") +
  facet_wrap(~ sink) +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.text = element_text(size = 12))


# Hard-Hit Locations
data %>% 
  filter(launch_speed >= 95) %>% 
  filter(stand == "R") %>% 
  mutate(sink = ifelse(sink == 1, "Sinker", "No Sinker")) %>% 
  ggplot(aes(x = -plate_x, y = plate_z)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "pitcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  # facet_grid(rows = vars(sink), cols = vars(pitch_name)) +
  facet_wrap(~ sink) +
  labs(title = "Hard Hit Balls",
       subtitle = "Full Arsenal to Right-Handed Hitters") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Hard-Hit Fastball Locations
data %>% 
  filter(launch_speed >= 95) %>% 
  filter(stand == "R",
         pitch_name %in% c("Sinker", "4-Seam Fastball", "Cutter")) %>% 
  mutate(sink = ifelse(sink == 1, "Sinker", "No Sinker")) %>% 
  ggplot(aes(x = -plate_x, y = plate_z)) +
  geom_zone() +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "density",
                         alpha = 0.8,
                         bins = 10,
                         show.legend = FALSE) +
  scale_fill_manual(values = heat_colors) +
  geom_plate(pov = "pitcher") +
  xlim(-2, 2) +
  ylim(-0.5, 5) +
  coord_fixed() +
  # facet_grid(rows = vars(sink), cols = vars(pitch_name)) +
  facet_wrap(~ sink) +
  labs(title = "Hard Hit Balls",
       subtitle = "Fastballs to Right-Handed Hitters") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text.y = element_blank())


# Look at RV100 on swings in each zone. Maybe they swing same but hit worse
# maybe dupe fastball and all arsenal heat maps and facet. That way heat maps on same color scale

data %>% group_by(sink, pitch_name) %>% 
  summarize(N = n()) %>% 
  View()

data %>% group_by(sink) %>% 
  summarize(min = min(game_date),
            max = max(game_date),
            N = n()) %>% 
  View()
