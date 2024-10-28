# Libraries ####
library(tidyverse)
library(patchwork)
library(knitr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(kableExtra)
library(magick)
library(gridExtra)
library(grid)
library(ggforce)

# Specify Game Date ####
date <- "09_05"
camp_name <- "Elon Baseball Fall Camp"
filename <- "TemplateExample_09_05"


# Creating Functions ####

# Colors
palettes <- list(
  "Normal" = c("#000000", "#E69F00", "#56B4E9", "#0072B2", 
               "#F0E442", "#009E73", "#D55E00", "#CC79A7"),
  "Set1" = brewer.pal(9, "Set1"),
  "Set2" = brewer.pal(8, "Set2"),
  "Set3" = brewer.pal(12, "Set3"))

# ggplot Functions
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


scale_color_trackman <- function(colors = "normal"){
  scale_color_manual(values = c("Fastball" = "black", "Cutter" = "purple", 
                                "Sinker" = "red", "Slider" = "blue",
                                "ChangeUp" = "chartreuse3", "Curveball" = "darkorange"))
}

scale_fill_trackman <- function(colors = "normal"){
  scale_fill_manual(values = c("Fastball" = "black", "Cutter" = "purple", 
                               "Sinker" = "red", "Slider" = "blue",
                               "ChangeUp" = "chartreuse3", "Curveball" = "darkorange"))
}


# Tilt Function
S2HM <- function(seconds) {
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  minutes_str <- sprintf("%02d", minutes)  # Add leading zero if needed
  return(paste0(hours, ":", minutes_str))
}

# Name Fixer
swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}


convert_date_format <- function(date_string) {
  date_string <- gsub("_", "/", date_string)
  date_string <- paste(date_string, format(Sys.Date(), "%Y"), sep = "/")
  date <- mdy(date_string)
  month_name <- toupper(month(date, label = TRUE, abbr = TRUE))
  day <- day(date)
  day_suffix <- ifelse(day %in% c(11, 12, 13), "TH",
                       ifelse(day %% 10 == 1, "ST",
                              ifelse(day %% 10 == 2, "ND",
                                     ifelse(day %% 10 == 3, "RD", "TH"))))
  formatted_date <- paste0(month_name, " ", day, day_suffix)
  return(formatted_date)
}


# Data ####

data <- read_csv(paste0("Training Camps/", filename, ".csv"))

data %>% 
  select(Pitcher) %>% 
  unique() %>% 
  arrange(Pitcher)

# Removing unnamed
data <- data %>% 
  filter(!(is.na(Pitcher))) %>% 
  filter(!str_detect(Pitcher, "Pitcher")) %>%
  filter(!str_detect(Pitcher, "Player"))

# Calculating Pitching Metrics ####
# Pitch Count
data <- data %>% 
  group_by(Pitcher) %>% 
  mutate(PitchCount = cumsum(Stadium == "LathamPark")) %>% 
  ungroup()

# First Pitch
data <- data %>% 
  mutate(First = ifelse(PitchofPA == 1, 1, 0))

# CAA Percentiles
caa <- read_csv("CAA_Pitching_Percentiles_2024.csv")

# Preparing Loop ####

# Creating folders
if (!dir.exists(paste0("Training Camps/", date, " ", camp_name, "/"))) {
  dir.create(paste0("Training Camps/", date, " ", camp_name, "/"))
}

if (!dir.exists(paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/"))) {
  dir.create(paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/"))
}

# Table Themes
custom_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = "white"),
    fg_params = list(fontsize = 12, fontfamily = "Times New Roman"),
    hline = gpar(col = "black", lwd = 1),
    vline = gpar(col = "black", lwd = 1)),
  colhead = list(
    fg_params = list(fontsize = 13, fontface = "bold", fontfamily = "Times New Roman"),
    hline = gpar(col = "black", lwd = 1)))

# Creating Reference Database
summary_data <- data %>% 
  distinct(Pitcher) %>% 
  mutate(TitleName = sapply(Pitcher, swap_names),
         FileName = Pitcher,
         FileName = str_replace(FileName, ", ", "")) %>% 
  rename(ProperName = Pitcher) %>% 
  arrange(FileName)

# Loop ####
### FOR LOOP STARTS HERE for()#

for (i in 1:nrow(summary_data)) {
# for (i in 3) {

name_loop <- subset(summary_data[i, 1])
name_loop <- name_loop$ProperName[1]

# Creating Titles
name_title <- summary_data %>% filter(ProperName == name_loop)
name_title <- name_title$TitleName[1]
name_file <- summary_data %>% filter(ProperName == name_loop)
name_file <- name_file$FileName[1]
name_date <- convert_date_format(date)

# Creating Graphics ####

pitcher_data <- data %>% 
  filter(Pitcher == name_loop)

# Pitch Movement Plot
max_abs_x <- max(abs(pitcher_data$PlateLocSide))
max_abs_y <- max(abs(pitcher_data$PlateLocHeight))
x_lim <- c(-max_abs_x, max_abs_x)
y_lim <- c(-max_abs_y, max_abs_y)

graph_pm <- pitcher_data %>% 
  ggplot(aes(x = HorzBreak, y = InducedVertBreak)) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_mark_ellipse(aes(fill = as.factor(TaggedPitchType)),
                    expand = unit(1.5,"mm"),
                    alpha = 0.1, size = 0, color = "white",
                    show.legend = FALSE)+
  geom_point(aes(color = TaggedPitchType), size = 1.75, alpha = 0.75) +
  xlim(-max(abs(data$HorzBreak)), max(abs(data$HorzBreak))) + 
  ylim(-max(abs(data$InducedVertBreak)), max(abs(data$InducedVertBreak))) +
  scale_color_trackman() +
  scale_fill_trackman() +
  coord_fixed() +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        axis.line = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(color = "Pitch Type",
       x = "Horizontal Break",
       y = "Induced Vertical Break")

file_graph_pm <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "pm.png")
ggsave(filename = file_graph_pm, plot = graph_pm, 
       width = 9, height = 8, dpi = 300) 


# Outing Metrics Table
table_om <- pitcher_data %>%
  mutate(Tilt2 = as.numeric(Tilt)) %>% 
  group_by(TaggedPitchType) %>%
  rename(Pitch = TaggedPitchType) %>%
  summarize("Avg Velo" = round(mean(RelSpeed, na.rm = TRUE), 1),
            "Velocity" = paste(round(min(RelSpeed, na.rm = TRUE), 1), 
                           "to", round(max(RelSpeed, na.rm = TRUE), 1)),
            "Spin Rate" = round(mean(SpinRate, na.rm = TRUE), 0),
            "VAA" = round(mean(VertApprAngle, na.rm = TRUE), 1),
            "Tilt" = S2HM(mean(Tilt2, na.rm = TRUE)),
            "Avg IVB" = round(mean(InducedVertBreak, na.rm = TRUE), 1),
            "IVB" = paste(round(min(InducedVertBreak, na.rm = TRUE), 0),
                                "to", round(max(InducedVertBreak, na.rm = TRUE), 0)),
            "Avg Hor.B" = round(mean(HorzBreak, na.rm = TRUE), 1),
            "Hor.B" = paste(round(min(HorzBreak, na.rm = TRUE), 0),
                                  "to", round(max(HorzBreak, na.rm = TRUE), 0))) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_om <- ggplot() + 
  annotation_custom(table_om) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20))

file_table_om <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "om.png")
ggsave(filename = file_table_om, plot = table_om, 
       width = 8, height = 1.5, dpi = 300) 


# Observated Outing Metrics Table
pitcher_data <- pitcher_data %>% 
  mutate(is_swing = ifelse(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall",
                                            "FoulBallNotFieldable", "FoulBallFieldable"),
                           1, 0),
         is_strike = ifelse(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall",
                                             "FoulBallNotFieldable", "FoulBallFieldable",
                                             "StrikeCalled"),
                            1, 0),
         is_ball = ifelse(PitchCall  %in% c("BallCalled", "BallinDirt", "HitByPitch"),
                          1, 0))

pc <- pitcher_data %>% 
  nrow() %>% 
  as.numeric()

table_obom <- pitcher_data %>% 
  group_by(TaggedPitchType) %>% 
  rename(Pitch = TaggedPitchType) %>% 
  summarize("Pitches" = n(),
            "Pitch%" = n() / pc,
            "Strikes" = sum(is_swing),
            "Balls" = sum(is_ball),
            "Strike %" = percent(sum(is_strike)/n()),
            "Swing & Miss" = sum(PitchCall == "StrikeSwinging"),
            "Swings" = sum(is_swing),
            "1st Pitch S%" = paste(sum(is_strike == 1 &First == 1), 
                                       "/", sum(First == 1))) %>%
  mutate(`Pitch%` = paste0(round(`Pitch%`, 3)*100, "%")) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_obom <- ggplot() + 
  annotation_custom(table_obom) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20))

file_table_obom <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "obom.png")
ggsave(filename = file_table_obom, plot = table_obom, 
       width = 8, height = 1.5, dpi = 300)


# Shove Score
pitcher_perc_data <- pitcher_data %>% 
  mutate(AB = case_when(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "FieldersChoice", "Error", "Sacrifice") ~ 1,
                        KorBB == "Strikeout" ~ 1,
                        KorBB == "Walk" ~ 1,
                        PitchCall == "HitByPitch" ~ 1)) %>% 
  filter(AB == 1)

AB_count <- nrow(pitcher_perc_data) %>% as.numeric()

walkpercent <- pitcher_perc_data %>% 
  filter(KorBB == "Walk") %>% 
  nrow() %>% 
  as.numeric() / AB_count

kpercent <- pitcher_perc_data %>% 
  filter(KorBB == "Strikeout") %>% 
  nrow() %>% 
  as.numeric() / AB_count

strikepercent <- pitcher_data %>% 
  summarize(strikepercent = mean(is_strike, na.rm = TRUE)) %>% 
  as.numeric()

firstpitchstrike <- pitcher_data %>% 
  filter(First == 1) %>% 
  summarize(firstpitchstrike = mean(is_strike, na.rm = TRUE)) %>% 
  as.numeric()

fbmeant <- pitcher_data %>% 
  filter(TaggedPitchType == "Fastball") %>% 
  summarize(m = mean(RelSpeed, trim = 0.15, na.rm = TRUE)) %>% 
  as.numeric()

fbmax <- pitcher_data %>% 
  filter(TaggedPitchType == "Fastball") %>% 
  summarize(m = max(RelSpeed, na.rm = TRUE)) %>% 
  as.numeric()

fbspin <- pitcher_data %>% 
  filter(TaggedPitchType == "Fastball") %>% 
  summarize(m = max(SpinRate, na.rm = TRUE, trim = 0.15)) %>% 
  as.numeric()

shove_matrix <- data.frame(kpercent = max(caa[caa$kpercent <= kpercent, "Percentile"]) %>% as.numeric()) %>% 
  mutate(walkpercent = max(caa[caa$walkpercent <= walkpercent, "Percentile"]) %>% as.numeric(),
         strikepercent = max(caa[caa$strikepercent <= strikepercent, "Percentile"]) %>% as.numeric(),
         firstpitchstrike = max(caa[caa$firstpitchstrike <= firstpitchstrike, "Percentile"]) %>% as.numeric(),
         fbmeant = max(caa[caa$fbmeant <= fbmeant, "Percentile"]) %>% as.numeric(),
         fbmax = max(caa[caa$fbmax <= fbmax, "Percentile"]) %>% as.numeric(),
         fbspin = max(caa[caa$fbspin <= fbspin, "Percentile"]) %>% as.numeric())

shove_matrix[shove_matrix == -Inf] <- 0
  
shove_score <- (shove_matrix$kpercent*3 + shove_matrix$walkpercent*3 + 
                shove_matrix$strikepercent*1 + shove_matrix$firstpitchstrike*2 +
                shove_matrix$fbmeant*1 + shove_matrix$fbmax*0.5 + shove_matrix$fbspin*0.5) / 11

table_s <- data.frame("A" = percent(kpercent), "B" = percent(walkpercent), 
                      "C" = percent(strikepercent), "D" = percent(firstpitchstrike), 
                      "E" = round(fbmeant, 1), "F" = round(fbmax, 1), "G" = round(fbspin)) %>% 
  rename("K%" = A, "BB%" = B, "Strike%" = C, "1st Pitch Strike%" = D, 
         "Avg. FB Velo" = E, "Max FB Velo" = F, "FB Spin Rate" = G)

table_s <- table_s %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_s <- ggplot() + 
  annotation_custom(table_s) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20))

file_table_s <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "s.png")
ggsave(filename = file_table_s, plot = table_s, width = 7, height = 0.75, dpi = 300)


# Pitch Location Plot
graph_pl <- pitcher_data %>% 
  ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
  geom_plate() +
  geom_zone() +
  geom_point(aes(color = TaggedPitchType),
             size = 3.5, alpha = 0.75) +
  xlim(-2.25, 2.25) + ylim(0, 4.25) + coord_fixed() +
  scale_color_trackman() +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.border = element_blank(),
        legend.title = element_blank()) +
  labs(color = "Pitch Type")

file_graph_pl <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "pl.png")
ggsave(filename = file_graph_pl, plot = graph_pl, 
       width = 8.5, height = 8.90, dpi = 300) 


# Release Front Plot
graph_rf <- pitcher_data %>% 
  ggplot() +
  coord_fixed() +
  geom_rect(aes(xmin = -3, xmax = 3, ymin = 0, ymax = 0.675), fill = "#76552B", color = "#76552B") +
  geom_curve(aes(x = -2.9675, y = 0.485, xend = 2.9675, yend = 0.485), size = 10, curvature = -0.10, color = "#76552B") +
  geom_rect(aes(xmin = -0.75, xmax = 0.75, ymin = 0.9, ymax = 1.075), fill = "white", color = "black") +
  geom_point(aes(x = -RelSide, y = RelHeight, color = TaggedPitchType),
             size = 3, alpha = 0.75, shape = 1, show.legend = FALSE) +
  scale_color_trackman() +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 15),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(color = "Pitch Type")

file_graph_rf <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "rf.png")
ggsave(filename = file_graph_rf, plot = graph_rf, 
       width = 4.25, height = 5, dpi = 300) 


# Release Side Plot
graph_rs <- pitcher_data %>% 
  ggplot() +
  coord_fixed() +
  geom_rect(aes(xmin = 0.0175, xmax = 7, ymin = 0, ymax = 0.25), fill = "#76552B", color = "#76552B") +
  geom_rect(aes(xmin = 0.0175, xmax = 4.5, ymin = 0.25, ymax = 0.65), fill = "#76552B", color = "#76552B") +
  geom_rect(aes(xmin = 4.5, xmax = 6, ymin = 0.25, ymax = 0.45), fill = "#76552B", color = "#76552B") +
  geom_curve(aes(x = 0, y = 0.75, xend = 7, yend = 0.2), size = 5, curvature = -0.05, color = "#76552B") +
  geom_rect(aes(xmin = 0.70, xmax = 0.95, ymin = 0.845, ymax = 0.97), fill = "white", color = "black") +
  geom_point(aes(x = Extension, y = RelHeight, color = TaggedPitchType),
             size = 3, alpha = 0.75, shape = 1, show.legend = FALSE) +
  scale_color_trackman() +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 15),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(color = "Pitch Type")

file_graph_rs <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "rs.png")
ggsave(filename = file_graph_rs, plot = graph_rs, 
       width = 4.9, height = 5, dpi = 300)


# Extension
graph_e <- pitcher_data %>%
  ggplot() +
  # geom_boxplot(aes(x = Extension, y = TaggedPitchType, color = TaggedPitchType), 
  #            show.legend = FALSE, alpha = 0) +
  geom_point(aes(x = Extension, y = TaggedPitchType, color = TaggedPitchType), 
             size = 4, alpha = 0.8, show.legend = FALSE) +
  xlim(4, 7) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "darkgray"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())

file_graph_e <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "e.png")
ggsave(filename = file_graph_e, plot = graph_e, 
       width = 7, height = 3, dpi = 300)


# Pitch Table
table_p_raw <- pitcher_data %>% 
  select(PitchCount, TaggedPitchType, RelHeight, RelSide, Extension, RelSpeed,
         InducedVertBreak, HorzBreak, Tilt, SpinRate) %>% 
  arrange(PitchCount) %>% 
  rename("#" = PitchCount,
         "Pitch Type" = TaggedPitchType,
         "Release Height" = RelHeight,
         "Release Side" = RelSide,
         "Velo" = RelSpeed,
         "IVB" = InducedVertBreak,
         "HB" = HorzBreak,
         "Spin Rate" = SpinRate) %>% 
  mutate(`Release Height` = round(`Release Height`, 2),
         `Release Side` = round(`Release Side`, 2),
         Extension = round(Extension, 2),
         Velo = round(Velo, 1),
         IVB = round(IVB, 1),
         HB = round(HB, 1),
         Tilt = as.character(Tilt),
         Tilt = substr(Tilt, 1, 5),
         `Spin Rate` = round(`Spin Rate`, 0))

table_p1 <- table_p_raw %>% 
  filter(`#` <= 25) %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_p1 <- ggplot() + 
  annotation_custom(table_p1) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20))

file_table_p1 <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "p1.png")
ggsave(filename = file_table_p1, plot = table_p1, width = 7.5, height = 7, dpi = 300)


table_p2 <- table_p_raw %>% 
  filter(`#` > 25) %>% 
  filter(`#` <= 50)

tbp2 <- table_p2

if(nrow(table_p2) > 0) {

table_p2 <- table_p2 %>% 
  tableGrob(rows = NULL, theme = custom_theme)

table_p2 <- ggplot() + 
  annotation_custom(table_p2) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        title = element_text(family = "Times New Roman", size = 20))

file_table_p2 <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "p2.png")
ggsave(filename = file_table_p2, plot = table_p2, width = 7.5, height = 7, dpi = 300)
}


table_p3 <- table_p_raw %>% 
  filter(`#` > 50) %>% 
  filter(`#` <= 75)

tbp3 <- table_p3

if(nrow(table_p3) > 0) {
  
  table_p3 <- table_p3 %>% 
    tableGrob(rows = NULL, theme = custom_theme)
  
  table_p3 <- ggplot() + 
    annotation_custom(table_p3) +
    theme_classic() +
    theme(text = element_text(family = "Times New Roman", size = 20),
          title = element_text(family = "Times New Roman", size = 20))
  
  file_table_p3 <- paste0("Training Camps/", date, " ", camp_name, "/", "/Graphics/", name_file, ".", "p3.png")
  ggsave(filename = file_table_p3, plot = table_p3, width = 7.5, height = 7, dpi = 300)
}


# Writing PDFs ####

# Reading In All Templates
Rep1 <- image_read_pdf("Report Template Pages/Training Camps 1.pdf")
Rep2 <- image_read_pdf("Report Template Pages/Training Camps 2.pdf")
Rep3 <- image_read_pdf("Report Template Pages/Training Camps 3.pdf")
Rep3B <- image_read("Report Template Pages/Training Camps 3 Pitch Blank.png")
if(nrow(tbp3) > 0) { 
Rep3C <- image_read_pdf("Report Template Pages/Training Camps 3.pdf")
}
Rep4 <- image_read_pdf("Report Template Pages/Training Camps 4.pdf")


### REPORT PAGE 1 ###

Rep1 <- image_annotate(Rep1, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep1 <- image_annotate(Rep1, toupper(camp_name), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")
Rep1 <- image_composite(Rep1, image_read(file_graph_pm), offset = "+200+750")
Rep1 <- image_composite(Rep1, image_resize(image_read(file_table_om), "2800x800"), offset = "+3075+650")
Rep1 <- image_composite(Rep1, image_resize(image_read(file_table_obom), "2800x800"), offset = "+3075+1300")
Rep1 <- image_composite(Rep1, image_resize(image_read(file_table_s), "2520x270"), offset = "+3235+2925")
Rep1 <- image_annotate(Rep1, paste("Shove Score:", round(shove_score, 0)), 
                       font = "Times New Roman", size = 55, weight = 1000, color = "#B59A57", location = "+3750+2600")

Rep2 <- image_annotate(Rep2, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep2 <- image_annotate(Rep2, toupper(camp_name), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")
Rep2 <- image_composite(Rep2, image_read(file_graph_pl), offset = "+125+550")
Rep2 <- image_composite(Rep2, image_read(file_graph_rf), offset = "+2950+550")
Rep2 <- image_composite(Rep2, image_read(file_graph_rs), offset = "+4425+550")
Rep2 <- image_composite(Rep2, image_read(file_graph_e), offset = "+3300+2325")

Rep3 <- image_annotate(Rep3, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep3 <- image_annotate(Rep3, toupper(camp_name), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")
Rep3 <- image_composite(Rep3, image_resize(image_read(file_table_p1), "2812x2625"), offset = "+100+600")
Rep3 <- image_composite(Rep3, image_resize(image_read(file_table_p2), "2812x2625"), offset = "+3100+600")

if(nrow(tbp2) == 0) { 
  Rep3 <- image_composite(Rep3, image_resize(Rep3B, "3000x3000"), offset = "+3000+375")
}

if(nrow(tbp3) > 0) { 
  Rep3C <- image_annotate(Rep3C, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
  Rep3C <- image_annotate(Rep3C, toupper(camp_name), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")
  Rep3C <- image_composite(Rep3C, image_resize(image_read(file_table_p3), "2812x2625"), offset = "+100+600")
  Rep3C <- image_composite(Rep3C, image_resize(Rep3B, "3000x3000"), offset = "+3000+375")
}

Rep4 <- image_annotate(Rep4, name_title, font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+150+100", gravity = "NorthEast")
Rep4 <- image_annotate(Rep4, toupper(camp_name), font = "Times New Roman", size = 40, weight = 700, color = "#B59A57", location = "+0+100", gravity = "North")

if(nrow(tbp3) == 0) { 
  Full_Rep <- image_join(Rep1, Rep2, Rep3, Rep4)
}

if(nrow(tbp3) > 0) { 
Full_Rep <- image_join(Rep1, Rep2, Rep3, Rep3C, Rep4)
}

image_write(Full_Rep, path=paste0("Training Camps/", date, " ", camp_name, "/", name_file, "_", date, ".pdf"), format="pdf", quality = 100, density = 300)

print(paste0(name_title, " Completed ", "(",i, " of ", nrow(summary_data), ")"))

if(i == nrow(summary_data)){
print("Loop Complete")
}

}

