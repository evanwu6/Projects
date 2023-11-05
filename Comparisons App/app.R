library(tidyverse)
library(ranger)
library(readxl)
library(shiny)
library(DT)

set.seed(3630)

# Data ####
data <- read_excel("Data 2015to23.xlsx", sheet = "Hitting") %>% 
  mutate(OPS = OBP + SLG) %>% 
  filter(Season != 2020) %>% 
  select(-BsR, -Off, -Def, -ISO, -BABIP) %>% 
  mutate(BB = round(`BB%`*PA, 0),
         SO = round(`K%`*PA, 0))

datap <- read_excel("Data 2015to23.xlsx", sheet = "Pitching") %>% 
  filter(Season != 2020) %>% 
  select(-`LOB%`, -`HR/FB`, -SV) %>% 
  rename(speed_ff = `vFA (pi)`)


# Hitting Models ####
model_woba <- ranger(wOBA ~ AVG + OBP + SLG, data = data)
model_hr <- ranger(HR ~ AVG + OBP + SLG, data = data)
model_r <- ranger(R ~ AVG + OBP + SLG, data = data)
model_rbi <- ranger(RBI ~ AVG + OBP + SLG, data = data)
model_bb_perc <- ranger(`BB%` ~ AVG + OBP + SLG, data = data)
model_k_perc <- ranger(`K%` ~ AVG + OBP + SLG, data = data)
model_wrc_plus <- ranger(`wRC+` ~ AVG + OBP + SLG, data = data)
model_war <- ranger(WAR ~ AVG + OBP + SLG, data = data)


# Pitching Models ####
model_L <- ranger(L ~ W + SO + ERA, data = datap)
model_IP <- ranger(IP ~ W + SO + ERA, data = datap)
model_H <- ranger(H ~ W + SO + ERA, data = datap)
model_BB <- ranger(BB ~ W + SO + ERA, data = datap)
model_HR <- ranger(HR ~ W + SO + ERA, data = datap)
model_BABIP <- ranger(BABIP ~ W + SO + ERA, data = datap)
model_GB <- ranger(`GB%` ~ W + SO + ERA, data = datap)
model_FIP <- ranger(FIP ~ W + SO + ERA, data = datap)
model_xFIP <- ranger(xFIP ~ W + SO + ERA, data = datap)
model_pWAR <- ranger(WAR ~ W + SO + ERA, data = datap)



# App ####
ui <- fluidPage(
  
  titlePanel("Baseball Hitting & Pitching Comparisons Engine"),
  
  tabsetPanel(
    tabPanel("Hitting Slash Line",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 numericInput(inputId = "S_AVG",
                             label = "AVG",
                             min = 0,
                             max = 1,
                             value = 0.3,
                             step = 0.001),
                 
                 numericInput(inputId = "S_OBP",
                             label = "OBP",
                             min = 0,
                             max = 1,
                             value = 0.4,
                             step = 0.001),
                 
                 numericInput(inputId = "S_SLG",
                             label = "SLG",
                             min = 0,
                             max = 4,
                             value = 0.5,
                             step = 0.001)
                 ),
               
               mainPanel(
                 tableOutput("slash"),
                 plotOutput("percentiles"),
                 br(),
                 br(),
                 br(),
                 textOutput("info")
               )
             )
    ), # Slash End
    
    tabPanel("Hitter Comparisons",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectizeInput(
                   "show_vars",
                   "Columns to show:",
                   choices = c("AVG", "OBP", "SLG", "OPS", "HR", "SB", 
                               "wOBA", "wRC+", "WAR"),
                   multiple = TRUE,
                   selected = c("OPS", "HR", "WAR")
                 ),
                 
                 numericInput(inputId = "PC_threshold",
                              label = "THRESHOLD",
                              min = 0,
                              max = 5,
                              value = 0.1,
                              step = 0.01),
                 
                 numericInput(inputId = "PC_AVG",
                              label = "AVG",
                              min = 0,
                              max = 1,
                              value = 0.27,
                              step = 0.001),
                 
                 numericInput(inputId = "PC_OBP",
                              label = "OBP",
                              min = 0,
                              max = 1,
                              value = 0.35,
                              step = 0.001),
                 
                 numericInput(inputId = "PC_SLG",
                              label = "SLG",
                              min = 0,
                              max = 1,
                              value = 0.45,
                              step = 0.001),
                 
                 numericInput(inputId = "PC_OPS",
                              label = "OPS",
                              min = 0,
                              max = 1,
                              value = 0.8,
                              step = 0.001),
                 
                 numericInput(inputId = "PC_HR",
                              label = "HR",
                              min = 0,
                              value = 20),
                 
                 numericInput(inputId = "PC_SB",
                              label = "SB",
                              min = 0,
                              value = 10),
                 
                 numericInput(inputId = "PC_wOBA",
                              label = "wOBA",
                              min = 0,
                              max = 1,
                              value = 0.34,
                              step = 0.001),
                 
                 numericInput(inputId = "PC_wRC_plus",
                              label = "wRC+",
                              min = 0,
                              value = 100),
                 
                 numericInput(inputId = "PC_WAR",
                              label = "fWAR",
                              min = 0,
                              max = 15,
                              value = 2.5,
                              step = 0.1)
               ),
               
               
               mainPanel(
                 tableOutput("comp"),
                 br(),
                 br(),
                 br(),
                 textOutput("info2")
               )
             )
    ), # Hitter Comps End  
    
    tabPanel("Pitching Triple Crown Slash",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 numericInput(inputId = "P3_W",
                              label = "Wins",
                              min = 0,
                              max = 35,
                              value = 15,
                              step = 1),
                 
                 numericInput(inputId = "P3_ERA",
                              label = "ERA",
                              min = 0,
                              max = 10,
                              value = 3.5,
                              step = 0.01),
                 
                 numericInput(inputId = "P3_SO",
                              label = "Strikeouts",
                              min = 0,
                              max = 400,
                              value = 200,
                              step = 1)
               ),
               
               mainPanel(
                 tableOutput("p3"),
                 plotOutput("percentilesp3"),
                 br(),
                 br(),
                 br(),
                 textOutput("infop")
               )
             )
    ), # Pitch 3C end
    
    
    tabPanel("Pitcher Comparisons",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectizeInput(
                   "show_vars2",
                   "Columns to show:",
                   choices = c("W", "L", "IP", "ER", "BB", "SO", "HR",
                               "ERA", "FIP", "H", "K/9", "BB/9", "WAR", 
                               "Fastball Speed"),
                   multiple = TRUE,
                   selected = c("ERA", "SO", "BB")
                 ),
                 
                 numericInput(inputId = "PC2_threshold",
                              label = "THRESHOLD",
                              min = 0,
                              max = 5,
                              value = 0.1,
                              step = 0.01),
                 
                 numericInput(inputId = "PC2_W",
                              label = "Wins",
                              min = 0,
                              max = 40,
                              value = 10,
                              step = 1),
                 
                 numericInput(inputId = "PC2_L",
                              label = "Losses",
                              min = 0,
                              max = 40,
                              value = 10,
                              step = 1),
                 
                 numericInput(inputId = "PC2_IP",
                              label = "Innings Pitched",
                              min = 0,
                              max = 400,
                              value = 170,
                              step = 1),
                 
                 numericInput(inputId = "PC2_ER",
                              label = "Earned Runs",
                              min = 0,
                              max = 150,
                              value = 75,
                              step = 1),
                 
                 numericInput(inputId = "PC2_BB",
                              label = "Walks",
                              min = 0,
                              max = 120,
                              value = 50,
                              step = 1),
                 
                 numericInput(inputId = "PC2_SO",
                              label = "Strikeouts",
                              min = 0,
                              max = 400,
                              value = 150,
                              step = 1),
                 
                 numericInput(inputId = "PC2_HR",
                              label = "Home Runs",
                              min = 0,
                              max = 50,
                              value = 20,
                              step = 1),
                 
                 numericInput(inputId = "PC2_ERA",
                              label = "ERA",
                              min = 1,
                              max = 10,
                              value = 4,
                              step = 0.01),
                 
                 numericInput(inputId = "PC2_FIP",
                              label = "FIP",
                              min = 1,
                              max = 10,
                              value = 4,
                              step = 0.01),
                 
                 numericInput(inputId = "PC2_H",
                              label = "Hits",
                              min = 0,
                              max = 300,
                              value = 150,
                              step = 1),
                 
                 numericInput(inputId = "PC2_K9",
                              label = "K/9",
                              min = 0,
                              max = 15,
                              value = 8,
                              step = 0.1),
                 
                 numericInput(inputId = "PC2_BB9",
                              label = "BB/9",
                              min = 0,
                              max = 7,
                              value = 2.6,
                              step = 0.1),
                 
                 numericInput(inputId = "PC2_WAR",
                              label = "fWAR",
                              min = -2,
                              max = 12,
                              value = 3,
                              step = 0.1),
                 
                 numericInput(inputId = "PC2_ffs",
                              label = "Fastball Speed",
                              min = 80,
                              max = 100,
                              value = 93,
                              step = 0.1)
                 
                 
               ),
               
               
               mainPanel(
                 tableOutput("comp_p"),
                 br(),
                 br(),
                 br(),
                 textOutput("infop2")
               )
             )
    ) # Pitcher Comps End 
    
    
  ) # tabPanel end
  
  
) # ui Fluid end


server <- function(input, output) {
  
  
  
  output$slash <- renderTable({
    
    pred <- data.frame(AVG = input$S_AVG, 
                       OBP = input$S_OBP, 
                       SLG = input$S_SLG)
    pred <- pred %>% 
      mutate(HR = predict(model_hr, pred)$predictions,
             R = predict(model_r, pred)$predictions,
             RBI = predict(model_rbi, pred)$predictions,
             `BB%` = predict(model_bb_perc, pred)$predictions,
             `K%` = predict(model_k_perc, pred)$predictions,
             wOBA = predict(model_woba, pred)$predictions,
             `wRC+` = predict(model_wrc_plus, pred)$predictions,
             WAR = predict(model_war, pred)$predictions) %>% 
      mutate(HR = round(HR, 0),
             R = round(R, 0),
             RBI = round(RBI, 0),
             `BB%` = round(`BB%`, 3),
             `K%` = round(`K%`, 3),
             wOBA = round(wOBA, 3),
             `wRC+` = round(`wRC+`, 0),
             WAR = round(WAR, 1)) 
      
    decimal_places <- c(3, 3, 3, 0, 0, 0, 3, 2, 3, 0, 1)
    
    for (i in 1:length(decimal_places)) {
      pred[, i] <- formatC(pred[, i], format = "f", digits = decimal_places[i])
    }
  
    print(pred)
    
  })
  
  
  output$percentiles <- renderPlot({
    
    pred <- data.frame(AVG = input$S_AVG, 
                       OBP = input$S_OBP, 
                       SLG = input$S_SLG)
    pred <- pred %>% 
      mutate(HR = predict(model_hr, pred)$predictions,
             R = predict(model_r, pred)$predictions,
             RBI = predict(model_rbi, pred)$predictions,
             `BB%` = predict(model_bb_perc, pred)$predictions,
             `K%` = predict(model_k_perc, pred)$predictions,
             wOBA = predict(model_woba, pred)$predictions,
             `wRC+` = predict(model_wrc_plus, pred)$predictions,
             WAR = predict(model_war, pred)$predictions)
    
    decimal_places <- c(3, 3, 3, 0, 0, 0, 3, 2, 3, 0, 1)
    
    for (i in 1:length(decimal_places)) {
      pred[, i] <- formatC(pred[, i], format = "f", digits = decimal_places[i])
    }
  
  perc <- c(ecdf(data$AVG)(pred$AVG)*100,
            ecdf(data$OBP)(pred$OBP)*100,
            ecdf(data$SLG)(pred$SLG)*100,
            ecdf(data$HR)(pred$HR)*100,
            ecdf(data$R)(pred$R)*100,
            ecdf(data$RBI)(pred$RBI)*100,
            ecdf(data$`BB%`)(pred$`BB%`)*100,
            ecdf(data$`K%`)(pred$`K%`)*100,
            ecdf(data$wOBA)(pred$wOBA)*100,
            ecdf(data$`wRC+`)(pred$`wRC+`)*100,
            ecdf(data$WAR)(pred$WAR)*100)
  
  labs <- c("AVG", "OBP", "SLG", "HR", "R", "RBI",
            "BB%", "K%", "wOBA", "wRC+", "WAR")
  
  percentile <- pred %>% 
    t() %>% 
    cbind(perc, labs) %>% 
    as.data.frame() %>% 
    rename("value" = V1) %>% 
    mutate(perc = as.numeric(perc),
           labs = factor(labs, levels = rev(labs)))
  
  percentile %>% 
    ggplot(aes(y = labs, x = perc, fill = perc)) +
    geom_col(show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 100, 25)) +
    geom_point(aes(x = perc, y = labs), size = 14.5, color = "white", show.legend = FALSE) +
    geom_point(aes(x = perc, y = labs, color = perc), size = 13.5, show.legend = FALSE) +
    geom_text(aes(x = perc - 1.25, label = round(perc, 0)), fontface = "bold", 
              hjust = -0.1, size = 4.5, family = "Courier", color = "white",
              show.legend = FALSE) +
    geom_text(aes(x = 102), label = percentile$value, hjust = -0.1, size = 4, 
              family = "Courier", color = "darkgray", fontface = "bold") +
    scale_fill_gradient2(high = "red", low = "steelblue", mid = "gray",
                         midpoint = 50, limits = c(0, 100)) +
    scale_color_gradient2(high = "red", low = "steelblue", mid = "gray",
                         midpoint = 50, limits = c(0, 100)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 15, color = "black",
                                   family = "Times New Roman"))
  
  })
  
  
  
  output$info <- renderText({
    "Data gathered from Fangraphs of qualified seasons (502 PA) from 2015 - 2023,
    excluding 2020. Counting statistics assume a full season (Average PA = 605)."
  })
  
  output$info2 <- renderText({
    "Data gathered from Fangraphs of qualified seasons (502 PA) from 2015 - 2023,
    excluding 2020."
  })
  
  output$comp <- renderTable({
    
    thresh <- input$PC_threshold
    
    vars <- input$show_vars
    
    sev <- 10
    
    vals <- data.frame(
      AVG = input$PC_AVG,
      OBP = input$PC_OBP,
      SLG = input$PC_SLG,
      OPS = input$PC_OPS,
      HR = input$PC_HR,
      SB = input$PC_SB,
      wOBA = input$PC_wOBA,
      wRC = input$PC_wRC_plus,
      WAR = input$PC_WAR)
    
    
    margin <- data.frame(
      AVG = mean(data$AVG)*thresh,
      OBP = mean(data$OBP)*thresh,
      SLG = mean(data$SLG)*thresh,
      OPS = mean(data$OPS)*thresh,
      HR = mean(data$HR)*thresh,
      SB = mean(data$SB)*thresh,
      wOBA = mean(data$wOBA)*thresh,
      wRC = mean(data$`wRC+`)*thresh,
      WAR = mean(data$WAR)*thresh)
    
    
    comps <- data %>%
      mutate(OPS = OBP + SLG) %>%
      filter(
        if ("AVG" %in% vars) AVG > (vals$AVG - margin$AVG) & AVG < (vals$AVG + margin$AVG) else TRUE,
        if ("OBP" %in% vars) OBP > (vals$OBP - margin$OBP) & OBP < (vals$OBP + margin$OBP) else TRUE,
        if ("SLG" %in% vars) SLG > (vals$SLG - margin$SLG) & SLG < (vals$SLG + margin$SLG) else TRUE,
        if ("OPS" %in% vars) OPS > (vals$OPS - margin$OPS) & OPS < (vals$OPS + margin$OPS) else TRUE,
        if ("HR" %in% vars) HR > (vals$HR - margin$HR) & HR < (vals$HR + margin$HR) else TRUE,
        if ("SB" %in% vars) SB > (vals$SB - margin$SB) & SB < (vals$SB + margin$SB) else TRUE,
        if ("wOBA" %in% vars) wOBA > (vals$wOBA - margin$wOBA) & wOBA < (vals$wOBA + margin$wOBA) else TRUE,
        if ("wRC" %in% vars) `wRC+` > (vals$wRC - margin$wRC) & `wRC+` < (vals$wRC + margin$wRC) else TRUE,
        if ("WAR" %in% vars) WAR > (vals$WAR - margin$WAR) & WAR < (vals$WAR + margin$WAR) else TRUE
      ) %>%
      select(Name, Season, G, PA, HR, R, RBI, SB, BB, SO, 
             AVG, OBP, SLG, OPS, WAR, wOBA, `wRC+`) %>% 
      as.data.frame()
    
    comps <- comps %>% 
      mutate(
        cAVG = ((AVG - vals$AVG) / mean(data$AVG) * sev)^2,
        cOBP = ((OBP - vals$OBP) / mean(data$OBP) * sev)^2,
        cSLG = ((SLG - vals$SLG) / mean(data$SLG) * sev)^2,
        cOPS = ((OPS - vals$OPS) / mean(data$OPS) * sev)^2,
        cHR = ((HR - vals$HR) / mean(data$HR) * sev)^2,
        cSB = ((SB - vals$SB) / mean(data$SB) * sev)^2,
        cwOBA = ((wOBA - vals$wOBA) / mean(data$wOBA) * sev)^2,
        cwRC = ((`wRC+` - vals$wRC) / mean(data$`wRC+`) * sev)^2,
        cWAR = ((WAR - vals$WAR) / mean(data$WAR) * sev)^2)
    
    comps <- comps %>% 
      mutate(sim = 1,
             sim = if ("AVG" %in% vars) sim * (cAVG + 1) else sim,
             sim = if ("OBP" %in% vars) sim * (cOBP + 1) else sim,
             sim = if ("SLG" %in% vars) sim * (cSLG + 1) else sim,
             sim = if ("OPS" %in% vars) sim * (cOPS + 1) else sim,
             sim = if ("HR" %in% vars) sim * (cHR + 1) else sim,
             sim = if ("SB" %in% vars) sim * (cSB + 1) else sim,
             sim = if ("wOBA" %in% vars) sim * (cwOBA + 1) else sim,
             sim = if ("wRC" %in% vars) sim * (cwRC + 1) else sim,
             sim = if ("WAR" %in% vars) sim * (cWAR + 1) else sim)
    
    comps <- comps %>% 
      arrange(sim) %>%
      mutate(Similarity = 1/sim) %>% 
      select(Name, Season, G, PA, HR, R, RBI, SB, BB, SO, 
             AVG, OBP, SLG, OPS, WAR, wOBA, `wRC+`, Similarity)
    
    

    decimal_places <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 1, 3, 0, 3)

    for (i in 1:length(decimal_places)) {
      comps[, i] <- formatC(comps[, i], format = "f", digits = decimal_places[i])
    }

    head(comps, 20)
    

    
    
  })
  
  
  output$p3 <- renderTable({
    
    predp <- data.frame(W = input$P3_W, 
                       ERA = input$P3_ERA, 
                       SO = input$P3_SO)
    predp <- predp %>% 
      mutate(L = predict(model_L, predp)$predictions,
           IP = predict(model_IP, predp)$predictions,
           H = predict(model_H, predp)$predictions,
           BB = predict(model_BB, predp)$predictions,
           HR = predict(model_HR, predp)$predictions,
           BABIP = predict(model_BABIP, predp)$predictions,
           "GB Rate" = predict(model_GB, predp)$predictions,
           FIP = predict(model_FIP, predp)$predictions,
           xFIP = predict(model_xFIP, predp)$predictions,
           fWAR = predict(model_pWAR, predp)$predictions)
    
    decimal_places <- c(0, 2, 0, 0, 1, 0, 0, 0, 3, 3, 2, 2, 1)
    
    for (i in 1:length(decimal_places)) {
      predp[, i] <- formatC(predp[, i], format = "f", digits = decimal_places[i])
    }
    
    print(predp)
    
  })
  
  
  
  output$percentilesp3 <- renderPlot({
    
    
    predp <- data.frame(W = input$P3_W, 
                        ERA = input$P3_ERA, 
                        SO = input$P3_SO)
    predp <- predp %>% 
      mutate(L = predict(model_L, predp)$predictions,
             IP = predict(model_IP, predp)$predictions,
             H = predict(model_H, predp)$predictions,
             BB = predict(model_BB, predp)$predictions,
             HR = predict(model_HR, predp)$predictions,
             BABIP = predict(model_BABIP, predp)$predictions,
             "GB Rate" = predict(model_GB, predp)$predictions,
             FIP = predict(model_FIP, predp)$predictions,
             xFIP = predict(model_xFIP, predp)$predictions,
             fWAR = predict(model_pWAR, predp)$predictions)
    
    decimal_places <- c(0, 2, 0, 0, 1, 0, 0, 0, 3, 3, 2, 2, 1)
    
    for (i in 1:length(decimal_places)) {
      predp[, i] <- formatC(predp[, i], format = "f", digits = decimal_places[i])
    }
    
    
    percp <- c(ecdf(datap$W)(predp$W)*100,
               100-ecdf(datap$ERA)(predp$ERA)*100,
               ecdf(datap$SO)(predp$SO)*100,
               100-ecdf(datap$L)(predp$L) * 100,
               ecdf(datap$IP)(predp$IP) * 100,
               100-ecdf(datap$H)(predp$H) * 100,
               100-ecdf(datap$BB)(predp$BB) * 100,
               100-ecdf(datap$HR)(predp$HR) * 100,
               100-ecdf(datap$BABIP)(predp$BABIP) * 100,
               ecdf(datap$`GB%`)(predp$`GB Rate`) * 100,
               100-ecdf(datap$FIP)(predp$FIP) * 100,
               100-ecdf(datap$xFIP)(predp$xFIP) * 100,
               ecdf(datap$WAR)(predp$fWAR) * 100)
    
    labsp <- c("Wins", "ERA", "Strikeouts", "Losses", "Innings Pitched",
              "Hits", "Walks", "Home Runs", "BABIP", "GB Rate",
              "FIP", "xFIP", "fWAR")
    
    percentilep <- predp %>% 
      t() %>% 
      cbind(percp, labsp) %>% 
      as.data.frame() %>% 
      rename("value" = V1) %>% 
      mutate(percp = as.numeric(percp),
             labsp = factor(labsp, levels = rev(labsp)))
    
    percentilep %>% 
      ggplot(aes(y = labsp, x = percp, fill = percp)) +
      geom_col(show.legend = FALSE) +
      scale_x_continuous(breaks = seq(0, 100, 25)) +
      geom_point(aes(x = percp, y = labsp), size = 14.5, color = "white", show.legend = FALSE) +
      geom_point(aes(x = percp, y = labsp, color = percp), size = 13.5, show.legend = FALSE) +
      geom_text(aes(x = percp - 1.25, label = round(percp, 0)), fontface = "bold", 
                hjust = -0.1, size = 4.5, family = "Courier", color = "white",
                show.legend = FALSE) +
      geom_text(aes(x = 103), label = percentilep$value, hjust = -0.1, size = 4, 
                family = "Courier", color = "darkgray", fontface = "bold") +
      scale_fill_gradient2(high = "red", low = "steelblue", mid = "gray",
                           midpoint = 50, limits = c(0, 100)) +
      scale_color_gradient2(high = "red", low = "steelblue", mid = "gray",
                            midpoint = 50, limits = c(0, 100)) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15, color = "black",
                                       family = "Times New Roman"))
    
  })
  
  
  output$infop <- renderText({
    "Data gathered from Fangraphs of seasons with 140 or more Innings Pitched from 2015 - 2023,
    excluding 2020. Counting statistics assume a full season (Average IP = 175)."
  })
  
  output$infop2 <- renderText({
    "Data gathered from Fangraphs of seasons with 140 or more Innings Pitched from 2015 - 2023,
    excluding 2020."
  })
  
  
  output$comp_p <- renderTable({
    
    thresh <- input$PC2_threshold
    
    vars <- input$show_vars2
    
    sev <- 10
    
    vals <- data.frame(
      W = input$PC2_W,
      L = input$PC2_L,
      IP = input$PC2_IP,
      ER = input$PC2_ER,
      BB = input$PC2_BB,
      SO = input$PC2_SO,
      HR = input$PC2_HR,
      ERA = input$PC2_ERA,
      FIP = input$PC2_FIP,
      H = input$PC2_H,
      K9 = input$PC2_K9,
      BB9 = input$PC2_BB9,
      WAR = input$PC2_WAR,
      FF = input$PC2_ffs)
    
    
    margin <- data.frame(
      W = mean(datap$W)*thresh,
      L = mean(datap$L)*thresh,
      IP = mean(datap$IP)*thresh,
      ER = mean(datap$ER)*thresh,
      BB = mean(datap$BB)*thresh,
      SO = mean(datap$SO)*thresh,
      HR = mean(datap$HR)*thresh,
      ERA = mean(datap$ERA)*thresh,
      FIP = mean(datap$FIP)*thresh,
      H = mean(datap$H)*thresh,
      K9 = mean(datap$`K/9`)*thresh,
      BB9 = mean(datap$`BB/9`)*thresh,
      WAR = mean(datap$WAR)*thresh,
      FF = mean(datap$speed_ff, na.rm = TRUE)*thresh)
    
    
    comps <- datap %>%
      filter(
        if ("W"  %in% vars) W > (vals$W - margin$W) & W < (vals$W + margin$W) else TRUE,
        if ("L" %in% vars) L > (vals$L - margin$L) & L < (vals$L + margin$L) else TRUE,
        if ("IP" %in% vars) IP > (vals$IP - margin$IP) & IP < (vals$IP + margin$IP) else TRUE,
        if ("ER" %in% vars) ER > (vals$ER - margin$ER) & ER < (vals$ER + margin$ER) else TRUE,
        if ("BB" %in% vars) BB > (vals$BB - margin$BB) & BB < (vals$BB + margin$BB) else TRUE,
        if ("SO" %in% vars) SO > (vals$SO - margin$SO) & SO < (vals$SO + margin$SO) else TRUE,
        if ("HR" %in% vars) HR > (vals$HR - margin$HR) & HR < (vals$HR + margin$HR) else TRUE,
        if ("ERA" %in% vars) ERA > (vals$ERA - margin$ERA) & ERA < (vals$ERA + margin$ERA) else TRUE,
        if ("FIP" %in% vars) FIP > (vals$FIP - margin$FIP) & FIP < (vals$FIP + margin$FIP) else TRUE,
        if ("H" %in% vars) H > (vals$H - margin$H) & H < (vals$H + margin$H) else TRUE,
        if ("K/9" %in% vars) `K/9` > (vals$K9 - margin$K9) & `K/9` < (vals$K9 + margin$K9) else TRUE,
        if ("BB/9" %in% vars) `BB/9` > (vals$BB9 - margin$BB9) & `BB/9` < (vals$BB9 + margin$BB9) else TRUE,
        if ("WAR" %in% vars) WAR > (vals$WAR - margin$WAR) & WAR < (vals$WAR + margin$WAR) else TRUE,
        if ("Fastball Speed" %in% vars) speed_ff > (vals$FF - margin$FF) & speed_ff < (vals$FF + margin$FF) else TRUE
        ) %>%
      as.data.frame() %>% 
      select(-TBF)
    
    comps <- comps %>% 
      mutate(
        cW = ((W - vals$W) / mean(datap$W, na.rm = TRUE) * sev)^2,
        cL = ((L - vals$L) / mean(datap$L, na.rm = TRUE) * sev)^2,
        cIP = ((IP - vals$IP) / mean(datap$IP, na.rm = TRUE) * sev)^2,
        cER = ((ER - vals$ER) / mean(datap$ER, na.rm = TRUE) * sev)^2,
        cBB = ((BB - vals$BB) / mean(datap$BB, na.rm = TRUE) * sev)^2,
        cSO = ((SO - vals$SO) / mean(datap$SO, na.rm = TRUE) * sev)^2,
        cHR = ((HR - vals$HR) / mean(datap$HR, na.rm = TRUE) * sev)^2,
        cERA = ((ERA - vals$ERA) / mean(datap$ERA, na.rm = TRUE) * sev)^2,
        cFIP = ((FIP - vals$FIP) / mean(datap$FIP, na.rm = TRUE) * sev)^2,
        cH = ((H - vals$H) / mean(datap$H, na.rm = TRUE) * sev)^2,
        cK9 = ((`K/9` - vals$K9) / mean(datap$`K/9`, na.rm = TRUE) * sev)^2,
        cBB9 = ((`BB/9` - vals$BB9) / mean(datap$`BB/9`, na.rm = TRUE) * sev)^2,
        cWAR = ((WAR - vals$WAR) / mean(datap$WAR, na.rm = TRUE) * sev)^2,
        cFF = ((speed_ff - vals$FF) / mean(datap$speed_ff, na.rm = TRUE) * sev)^2)
    
    comps <- comps %>% 
      mutate(sim = 1,
             sim = if ("W" %in% vars) sim * (cW + 1) else sim,
             sim = if ("L" %in% vars) sim * (cL + 1) else sim,
             sim = if ("IP" %in% vars) sim * (cIP + 1) else sim,
             sim = if ("ER" %in% vars) sim * (cER + 1) else sim,
             sim = if ("BB" %in% vars) sim * (cBB + 1) else sim,
             sim = if ("SO" %in% vars) sim * (cSO + 1) else sim,
             sim = if ("HR" %in% vars) sim * (cHR + 1) else sim,
             sim = if ("ERA" %in% vars) sim * (cERA + 1) else sim,
             sim = if ("FIP" %in% vars) sim * (cFIP + 1) else sim,
             sim = if ("H" %in% vars) sim * (cH + 1) else sim,
             sim = if ("K/9" %in% vars) sim * (cK9 + 1) else sim,
             sim = if ("BB/9" %in% vars) sim * (cBB9 + 1) else sim,
             sim = if ("WAR" %in% vars) sim * (cWAR + 1) else sim,
             sim = if ("Fastball Speed" %in% vars) sim * (cFF + 1) else sim)
    
    comps <- comps %>% 
      arrange(sim) %>%
      mutate(Similarity = 1/sim) %>% 
      select(-cW:-sim) %>% 
      select(Name, Season, W:Similarity) %>% 
      rename("Fastball Speed" = speed_ff,
             "GB Rate" = `GB%`)
    
    
    
    decimal_places <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3, 3, 
                        1, 2, 2, 2, 1, 3)
    
    for (i in 1:length(decimal_places)) {
      comps[, i] <- formatC(comps[, i], format = "f", digits = decimal_places[i])
    }
    
    head(comps, 20)
    
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)