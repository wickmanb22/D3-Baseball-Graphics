# Visualizations
# Brian Wickman
# October 2022

# Load packages
  library(dplyr)
  library(tidyverse)  
  library(ggplot2)
  library(ggrepel)
  library(ggthemes)

# Individual Batter  ------------------------------------------------------
  # Load data
    rm(list = ls())
    bat <- read.csv("~/GitProjects/D3-Baseball-Graphics/Data/Batting_Individual.csv")

  # On-base percentage vs. slugging graph
    ggplot(subset(bat, PA >= 50), aes(OBP, SLG)) + geom_jitter(alpha = .4) + 
      xlab("On Base Percentage") + ylab("Slugging Percentage") +
      geom_abline(slope = -1, intercept = seq(0.6, 1.4, by = 0.2)) +
      annotate("text", x = c(.2, .25, .3) , y = c(.5, .67, .83), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
      annotate("text", x = .35 , y = .95, label = "OPS = 1.2") +
      annotate("text", x = .4 , y = 1.1, label = "OPS = 1.4") +
      labs(title = "On-Base vs. Slugging Percentage",
           caption = "Includes hitters with >50 plate appearances") +
      theme_clean() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text (hjust = 0))
    
  # Runs created vs. plate appearances  
    # Calculate percentile values
      Q25 = aggregate(bat$RC, 
                      list(bat$PA), quantile, 0.25)
      Q75 = aggregate(bat$RC, 
                      list(bat$PA), quantile, 0.75)
      colnames(Q25) <- c("PA","RC")
      colnames(Q75) <- c("PA","RC")
    
    # Make plot
      ggplot(subset(bat, RC<100), aes(x=PA, y=RC)) + geom_jitter(alpha = 0.4) + 
        labs(x = "Plate Appearances", y = "Runs Created",
             title = "Runs Scored vs. Plate Appearances",
             caption = "Ryan McCarty (138.1 RC) omitted from graph") +
        geom_smooth(data = Q25, level = 0) +
        geom_smooth(data = Q75, level = 0) +
        geom_point(data = subset(bat, School == "Wash. & Lee"), aes(x=PA, y=RC), color = "red")
        theme_clean() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text (hjust = 0))

# Team Hitting ------------------------------------------------------------
  # Load data
      rm(list =ls())
      tbat <- read.csv("~/GitProjects/D3-Baseball-Graphics/Data/Batting_Team.csv")

  # Team on-base percentage vs slugging        
    ggplot(tbat, aes(OBP, SLG)) + geom_jitter(alpha = 0.55) +
      labs(x = "On Base Percentage", y = "Slugging Percentage",
           title = "Team On-Base vs Slugging Percentage") +
        geom_abline(slope = -1, intercept = seq(0.6, 1.0, by = 0.2)) +
        annotate("text", x = c(0.3,0.345,0.38) , y = c(.34, .49, .59), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
      theme_clean() +
      theme(plot.title = element_text(hjust = 0.5))
    
  # Team productivity
    # Add team records to the data
      # Load in team records
        team_wl <- read.csv("~/GitProjects/D3-Baseball-Graphics/Data/Team_Records.csv")
        team_wl1 <- team_wl %>% transmute(School, GP = W + L + T)
        tbat2 <- merge(tbat, team_wl1, by = "School")
  
    # Visualization
      ggplot(tbat2, aes(GP, R)) + geom_jitter(alpha = 0.55)+
        labs(x = "Games Played" , y = "Runs Scored",
             title = "Team Productivity") +
          geom_smooth(method = 'lm') +
        geom_point(data = subset(tbat2, Conference == "ODAC"), size = 3, shape = 16, color = "#FF0000") +
        theme_clean() +
        theme(plot.title = element_text(hjust = 0.5))
     

# Individual Pitching -----------------------------------------------------
    # Load data
      rm(list =ls())
      pitch <- read.csv("~/GitProjects/D3-Baseball-Graphics/Data/Pitching_Individual.csv")
      
    # ERA vs. WHIP graph
      ggplot(subset(pitch, IP >= 10), aes(ERA, WHIP)) + geom_jitter( alpha = .3) +
       xlim(0,16.5) + ylim(.2,3.10) +
        labs(x = "Earned Run Average", y = "WHIP",
             title = "ERA vs. WHIP",
             caption = "Includes pitchers with >10 innings pitched \n64 (of 6199) pitchers removed from sample") +
        geom_smooth() +
      theme_clean() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text (hjust = 0))
      
    # ERA vs. FIP graph
      ggplot(subset(pitch, IP >= 10), aes(ERA, FIP)) + geom_jitter( alpha = .3) +
        xlim(0,10) + ylim(0,10) +
        labs(x = "Earned Run Average", y = "Fielding Independent Pitching",
             title = "ERA vs. FIP",
             caption = "Includes pitchers with >10 innings pitched and an earned run average <10 \n445 (of 6199) pitchers removed from sample") +
        geom_smooth() +
        theme_clean() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text (hjust = 0))

# Team Pitching -----------------------------------------------------------
    # Load data
      rm(list =ls())
      tpitch <- read.csv("~/GitProjects/D3-Baseball-Graphics/Data/Pitching_Team.csv")
      tpitch <- tpitch %>% mutate(WHIP = round(((BB + H)/IP),2))

    # Team ERA vs. Whip  
      ggplot(tpitch, aes(ERA, WHIP)) + geom_jitter(alpha = .5) +
        xlim(3,12.5) + ylim(1,2.75) +
        labs(x = "Earned Run Average", y = "WHIP",
             title = "Team ERA vs. WHIP",
             caption = "7 (of 387) teams removed from sample due to bounds on ERA and WHIP") +
        geom_smooth() +
        theme_clean() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text (hjust = 0))
      
    # Team IP vs. R
      ggplot(tpitch, aes(IP, ER)) + geom_jitter(alpha = .5) +
        xlim(200, 470) + ylim(100,400) +
        labs(x = "Innings Pitched", y = "Runs Conceded",
             title = "Team Innings Pitched vs. Runs Conceded",
             caption = "10 (of 387) teams removed from sample due to bounds on IP and ER") +
        geom_smooth() +
        theme_clean() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text (hjust = 0))
      
    # Team IP vs. Ks
      ggplot(tpitch, aes(IP, SO)) + geom_jitter(alpha = .5) +
        #xlim(200, 470) + ylim(100,400) +
        labs(x = "Innings Pitched", y = "Strikeouts",
             title = "Team Innings Pitched vs. Strikeouts",
             caption = "10 (of 387) teams removed from sample due to bounds on IP and SO") +
        geom_smooth() +
        theme_clean() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text (hjust = 0))
      

      
    
      
        