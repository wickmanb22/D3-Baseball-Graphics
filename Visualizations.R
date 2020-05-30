## Graphics

setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/app2")

#load packages
library(baseballr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(stringr)

#import hitting graphics
Bat <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/app2/FinalBatAll1.csv")
Bat <- Bat %>% filter(Year != 2018)
  #write.csv(Bat, "FinalBatAll1920.csv", row.names = FALSE)

#Bring in BatAA
  BatAA <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/app2/BatAA.csv")
  BatAA <- BatAA %>% filter(Year != 2018)
  Bat$AA = BatAA$AA

#Hitter Performance
  #Individual Performance
    #Select relevant subset, only hitters with 25+ at bats
      D3OPS <- Bat %>%
        filter(AB >= 25) %>%
        select(Player, School, Conference, AA, Year, OBP, SLG)
    
    #Create subset for app, ihp = individual hitter performance
      #write.csv(D3OPS, "ihperf.csv", row.names = FALSE)
  
    #IHPerf visual
      OPS <- ggplot(D3OPS %>% filter(Year == 2020) , aes(OBP, SLG)) + geom_jitter(alpha = .4) + 
        xlab("On Base Percentage") + ylab("Slugging Percentage") +
        geom_abline(slope = -1, intercept = seq(0.6, 1.4, by = 0.2)) +
        annotate("text", x = rep(.15, 3) , y = c(.53, .73, .93), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
        annotate("text", x = .27 , y = 1, label = "OPS = 1.2") +
        annotate("text", x = .35 , y = 1.12, label = "OPS = 1.4") +
        ggtitle("Scatterplot of OBP and SLG Values for Hitters with 25+ At-bats")  +
        geom_point(data = filter(D3OPS %>% filter(Year == 2020), Player == "Wickman, Brian"), size = 3, shape = 16, color = "#FF0000")
      OPS
  
      #Team Performance
    #Run team productivity section so TeamHit1 will be available
      #load in IDs
      D3IDs <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/DataPrep/D3IDs1920.csv")
      D3IDs$X <- NULL
      #select relevant subset
      TeamHit <- Bat %>%
        group_by(School, Year) %>%
        summarise(
          H1 = sum(H),
          BB1 = sum(BB),
          HBP1 = sum(HBP),
          AB1 = sum(AB),
          SH1 = sum(SH),
          SF1 = sum(SF),
          x2B1 = sum(X2B),
          x3B1 = sum(X3B),
          HR1 = sum(HR),
          R1 = sum(R)) %>%
        mutate(x1B = (H1-x2B1-x3B1-HR1),
               SLG = ((x1B+2*x2B1+3*x3B1+4*HR1)/(AB1)),
               OBP = ((H1+BB1+HBP1)/(AB1+BB1+HBP1+SF1+SH1)),
               PA = AB1+BB1+HBP1+SF1+SH1)
      
      #Prepare D3IDs for lateral merge
      colnames(D3IDs)[colnames(D3IDs) == "school"] <- "School"
      colnames(D3IDs)[colnames(D3IDs) == "year"] <- "Year"
      
      #Merge by Year and School
      TeamHit1 <- merge(TeamHit, D3IDs, by = c("School", "Year")) %>%
        select(School, conference, Year, OBP, SLG, "R"=R1, PA)
      
      TeamHit2 <- TeamHit1 %>% filter(PA >= 200)
      TeamHit2$R1 <- NULL
      TeamHit2$PA <- NULL
      TeamHit2$OBP <- round(TeamHit2$OBP, digits = 3)
      TeamHit2$SLG <- round(TeamHit2$SLG, digits = 3)
      #write.csv(TeamHit2, "thperf.csv", row.names = FALSE)
      
    #THPerf Visual
      thperf <- ggplot(TeamHit2 %>% filter(Year == 2019), aes(OBP, SLG)) + geom_jitter(alpha = 0.4) + 
        xlab("On Base Percentage") + ylab("Slugging Percentage") +
        geom_abline(slope = -1, intercept = seq(0.6, 1.0, by = 0.2)) +
        annotate("text", x = rep(.27, 3) , y = c(.37, .57, .77), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
        ggtitle("Scatterplot of OBP and SLG Values for Teams with 200+ At-bats")  +
        geom_point(data = filter(TeamHit2 %>% filter(Year == 2019), School == "Wash. & Lee"), size = 3, shape = 16, color = "#0000FF")
      thperf
      
#Hitter Productivity
  #Individual productivity
    #relevant susbet
      D3RC <- Bat %>%
        mutate(PA = AB+HBP+BB+SF+SH) %>%
        filter(RC >= 0) %>%
        select(Player, School, Conference, Year, AA, PA, RC)
      
    #create subset as csv for app
      #write.csv(D3RC, "ihprod.csv", row.names = FALSE)
      
    #calculate productivity percentiles
      #Calculate producitvity centiles
      Q25 = aggregate(D3RC$RC, 
                      list(D3RC$PA), quantile, 0.25)
      Q75 = aggregate(D3RC$RC, 
                      list(D3RC$PA), quantile, 0.75)
      
      #Edit data to rename columsn to PA, RC
      Q25 <- Q25 %>% rename(PA = Group.1, RC = x)
      Q75 <- Q75 %>% rename(PA = Group.1, RC = x)
      
      #Write csv files for quantiles
        #write.csv(Q25, "Q25.csv", row.names = FALSE)
        #write.csv(Q75, "Q75.csv", row.names = FALSE)
        
      #IHProd visual
      RC <- ggplot(D3RC %>% filter(Year == 2019), aes(PA, RC)) + geom_jitter(alpha = 0.4) + 
        xlab("Plate Appearances") + ylab("Runs Created") +
        geom_smooth(data = Q25) +
        geom_smooth(data = Q75) +
        geom_point(data = filter(D3RC %>% filter(Year == 2019), Player == "Wickman, Brian"), size = 3, shape = 16, color = "#FF0000") +
        ggtitle("Scatterplot of Runs Scored against Plate Appearances")
      RC
      
  #Team Prod
      
    #write csv for app
      #TeamHit1$OBP <- NULL
      #TeamHit1$SLG <- NULL
      write.csv(TeamHit1, "thprod.csv", row.names = FALSE)
      
    #Thprod visual
      thprod <-ggplot(TeamHit1 %>% filter(Year == 2019), aes(PA, R1)) + geom_jitter(alpha = 0.7)+
        xlab("Team Plate Appearances") + ylab("Runs Scored") + geom_smooth(method = 'lm') +
        geom_point(data = filter(TeamHit1 %>% filter(Year == 2019), School == "Ursinus"), size = 3, shape = 16, color = "#FF0000") +
        ggtitle("Team Productivity")
      thprod

#Pitcher Performance
   #Individual
     #Import pitching statistics
        Pitch1 <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/app2/FinalPitchAll1.csv")
        Pitch1 <- Pitch1 %>% filter(Year != 2018) %>% arrange(desc(IP))
        Pitch1$AA = 0
        Pitch2 <- Pitch1[c(1,2,27,3:26)]
        #write.csv(Pitch2, "PitchAA.csv", row.names = FALSE)
      #Get AA labeled
        PitchAA <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/app2/PitchAA.csv")
        Pitch1$AA = PitchAA$AA
        
      #Create Relevant Subset
          ipperf <- Pitch1 %>% filter(IP >= 10)
          #write.csv(ipperf, "ipperf.csv", row.names = FALSE)
        
        #Graphic
          #Upper bound on ERA of 16.5, WHIP of 3.1. (Less than 1% of the data removed)
        ipperf <- ggplot(Pitch %>% filter(Year == 2019), aes(ERA, WHIP), color = IP) + geom_jitter( alpha = .3) +
          xlim(0,16.5) + ylim(.2,3.10) +
          xlab("Earned Run Average") + ylab("WHIP") +
          geom_point(data = filter(Pitch %>% filter(Year == 2019), Player == "Devereux, Danny"), size = 3, shape = 16, color = "#FF0000") +
          ggtitle("Scatterplot of ERA and WHIP values for pitchers with 10+ IP")
        ipperf

  #Team
      #Create Relevant subset, D3IDs already loaded in adn colnames revised
        #42 innings chosen because thats the minimum for 5 games? Wait no you'd have to pitch 9 innings to win
        TeamPitch <- Pitch1 %>% mutate(addl_outs = ((IP %% 1)*10),
                                              ip_outs = ((floor(IP)*3 + addl_outs)),
                                              Ip1 = ip_outs / 3) %>%
                                              group_by(School, Year) %>%
                                              summarise(
                                                ER1 = sum(ER),
                                                IP1 = sum(Ip1),
                                                BB1 = sum(BB),
                                                H1 = sum(H)) %>%
                                              mutate(
                                                ERA = 9*(ER1/IP1),
                                                WHIP = ((BB1+H1)/IP1)) %>% 
                                                filter(IP1 >= 45)
        #Rename D3IDs columns
          D3IDs <- rename(D3IDs, "School" = "school")
          D3IDs <- rename(D3IDs, "Year" = "year")
          
        #Merge with D3IDs to add conference information
        TeamPitch2 <- merge(TeamPitch, D3IDs, by = c("School", "Year")) %>%
          select(School, conference, Year, ERA, WHIP)
        
        TeamPitch2$ERA <- round(TeamPitch2$ERA, digits = 2)
        TeamPitch2$WHIP <- round(TeamPitch2$WHIP, digits = 2)
        #write.csv(TeamPitch2, "tpperf.csv", row.names = FALSE)
        
        #Actual Graphic (less thasn 1% of data (6 observations) removed due to upper bounds)
        tpperf <- ggplot(TeamPitch1 %>% filter(Year == 2019), aes(ERA, WHIP), color = IP) + geom_jitter(alpha = .3) +
          xlim(0,12.5) + ylim(.75,2.75) +
          xlab("Earned Run Average") + ylab("WHIP") +
          geom_point(data = filter(TeamPitch1 %>% filter(Year == 2019), School == "Wash. & Lee"), size = 3, shape = 16, color = "#FF0000") +
          ggtitle("Scatterplot of team ERA and WHIP values for teams with 5+ GP")
        tpperf
        
    