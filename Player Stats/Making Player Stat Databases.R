
setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/DataPrep")

#Load in packages
library(baseballr)
library(tidyverse)
library(rvest)

#Add additional year
  D32020IDs <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/D3 Player Stats/IDs2020D3.csv")
  D32020IDs$year = 2018
  #Create 2018 IDs (Drop St. Elizabeth because new program as of 2020)
  D318IDs <- D32020IDs[-c(387),]
  #Merge with D3IDs, will need those for visualizations later
    D3IDs1920 <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/DataPrep/D3IDs1920.csv")
    D3IDs1920$X <- NULL
  D3IDs181920 <- rbind(D318IDs, D3IDs1920)
  #write.csv(D3IDs181920, "D3IDs890.csv", row.names = FALSE)
  
#Changed to do the 2019 batting season
#get list of hitting and pitching data by team
Batting <- list()
Pitching <- list()
for (i in 1:nrow(D318IDs)) {
  #tryCatch function overwrites the error of no data availabe (if any) to go to the next school IDs
  tryCatch({
    Batting[[i]] <- ncaa_scrape(D318IDs$school_id[i], year = D318IDs$year[i], type = "batting")
    Pitching[[i]] <- ncaa_scrape(D318IDs$school_id[i], year = D318IDs$year[i], type = "pitching")
  },error=function(e){})
}

#Call D3 Batting and Pitching into a data frame
D3Bat = plyr::ldply(Batting, data.frame)
D3Pitch = plyr::ldply(Pitching, data.frame)

#Replace NA with 0
D3Bat[is.na(D3Bat)] <- 0
D3Pitch[is.na(D3Pitch)] <- 0

#Remove Totals and Opponent Totals rows and filter by PA or BF
D3Bat$AB <- as.numeric(D3Bat$AB)
D3Bat$BB <- as.numeric(D3Bat$BB)
D3Bat$HBP <- as.numeric(D3Bat$HBP)
D3Bat$SF <- as.numeric(D3Bat$SF)
D3Bat$SH <- as.numeric(D3Bat$SH)

D3Bat$PA = D3Bat$AB+D3Bat$BB+D3Bat$HBP+D3Bat$SF+D3Bat$SH
D3Bat = D3Bat %>% filter(PA > 0, !grepl("Totals|Opponent Totals", Player))
D3Pitch = D3Pitch %>% filter(BF > 0, !grepl("Totals|Opponent Totals", Player))

#Add stats/ win shares/ WHIP to the databases
#Create OPS column
D3Bat$OPS = D3Bat$OBPct+D3Bat$SlgPct

#Import D3BatAll and D3PitchAll and merge 18 data
D3Bat1 <- rbind(D3Bat, D3BatAll)
D3Pitch1 <- rbind(D3Pitch, D3PitchAll)

#Put into a csv file so don't have to keep running code
write.csv(D3Bat1,"D3Bat18.csv", row.names = F)
write.csv(D3Pitch1,"D3Pitch18.csv", row.names = F)
