
setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/DataPrep")

#Load in packages
library(baseballr)
library(tidyverse)
library(rvest)

#Load in D3IDs1920 (in same folder as this file 

#Changed to do the 2019 batting season
#get list of hitting and pitching data by team
Batting <- list()
Pitching <- list()
for (i in 1:nrow(D3IDs1920)) {
  #tryCatch function overwrites the error of no data availabe (if any) to go to the next school IDs
  tryCatch({
    Batting[[i]] <- ncaa_scrape(D3IDs1920$school_id[i], year = D3IDs1920$year[i], type = "batting")
    Pitching[[i]] <- ncaa_scrape(D3IDs1920$school_id[i], year = D3IDs1920$year[i], type = "pitching")
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

#Save as csv files that will be used in the subsequent cleaning R scripts
#write.csv(D3Bat1,"D3Bat18.csv", row.names = F)
#write.csv(D3Pitch1,"D3Pitch18.csv", row.names = F)
