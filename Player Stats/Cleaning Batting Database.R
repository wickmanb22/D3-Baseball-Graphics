#Formatting database

setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/DataPrep")

#Load in packages
library(baseballr)
library(tidyverse)
library(rvest)
library(stringr)
library(Hmisc)

#load in data
D3Bat <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG2/DataPrep/D3Bat18.csv")

#Make GPGS
D3Bat$GPGS = paste(D3Bat$GP, D3Bat$GS, sep="-")
D3Bat$GPGS = str_replace(D3Bat$GPGS, "NA", "0")


#Select columns
#D3Bat[770,] %>% select(Player, school, Yr, Pos, BA, OPS, GPGS, AB, R, H, X2B, X3B, HR, RBI, TB, SlgPct, BB, HBP, K, DP, OBPct, SF, SH, CS, SB, conference, conference_id, player_url)

#Make a runs created variable
#D3Bat[770,] %>% mutate(RcT = (TB*(H+BB-CS+HBP-DP)*(.26*(BB+HBP))+(.52*(SH+SF+SB)))/(AB+BB+HBP+SH+SF))

#Extrapolate to entire database
D3Bat2 <- D3Bat %>% select(Player, school, Yr, Pos, BA, OPS, GPGS, AB, R, H, X2B, X3B, HR, RBI, TB, SlgPct, BB, HBP, K, DP, OBPct, SF, SH, CS, SB, conference, year)

#Fix CSE column
D3Bat2$school = str_replace(D3Bat2$school, "0", "St. Elizabeth")
D3Bat2$conference = str_replace(D3Bat2$conference, "0", "CSAC")

#Round the columns correctly
D3Bat2$OBPct <- as.numeric(D3Bat2$OBPct)
D3Bat2$OBPct <- round(D3Bat2$OBPct, digits = 3)
D3Bat2$BA <- as.numeric(D3Bat2$BA)
D3Bat2$BA <- round(D3Bat2$BA, digits = 3)


#Clean up conference name
D3Bat2$conference <- str_replace(D3Bat2$conference,"Michigan Intercol. Ath. Assn.", "MIAA")

#Last step rename GPGS column
D3Bat2 <- rename(D3Bat2, "GP-GS" = "GPGS")
D3Bat2 <- rename(D3Bat2, "2B" = "X2B")
D3Bat2 <- rename(D3Bat2, "3B" = "X3B")
D3Bat2 <- rename(D3Bat2, "OBP" = "OBPct")
D3Bat2 <- rename(D3Bat2, "SLG" = "SlgPct")
D3Bat2 <- rename(D3Bat2, "AVG" = "BA")


#Clean up position vector
D3Bat2$Pos <- toupper(D3Bat2$Pos)
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PR/DH", "OF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/CF", "OF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "UT", "UTIL")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/CF", "OF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "3B/SS", "INF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/2B", "2B")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/RF", "RF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/3B", "3B")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "SS/3B", "INF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "P/RF", "RF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/LF", "LF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/1B", "1B")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PH/C", "C")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "1B/RF", "UTIL")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "3B/1B", "INF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "2B/P", "2B")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "PR/CF", "CF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "SS/RF", "UTIL")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "2B/SS", "INF")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "DH/1B", "1B")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "UTILIL", "UTIL")
D3Bat2$Pos = str_replace(D3Bat2$Pos, "RF/P", "RF")

#Put in empty positons with UTIL designations
D3Bat2$Pos <- sub("^$", "UTIL", D3Bat2$Pos)

#Generalize the positions, there are too many specifics makes sorting the table to find position misrepresentative
D3Bat2$Pos[D3Bat2$Pos == "1B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "2B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "3B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "SS"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "PR"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "PH"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "DH"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "P"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "LF"] <- "OF"
D3Bat2$Pos[D3Bat2$Pos == "RF"] <- "OF"
D3Bat2$Pos[D3Bat2$Pos == "CF"] <- "OF"
D3Bat2$Pos[D3Bat2$Pos == "CF/LF"] <- "OF"
D3Bat2$Pos[D3Bat2$Pos == "SS/P"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "PH/SS"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "RF/1B"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "3B/C"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "SS/2B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "PR/2B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "PR/1B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "RP"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "PH/DH"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "DH/P"] <- "UTIL"
D3Bat2$Pos[D3Bat2$Pos == "PR/LF"] <- "OF"
D3Bat2$Pos[D3Bat2$Pos == "1B/P"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "PR/3B"] <- "INF"
D3Bat2$Pos[D3Bat2$Pos == "RF/LF"] <- "OF"
D3Bat2$Pos[D3Bat2$Pos == "DH/2B"] <- "INF"

#Check position vector, should only be four positions
pos <- unique(D3Bat2$Pos)
pos

#Calculate RC
D3Bat3 <- D3Bat2 %>% mutate(RC = ((H+BB-CS+HBP-DP)*(TB+(.26*(BB+HBP))+(.52*(SH+SF+SB))))/(AB+BB+HBP+SH+SF))

#Round slg
D3Bat3$SLG <- as.numeric(D3Bat3$SLG)
D3Bat3$SLG <- signif(D3Bat3$SLG, digits = 4)
D3Bat3 <- D3Bat3 %>% arrange(desc(RC))

#Round RC
D3Bat3$RC <- as.numeric(D3Bat3$RC)
D3Bat3$RC <- signif(D3Bat3$RC, digits = 3)

#reorder columns
D3Bat4 <- D3Bat3[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,28,26,27)]

#Delete unknown players, requires attention to detail
D3Bat4 <- D3Bat4 %>% arrange(Player)
D3Bat4 <- D3Bat4[-c(1:3),]

#Arranging Data
D3Bat4 <- D3Bat4 %>% arrange(school)
D3Bat4 <- D3Bat4 %>% arrange(desc(Yr))
D3Bat5 <- D3Bat4 %>% arrange(desc(RC))

#Fix year column
D3Bat5$Yr <- str_replace(D3Bat5$Yr, "N/A", " ")    

#Correct column names
colnames(D3Bat5)[colnames(D3Bat5) == "school"] <- "School"
colnames(D3Bat5)[colnames(D3Bat5) == "conference"] <- "Conference"
colnames(D3Bat5)[colnames(D3Bat5) == "year"] <- "Year"
colnames(D3Bat5)[colnames(D3Bat5) == "Yr"] <- "Class"

#change working directory to official app
#write.csv(D3Bat5, "FinalBatAll1.csv", row.names = F)          
          
          