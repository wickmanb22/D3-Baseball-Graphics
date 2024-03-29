#Formatting pitching database

#Install/ load in packages
library(baseballr)
library(tidyverse)
library(rvest)
library(stringr)
library(Hmisc)

#load in pitching data from "Making stat database.R"
#Set to your own directory
#D3Pitch <- D3Pitch1920

#Add column for WHIP
D3Pitch1 <- mutate(D3Pitch, WHIP = (BB+H)/(IP))

#Win Loss column
D3Pitch1$WL = paste(D3Pitch1$W, D3Pitch1$L, sep="-")

#Opponents batting average
D3Pitch1 <- mutate(D3Pitch1, POAB = BF-(BB+HB+SHA+SFA+IBB))
D3Pitch1 <- mutate(D3Pitch1, BAA = H/POAB)
D3Pitch1$P.OAB <- NULL

#Now to calculate FIP
#Since FIP depends on run scoring environment in ech year, we have to calculate them separately
#Break databases into three for FIP calculation
D3Pitch19 <- D3Pitch1 %>%
  filter(year == "2019")

D3Pitch20 <- D3Pitch1 %>%
  filter(year == "2020")

#Run each database through fip calculations and then rbind them  
    #FIP for 2019 
      D3Pitch19 <- D3Pitch19 %>% mutate(addl_outs = ((IP %% 1)*10),
                            ip_outs = ((floor(IP)*3 + addl_outs)),
                            Ip1 = ip_outs / 3)
  
      #Get subset for BF > 25
        BF2519 <- D3Pitch19 %>% filter(BF >= 25) 
      #League ERA
        lgER = sum(BF2519$ER)
        lgIp1 = sum(BF2519$Ip1)
        lgERA = (lgER/lgIp1)*9
      #League HR
        lgHR = sum(BF2519$HR)
        lgBB = sum(BF2519$BB)
        lgHBP = sum(BF2519$HB)
        lgK = sum(BF2519$SO)
      #Actual Constant
        FIPC19 = lgERA-(((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIp1)
  
      #Calculate FIP per player
        D3Pitch19 <- D3Pitch19 %>% mutate(FIP = ifelse(BF >= 25, (((13*HR.A)+(3*(IBB+HB))-(2*SO))/Ip1) + FIPC19, NA))
        
    #FIP for 2020
        D3Pitch20 <- D3Pitch20 %>% mutate(addl_outs = ((IP %% 1)*10),
                                          ip_outs = ((floor(IP)*3 + addl_outs)),
                                          Ip1 = ip_outs / 3)
        
        #Get subset for BF > 25
        BF2520 <- D3Pitch20 %>% filter(BF >= 25) 
        #League ERA
        lgER = sum(BF2520$ER)
        lgIp1 = sum(BF2520$Ip1)
        lgERA = (lgER/lgIp1)*9
        #League HR
        lgHR = sum(BF2520$HR)
        lgBB = sum(BF2520$BB)
        lgHBP = sum(BF2520$HB)
        lgK = sum(BF2520$SO)
        #Actual Constant
        FIPC20 = lgERA-(((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIp1)
        
        #Calculate FIP per player
        D3Pitch20 <- D3Pitch20 %>% mutate(FIP1 = ifelse(BF >= 25, (((13*HR.A)+(3*(IBB+HB))-(2*SO))/Ip1) + FIPC20, NA)) %>%
          mutate(FIP = ifelse(FIP1 <= 0, 0, FIP1))
        D3Pitch20$FIP1 <- NULL

#combine both databases with their respective FIP calculations
  FipPitch20 <- rbind(D3Pitch19, D3Pitch20)  
      
#Delete the 4 unnamed players, they seem to be copies of other, already accounted for players
    FipPitch20 <- FipPitch20 %>% arrange(Player)
    FipPitch20 <- FipPitch20[-c(1:4),] 
    
#Fix CSE column
    FipPitch20$school = str_replace(FipPitch20$school, "0", "St. Elizabeth")
    FipPitch20$conference = str_replace(FipPitch20$conference, "0", "CSAC")
    
#Replace N/A school years with  " "
    FipPitch20$Yr <- str_replace(FipPitch20$Yr, "N/A", "")    

#Clean up unused columns
FipPitch21 <- FipPitch20 %>% select(Player, school, Yr, ERA, WHIP, WL, SV, IP, H, R, ER, BB, SO, X2B.A, X3B.A, HR.A, BF, BAA, WP, HB, GO, FO, IBB, FIP, conference, year)

#Round columns correctly
FipPitch21$WHIP <- round(FipPitch21$WHIP, digits = 2)
FipPitch21$BAA <- round(FipPitch21$BAA, digits = 3)
FipPitch21$FIP <- round(FipPitch21$FIP, digits = 2)

#Name columns correctly
colnames(FipPitch21)[colnames(FipPitch21) == "school"] <- "School"
colnames(FipPitch21)[colnames(FipPitch21) == "X2B.A"] <- "x2B"
colnames(FipPitch21)[colnames(FipPitch21) == "X3B.A"] <- "x3B"
colnames(FipPitch21)[colnames(FipPitch21) == "HR.A"] <- "HR"
colnames(FipPitch21)[colnames(FipPitch21) == "HB"] <- "HBP"
colnames(FipPitch21)[colnames(FipPitch21) == "conference"] <- "Conference"
colnames(FipPitch21)[colnames(FipPitch21) == "year"] <- "Year"
colnames(FipPitch21)[colnames(FipPitch21) == "Yr"] <- "Class"

FinalPitchAll1 <- FipPitch21 %>% arrange(desc(IP))

#This is the pitching stats table as seen on the application
