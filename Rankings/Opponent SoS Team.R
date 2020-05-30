---------------
title: 'Washington and Lee Opponents Opponents Strength of Schedule'
---------------
setwd("~/R/Baseball Analysis/Intro Baseball Analysis/D3 Rankings")

#Install Necessary Packages
library(plyr)
library(baseballr)
library(tidyverse)
library(rvest)
library(stringr)
library(readxl)

#Scrape W&L Schedule, adjust 9684 for teamID on stats.ncaa.org
WLO <- get_ncaa_schedule_info(9684, 2020)%>% 
  filter(result!="NA") %>% 
  select(opponent) %>% 
  group_by(opponent) %>%
  dplyr::summarise(number=dplyr::n())

#Clean up opponent name, adjust for each team (delete @s and extra spaces)
WLO$opponent <- str_remove(WLO$opponent, "@Myrtle Beach, SC") 
WLO$opponent <- str_remove(WLO$opponent, "@") 
WLO$opponent = str_squish(WLO$opponent)

#Merge with D3WLT
OProfile <- read_excel("OpponentProfile.xlsx")
WLOWLT <- merge(OProfile, WLO, by = 'opponent')

#Scrape Schedule of Opponents, adjust school_id = 9684
WLList <- list()
for (i in 1:nrow(WLOWLT)) {
  WLList[[i]] =  get_ncaa_schedule_info(WLOWLT$opponent_id[i], 2020)%>% 
    filter(result!="NA") %>% select(opponent) %>% 
    group_by(opponent) %>% 
    dplyr::summarise(number=dplyr::n()) %>% 
    mutate(school_id = 9684)}

#Move back to dataframe ASAP
WLOSOS <- do.call("rbind", WLList)

#Clean up dataframe of all other teams, adjust for each team
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Myrtle Beach, SC") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Fayetteville, NC") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Auburndale, FL") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Montgomery, AL")
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Emmitsburg, MD") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Emmittsburg, Md.") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Aston, PA") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@Yaphank, NY") 
WLOSOS$opponent <- str_remove(WLOSOS$opponent, "@") 
WLOSOS$opponent = str_squish(WLOSOS$opponent)

##Merge with D3WLT
OProfile <- read_excel("OpponentProfile.xlsx")
WLOWLT <- merge(OProfile, WLOSOS, by = 'opponent')

#Make new column that shows nW and n(GP)
WLOWLT <- WLOWLT %>% mutate(NWinPCT = WinPCT * number, TN = sum(number), TNWP = sum (NWinPCT), OSOS = TNWP/TN) 
WLOWLT$OSOS[1]
#Group by and prepare to merge
#WLOOSOS <- WLOWLT %>% group_by(school_id) %>%
#  summarise(TNW = sum(NWinPCT), TNGP = sum (NGP)) %>%
 # transmute(school_id, OSOS = TNW/TNGP) 
#WLOOSOS



