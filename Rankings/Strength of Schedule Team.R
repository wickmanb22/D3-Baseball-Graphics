---------------
title: 'Individual Team Strength of Schedule'
---------------
setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings")

#Install Necessary Packages
library(plyr)
library(baseballr)
library(tidyverse)
library(rvest)
library(stringr)
library(readxl)

#Scrape W&L Schedule
WLO <- get_ncaa_schedule_info(9684, 2020)%>% filter(result!="NA") %>% select(opponent) %>% group_by(opponent) %>% dplyr::summarise(number=dplyr::n())

#Clean up opponent name (delete @s and extra spaces)
WLO$opponent <- str_remove(WLO$opponent, "@Myrtle Beach, SC") 
WLO$opponent <- str_remove(WLO$opponent, "@") 
WLO$opponent = str_squish(WLO$opponent)

#Merge with D3WLT
OProfile <- read_excel("OpponentProfile.xlsx")
WLOWLT <- merge(OProfile, WLO, by = 'opponent')

#Make new column that shows nW and n(GP)
WLOWLT1 <- WLOWLT %>% mutate(NWinPCT = WinPCT * number, TN = sum(number), TNWP = sum (NWinPCT), SOS = TNWP/TN) 
WLOWLT1$SOS[1]


