# D3 Rankings 2022 (Part 2)
# October 15, 2022
# Brian Wickman

# Load packages
  library(tidyverse)
  library(baseballr)
  library(stringr)

# Load workspace (from D3Rankings_v2_p1.R)
  load("~/GitProjects/D3-Baseball-Graphics/Rankings/D3Rankings_v2_p1_Workspace.RData")


# Scrape data from NCAA website -------------------------------------------
  # Scrape batting and pitching statistics
    Batting <- list()
    Pitching <- list()
    for (i in 1:nrow(D3IDs3)) {
      #tryCatch function overwrites the error of no data availabe (if any) to go to the next school IDs
      #tryCatch({
        Batting[[i]] <- ncaa_scrape(D3IDs3$school_id[i], year = 2022, type = "batting")
        Pitching[[i]] <- ncaa_scrape(D3IDs3$school_id[i], year = 2022, type = "pitching")
      #},error=function(e){})
    }
    
  # Move to dataframes
    D3Bat <- rbindlist(Batting)
    D3Pitch <- rbindlist(Pitching)
    
  # Replace NAs with 0
    D3Bat[is.na(D3Bat)] <- 0
    
  # Calculate general statistics  
    D3Bat$PA = D3Bat$AB+D3Bat$BB+D3Bat$HBP+D3Bat$SF+D3Bat$SH
    D3Bat$OPS = D3Bat$OBPct+D3Bat$SlgPct

# Clean Individual Batting Dataframe --------------------------------------
  # Filter individual batters with 0 PAs and pitchers with 0 innings  
    D3Bat_players = D3Bat %>% filter(PA > 0, !grepl("Totals|Opponent Totals", Player))
    
  # Select relevant columns  
    D3Bat2_players <- D3Bat_players %>% select(Player, school, Yr, Pos, GP, GS, PA, AB,
                               AB, R, H, '2B', '3B', HR, RBI, TB,
                               BB, HBP, K, DP, OBPct, SF, SH, CS, SB,
                               BA, OBPct, SlgPct, OPS, conference)
    
  # Rename columns
    D3Bat2_players <- rename(D3Bat2_players, "SLG" = "SlgPct")
    D3Bat2_players <- rename(D3Bat2_players, "OBP" = "OBPct")
    
  # Clean up position column
    D3Bat2_players$Pos <- toupper(D3Bat2_players$Pos)
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PR/DH", "OF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/CF", "OF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "UT", "UTIL")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/CF", "OF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "3B/SS", "INF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/2B", "2B")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/RF", "RF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/3B", "3B")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "SS/3B", "INF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "P/RF", "RF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/LF", "LF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/1B", "1B")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PH/C", "C")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "1B/RF", "UTIL")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "3B/1B", "INF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "2B/P", "2B")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PR/CF", "CF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "SS/RF", "UTIL")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "2B/SS", "INF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "DH/1B", "1B")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "RF/P", "RF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PR/LF", "LF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "PR/3B", "3B")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "2B/RF", "UTIL")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "DH/LF", "LF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "RFH", "RF")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "LF/1B", "UTIL")
    D3Bat2_players$Pos = str_replace(D3Bat2_players$Pos, "SS/P", "SS")
    D3Bat2_players$Pos[D3Bat2_players$Pos == ""] <- "UTIL"   
    
# Generalize positions
    D3Bat2_players$Pos[D3Bat2_players$Pos == "1B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "2B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "3B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "SS"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PR"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PH"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "DH"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "P"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "LF"] <- "OF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "RF"] <- "OF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "CF"] <- "OF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "CF/LF"] <- "OF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "SS/P"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PH/SS"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "RF/1B"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "3B/C"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "SS/2B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PR/2B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PR/1B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "RP"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PH/DH"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "DH/P"] <- "UTIL"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PR/LF"] <- "OF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "1B/P"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "PR/3B"] <- "INF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "RF/LF"] <- "OF"
    D3Bat2_players$Pos[D3Bat2_players$Pos == "DH/2B"] <- "INF"
    
  # Calculate Runs Created statistic
   D3Bat3_players <- D3Bat2_players %>% mutate(RC = round(((H+BB-CS+HBP-DP)*(TB+(.26*(BB+HBP))+(.52*(SH+SF+SB))))/(AB+BB+HBP+SH+SF),digits = 1))
   
  # Delete unknown players, requires attention to detail
   D3Bat3_players <- D3Bat3_players %>% arrange(Player)
   D3Bat4_players<- D3Bat3_players[-c(1:2),]
   
  # Fix NA in Yr column
   D3Bat4_players$Yr <- str_replace(D3Bat4_players$Yr, "N/A", "")    
   
  # Fix column names
   colnames(D3Bat4_players)[colnames(D3Bat4_players) == "school"] <- "School"
   colnames(D3Bat4_players)[colnames(D3Bat4_players) == "conference"] <- "Conference"
   colnames(D3Bat4_players)[colnames(D3Bat4_players) == "Yr"] <- "Class"
   
  # Order columns for final table
   Batters_Individual <- D3Bat4_players %>% 
     select(1:19,21:25,20,26,27,29,28) %>% 
     arrange(desc(RC),desc(Player))
    #write_csv(Batters_Individual, "Batting_Individual.csv")
   
  #clean workspace
   rm(list = setdiff(ls(), c("D3Bat","D3Pitch","Batters_Individual")))
   
# Clean Team Batting ------------------------------------------------------
  # Filter team data and select columns
   Batters_Team = D3Bat %>% filter(Player == "Totals") %>%
     select(School = 2, Conference = 3, 11, OBP = 12, SLG = 13, 37, 14, 16:30,32,33)
   
  # Save team df
   #write_csv(Batters_Team, "Batting_Team.csv")
   
  # Clean workspace
   rm(list = setdiff(ls(), c("D3Bat","D3Pitch")))

# Clean Individual Pitching ------------------------------------------------
  # Filter player pitching stats
   D3Pitch_players = D3Pitch %>% filter(BF > 0, !grepl("Totals|Opponent Totals", Player))
   
  # Add WHIP and BAA
   D3Pitch1 <- D3Pitch_players %>% mutate(WHIP = round((BB+H)/(IP),2),
                                          BAA = round(H/(BF-(BB+HB+SHA+SFA+IBB)),3))
   
  # Add FIP
   D3Pitch2 <- D3Pitch1 %>% mutate(addl_outs = ((IP %% 1)*10),
                                     ip_outs = ((floor(IP)*3 + addl_outs)),
                                     Ip1 = ip_outs / 3)
   
   # Get subset for BF > 25
     BF25 <- D3Pitch2 %>% filter(BF >= 25) 
     
   # League ERA
     lgER = sum(BF25$ER)
     lgIp1 = sum(BF25$Ip1)
     lgERA = (lgER/lgIp1)*9
     
   # League HR
     lgHR = sum(BF25$HR)
     lgBB = sum(BF25$BB)
     lgHBP = sum(BF25$HB)
     lgK = sum(BF25$SO)
   
   # Actual Constant
     FIPC20 = lgERA-(((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIp1)
   
   # Calculate FIP per player
     #Rename HR-A column
     colnames(D3Pitch2)[25] <- "HR.A"
     D3Pitch3 <- D3Pitch2 %>% mutate(FIP1 = ifelse(BF >= 25, (((13*HR.A)+(3*(IBB+HB))-(2*SO))/Ip1) + FIPC20, NA)) %>%
       mutate(FIP = round(ifelse(FIP1 <= 0, 0, FIP1),2))
     D3Pitch3$FIP1 <- NULL
     
   # Remove unnamed players
     D3Pitch3 <- D3Pitch3 %>% arrange(Player)
     D3Pitch4 <- D3Pitch3[-1,] 
     
   # Fix missing class years
     D3Pitch4$Yr <- str_replace(D3Pitch4$Yr, "N/A", "")    
     
   # Select relevant columns
     D3Pitch5 <- D3Pitch4 %>% select(6,School = 2, Class = 7,10,11,36:38,13,20,14:17,HBP=27,28,18,"2B.A" = 22,"3B.A" = 23,25,34,35,44,45,12,49,Conference = 3) %>%
       arrange(desc(IP))
     
   # Save
     rm(list = setdiff(ls(), c("D3Bat", "D3Pitch")))
     #write_csv(D3Pitch5, "Pitching_Individual.csv")
               
# Clean Team Pitching ------------------------------------------------
    # Filter team data
      Pitch_Team = D3Pitch %>% filter(Player == "Totals") %>%
       mutate("K/9" = round(9*SO/IP,2)) %>%
       select(School = 2, Conference = 3, 13:17,HBP = 27,28,18,"2B.A"=22,"3B.A"=23,"HR.A"=25,34,35,24,44,12)
       
    # Save pitching data
      #write_csv(Pitch_Team,"Pitching_Team.csv")
     