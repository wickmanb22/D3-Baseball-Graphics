# D3 Rankings Version 2, part 2
# October 14, 2022
# Brian Wickman

# Load packages
  library(tidyverse)
  library(baseballr)
  library(stringr)
  library(data.table)
  library(ggplot2)

# Load workspace from part 1
  load("~/GitProjects/D3-Baseball-Graphics/Rankings/D3Rankings_v2_p1_Workspace.RData")
  
# Strength of Schedule ----------------------------------------------------
  # Clean D3WList opponent (muddled with @'s and such)
    #Set up location columns
      home_school_loc <- D3IDs3[,c(1,2)] %>% arrange(School)
      away_school_loc <- D3IDs3[,c(1,2)]
      colnames(home_school_loc)[2] <- "home_location"
      colnames(away_school_loc)[2] <- "away_location"
      colnames(away_school_loc)[1] <- "Opponent"
      
    # Identify opponents and location
      clean_list <- list()
      for (i in 1:length(D3WList)){
        # Select dataframe from D3WList  
          test <- D3WList[[i]]
          test[,5:6] <- home_school_loc[i]
          test[,7:8] <- str_split_fixed(test$opponent, "@", n = 2)
          test[test == ""] <- NA
        
        # Identify opponents  
          test$Opponent <- NA
          for (j in 1:nrow(test)){
            if (is.na(test$V1)[j]){
              test$Opponent[j] = str_trim(test$V2[j], side = "both")
            } else {
              test$Opponent[j] = str_trim(test$V1[j], side = "both")
            }
          }  
          test1 <- merge(test,away_school_loc,by="Opponent",all.x = TRUE)
        
        # Add location to games
          test1$delim_count <- str_count(test1$opponent, pattern = "@")
          test1$clean_location <- NA
          for (k in 1:nrow(test1)){
            if (test1$delim_count[k] == 1){
              if (str_detect(test1$V2[k], ",")){
                test1$clean_location[k] = str_trim(test1$V2[k], side = "both")
              } else{
                test1$clean_location[k] = str_trim(test1$away_location[k], side = "both")
              }
            } else {
              test1$clean_location[k] = str_trim(test1$home_location[k], side = "both")
            }
          }  
        
        # Select relevant columns
        clean_list[[i]] <- test1 %>% select(date, school = School, opponent = Opponent, location = clean_location, result, score)
        names(clean_list)[i] <- names(D3WList)[i]
      }
  
  # Identify opponents
    D3IDs4 <- D3IDs3 %>% arrange(School)
    SOSList <- list()
    for (i in 1:length(clean_list)) {
      SOSList[[i]] <- clean_list[[i]] %>%
        group_by(opponent) %>% 
        summarise(number = n()) %>%
        mutate(school_id = D3IDs4$school_id[i])
      colnames(SOSList[[i]])[1] <- "School"
      names(SOSList)[i] <- D3IDs4$School[i]
      }
  
  # Move to dataframe
    SOSdf <- rbindlist(SOSList, idcol = TRUE)
    colnames(SOSdf)[1] <- "School"
  
  # Win Loss dataframe
    wl_df <- D3IDs4 %>% select(School = 1,7:9)
    wl_df[,2:4] <- lapply(wl_df[,2:4],as.numeric) # Convert W,L,T columns from character to numeric

  # Calculate opponents WL%
    SOS_OppWL <- list()
    for (i in 1:length(SOSList)){
      df1 <- merge(SOSList[[i]], wl_df, by = "School", all.x = TRUE) %>% drop_na()
      df2 <- df1 %>% mutate(W = sum(number * W), L = sum(number * L), T = sum(number * T))
      SOS_OppWL[[i]] <- df2[1,4:6]
      names(SOS_OppWL)[i] <- wl_df$School[i]
    }
    
    # Bind list to dataframe
      team_opp_wl <- rbindlist(SOS_OppWL, idcol = "School") %>%
        mutate(opp_win_pct = (2*(W+T))/(2*(W+L+T))*100)
      
  # Calculate Schools' Schools' WL%
    SOS_OppOppWL <- list()
    for (i in 1:length(SOSList)){
        df1 <- merge(SOSList[[i]], team_opp_wl[,-5], by = "School", all.x = TRUE) %>% drop_na()
        df2 <- df1 %>% mutate(W = sum(number * W), L = sum(number * L), T = sum(number * T))
        SOS_OppOppWL[[i]] <- df2[1,4:6]
        names(SOS_OppOppWL)[i] <- team_opp_wl$School[i]
    }
    
    # Bind list to dataframe and do calculations
      team_oppopp_wl <- rbindlist(SOS_OppOppWL, idcol = "School") %>% 
        mutate(opp_opp_win_pct = (2*(W+T))/(2*(W+L+T))*100)
  
  # Bring relevant data back to one dataframe
    # Convert school column to factor
      D3IDs4$School <- as.factor(D3IDs4$School)
      winpct_calc$School <- as.factor(winpct_calc$School)
      winpct_calc <- merge(team_opp_wl[,c(1,5)],team_oppopp_wl[,c(1,5)], by = "School")
      D3IDs5 <- merge(D3IDs4, winpct_calc, by = "School")
      
    # Strength of schedule calculation
      D3IDs5[,7:9] <- lapply(D3IDs5[,7:9],as.numeric) # Convert W,L,T columns from character to numeric
      d3_team <- D3IDs5 %>% mutate(win_pct = round((W+0.5*T)/(W+L+T),3),
                                   SOS = round((2*opp_win_pct + opp_opp_win_pct)/3,2),
                                   RPI = round(((0.25 * win_pct) + (0.5*opp_win_pct) + (0.25*opp_opp_win_pct)),2)) %>% arrange(desc(win_pct))

  # Select relevant columns
      d3_team_clean <- d3_team[,c(1,2,3,8,7,9,12,13,14)] 
      colnames(d3_team_clean)[7] <- "WinPct"
      #write.csv(d3_team_clean, "Team_Records.csv", row.names=FALSE)
