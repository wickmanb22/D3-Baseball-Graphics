# D3 Rankings Version 2, part 2
# October 14, 2022
# Brian Wickman

# Load packages
  library(tidyverse)
  library(baseballr)
  library(stringr)

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
      names(SOSList)[i] <- D3IDs4$School[i]}
  
  # Move to dataframe
    SOSdf <- rbindlist(SOSList, idcol = TRUE)
    colnames(SOSdf)[1] <- "school"
  
  # Win Loss dataframe
    wl_df <- D3IDs4 %>% select(opponent = 1,7:9)
    wl_df[,2:4] <- lapply(wl_df[,2:4],as.numeric) # Convert W,L,T columns from character to numeric
    wl_df$wpct <- round((wl_df$W + 0.5*wl_df$T)/(wl_df$W + wl_df$L + wl_df$T),3)
    wl_df2 <- wl_df[,c(1,5)]
    
  # Calculate opponents WL%, match uchicago
    setdiff(SOSdf$opponent, wl_df2$opponent) # jeez
    oppDF <- merge(SOSdf,wl_df2, all.x = TRUE, by = "opponent")
    
  # Group by school, calculate number * winpct
    
