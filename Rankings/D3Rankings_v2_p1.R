# D3 Rankings Version 2
# October 14, 2022
# Brian Wickman

# Load packages
  library(tidyverse)
  library(baseballr)
  library(rvest)
  library(data.table)
  library(stringr)

# Download list of D3 teams -----------------------------------------------
  # Scrape list of team names from D3baseball.com
  team_names <- list()
  for (num in 1:10){ # 10 regions on d3baseball.com
    url <- paste("https://d3baseball.com/teams/region/Region",num,sep="-")
    team_names[[num]] <- read_html(url) %>% html_nodes(xpath = "//*[@id='mainbody']/table") %>% html_table() %>% 
      data.frame(stringsAsFactors = F)
    names(team_names[[num]]) <- team_names[[num]][1,] # Convert first row to header of dataframe
    team_names[[num]] <- team_names[[num]][-1,] # Delete first row 
  }  

  # Merge all dataframes in team_names
    D3 <- rbindlist(team_names)
    
  # Remove all periods (such as (La.))  
    #D3 <- D3 %>%
    #  mutate_all(~str_remove_all(as.character(.x), '\\.')) %>%
    #  arrange(School)

  # Fix team names with abbreviations
    D3$School = str_replace(D3$School,"North Carolina","N.C.")
    D3$School = str_replace(D3$School,"Tenn.","TN.")
    D3$School = str_replace(D3$School,"Birmingham-Southern","Birmingham-So.")
    D3$School = str_replace(D3$School,"Christopher","Chris.")
    D3$School = str_replace(D3$School,"Eastern Mennonite","East. Mennonite")
    D3$School = str_replace(D3$School,"Emory and Henry","Emory & Henry")
    D3$School = str_replace(D3$School,"Southern Virginia","Southern Va.")
    D3$School = str_replace(D3$School,"Md.","MD.")
    D3$School = str_replace(D3$School,"Washington and Lee","Wash. & Lee")
    D3$School = str_replace(D3$School,"Virginia Wesleyan","Va. Wesleyan")
    D3$School = str_replace(D3$School,"Brockport","SUNY Brockport")
    D3$School = str_replace(D3$School,"Case Western Reserve","CWRU")
    D3$School = str_replace(D3$School,"State","St.")
    D3$School = str_replace(D3$School,"Massachusetts College","MCLA")
    D3$School = str_replace(D3$School,"Massachusetts Maritime","Mass. Maritime")
    D3$School = str_replace(D3$School,"Mass-Boston","UMass Boston")
    D3$School = str_replace(D3$School,"Mass-Dartmouth","UMass Dartmouth")
    D3$School = str_replace(D3$School,"SUNY-","SUNY ")
    D3$School = str_replace(D3$School,"Maine","Me.")
    D3$School = str_replace(D3$School,"Johnson and Wales","JWU")
    D3$School = str_replace(D3$School,"SUNY Cobleskill","Cobleskill St.")
    D3$School = str_replace(D3$School,"Eastern Connecticut","Eastern Conn. St.")
    D3$School = str_replace(D3$School,"College","Col.")
    D3$School = str_replace(D3$School,"Penn Col.","Penn College")
    D3$School = str_replace(D3$School,"Western Connecticut","Western Conn. St.")
    D3$School = str_replace(D3$School,"Western New England","Western New Eng.")
    D3$School = str_replace(D3$School,"Mount St. Joseph","Mt. St. Joseph")
    D3$School = str_replace(D3$School,"Mount St. Mary","Mt. St. Mary")
    D3$School = str_replace(D3$School,"Mount St. Vincent","CMSV")
    D3$School = str_replace(D3$School,"Rochester Tech","RIT")
    D3$School = str_replace(D3$School,"RPI","Rensselaer")
    D3$School = str_replace(D3$School,"SUNY Old Westbury","Old Westbury")
    D3$School = str_replace(D3$School,"SUNY Purchase","Purchase")
    D3$School = str_replace(D3$School,"New York University","NYU")
    D3$School = str_replace(D3$School,"Penn St.-Harrisburg","Penn St. Harrisburg")
    D3$School = str_replace(D3$School,"William Paterson","Wm. Paterson")
    D3$School = str_replace(D3$School,"Franklin and Marshall","Franklin & Marshall")
    D3$School = str_replace(D3$School,"St. Elizabeth","Saint Elizabeth")
    D3$School = str_replace(D3$School,"Pitt-Bradford","Pitt.-Bradford")
    D3$School = str_replace(D3$School,"Pitt-Greensburg","Pitt.-Greensburg")
    D3$School = str_replace(D3$School,"St. Vincent","Saint Vincent")
    D3$School = str_replace(D3$School,"Washington and Jefferson","Wash. & Jeff.")
    D3$School = str_replace(D3$School,"Concordia-Chicago","Concordia Chicago")
    D3$School = str_replace(D3$School,"Illinois Tech","IIT")
    D3$School = str_replace(D3$School,"Illinois Wesleyan","Ill. Wesleyan")
    D3$School = str_replace(D3$School,"Milwaukee Engineering","MSOE")
    D3$School = str_replace(D3$School,"Washington U.","WashU")
    D3$School = str_replace(D3$School,"Wisconsin Lutheran","Wis. Lutheran")
    D3$School = str_replace(D3$School,"Concordia-Moorhead","Concordia-M'head")
    D3$School = str_replace(D3$School,"Gustavus Adolphus","Gust. Adolphus")
    D3$School = str_replace(D3$School,"Illinois Colllege","Illinois Col.")
    D3$School = str_replace(D3$School,"Minnesota-Morris","Minn.-Morris")
    D3$School = str_replace(D3$School,"Nebraska Wesleyan","Neb. Wesleyan")
    D3$School = str_replace(D3$School,"St. John's","Saint John's")
    D3$School = str_replace(D3$School,"UW-","Wis.-")
    D3$School = str_replace(D3$School,"Claremont-Mudd-Scripps","Claremont-M-S")
    D3$School = str_replace(D3$School,"East Texas Baptist","East Tex. Baptist")
    D3$School = str_replace(D3$School,"Lewis and Clark","Lewis & Clark")
    D3$School = str_replace(D3$School,"University of Dallas","Dallas")
    D3$School = str_replace(D3$School,"University of the Ozarks","Ozarks")

  # Fix conference names
    D3$Conference = str_replace(D3$Conference,"GNAC","Great Northeast")
    D3$Conference = str_replace(D3$Conference,"NAC","North Atlantic")
    D3$Conference = str_replace(D3$Conference,"North AtlanticC","NACC")
    D3$Conference = str_replace(D3$Conference,"LL","Liberty League")
    D3$Conference = str_replace(D3$Conference,"MACF","MAC Freedom")
    D3$Conference = str_replace(D3$Conference,"ARC","American Rivers")

  # Remove provisional members
    D3 <- D3[!(D3$School == "Asbury"),]
    D3 <- D3[!(D3$School == "Bob Jones"),]
    D3 <- D3[!(D3$School == "Hobart"),] # New to D3 in 22-23
    D3 <- D3[!(D3$School == "Sage"),] # Not found with web scraper (new in 21-22)
    D3 <- D3[!(D3$School == "Lycoming"),] # New to D3 in 22-23
    D3 <- D3[!(D3$School == "Rosemont"),] # Not found with web scraper (new in 21-22)
    D3 <- D3[!(D3$School == "Mary Baldwin"),] # Not found with web scraper (new in 20-21)
    D3 <- D3[!(D3$School == "MUW"),] # New to D3 in 22-23
    D3 <- D3[!(D3$School == "Wis.-Eau Claire"),] # New to D3 in 20-21
    
  # Separate Location from String (ex: (NJ))
    test <- data.frame(str_split_fixed(D3$School, '\\(', n = 2))
    D3$School <- test$X1

  # Loop to create List of D3 Schools using baseballr
    # Create placeholder tibble for teams that are not found with ncaa_school_id_lu()
      placeholder_list <- list()
      for (i in 1:nrow(D3)){
      placeholder_list[[i]] <- tibble(school = D3$School[i], conference = NA, school_id = NA,
                            year = NA, division = NA, conference_id = NA)
      }
    
    # Scrape school info with ncaa_school_id_lu()  
      D3list <- list()
      for (i in 1:nrow(D3)) {
        screen1 <- ncaa_school_id_lu(D3$School[i]) %>% filter(year==2022, division == 3)
        if (nrow(screen1) > 1){
          screen2 <- screen1 %>% filter(conference == D3$Conference[i])
          if (nrow(screen2) == 0){
            D3list[[i]] = placeholder_list[[i]]
          } else {
            D3list[[i]] = screen2 %>% slice(1)
          }
        } else if (nrow(screen1) == 0) {
          D3list[[i]] <- placeholder_list[[i]]
        } else{
          D3list[[i]] = screen1
          names(D3list)[i] <- screen1$school
        }
      }
      
  # Fix teams with similar names
    D3list[[91]] = school_id_lu("St. Joseph's") %>% filter(year==2022, division == 3) %>% slice(1)
    D3list[[92]] = school_id_lu("St. Joseph's") %>% filter(year==2022, division == 3) %>% slice(2)
    D3list[[139]] = school_id_lu("Centenary") %>% filter(year==2022, division == 3) %>% slice(2)
    D3list[[143]] = school_id_lu("Eastern") %>% filter(year==2022, division == 3) %>% slice(1)
    D3list[[266]] = school_id_lu("Concordia") %>% filter(year==2022, division == 3) %>% slice(4)
    D3list[[329]] = school_id_lu("Northwestern") %>% filter(year==2022, division == 3) %>% slice(1)
    D3list[[333]] = school_id_lu("Saint Mary's") %>% filter(year==2022, division == 3) %>% slice(1)
    D3list[[350]] = school_id_lu("Concordia") %>% filter(year==2022, division == 3) %>% slice(2)

  # Merge with school info
    D3IDs = cbind(rbindlist(D3list),D3) %>%
      select(School = 1,8,9,10,3,6) %>%
      arrange(School)
    
# Add game results -----------------------------------------------------
  # Clear workspace
    rm(list = setdiff(ls(),"D3IDs"))
    
  # Sideline St. Joseph's (ME), school_id scrape returns weirdly-structured dataframes
    D3IDs2 <- D3IDs[-298,]

  # Scrape schedule information
    D3WList <- list()
    for (i in 1:nrow(D3IDs2)) {
      D3WList[[i]] =  get_ncaa_schedule_info(D3IDs2$school_id[i], 2022)%>% 
        select(date, opponent, result, score) %>%
        filter(result!="NA")
      names(D3WList)[i] <- D3IDs2$School[i]}
  
  # Add wins and losses to D3IDs
    Results <- list()
    for (i in 1:length(D3WList)){
      results1 <- data.frame(D3WList[[i]] %>% group_by(result) %>%  summarise(number = n()))
      ex <- data.frame(t(results1))
      colnames(ex) <- ex[1,]
      row.names(ex) <- NULL
      ex1 <- ex[-1,]
      if (ncol(ex)==2){
        Results[[i]] <- cbind(ex1,data.frame(T=0))
      } else if (ncol(ex)==1) {
        Results[[i]] <- cbind(L=ex1,data.frame(T=0,W=0))
      } else {
        Results[[i]] <- ex1
      }
    }
    names(Results) <- D3IDs2$School
    results_df <- rbindlist(Results, idcol=TRUE,use.names = TRUE)
    colnames(results_df)[1] <- "School"
  
  # Merge with D3IDs2
    D3IDs3 <- merge(D3IDs2,results_df,by="School",all=TRUE)
    
  # Clean and save workspace
    rm(list = setdiff(ls(), c("D3IDs3","D3WList")))
    save.image("~/GitProjects/D3-Baseball-Graphics/Rankings/D3Rankings_v2_p1_Workspace.RData")
