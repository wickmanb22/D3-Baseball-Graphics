---------------
#title: 'D3 RPI Rankings'
---------------
setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings")

#Load packages
library(pacman)
p_load(dplyr, plyr, baseballr, tidyverse, rvest, stringr, readxl, formattable)

# Compiling D3 Database ---------------------------------------------------

#Load excel document with list of D3 teams
#Some names have been edited right on the spreadsheet
D3 <- read_excel("D3.xlsx")
D3$school = str_squish(D3$school)


#replace strings to correctly pull teamIDs
D3$school = str_replace(D3$school,"North Carolina","N.C.")
D3$school = str_replace(D3$school,"Tenn.","TN")
D3$school = str_replace(D3$school,"Birmingham-Southern","Birmingham-So.")
D3$school = str_replace(D3$school,"Christopher","Chris.")
D3$school = str_replace(D3$school,"Eastern Mennonite","East. Mennonite")
D3$school = str_replace(D3$school,"Emory and Henry","Emory & Henry")
#D3$school = str_replace(D3$school,"Va.","VA")
D3$school = str_replace(D3$school,"Southern Virginia","Southern Va.")
D3$school = str_replace(D3$school,"Md.","MD")
#D3$school = str_replace(D3$school,"Pa.","PA")
D3$school = str_replace(D3$school,"Washington and Lee","Wash. & Lee")
D3$school = str_replace(D3$school,"Virginia Wesleyan","Va. Wesleyan")

#Take parantheses out of name, should not be needed anymore
#D3$school[23] = "Marymount"
#D3$school[24] = "Maryville"
#D3$school[43] = "York"

#For Loop to create List of D3 Schools
D3list <- list()
for (i in 1:nrow(D3)) {
  D3list[[i]] = baseballr::school_id_lu(D3$school[i]) %>% filter(year==2020) %>% slice(1)
}

#Check Schools are correct on D3List, adjust if not
D3list[[43]] = school_id_lu("York") %>% filter(year==2020) %>% slice(2)
D3list[[24]] = school_id_lu("Maryville") %>% filter(year==2020) %>% slice(2)
D3list[[38]] = school_id_lu("Mary") %>% filter(year==2020) %>% slice(13)
D3list[[41]] = school_id_lu("Wesley") %>% filter(year==2020) %>% slice(9)
D3list[[53]] = school_id_lu("Cornell") %>% filter(year==2020) %>% slice(2)
D3list[[66]] = school_id_lu("Luther") %>% filter(year==2020) %>% slice(3)
D3list[[69]] = school_id_lu("Monmouth") %>% filter(year==2020) %>% slice(2)
D3list[[72]] = school_id_lu("North Park") %>% filter(year==2020) %>% slice(1)
D3list[[82]] = school_id_lu("Wheaton") %>% filter(year==2020) %>% slice(1)
D3list[[89]] = school_id_lu("Centenary") %>% filter(year==2020) %>% slice(2)
D3list[[116]] = school_id_lu("Montclair") %>% filter(year==2020) %>% slice(1)
D3list[[199]] = school_id_lu("Westminster") %>% filter(year==2020) %>% slice(2)
D3list[[200]] = school_id_lu("Wilmington") %>% filter(year==2020) %>% slice(2)
D3list[[225]] = school_id_lu("North Central") %>% filter(year==2020) %>% slice(2)
D3list[[227]] = school_id_lu("Northwestern") %>% filter(year==2020) %>% slice(4)
D3list[[229]] = school_id_lu("Saint John's") %>% filter(year==2020) %>% slice(1)
D3list[[230]] = school_id_lu("Saint Mary's") %>% filter(year==2020) %>% slice(2)
D3list[[294]] = school_id_lu("St. Joseph") %>% filter(year==2020) %>% slice(4)
D3list[[293]] = school_id_lu("Springfield") %>% filter(year==2020) %>% slice(2)
D3list[[296]] = school_id_lu("Thomas") %>% filter(year==2020) %>% slice(3)
D3list[[297]] = school_id_lu("Trinity") %>% filter(year==2020) %>% slice(1)
D3list[[300]] = school_id_lu("Wesleyan") %>% filter(year==2020) %>% slice(9)
D3list[[304]] = school_id_lu("Wheaton") %>% filter(year==2020) %>% slice(2)
D3list[[305]] = school_id_lu("Williams") %>% filter(year==2020) %>% slice(2)
D3list[[325]] = school_id_lu("CMSV") %>% filter(year==2020) %>% slice(1)
D3list[[334]] = school_id_lu("Joseph") %>% filter(year==2020) %>% slice(3)
D3list[[335]] = school_id_lu("Joseph") %>% filter(year==2020) %>% slice(4)
D3list[[346]] = school_id_lu("Union") %>% filter(year==2020) %>% slice(2)
D3list[[358]] = school_id_lu("Concordia") %>% filter(year==2020) %>% slice(3)
D3list[[372]] = school_id_lu("Pacific") %>% filter(year==2020) %>% slice(5)
D3list[[377]] = school_id_lu("Southwestern") %>% filter(year==2020) %>% slice(2)
D3list[[381]] = school_id_lu("Trinity") %>% filter(year==2020) %>% slice(2)

#place all ids into one data.frame
#Make sure the D3 and D3IDs have same number of rows or some teams have been left out
D3IDs = plyr::ldply(D3list,data.frame)

#CSV file of SchoolIDS
write.csv(D3IDs,"IDs2020D3.csv", row.names = F)


# Recording Wins, Losses, and Ties ----------------------------------------
#Bring the csv fil
IDs2020D3 <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/IDs2020D3.csv")

#Loop for scraping schedules
D3WList <- list()
for (i in 1:nrow(IDs2020D3)) {
  D3WList[[i]] =  get_ncaa_schedule_info(IDs2020D3$school_id[i], 2020)%>% select(result) %>% filter(result!="NA") %>% group_by(result) %>% dplyr::summarise(number=dplyr::n())}

#Move list to dataframe
D3WL <- D3WList %>% reduce(full_join, by = "result")

#Transpose dataframe
D3WL <- t(D3WL)

#Drop first row of W, L
D3WL = D3WL[-1,]

#Rename columns L, W, remove rownames, reorder columns
colnames(D3WL) <- c("L", "W", "T")
rownames(D3WL) <- NULL
D3WL <- D3WL[,c(2,1,3)]

#Replace na's with 0
D3WL[is.na(D3WL)] <- 0

#Send all school names to a vector
D3school <- IDs2020D3$school

#Add this vector as a column
school <- c(D3school)
D3WLschool <- data.frame(school, D3WL)
rownames(D3WLschool) <- NULL

#Merge with D3IDs df
D3Records <- merge(IDs2020D3, D3WLschool, by = 'school')

#Write to CSV for next step
write.csv(D3Records,"D3Records.csv", row.names = F)

# Strength of Schedule Compilation ----------------------------------------

#Bring in OD3Records with WLT
D3Records <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3Records.csv")

#Loop for scraping schedules
D3List <- list()
for (i in 1:nrow(D3Records)) {
  D3List[[i]] =  get_ncaa_schedule_info(D3Records$school_id[i], 2020)%>% 
    filter(result!="NA") %>% select(opponent) %>% 
    group_by(opponent) %>% 
    dplyr::summarise(number=dplyr::n()) %>% 
    mutate(school_id = D3Records$school_id[i])}

#Move back to dataframe ASAP
D3SOS1 <- do.call("rbind", D3List)

#Clean dataframe, same commands as before
#Check dataframe before continuing to make no more weird names have been added
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Port Charlotte, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myers, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Auburndale, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Atlanta, GA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Rome, GA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Montgomery, AL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Myrtle Beach, SC") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Pierce, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@San Antonio, TX") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Grand Prairie, TX")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Tucson, AZ") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@High Point, NC")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Austin, TX")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Houston, TX") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Tomball, Texas")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ferrum, VA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Lexington, SC")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Myers, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Newport News, VA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hillsboro, OR")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ridgefield, WA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@St. Mary`s City, MD")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Milwaukie, OR") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Winter Haven, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Orlando, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fayetteville, NC")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Baltimore, MD") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Pierce, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Elizabeth City, NC") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Langhorne, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@St. Mary's City, Md.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Winter Haven, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Leesburg, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jackson, MS")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Topeka, Kan.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Topeka, KS") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jacksonville, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Memphis, TN") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jackson, Miss.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hackettstown, N.J.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Altamonte Springs, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Lakeland, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Saint Leo, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@St.Leo, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Auburndale, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Decatur, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Vero Beach, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Aston, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myers, Fla.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Westfield, Ind.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Punta Gorda, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jacksonville, Ill.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northborough, Mass.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Aberdeen, MD") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Auburndale, Fla")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myers, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Berea, OH") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Westfield, IN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Epharta, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northorough, MA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ephrata, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Washington, DC") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Sauget, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@West Windsor, NJ") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Westfield, Ind.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Carbondale, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northboro, MA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Louisville, KY")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Kissimmee, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Danville, KY")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Myrtle Beach, S.C.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Lauderdale, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myera, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Beckley, WV")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Union, NJ")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Lake Myrtle, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmitsburg, Md.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Pierce, Fla")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Yaphank, NY") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Minneapolis, MN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ashland, VA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Longwood, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Danvers, MA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Kissimmee, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Minneapolis, MN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northborough, MA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Clayton, MO") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmitsburg, MD")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Mont Alto, Pa.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Sanford, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmittsburg, Md.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Greenville, IL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hartford, CT")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Pierce, Fl.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ambler, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Port Charlotte, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Minneapolis, Minn.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@New Market, VA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Pierce, Fl.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmittsburg, Md.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Indianapolis, IN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hartford, Conn.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Port Charlotte, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Joliet, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Greenville, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Tuscon, Ariz.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Norothboro, MA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Flemington, NJ")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Norothboro, MA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@") 
D3SOS1$opponent = str_squish(D3SOS1$opponent)

#Merge with ODACIDs by opponent
#OProfile <- read_excel("OpponentProfile.xlsx")
D3OWLT <- merge(OProfile, D3SOS1, by = 'opponent') %>% arrange(desc(school_id))

#Make new column that shows nWinPCT so we can add it up and divide by number of games played
D3OWLT1 <- D3OWLT %>% mutate(NWinPCT = WinPCT * number)

#Group by of all group bys and prepare to merge
D3OppSOS <- D3OWLT1 %>% group_by(school_id) %>%
  summarise(TN = sum(number), TNWP = sum (NWinPCT)) %>%
  transmute(school_id, SOS = TNWP/TN) 

#Merge with D3Records
D3SOS <- merge(D3Records, D3OppSOS, by = 'school_id')

#Rename school column for next step
names(D3SOS)[names(D3SOS)=="school"] <- "opponent"

#Delete first column too
write.csv(D3SOS,"D3SOS.csv")


# Opponents Strength of Schedule ------------------------------------------

#Merge with D3SOS to keep building on it
D3OWLTSOS <- merge(OProfile, D3SOS1, by = 'opponent')

#Scrape Schedule of ODAC Opponents
setTimeLimit(0)
D3OList <- list()
for (i in 1:nrow(D3OWLTSOS)) {
  D3OList[[i]] =  get_ncaa_schedule_info(D3OWLTSOS$opponent_id[i], 2020)%>% 
    filter(result!="NA") %>% select(opponent) %>% 
    group_by(opponent) %>% 
    dplyr::summarise(number=dplyr::n()) %>% 
    mutate(school_id = D3OWLTSOS$school_id[i])
}
#Move back to dataframe ASAP
D3SOS1 <- do.call("rbind", D3OList)

#Clean dataframe, same commands as before
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Port Charlotte, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myers, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Auburndale, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Atlanta, GA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Rome, GA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Montgomery, AL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Myrtle Beach, SC") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Pierce, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@San Antonio, TX") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Grand Prairie, TX")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Tucson, AZ") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@High Point, NC")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Austin, TX")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Houston, TX") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Tomball, Texas")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ferrum, VA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Lexington, SC")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Myers, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Newport News, VA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hillsboro, OR")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ridgefield, WA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@St. Mary`s City, MD")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Milwaukie, OR") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Winter Haven, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Orlando, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fayetteville, NC")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Baltimore, MD") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Pierce, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Elizabeth City, NC") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Langhorne, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@St. Mary's City, Md.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Winter Haven, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Leesburg, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jackson, MS")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Topeka, Kan.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Topeka, KS") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jacksonville, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Memphis, TN") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jackson, Miss.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hackettstown, N.J.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Altamonte Springs, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Lakeland, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Saint Leo, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@St.Leo, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Auburndale, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Decatur, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Vero Beach, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Aston, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myers, Fla.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Westfield, Ind.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Punta Gorda, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Jacksonville, Ill.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northborough, Mass.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Aberdeen, MD") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Auburndale, Fla")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myers, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Berea, OH") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Westfield, IN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Epharta, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northorough, MA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ephrata, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Washington, DC") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Sauget, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@West Windsor, NJ") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Westfield, Ind.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Carbondale, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northboro, MA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Louisville, KY")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Kissimmee, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Danville, KY")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Myrtle Beach, S.C.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Lauderdale, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Myera, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Beckley, WV")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Union, NJ")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Lake Myrtle, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmitsburg, Md.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ft. Pierce, Fla")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Yaphank, NY") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Minneapolis, MN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ashland, VA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Longwood, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Danvers, MA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Kissimmee, FL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Minneapolis, MN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Northborough, MA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Clayton, MO") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmitsburg, MD")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Mont Alto, Pa.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Sanford, FL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmittsburg, Md.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Greenville, IL") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hartford, CT")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Pierce, Fl.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Ambler, PA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Port Charlotte, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Minneapolis, Minn.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@New Market, VA")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Fort Pierce, Fl.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Emmittsburg, Md.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Indianapolis, IN")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Hartford, Conn.") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Port Charlotte, Fla.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Joliet, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Greenville, IL")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Tuscon, Ariz.")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Norothboro, MA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Flemington, NJ")
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@Norothboro, MA") 
D3SOS1$opponent <- str_remove(D3SOS1$opponent, "@") 
D3SOS1$opponent = str_squish(D3SOS1$opponent)

##Merge with D3WLT
OProfile <- read_excel("OpponentProfile.xlsx")
D3OWLT1 <- merge(OProfile, D3SOS1, by = 'opponent')

#Make new column that shows nW and n(GP)
D3OWLT2 <- D3OWLT1 %>% mutate(NWinPCT = WinPCT * number)


#Group by and prepare to merge
D3OSOS <- D3OWLT2 %>% group_by(school_id) %>%
  summarise(TN = sum(number), TNWP = sum (NWinPCT)) %>%
  transmute(school_id, OSOS = TNWP/TN)

#Merge with ODACIDs
D3SOS <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3SOS.csv")
D3Rankings <- merge(D3SOS, D3OSOS, by = 'school_id')
D3Rankings = D3Rankings %>% mutate(GP=W+L+T, WinPCT = W/GP, RPI = .25 * WinPCT+ .5 * SOS+.25 * OSOS) %>% 
  filter(GP >= 10) %>%
  arrange(desc(RPI))

#Add state and enrollment dataframe
StatesEnrollment <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/StatesEnrollment.csv")
D3Rankings$X.1 <- NULL
D3Rankings$X <- NULL
D3Rankings <- left_join(D3Rankings, StatesEnrollment, by = 'opponent')

#Create new SOS+WinPCT variable
#Delete first column too
write.csv(D3Rankings,"D3Rankings.csv")


# Clean RPI Table for Presentation --------------------------------------------

#Read in D3Rankings CSV
D3Rankings <- read.csv("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3Rankings.csv")

#Drop columns
D3RPI <- D3Rankings %>% select(opponent, conference, W, L, T, SOS, RPI, State) %>% arrange(desc(RPI))

#Add missing teams before ranking
#Add Missing teams (school name, conference, W, L, T, SOS, RPI)
CSE <- c("St. Elizabeth","CSAC",2,10,0,0.5247741,0.43090084, "NJ")
D3RPI <- rbind(D3RPI,  CSE)

#Create RPI rank column
D3RPI <- D3RPI %>% arrange(desc(RPI))
D3RPI$Rank = 1:nrow(D3RPI)

#Arrange by SOS and create SOS column
D3RPI <- D3RPI %>% arrange(desc(SOS))
D3RPI$SoS = 1:nrow(D3RPI)

#Combine W,L,T, makes it very hard to sort
#D3RPI <- unite(D3RPI, "Record", c("W","L","T"), sep = "-", remove = TRUE)
#D3RPI2 <- D3RPI$Record %>% str_remove("-0")
#D3RPI$Record = D3RPI2

#Reorder Columns to Satisfaction, get rid of others
D3RPI <- D3RPI[,c(9,1,8,2,3,4,5,7,10)] %>% arrange(desc(RPI))
x <- 100/as.numeric(D3RPI$RPI[1])
D3RPI <- D3RPI %>% mutate(RPI = as.numeric(RPI) *as.numeric(x))
D3RPI$RPI <- round(D3RPI$RPI,digits=2)
D3RPI <- D3RPI[,c(2,3,4,5,6,7,8,9)]
names(D3RPI)[names(D3RPI)=="RPI"] <- "Rating"
names(D3RPI)[names(D3RPI)=="opponent"] <- "School"

#Replace Conference Initials with names
D3RPI$conference <- str_replace(D3RPI$conference,"ODAC", "Old Dominion Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"SCIAC", "Southern Calif. Intercollegiate Athletic Conf.")
D3RPI$conference <- str_replace(D3RPI$conference,"SAA", "Southern Athletic Association")
D3RPI$conference <- str_replace(D3RPI$conference,"NJAC", "New Jersey Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"NECC", "New England Collegiate Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"UAA", "University Athletic Association")
D3RPI$conference <- str_replace(D3RPI$conference,"Centennial", "Centennial Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"CCIW", "College Conference of Illinois and Wisconsin")
D3RPI$conference <- str_replace(D3RPI$conference,"HCAC", "Heartland Collegiate Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"NCAC", "North Coast Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"SUNYAC", "State University of New York Athletic Conf.")
D3RPI$conference <- str_replace(D3RPI$conference,"Landmark", "Landmark Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"SCAC", "Southern Collegiate Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"OAC", "Ohio Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"USA South", "USA South Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"CSAC", "Colonial States Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"MAC Commonwealth", "Middle Atlantic Conference Commonwealth")
D3RPI$conference <- str_replace(D3RPI$conference,"WIAC", "Wisconsin Intercollegiate Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"North Atlantic", "North Atlantic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"CUNYAC", "City University of New York Athletic Conf.")
D3RPI$conference <- str_replace(D3RPI$conference,"Little East", "Little East Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"UMAC", "Upper Midwest Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"SLIAC", "St. Louis Intercollegiate Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"NWC", "Northwest Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"ASC", "American Southwest Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"PAC", "Presidents' Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"AMCC", "Allegheny Mountain Collegiate Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"Atlantic East", "Atlantic East Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"Skyline", "Skyline Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"Great Northeast", "Great Northeast Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"NEWMAC", "New Eng. Women's and Men's Athletic Conf.")
D3RPI$conference <- str_replace(D3RPI$conference,"NEAC", "North Eastern Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"MIAC", "Minnesota Intercollegiate Athletic Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"American Rivers", "American Rivers Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"CCC", "Commonwealth Coast Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"MAC Freedom", "Middle Atlantic Conference Freedom")
D3RPI$conference <- str_replace(D3RPI$conference,"NACC", "Northern Athletics Collegiate Conference")
D3RPI$conference <- str_replace(D3RPI$conference,"Michigan Intercol. Ath. Assn.", "Michigan Intercollegiate Athletic Association")
D3RPI$conference <- str_replace(D3RPI$conference,"CAC", "Capital Athletic Conference")

#Rename conference variable
names(D3RPI)[names(D3RPI)=="conference"] <- "Conference"
D3RPI <- D3RPI %>% arrange(desc(Rating)) 

#Format the table nicely
#D3RPI1<- formattable(D3RPI, list(
#  SoS = color_tile("red", "green", 0.2)
#))
#D3RPI1

write.csv(D3RPI, "D3RPITABLEC.csv")

