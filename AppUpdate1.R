#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load packages
    library(dplyr)
    library(ggplot2)
    library(shiny)
    library(DT)
    library(ggrepel)
    library(tidyr)
    library(shinycssloaders)
    library(shinythemes)
    library(leaflet)
    library(ICON)
    library(usmap)
    library(mosaic)
    library(data.table)
    
pdf(NULL)

#Import data
    #RPI Rankings
        D3RPITABLE <- read.csv("D3RPITABLEC.csv")
        D3RPITABLE$X <- NULL

    #Batting stats for table/ visualizations
        BatTable <- read.csv("FinalBatAll1920.csv")
        BatTable <- rename(BatTable, "GP-GS" = "GP.GS")
        BatTable <- rename(BatTable, "2B" = "X2B")
        BatTable <- rename(BatTable, "3B" = "X3B")
        BatTable$SLG <- signif(BatTable$SLG, digits = 4)

    #Pitching stats for stat tables
        PitchTable <- read.csv("FinalPitchAll1920.csv")
        PitchTable <- rename(PitchTable, "W-L" = "WL")

    #Import data for visualizations
        #Individual Hitter Performance
            ihperf <- read.csv("ihperf.csv")
        #Team Hitter Performance
            thperf <- read.csv("thperf.csv")
        #Individual Hitter Productivity
            #Data
                ihprod <- read.csv("ihprod.csv")
            #Load in the 25th and 75th percentiles
                Q25 <- read.csv("Q25.csv")
                Q75 <- read.csv("Q75.csv")
        #Team Hitter Productivity
            thprod <- read.csv("thprod.csv")
        #Individual Pitcher Performance
            ipperf <- read.csv("ipperf.csv")
        #Team Pitcher Performance
            tpperf <- read.csv("tpperf.csv")
    
    #Import data for maps
        #Specific Program Locator
            #map3 <- read.csv("map3.csv")

    #Make custom icon for map
        #baseballIcon <- makeIcon(
            #iconUrl = "baseballmarker.png",
            #iconWidth = 25, iconHeight = 25)

# Define UI for application that shows the interactive table
ui <- 

navbarPage("D3 Baseball Graphics", 
               
        tabPanel("Rankings", fluid = TRUE, icon = icon("list ol"),
                        
           fluidPage(theme = "bootstrap.css",
              titlePanel("D3 Baseball 2020 RPI Rankings"),
                  mainPanel(
                      h3("Overview of Rankings"),
                       p("1. The table only includes teams who played at least 10 games in the 2020 season."),
                       p("2. The 'Rating' column represents Ratings Percentage Index (RPI) values."),
                       p("3. The 'SoS' column measures the strength of a team's schedule from the hardest schedule (1) to the easiest (210)."),
                       p("4. For more information about the stats, see the 'More Info' tab."),
                       br(),
                                fluidRow(
                                    column(4,
                                           selectInput("conf",
                                                       "Conference:",
                                                       c("All",
                                                         sort(D3RPITABLE$Conference, decreasing = FALSE)))
                                    ),
                                    column(4,
                                           selectInput("state",
                                                       "State:",
                                                       c("All",
                                                         sort(D3RPITABLE$State, decreasing = FALSE)))
                                    ))),
                            
                            #Create interactive table
                            DT::dataTableOutput("D3RPI")
                        )
               ),
               
               
               tabPanel("Batting Statistics", icon = icon("clipboard"),
                        # Page title
                        fluidPage(
                            titlePanel("D3 Batting Statistics"),
                            
                            #Add conference selection
                            mainPanel(
                                h3("Overview of Batting Statistics"),
                                p("1. This table includes every player with at least one plate appearance during the 2019 or 2020 season."),
                                p("2. The 'RC' column measures Runs Created."),
                                p("3. For more information about the stats see the 'More Info' tab."),
                                br(),
                                fluidRow(
                                    column(4,
                                           selectInput("year", "Select Year:", 
                                                       choices = list("2020" = 2020, "2019" = 2019), 
                                                       selected = 2020)),
                                    column(4,
                                           selectInput("conf1",
                                                       "Conference:",
                                                       c("All",
                                                         sort(BatTable$Conference, decreasing = FALSE)))
                                    ),
                                    column(4,
                                           selectInput("class",
                                                       "Class:",
                                                       c("All",
                                                         sort(BatTable$Class, decreasing = FALSE)))
                                    ),
                                    column(4,
                                           selectInput("pos",
                                                       "Position:",
                                                       c("All",
                                                         sort(BatTable$Pos, decreasing = FALSE)))
                                    ),
                                    column(4,
                                           numericInput("num",
                                                        "Minimum At-Bats:",
                                                        value = "0"),
                                           helpText("Must input a value to view the data")))
                                ),
                            
                            #Create interactive table
                            DT::dataTableOutput("BatTable1")
                        )),
               tabPanel("Pitching Statistics", icon = icon("clipboard"),
                        # Page title
                        fluidPage(
                            titlePanel("D3 Pitching Statistics"),
                            #Add conference selection
                            mainPanel(
                                h3("Overview of Pitching Statistics"),
                                p("1. This table includes every pitcher who faced at least one batter during the 2019 or 2020 season."),
                                p("2. The 'FIP' column measures Fielding Independent Pitching."),
                                p("3. FIP values are only calculated for pitchers who faced at least 25 batters."),
                                p("4. For more information about the stats, see the 'More Info' tab."),
                                br(),
                                fluidRow(
                                    column(4,
                                           selectInput("year1", "Select Year:", 
                                                       choices = list("2020" = 2020, "2019" = 2019), 
                                                       selected = 2020)),
                                    column(4,
                                           selectInput("conf2",
                                                       "Conference:",
                                                       c("All",
                                                         sort(PitchTable$Conference, decreasing = FALSE)))
                                    )),
                                fluidRow(
                                    column(4,
                                           selectInput("class1",
                                                       "Class:",
                                                       c("All",
                                                         sort(PitchTable$Class, decreasing = FALSE)))
                                    ),
                                    column(4,
                                           numericInput("num1",
                                                        "Minimum Innings Pitched:",
                                                        value = "0",
                                           helpText("Must input a value to view the data")))
                                )),
                            
                            #Create interactive table
                            DT::dataTableOutput("PitchTable1")
                        )),
               
               
               navbarMenu("Visualizations", icon = icon("chart-area"),
                          tabPanel("Hitter Performance", fluid = TRUE, icon = icon("chart-line"),
                                   fluidPage(
                                       titlePanel("Hitter Performance"),
                                       tags$hr(style="border-color: gray;"),
                                       mainPanel(
                                           h3("Individual Performance"),
                                           p("1. Only players with at least 25 at bats are represented on the scatterplot."),
                                           p("2. To find a specific player/ team/ conference, make sure the name",
                                             strong("exactly"),
                                             "corresponds to the name on the 'Batting Statistics' table."),
                                           p("3. This is an", strong("interactive plot"), "; click on a point to see the details below the plot."),
                                           br(),
                                           fluidRow(
                                             column(4,
                                                    textInput("playerfind", 
                                                              "Player Finder:",
                                                              placeholder = "Monson, Ryan")),                    
                                             column (4,
                                                     textInput("schoolfind", 
                                                               "School Finder:",
                                                               placeholder = "Wash. & Lee")                    
                                             ),
                                             column (4,
                                                     textInput("conffind", 
                                                               "Conference Finder:",
                                                               placeholder = "USA South"))),
                                           fluidRow(
                                             column(3,
                                                    selectInput("year2", "Select Year:", 
                                                                choices = list("2020" = 2020, "2019" = 2019), 
                                                                selected = 2020)),
                                             column(4,
                                                     selectInput("aafind", 
                                                                 "Show All-Americans",
                                                                 choices = list("None", "1st Team" = 1, "2nd Team" = 2, "3rd Team" = 3, "4th Team" = 4, "Honorable Mention" = 5))),
                                             column(3,
                                                    numericInput("min1",
                                                              "Min At Bats:",
                                                              value = "25", min = "25")),
                                             column(2,
                                                    actionButton("action2", label = "Update"))),
                                           
                                           plotOutput("ihperf1",  click = "clickr4"),
                                           verbatimTextOutput("ihperf1info")), 
                                       
                                       mainPanel(
                                           h3("Team Performance"),
                                           p("1. Only teams with at least 200 plate appearances are represented on the scatterplot."),
                                           p("2. To find a specific team or conference, make sure the name",
                                             strong("exactly"),
                                             "corresponds to the name on the 'Batting Statistics' table."),
                                           p("3. This is an", strong("interactive plot"), "; click on a point to see the details below the plot."),
                                           br(),
                                           fluidRow(
                                               column(4,
                                                      selectInput("year3", "Select Year:", 
                                                                  choices = list("2020" = 2020, "2019" = 2019), 
                                                                  selected = 2020)),
                                               column (4,
                                                       textInput("schoolfind1", 
                                                                 "School Finder:",
                                                                 placeholder = "Chris. Newport")                    
                                               )),
                                           fluidRow(
                                               column (4,
                                                       textInput("conffind1", 
                                                                 "Conference Finder:",
                                                                 placeholder = "SCIAC")),
                                               column(4,
                                                      actionButton("action3", label = "Update Graph"))
                                           ),
                                           plotOutput("thperf1", click = "clickr1"),
                                           verbatimTextOutput("thperf1info")
                                       ))),
                          tabPanel("Hitter Productivity", fluid = TRUE, icon = icon("chart-line"),
                                   fluidPage(
                                       titlePanel("Hitter Productivity"),
                                       tags$hr(style="border-color: gray;"),
                                       mainPanel(
                                           h3("Individual Hitter Productivity"),
                                           p("1. Every player who collected a plate appearance during the 2019 or 2020 is represented on the scatterplot."),
                                           p("2. Productivity is proxied by Runs Created."),
                                           p("3. The bottom blue line represents the 25th percentile of productivity for a specific plate appearance value. The top blue line represents the 75th percentile. Points located between the lines are within the D3 league average."),
                                           p("4. To find a specific player/ team/ conference, make sure the name",
                                             strong("exactly"),
                                             "corresponds to the name on the 'Batting Statistics' table."),
                                           p("5. This is an", strong("interactive plot"), "; click on a point to see the details below the plot."),
                                           br(),
                                           fluidRow(
                                               column(4,
                                                      textInput("playerfind1", 
                                                                "Player Finder:",
                                                                placeholder = "Volpicelli, Jason")),                    
                                               column (4,
                                                       textInput("schoolfind2", 
                                                                 "School Finder:",
                                                                 placeholder = "Montclair St.")                    
                                               ),
                                               column (4,
                                                       textInput("conffind2", 
                                                                 "Conference Finder:",
                                                                 placeholder = "ODAC"))),
                                           fluidRow(
                                               column(4,
                                                      selectInput("year4", "Select Year:", 
                                                                  choices = list("2020" = 2020, "2019" = 2019), 
                                                                  selected = 2020)),
                                               column (4,
                                                       selectInput("aafind1", 
                                                                   "Show D3Baseball.com All-Americans",
                                                                   choices = list("None", "1st Team" = 1, "2nd Team" = 2, "3rd Team" = 3, "4th Team" = 4, "Honorable Mention" = 5))),
                                               column(4,
                                                      actionButton("action4", label = "Update Graph"))),
                                           
                                           plotOutput("ihprod1", click = "clickr5"),
                                       verbatimTextOutput("ihprod1info")),
                                       mainPanel(
                                           h3("Team Run Production Efficiency"),
                                           p("1. Every team that played at least one game is represented on the scatterplot."),
                                           p("2. The blue line represents the expected number of runs scored for a certain number of plate appearances. Teams above the line had better than expected run production."),
                                           p("3. To find a specific team or conference, make sure the name",
                                             strong("exactly"),
                                             "corresponds to the name on the 'Batting Statistics' table."),
                                           p("4. This is an", strong("interactive plot"), "; click on a point to see the details below the plot."),
                                           br(),
                                           fluidRow(
                                               column(4,
                                                      selectInput("year5", "Select Year:", 
                                                                  choices = list("2020" = 2020, "2019" = 2019), 
                                                                  selected = 2020)),
                                               column (4,
                                                       textInput("schoolfind3", 
                                                                 "School Finder:",
                                                                 placeholder = "Haverford")                    
                                               )),
                                           fluidRow(
                                               column (4,
                                                       textInput("conffind3", 
                                                                 "Conference Finder:",
                                                                 placeholder = "Centennial")), 
                                               column(4,
                                                      actionButton("action5", label = "Update Graph"))),
                                           plotOutput("thprod1", click = "clickr2"),
                                           verbatimTextOutput("thprod1info")
                                       ))),
                          tabPanel("Pitcher Performance", fluid = TRUE, icon = icon("chart-line"),
                                   fluidPage(
                                       titlePanel("Pitcher Performance"),
                                       tags$hr(style="border-color: gray;"),
                                       mainPanel(
                                           h3("Individual Pitcher Performance"),
                                           p("1. Only players who pitched at least 10 innings are represented on the scatterplot."),
                                           p("2. To find a specific player/ team/ conference, make sure the name",
                                             strong("exactly"),
                                             "corresponds to the name on the 'Pitching Statistics' table."),
                                           p("3. This is an", strong("interactive plot"), "; click on a point to see the details below the plot."),
                                           
                                           br(),
                                           fluidRow(
                                             column(4,
                                                    textInput("playerfind2", 
                                                              "Player Finder:",
                                                              placeholder = "McKoon, Carson")),                    
                                             column (4,
                                                     textInput("schoolfind4", 
                                                               "School Finder:",
                                                               placeholder = "Chapman")                    
                                             ),
                                             column (4,
                                                     textInput("conffind4", 
                                                               "Conference Finder:",
                                                               placeholder = "SCAC"))),
                                           fluidRow(
                                             column(3,
                                                    selectInput("year6", "Select Year:", 
                                                                choices = list("2020" = 2020, "2019" = 2019), 
                                                                selected = 2020)),
                                             column (4,
                                                     selectInput("aafind2", 
                                                                 "Show D3Baseball.com All-Americans",
                                                                 choices = list("None", "1st Team" = 1, "2nd Team" = 2, "3rd Team" = 3, "4th Team" = 4, "Honorable Mention" = 5))),
                                             column(3,
                                                    numericInput("min2",
                                                                 "Min IP:",
                                                                 value = "10", min = "10")),
                                             column(2,
                                                    actionButton("action6", label = "Update"))),
                                           
                                           plotOutput("ipperf1", click = "clickr6"),
                                           verbatimTextOutput("ipperf1info"))
                                       ),
                                   mainPanel(
                                       h3("Pitching Staff Performance"),
                                       p("1. Every team that played at least five games is represented on the scatterplot."),
                                       p("2. To find a specific team or conference, make sure the name",
                                         strong("exactly"),
                                         "corresponds to the name on the 'Pitching Statistics' table."),
                                       p("3. This is an", strong("interactive plot"), "; click on a point to see the details below the plot."),
                                       br(),
                                       fluidRow(
                                           column(4,
                                                  selectInput("year7", "Select Year:", 
                                                              choices = list("2020" = 2020, "2019" = 2019), 
                                                              selected = 2020)),
                                           column (4,
                                                   textInput("schoolfind5", 
                                                             "School Finder:",
                                                             placeholder = "Ursinus")                    
                                           )),
                                       fluidRow(
                                           column (4,
                                                   textInput("conffind5", 
                                                             "Conference Finder:",
                                                             placeholder = "ASC")),
                                           column(4,
                                                  actionButton("action7", label = "Update Graph"))
                                           ),
                                       plotOutput("tpperf1", click = "clickr3"),
                                       verbatimTextOutput("tpperf1info")
                                   )),
                          tabPanel("Program Locater", fluid = TRUE, icon = icon("globe-americas"),
                                   h3("Where are D3 Baseball Programs Located?", align = "center"),
                                   tags$hr(style="border-color: gray;"),
                                   br(),
                                  div(img(src = "D3SchoolLocation.png", align = "center", height = 485, width = 600), style="text-align: center;"))),
                                   #h3("Find a specific program:", align = "center"),
                                  # br(),
                                   #leafletOutput("mymap"))
        
                         tabPanel("Stat Creator+", fluid = TRUE, icon = icon("portrait"),
                              sidebarLayout(
                                sidebarPanel(
                                  h3("For Hitters", align = "center"),
                                    tags$hr(style="border-color: gray;"),
                                      fluidRow(
                                        column(12,
                                                 textInput("playerfind3", "Player:", value = "Gazin, Jackson"))),
                                      fluidRow(
                                        column(12,
                                               selectInput("year8", "Select Year:", 
                                                             choices = list("2020" = 2020, "2019" = 2019), 
                                                             selected = 2020))),
                                      fluidRow(
                                        column(12,
                                           selectInput("stat1", "Select Stat:", 
                                                             choices = list("RC", "AB", "R", "H", "2B", "3B", "HR", "RBI", "TB", "SLG", "BB", "HBP", "K", "DP", "OBP", "SF", "SH", "CS", "SB", "OPS", "AVG"), 
                                                             selected = "SB"))),
                                      fluidRow(
                                        column(12,
                                            p("Consider setting a lower bound on at bats to filter out players with skewed AVG and OBP."),
                                           numericInput("num2",
                                                              "Minimum At-Bats:",
                                                              value = "0"))),
                                      fluidRow(
                                        column(12,
                                               actionButton("action", label = "Create Stat!")))),
                                mainPanel(h2("Stat Creator for Hitters"),
                                          tags$hr(style="border-color: gray;"),
                                          h3("How it works:"),
                                          p("1. Simply select a player, year and stat that corresponds to the 'Batting Statistics' table."),
                                          p("2. Set the minimum at bat requirement or leave it at 0."),
                                          p("3. Make sure the player has more at bats than the at bat requirement in that year."),
                                          p("4. To understand how it works, see the 'More Info' tab."),
                                          p("5. Check below for the stat that you've created!"),
                                          tags$hr(style="border-color: gray;"),
                                          h4(textOutput("SCMachine"), align = "center"))),
                              tags$hr(style="border-color: gray;"),
                              sidebarLayout(position = c("right"),
                                sidebarPanel(
                                  h3("For Pitchers", align = "center"),
                                    tags$hr(style="border-color: gray;"),
                                  fluidRow(
                                    column(12,
                                           textInput("playerfind4", "Player:", value = "Kellogg, Tom"))),
                                  fluidRow(
                                    column(12,
                                           selectInput("year9", "Select Year:", 
                                                       choices = list("2020" = 2020, "2019" = 2019), 
                                                       selected = 2020))),
                                  fluidRow(
                                    column(12,
                                           selectInput("stat2", "Select Stat:", 
                                                       choices = list("ERA", "WHIP", "SV", "IP", "H", "R", "ER", "BB", "SO", "x2B", "x3B", "HR", "BF", "BAA", "WP", "HBP", "GO", "FO", "IBB", "FIP"), 
                                                       selected = "IP"))),
                                  fluidRow(
                                    column(12,
                                           p("Consider setting a lower bound on at bats to filter out players with skewed ERA and WHIP."),
                                           numericInput("num3",
                                                        "Minimum Innings Pitched:",
                                                        value = "0"))),
                                  fluidRow(
                                    column(12,
                                           actionButton("action1", label = "Create Stat!")))
                                ),
                                mainPanel(h2("Stat Creator for Pitchers"),
                                          tags$hr(style="border-color: gray;"),
                                          h3("How it works:"),
                                          p("1. Simply select a player, year and stat that corresponds to the 'Pitching Statistics' table."),
                                          p("2. Set the minimum innings pitched requirement or leave it at 0."),
                                          p("3. Make sure the player of interest has more innings pitched than the innings pitched requirement in that year."),
                                          p("4. If the stat is FIP, set the minimum innings pitched requirement to 8 because FIP is only calculated for pitchers with more than 25 batters faced."),
                                          p("5. If the stat is ERA or WHIP, set the minimum innings pitched requirement to 1 to filter out pitchers with an infinite WHIP or ERA."),
                                          p("6. To understand how it works, see the 'More Info' tab."),
                                          p("7. Check below for the stat that you've created!"),
                                          tags$hr(style="border-color: gray;"),
                                          h4(textOutput("SCMachine1")))
                              )),
               navbarMenu("More Info", icon = icon("info-circle"),
                          tabPanel("Methodology", fluid = TRUE, icon = icon("square-root-alt"),
                                   fluidPage(
                                       titlePanel(tags$u("More Information about the Stats")),
                                       mainPanel(
                                           h3("Ratings Percentage Index (RPI)"),
                                           p("Winning percentage was the go-to metric for ranking sports teams for a long time despite its many flaws. Most notably, winning percentage fails to account for a team's strength of schedule. For example, winning a game against the Bad News Bears is considered to be the same as beating a top-25 ranked opponent. If rankings were still based upon winning percentage, there would be no incentive for a team to challenge themselves."),
                                           br(),
                                           p("This is where Ratings Percentage Index, or RPI, comes in. RPI provides a more holistic team overview because it accounts for three contributing factors to team quality: winning percentage, opponents' winning percentage, and opponents' opponents' winning percentage. The NCAA baseball formula for RPI is as follows:"),
                                           img(src = "RPI.jpg", align = "center"),
                                           br(),
                                           p("The NCAA's Ratings Percentage Index assigns different weights to home and road victories to account for home field advantage, although I did not make this adjustment in my rankings. My thought process was that D3 schools tend to be clustered together, thus eliminating long distance travel and nullifying the benefit of home field advantage. Additionally, the RPI values from the 2020 D3 baseball season ranged from .356 to .624 and were separated by a thousandth at times. In order to make the RPI values easier to interpret, I scaled them so that the team with the top RPI would have an RPI value of 100."),
                                           tags$hr(style="border-color: gray;"),
                                           h3("Strength of Schedule (SoS)"),
                                           p("Strength of schedule (SoS) quantifies the difficulty or ease of a team's schedule and is an integral part of the RPI calculation. The SoS formula below should look very similar to the aforementioned RPI formula:"),
                                           img(src = "SoS.jpg", align = "center"),
                                           br(),
                                           p("In order to make the SoS values easier to interpret, I ranked the SoS values from the hardest schedule (1) to the easiest schedule (210)."),
                                           tags$hr(style="border-color: gray;"),
                                           h3("Runs Created (RC)"),
                                           p("Runs Created, or RC, is a statistic introduced in the", tags$i("New Bill James Historical Abstract"), "that seeks to quantify the run contribution of an individual batter to their team. There are many variations of RC, of which I chose to use the technical definition:"),
                                           img(src = "RC.jpg", align = "center"),
                                           br(),
                                           p("There are many benefits of using RC as a proxy for hitter productivity, although it is beyond the scope of this description to discuss these in depth."),
                                           tags$hr(style="border-color: gray;"),
                                           h3("Fielding Independent Pitching (FIP)"),
                                           p("Fielding Independent Pitching, or FIP, measures a pitcher's performance by only considering outcomes that are directly in the pitcher's control: home runs, walks, hit by pitches, and strikeouts. Essentially, FIP takes away defensive performance and luck in order to accurately quantify a pitcher's run prevention ability. The formula for FIP is as follows:"),
                                           img(src = "FIP.jpg", align = "center"),
                                           br(),
                                           p("Please note that the accuracy of FIP increases with sample size. I set a relatively low threshold of 25 batters faced to receive a FIP on the 'Pitching Statistics' table, although in a perfect world with more information the bar would be set at a minimum of 100 batters faced. Moreover, as a result of the small sample size and the lack of homeruns (compared to the MLB), FIP favors pitchers with lots of strikeouts and few walks. Please keep this in mind when considering the FIP values."),
                                           tags$hr(style="border-color: gray;"),
                                           h3("Stat Creator+"),
                                           p("The Stat Creator calls the Batting or Pitching Statistic table and allows the user to select a stat (column). The Stat Creator normalizes that data, calculates the z-score for the specified player, and then looks up the value for the cumulative distribution function. The formula looks something like:"),
                                           img(src = "StatCreator.jpg", align = "center"),
                                           br(),
                                           p("This returns a percentile which is multiplied by 100 and displayed back to the user. By constantly normalizing the data, the application affords the user the ability to set a lower bound on at bats or innnings pitched."),
                                           tags$hr(style="border-color: gray;"),
                                           h3("Comments"),
                                           p("This application focuses on statistics and outcomes from the 2019 & 2020 D3 baseball seasons and is meant as a dry run for the 2021 season. The tables and visualizations are coded to automatically update themselves as the 2021 season progresses. If you like what you see, check back next year for up to date rankings and stats!"),
                                           tags$hr(style="border-color: gray;"),
                                           h3("Resources"),
                                           p("All basic player statistics, team win loss records and schedules were collected from", tags$i("ncaa.statistics.com."))
                                       )
                                   )
                          ),
                          tabPanel("About me", fluid = TRUE, icon = icon("address-card"),
                                   fluidPage(
                                       titlePanel("About the Creator"),
                                       tags$hr(style="border-color: gray;"),
                                       mainPanel( 
                                           img(src = "hit10.jpg", height = 500, width = 750),
                                           br(),
                                           tags$hr(style="border-color: gray;"),
                                           p("My name is Brian Wickman and I play at Washington & Lee University. 
                        I made this application because I was frustrated with the lack of empirical rankings at the Division 3 level. 
                        Additionally, I wanted to touch up on some programming skills and play around with some data visualization techniques. 
                        Hopefully y'all enjoy what I've made and get something out of it. 
                        If you have any comments/ questions/ ideas, you can reach me at bwickman5@gmail.com. Please do not copy the figures or rankings without contacting me and receiving permission beforehand.")
                                       )
                                   )
                          )
               ))           
# Define server logic required to show the table

server <- function(input, output) {
    
    output$D3RPI <- DT::renderDataTable(DT::datatable({
        D3RPI <- D3RPITABLE
        if (input$conf != "All") {
            D3RPI <- D3RPI[D3RPI$Conference == input$conf,]}
        if (input$state != "All") {
            D3RPI <- D3RPI[D3RPI$State == input$state,]}
        D3RPI
    }))
    
    output$BatTable1 <- DT::renderDataTable(DT::datatable({
        BatTable1 <- BatTable
            BatTable <- BatTable[BatTable$Year == input$year,]
        if (input$conf1 != "All") {
            BatTable <- BatTable[BatTable$Conference == input$conf1,]}
        if (input$class != "All") {
            BatTable <- BatTable[BatTable$Class == input$class,]}
        if (input$pos != "All") {
            BatTable <- BatTable[BatTable$Pos == input$pos,]}
        if (input$num > "0") {
            BatTable <- BatTable %>% filter(AB >= input$num)}
        BatTable}))
  
    
    output$PitchTable1 <- DT::renderDataTable(DT::datatable({
        PitchTable1 <- PitchTable
            PitchTable <- PitchTable[PitchTable$Year == input$year1,]
        if (input$conf2 != "All") {
            PitchTable <- PitchTable[PitchTable$Conference == input$conf2,]}
        if (input$class1 != "All") {
            PitchTable <- PitchTable[PitchTable$Class == input$class1,]}
        if (input$num1 > "0") {
            PitchTable <- PitchTable %>% filter(IP >= input$num1)}
            PitchTable
    }))
    
    output$ihperf1 <- renderPlot({
        input$action2
        isolate(
        ggplot(ihperf %>% filter(Year == input$year2, AB >= input$min1) , aes(OBP, SLG)) + geom_jitter(alpha = .4) + 
            xlab("On Base Percentage") + ylab("Slugging Percentage") +
            geom_abline(slope = -1, intercept = seq(0.6, 1.4, by = 0.2)) +
            annotate("text", x = rep(.15, 3) , y = c(.53, .73, .93), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
            annotate("text", x = .27 , y = 1, label = "OPS = 1.2") +
            annotate("text", x = .35 , y = 1.12, label = "OPS = 1.4") +
            ggtitle("Scatterplot of OBP and SLG Values for Hitters with 25+ At-bats")  +
            geom_point(data = filter(ihperf %>% filter(Year == input$year2), Player == input$playerfind), size = 5, shape = 16, color = "#FF0000") +
            geom_point(data = filter(ihperf %>% filter(Year == input$year2), School == input$schoolfind), size = 4, shape = 16, color = "#00CCFF") +
            geom_point(data = filter(ihperf %>% filter(Year == input$year2), Conference == input$conffind), size = 3, shape = 16, color = "#FFCC00") +
            geom_point(data = filter(ihperf %>% filter(Year == input$year2), AA == input$aafind), size = 3, shape = 16, color = "#FF3399"))
    })
    
    output$ihperf1info <- renderPrint({nearPoints(ihperf %>% filter(Year == input$year2, AB >= input$min1), input$clickr4, threshold = 10, maxpoints = 5,
                                                  addDist = FALSE)
      })
    
    output$thperf1 <- renderPlot({
        input$action3
        isolate(
        ggplot(thperf %>% filter(Year == input$year3), aes(OBP, SLG)) + geom_jitter(alpha = 0.4) + 
            xlab("On Base Percentage") + ylab("Slugging Percentage") +
            geom_abline(slope = -1, intercept = seq(0.6, 1.0, by = 0.2)) +
            annotate("text", x = rep(.27, 3) , y = c(.37, .57, .77), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
            ggtitle("Scatterplot of OBP and SLG Values for Teams with 200+ At-bats")  +
            geom_point(data = filter(thperf %>% filter(Year == input$year3), School == input$schoolfind1), size = 4, shape = 16, color = "#00CCFF") +
            geom_point(data = filter(thperf %>% filter(Year == input$year3), conference == input$conffind1), size = 3, shape = 16, color = "#FFCC00"))
    })
    
    output$thperf1info <- renderPrint({nearPoints(thperf %>% filter(Year == input$year3), input$clickr1, threshold = 10, maxpoints = 5,
                 addDist = FALSE)
    })
    
    output$ihprod1 <- renderPlot({
      input$action4
      isolate(
        ggplot(ihprod %>% filter(Year == input$year4), aes(PA, RC)) + geom_jitter(alpha = 0.4) + 
            xlab("Plate Appearances") + ylab("Runs Created") +
            geom_smooth(data = Q25) +
            geom_smooth(data = Q75) +
            ggtitle("Scatterplot of Runs Scored against Plate Appearances") +
            geom_point(data = filter(ihprod %>% filter(Year == input$year4), Player == input$playerfind1), size = 5, shape = 16, color = "#FF0000") +
            geom_point(data = filter(ihprod %>% filter(Year == input$year4), School == input$schoolfind2), size = 4, shape = 16, color = "#00CCFF") +
            geom_point(data = filter(ihprod %>% filter(Year == input$year4), Conference == input$conffind2), size = 3, shape = 16, color = "#FFCC00") +
            geom_point(data = filter(ihprod %>% filter(Year == input$year4), AA == input$aafind1), size = 3, shape = 16, color = "#FF3399"))
    })
    
    output$ihprod1info <- renderPrint({nearPoints(ihprod %>% filter(Year == input$year4), input$clickr5, threshold = 10, maxpoints = 5,
                                                  addDist = FALSE)})
      
    output$thprod1 <-renderPlot({
        input$action5
        isolate(
       ggplot(thprod %>% filter(Year == input$year5), aes(PA, R)) + geom_jitter(alpha = 0.4)+
            xlab("Team Plate Appearances") + ylab("Runs Scored") + geom_smooth(method = 'lm') +
            geom_point(data = filter(thprod %>% filter(Year == input$year5), School == input$schoolfind3), size = 4, shape = 16, color = "#00CCFF") +
            geom_point(data = filter(thprod %>% filter(Year == input$year5), conference == input$conffind3), size = 3, shape = 16, color = "#FFCC00") +
        ggtitle("Team Productivity"))
    })

    output$thprod1info <- renderPrint({nearPoints(thprod, input$clickr2, threshold = 10, maxpoints = 5,
                                                  addDist = FALSE)
    })
    
    output$ipperf1 <- renderPlot ({
        input$action6
        isolate(
        ggplot(ipperf %>% filter(Year == input$year6, IP >= input$min2), aes(ERA, WHIP), color = IP) + geom_jitter( alpha = .3) +
            xlim(0,16.5) + ylim(.2,3.10) +
            xlab("Earned Run Average") + ylab("WHIP") +
            geom_point(data = filter(ipperf %>% filter(Year == input$year6, IP >= input$min2), Player == input$playerfind2), size = 5, shape = 16, color = "#FF0000") +
            geom_point(data = filter(ipperf %>% filter(Year == input$year6, IP >= input$min2), School == input$schoolfind4), size = 4, shape = 16, color = "#00CCFF") +
            geom_point(data = filter(ipperf %>% filter(Year == input$year6, IP >= input$min2), Conference == input$conffind4), size = 3, shape = 16, color = "#FFCC00") +
            geom_point(data = filter(ipperf %>% filter(Year == input$year6, IP >= input$min2), AA == input$aafind2), size = 3, shape = 16, color = "#FF3399") +
            ggtitle("Scatterplot of ERA and WHIP values for pitchers with 10+ IP"))
    })
    
    output$ipperf1info <- renderPrint({nearPoints(ipperf %>% filter(Year == input$year6, IP >= input$min2), input$clickr6, threshold = 10, maxpoints = 5,
                                                  addDist = FALSE)})
    
    output$tpperf1 <- renderPlot({
        input$action7
        isolate(
        ggplot(tpperf %>% filter(Year == input$year7), aes(ERA, WHIP), color = IP) + geom_jitter(alpha = .3) +
            xlim(0,12.5) + ylim(.75,2.75) +
            xlab("Earned Run Average") + ylab("WHIP") +
            geom_point(data = filter(tpperf %>% filter(Year == input$year7), School == input$schoolfind5), size = 4, shape = 16, color = "#00CCFF") +
            geom_point(data = filter(tpperf %>% filter(Year == input$year7), conference == input$conffind5), size = 3, shape = 16, color = "#FFCC00") +
            ggtitle("Scatterplot of team ERA and WHIP values for teams with 5+ GP"))
    })
    
    output$tpperf1info <- renderPrint({nearPoints(tpperf %>% filter(Year == input$year7), input$clickr3, threshold = 10, maxpoints = 5,
                                                  addDist = FALSE)
    })
    
    #SCM = StatCreatorMachine for batters
    BatTable1 <- reactive({BatTable %>% filter(Year == input$year8, AB >= input$num2)})
    
    SCM <- reactive({BatTable %>% filter(Year == input$year8, AB >= input$num2) %>%
        transmute(Player, School, stat = zscore(BatTable1()[,input$stat1])) %>%
        filter(Player == input$playerfind3)})
    
    SOI <- reactive({SCM()[1,3]})
    
    percentile <- reactive({round(pnorm(SOI())*100, digits = 2)})
    
    batternumber <- reactive({nrow(BatTable1())})
    
    batnumber <- reactive({round((batternumber()*percentile()/100), digits = 0)})
    
    output$SCMachine <- renderText ({
      input$action
      isolate(paste(input$playerfind3, 
                    " is in the ",
                    percentile(),
                    "th percentile for ",
                    input$stat1,
                    " out of all qualified hitters in ", input$year8, ". This is more than approximately ", 
                    batnumber(), " of the other ", batternumber(), " hitters who met the minimum at bat requirement.", sep = ""))})
    #SCM for pitchers
    PitchTable1 <- reactive({PitchTable %>% filter(Year == input$year9, IP >= input$num3)})
    
    SCM1 <- reactive({PitchTable %>% filter(Year == input$year9, IP >= input$num3) %>%
        transmute(Player, School, stat = zscore(PitchTable1()[,input$stat2])) %>%
        filter(Player == input$playerfind4)})
    
    SOI1 <- reactive({SCM1()[1,3]})
    
    percentile1 <- reactive({round(pnorm(SOI1())*100, digits = 2)})
    
    pitchernumber <- reactive({nrow(PitchTable1())})
    
    pitchnumber <- reactive({round((nrow(PitchTable1())*percentile1()/100), digits = 0)})
    
    output$SCMachine1 <- renderText ({
      input$action1
      isolate(paste(input$playerfind4, 
                    " is in the ",
                    percentile1(),
                    "th percentile for ",
                    input$stat2,
                    " out of all qualfied pitchers in ", input$year9, ". This is more than approximately ", 
                    pitchnumber(), " of the other ", pitchernumber(), " pitchers who met the minimum innings pitched requirement.", sep = ""))})
}    
    # Run the application 
shinyApp(ui = ui, server = server)



