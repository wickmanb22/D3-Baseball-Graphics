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


#Load data
#setwd("~/R/Baseball Analysis/Intro Baseball Analysis/FullD3Rankings/D3BG")



pdf(NULL)

D3RPITABLE <- read.csv("D3RPITABLEC.csv")
D3RPITABLE$X <- NULL
D3Bat20 <- read.csv("PosFinalD3Bat20.csv")
D3Bat20 <- rename(D3Bat20, "School" = "school")
D3Bat20 <- rename(D3Bat20, "Conference" = "conference")
D3Bat20 <- rename(D3Bat20, "GP-GS" = "GP.GS")
D3Bat20 <- rename(D3Bat20, "2B" = "X2B")
D3Bat20 <- rename(D3Bat20, "3B" = "X3B")
D3Bat20$SLG <- signif(D3Bat20$SLG, digits = 4)
D3Bat20$X <- NULL


D3Pitch20 <- read.csv("FipPitchFinal.csv")
D3Pitch20 <- rename(D3Pitch20, "W-L" = "WL")
D3Pitch20$X <- NULL

D3OPS <- read.csv("D3OPS.csv")
D3OPS$X <- NULL

D3RC <- read.csv("D3RC.csv")
D3RC$X <- NULL

Q25 = aggregate(D3RC$RC, 
                list(D3RC$PA), quantile, 0.25)
Q75 = aggregate(D3RC$RC, 
                list(D3RC$PA), quantile, 0.75)

#Edit data to rename columsn to PA, RC
Q25 <- Q25 %>% rename(PA = Group.1, RC = x)
Q75 <- Q75 %>% rename(PA = Group.1, RC = x)

IP10 <- read.csv("IP10.csv")
IP10$X <- NULL

map3 <- read.csv("map3.csv")

usdf <- read.csv("usdf.csv")
usdf$X <- NULL

uswpct <- read.csv("uswpct.csv")
uswpct$X <- NULL

#Team Hitting Graphics 
TeamHit1 <- read.csv("TeamHit1.csv")
TeamHit1$X <- NULL
TeamHit2 <- TeamHit1 %>% filter(PA >= 200)

#Team Pitch
TeamPitch2 <- read.csv("TeamPitch.csv")
TeamPitch2$X <- NULL

#Make custom icon for map
baseballIcon <- makeIcon(
  iconUrl = "baseballmarker.png",
  iconWidth = 25, iconHeight = 25
)


# Define UI for application that shows the interactive table
ui <- 
    #Structure NavBar

  navbarPage("D3 Baseball Graphics", theme = shinytheme("lumen"),
        
        tabPanel("Rankings", fluid = TRUE, icon = icon("list ol"),
    
        fluidPage(
    
    # Page title
        titlePanel("D3 Baseball 2020 RPI Rankings"),
    
    #Add conference selection
    
        mainPanel(
          h3("Overview of Rankings"),
            p("1. The table only includes teams who played at least 10 games in the 2020 season"),
            p("2. The 'Rating' column represents Ratings Percentage Index (RPI) values"),
            p("3. The 'SoS' column measures the strength of a team's schedule from the hardest schedule (1) to the easiest (210)"),
            p("4. For more information about the stats see the 'More Info' tab"),
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
      p("1. This table includes every player with at least one plate appearance during the 2020 season"),
      p("2. The 'RC' column measures Runs Created"),
      p("3. For more information about the stats see the 'More Info' tab"),
      br(),
      fluidRow(
          column(4,
             selectInput("conf2",
                         "Conference:",
                         c("All",
                           sort(D3Bat20$Conference, decreasing = FALSE)))
          ),
        column(4,
             selectInput("year",
                         "Class:",
                         c("All",
                           sort(D3Bat20$Yr, decreasing = FALSE)))
          ),
        column(4,
               selectInput("pos",
                           "Position:",
                           c("All",
                             sort(D3Bat20$Pos, decreasing = FALSE)))
        
        ),
        column(4,
               numericInput("num2",
                            "Minimum At-Bats:",
                            value = "0"),
              helpText("Must input a value to view the data"))
              
          )),
    
    #Create interactive table
    DT::dataTableOutput("D3Bat20")
    )),
    
    tabPanel("Pitching Statistics", icon = icon("clipboard"),
             # Page title
        fluidPage(
          titlePanel("D3 Pitching Statistics"),
               
      #Add conference selection
      mainPanel(
        h3("Overview of Pitching Statistics"),
        p("1. This table includes every pitcher who faced at least one batter during the 2020 season"),
        p("2. The 'FIP' column measures Fielding Independent Pitching"),
        p("3. FIP values are only calculated for pitchers who faced at least 25 batters"),
        p("4. For more information about the stats see the 'More Info' tab"),
        br(),
        fluidRow(
              column(4,
                selectInput("conf3",
                            "Conference:",
                            c("All",
                              sort(D3Pitch20$conference, decreasing = FALSE)))
           ),
              column(4,
                selectInput("year1",
                            "Class:",
                           c("All",
                             sort(D3Pitch20$Yr, decreasing = FALSE)))
            ),
           column(4,
                  numericInput("num1",
                              "Minimum Innings Pitched:",
                              value = "0"),
                  helpText("Must input a value to view the data"))
           )),
               
     #Create interactive table
     DT::dataTableOutput("D3Pitch20")
         )),
    
      navbarMenu("Visualizations", icon = icon("chart-area"),
      tabPanel("Hitter Performance", fluid = TRUE, icon = icon("chart-line"),
             fluidPage(
              titlePanel("Hitter Performance"),
              mainPanel(
                h3("Individual Performance"),
                p("1. Only players with at least 25 at bats are represented on the scatterplot"),
                p("2. To find a specific player/ team/ conference, make sure the name",
                strong("exactly"),
                "corresponds to the name on the 'Batting Statistics' table"),
                br(),
               fluidRow(
                 column (4,
                  textInput("playerfind", 
                            "Player Finder:",
                            placeholder = "Burnette, Bryce")                    
                         ),
               
                  column (4,
                       textInput("schoolfind", 
                                 "School Finder:",
                                 placeholder = "Wash. & Lee")                    
               ),
                column (4,
                       textInput("conffind", 
                                 "Conference Finder:",
                                 placeholder = "ODAC")                
               
               )),

                 plotOutput("OPSOBP", width = "100%")), 
             mainPanel(
               h3("Team Performance"),
               p("1. Only teams with at least 200 plate appearances are represented on the scatterplot"),
               p("2. To find a specific team or conference, make sure the name",
                 strong("exactly"),
                 "corresponds to the name on the 'Batting Statistics' table"),
               br(),
               fluidRow(
                 column (4,
                         textInput("schoolfind3", 
                                   "School Finder:",
                                   placeholder = "Chris. Newport")                    
                 ),
                 column (4,
                         textInput("conffind3", 
                                   "Conference Finder:",
                                   placeholder = "SCIAC")                
                         
                 )),
               plotOutput("TeamHit")
               
             ))),
    
    tabPanel("Hitter Productivity", fluid = TRUE, icon = icon("chart-line"),
             fluidPage(
               titlePanel("Hitter Productivity"),
               mainPanel(
                 h3("Individual Hitter Productivity"),
                 p("1. Every player who collected a plate appearance during the 2020 is represented on the scatterplot"),
                 p("2. Productivity is proxied by Runs Created"),
                 p("3. The bottom blue line represents the 25th percentile of productivity for a specific plate appearance value. The top blue line represents the 75th percentile. Points located between the lines are within the D3 league average."),
                 p("4. To find a specific player/ team/ conference, make sure the name",
                   strong("exactly"),
                   "corresponds to the name on the 'Batting Statistics' table"),
                 br(),
               fluidRow(
                 column (4,
                 textInput("playerfind1", 
                           "Player Finder:",
                           placeholder = "Monson, Ryan")                    
               ),
               
               column (4,
                       textInput("schoolfind1", 
                                 "School Finder:",
                                 placeholder = "Montclair St.")                    
               ),
               column (4,
                       textInput("conffind1", 
                                 "Conference Finder:",
                                 placeholder = "USA South")                
                       
               )),
               
                  plotOutput("PARC")),
             mainPanel(
               h3("Team Run Production Efficiency"),
               p("1. All teams that played at least one game during the 2020 season are represented on the scatterplot"),
               p("2. The blue line represents the expected number of runs scored for a certain number of plate appearances. Teams above the line had better than expected run production."),
               p("3. To find a specific team or conference, make sure the name",
                 strong("exactly"),
                 "corresponds to the name on the 'Batting Statistics' table"),
               br(),
               fluidRow(
                 column (4,
                         textInput("schoolfind4", 
                                   "School Finder:",
                                   placeholder = "Haverford")                    
                 ),
                 column (4,
                         textInput("conffind4", 
                                   "Conference Finder:",
                                   placeholder = "Centennial")                
                         
                 )),
               plotOutput("Protiv")
               
             ))),
    tabPanel("Pitcher Performance", fluid = TRUE, icon = icon("chart-line"),
             fluidPage(
               titlePanel("Pitcher Performance"),
               mainPanel(
                 h3("Individual Pitcher Performance"),
                 p("1. Only players who pitched at least 10 innings are represented on the scatterplot"),
                 p("2. To find a specific player/ team/ conference, make sure the name",
                   strong("exactly"),
                   "corresponds to the name on the 'Pitching Statistics' table"),
                 br(),
               fluidRow(
                 column (4,
                         textInput("playerfind2", 
                                   "Player Finder:",
                                   placeholder = "McKoon, Carson")                    
                 ),
                 
                 column (4,
                         textInput("schoolfind2", 
                                   "School Finder:",
                                   placeholder = "Wash. & Lee")                    
                 ),
                 column (4,
                         textInput("conffind2", 
                                   "Conference Finder:",
                                   placeholder = "ODAC")                
                         
                 )),
               
               plotOutput("EWHIP"))),
             mainPanel(
               h3("Pitching Staff Performance"),
               p("1. All teams that played at least five game are represented on the scatterplot"),
               p("2. To find a specific team or conference, make sure the name",
                 strong("exactly"),
                 "corresponds to the name on the 'Pitching Statistics' table"),
               br(),
               fluidRow(
                 column (4,
                         textInput("schoolfind5", 
                                   "School Finder:",
                                   placeholder = "Ursinus")                    
                 ),
                 column (4,
                         textInput("conffind5", 
                                   "Conference Finder:",
                                   placeholder = "ASC")                
                         
                 )),
               plotOutput("TEWHIP")
             )),
    tabPanel("Program Locater", fluid = TRUE, icon = icon("globe-americas"),
               h3("Where are D3 Baseball Programs Located?", align = "center"),
                br(),
               div(img(src = "D3SchoolLocation.png", align = "center", height = 485, width = 600), style="text-align: center;"),
               h3("Find a specific program:", align = "center"),
               br(),
               leafletOutput("mymap"))
      ),
    
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
                   h3("Strength of Schedule (SoS)"),
                    p("Strength of schedule (SoS) quantifies the difficulty or ease of a team's schedule and is an integral part of the RPI calculation. The SoS formula below should look very similar to the aforementioned RPI formula:"),
                   img(src = "SoS.jpg", align = "center"),
                    br(),
                    p("In order to make the SoS values easier to interpret, I ranked the SoS values from the hardest schedule (1) to the easiest schedule (210)."),
                   h3("Runs Created (RC)"),
                    p("Runs Created, or RC, is a statistic introduced in the", tags$i("New Bill James Historical Abstract"), "that seeks to quantify the run contribution of an individual batter to their team. There are many variations of RC, of which I chose to use the technical definition:"),
                   img(src = "RC.jpg", align = "center"),
                    br(),
                    p("There are many benefits of using RC as a proxy for hitter productivity, although it is beyond the scope of this description to discuss these in depth."),
                   h3("Fielding Independent Pitching (FIP)"),
                    p("Fielding Independent Pitching, or FIP, measures a pitcher's performance by only considering outcomes that are directly in the pitcher's control: home runs, walks, hit by pitches, and strikeouts. Essentially, FIP takes away defensive performance and luck in order to accurately quantify a pitcher's run prevention ability. The formula for FIP is as follows:"),
                   img(src = "FIP.jpg", align = "center"),
                    br(),
                    p("Please note that the accuracy of FIP increases with sample size. I set a relatively low threshold of 25 batters faced to receive a FIP on the 'Pitching Statistics' table, although in a perfect world with more information the bar would be set at a minimum of 100 batters faced. Moreover, as a result of the small sample size and the lack of homeruns (compared to the MLB), FIP favors pitchers with lots of strikeouts and few walks. Please keep this in mind when considering the FIP values."),
                   h3("Comments"),
                    p("This application focuses on statistics and outcomes from the 2020 D3 baseball season and is meant as a dry run for the 2021 season. The tables and visualizations are coded to automatically update themselves as the 2021 season progresses. If you like what you see, check back next year for up to date rankings and stats!"),
                   h3("Resources"),
                    p("All basic player statistics, team win loss records and schedules were collected from", tags$i("ncaa.statistics.com."))
                 )
               )
               ),
      tabPanel("About me", fluid = TRUE, icon = icon("address-card"),
               fluidPage(
                 titlePanel("About the Creator"),
                 mainPanel( 
                   img(src = "hit10.jpg", height = 500, width = 750),
                   br(),
                   br(),
                   p("My name is Brian Wickman and I play at Washington & Lee University. 
                        I made this application because I was frustrated with the lack of empirical rankings at the Division 3 level. 
                        Additionally, I wanted to touch up on some programming skills and play around with some data visualization techniques. 
                        Hopefully y'all enjoy what I've made and get something out of it. 
                        If you have any comments/ questions/ ideas you can reach me at bwickman5@gmail.com. Please do not copy the figures or rankings exactly without contacting me and receiving permission beforehand.")
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
    
    output$D3Bat20 <- DT::renderDataTable(DT::datatable({
      D3Bat20 <- D3Bat20
      if (input$conf2 != "All") {
        D3Bat20 <- D3Bat20[D3Bat20$Conference == input$conf2,]}
      if (input$year != "All") {
        D3Bat20 <- D3Bat20[D3Bat20$Yr == input$year,]}
      if (input$pos != "All") {
        D3Bat20 <- D3Bat20[D3Bat20$Pos == input$pos,]}
      if (input$num2 > "0") {
        D3Bat20 <- D3Bat20 %>% filter(AB >= input$num2)}
      D3Bat20
    }))
    
    output$D3Pitch20 <- DT::renderDataTable(DT::datatable({
      D3Pitch20 <- D3Pitch20
      if (input$conf3 != "All") {
        D3Pitch20 <- D3Pitch20[D3Pitch20$conference == input$conf3,]}
      if (input$year1 != "All") {
        D3Pitch20 <- D3Pitch20[D3Pitch20$Yr == input$year1,]}
      if (input$num1 > "0") {
        D3Pitch20 <- D3Pitch20 %>% filter(IP >= input$num1)}
      D3Pitch20
    }))
    
    output$OPSOBP <- renderPlot({
      ggplot(D3OPS, aes(OBP, SLG)) + geom_point(alpha = .4) + 
        xlab("On Base Percentage") + ylab("Slugging Percentage") +
        geom_abline(slope = -1, intercept = seq(0.6, 1.4, by = 0.2)) +
        annotate("text", x = rep(.15, 3) , y = c(.53, .73, .93), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
        annotate("text", x = .27 , y = 1, label = "OPS = 1.2") +
        annotate("text", x = .35 , y = 1.12, label = "OPS = 1.4") +
        geom_text_repel(data = filter(D3OPS, OBP > 0.7 | SLG > 1.05), aes(OBP, SLG, label = Player)) +
        ggtitle("Scatterplot of OBP and SLG Values for hitters with 25+ ABs")  +
        geom_point(data = filter(D3OPS, Player == input$playerfind), size = 5, shape = 16, color = "#FF0000") +
        geom_point(data = filter(D3OPS, school == input$schoolfind), size = 4, shape = 16, color = "#00CCFF") +
        geom_point(data = filter(D3OPS, conference == input$conffind), size = 3, shape = 16, color = "#FFCC00")
    })
    
    output$TeamHit <- renderPlot({
      ggplot(TeamHit2, aes(OBP, SLG)) + geom_point(alpha = .4) + 
        xlab("On Base Percentage") + ylab("Slugging Percentage") +
        xlim(0.25,0.5) + ylim(0.2,0.8) +
        geom_abline(slope = -1, intercept = seq(0.6, 1.0, by = 0.2)) +
        annotate("text", x = rep(.27, 3) , y = c(.37, .57, .77), label = paste("OPS =", c(0.6, 0.8 , 1.0))) +
        #label the top data points
        geom_text_repel(data = filter(TeamHit2, OBP > 0.475 & SLG > .650), aes(OBP, SLG, label = school)) +
        #Title the graph
        ggtitle("Scatterplot of OBP and SLG Values for teams with 200+ ABs")  +
        geom_point(data = filter(TeamHit2, school == input$schoolfind3), size = 4, shape = 16, color = "#00CCFF") +
        geom_point(data = filter(TeamHit2, conference == input$conffind3), size = 3, shape = 16, color = "#FFCC00")
    })
    
    output$PARC <- renderPlot({
        ggplot(D3RC, aes(PA, RC)) + geom_jitter(alpha = .4, width = 1) + 
        xlab("Plate Appearances") + ylab("Runs Created") +
        geom_smooth(data = Q25) +
        geom_smooth(data = Q75) +
        geom_text_repel(data = filter(D3RC, RC >= 30), aes(PA, RC, label = Player)) +
        ggtitle("Scatterplot of Runs Created against Plate Appearances for hitters with 25+ ABs") +
        geom_point(data = filter(D3RC, Player == input$playerfind1), size = 5, shape = 16, color = "#FF0000") +
        geom_point(data = filter(D3RC, school == input$schoolfind1), size = 4, shape = 16, color = "#00CCFF") +
        geom_point(data = filter(D3RC, conference == input$conffind1), size = 3, shape = 16, color = "#FFCC00")
    })
    
    output$Protiv <-renderPlot({
      ggplot(TeamHit1, aes(PA, R1)) + geom_point()+
        xlab("Team Plate Appearances") + ylab("Runs Scored") + geom_smooth(method = 'lm') +
        geom_text_repel(data = filter(TeamHit1, R1 > 155), aes(PA, R1, label = school)) +
        geom_point(data = filter(TeamHit1, school == input$schoolfind4), size = 4, shape = 16, color = "#00CCFF") +
        geom_point(data = filter(TeamHit1, conference == input$conffind4), size = 3, shape = 16, color = "#FFCC00") +
      ggtitle("Scatterplot of Runs Scored against Plate Appearances")
    })
    
    output$EWHIP <- renderPlot ({
      ggplot(IP10, aes(ERA, WHIP)) + geom_jitter(alpha = .5) +
        xlim(0,16.5) + ylim(0,3.10) +
        xlab("Earned Run Average") + ylab("WHIP") +
        geom_text_repel(data = filter(IP10, ERA == 0 & WHIP <= .40), aes(ERA, WHIP, label = Player)) +
        geom_point(data = filter(IP10, Player == input$playerfind2), size = 5, shape = 16, color = "#FF0000") +
        geom_point(data = filter(IP10, School == input$schoolfind2), size = 4, shape = 16, color = "#00CCFF") +
        geom_point(data = filter(IP10, conference == input$conffind2), size = 3, shape = 16, color = "#FFCC00") +
        ggtitle("Scatterplot of ERA and WHIP values for pitchers with 10+ IP")
    })
    
    output$TEWHIP <- renderPlot({
      ggplot(TeamPitch2, aes(ERA, WHIP), color = IP) + geom_point(alpha = .3) +
        xlim(0,13.5) + ylim(.5,3.0) +
        xlab("Earned Run Average") + ylab("WHIP") +
        geom_text_repel(data = filter(TeamPitch2, ERA <= 2.5 & WHIP <= 1), aes(ERA, WHIP, label = School)) +
        ggtitle("Scatterplot of team ERA and WHIP values for teams with 5+ GP") +
        geom_point(data = filter(TeamPitch2, School == input$schoolfind5), size = 4, shape = 16, color = "#00CCFF") +
        geom_point(data = filter(TeamPitch2, conference == input$conffind5), size = 3, shape = 16, color = "#FFCC00")
      
    })
    
    output$map1 <- renderPlot ({
      plot_usmap(data = usdf, values = "N") +
        scale_fill_gradient(low="seashell", high="orangered2", na.value = "white", name = "Schools per State")
    })

    output$mymap <- renderLeaflet({
      map3 %>%
        leaflet() %>% 
        addTiles() %>% 
        addProviderTiles(providers$Thunderforest.Outdoors) %>% 
        setView(mean(map3$long),mean(map3$lat),4) %>%
        addMarkers(~long, ~lat, label = ~school, icon = baseballIcon)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
