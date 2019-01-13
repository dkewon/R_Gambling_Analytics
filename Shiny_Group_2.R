#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 
#setwd("~/Documents/GitHub/group-assignment-open-source-programming-bwin-group-2/shiny")
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")


load("basetable.RData")


# Define UI for application that draws a histogram
ui <- navbarPage(
    title = div(img(src="https://arbers.org/wp-content/uploads/2017/08/bwin1.jpg",height = 32, width = 80), "Sports Gambling Activity"),
    
    tabPanel("Stakes by Country - Top 10",
           sidebarLayout(
             sidebarPanel(
                 selectInput("Stakes",
                             "Select the game:",
                             names(basetable[grepl( "stakes_" , names( basetable) ) ]), width='100%')
              ),
              mainPanel(
                 plotOutput("distPlot_stakes")
                 
              )
           )
    ),
    
    tabPanel("Wins by Country - Top 10",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Wins",
                             "Select the game:",
                             names(basetable[grepl( "wins_" , names( basetable) ) ]), width='100%')
               ),
               mainPanel(
                 plotOutput("distPlot_wins")
                 
               )
             )
    ),
    
    tabPanel("Bets by Country - Top 10",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Bets",
                             "Select the game:",
                             names(basetable[grepl( "bets_" , names( basetable) ) ]), width='100%')
               ),
               mainPanel(
                 plotOutput("distPlot_bets")
                 
               )
             )
    ),
    
    
    tabPanel("Wins and Stakes for different Games",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("country1",
                             "Country:",
                             unique(basetable$Country_Name)),
                 radioButtons("gender1",
                              "Gender:",
                              #unique(basetable$Gender)[1:2]
                              c("Male"=1,"Female"=0))
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("winPlot"),
                 plotOutput('stakesPlot')
               )
             )
             
    ),
    
    
    tabPanel("Frequency of different application",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("Application",
                             "Application used:",
                             unique(basetable$Application_Description))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("application_boxplot"),
                 textOutput("maximum"),
                 textOutput("average"),
                 textOutput("median"),
                 textOutput("minimum"),
                 textOutput("players")
               )
             )         
             
    ),
    
    tabPanel("Poker Chips bought vs Chips sold",
          
             
             # sidebar with country selection, gender selection and slider for upper/lower boarder
             sidebarLayout(
               sidebarPanel(
                 selectInput("country2",
                             "Country:",
                             unique(basetable$Country_Name)),
                 selectInput("gender2",
                             "Gender",
                             #unique(basetable$Gender)[1:2])
                             c("Male"=1,"Female"=0)),
                 sliderInput("upper",
                             "upper limit:",
                             min = 0,
                             max = 1000,
                             value = 100),
                 sliderInput("lower",
                             "lower limit:",
                             min = -1000,
                             max = -1,
                             value = -100)
                 
               ),
               
               # Show a plots
               mainPanel(
                 plotOutput("scatterPlot"),
                 plotOutput("histogram")
               )
             )        
             
    ),
    
    tabPanel("Players profit per country",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("country3",
                             "Country:",
                             unique(basetable$Country_Name))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("profit")
               )
             )     
             
    )
    
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ############################################################
  ####      Output: Stakes by Country - Top 10           #####
  ############################################################
  
  output$distPlot_stakes <- renderPlot({
      x <- select(basetable,Country_Name, input$Stakes)
      x_df<- data.frame(as.factor(x[[1]]),x[[2]])
      names(x_df)=c("Country","Stakes")
      z<-x_df%>%
          group_by(Country)%>%
            summarise(sum_stakes=sum(Stakes))%>%
              arrange(desc(sum_stakes))
      #options(scipen=5)
      p<-barplot(z$sum_stakes[1:10],names.arg=z$Country[1:10],las=2, main = "Stakes by Country - Top 10") #ylab = "Sum_stakes"
      #text(p, z$sum_stakes[1:10], labels = z$sum_stakes[1:10], adj = c(0.5,-0.5) ,cex = 0.8)
  })
  
  
  ############################################################
  ####      Output: Wins by Country - Top 10             #####
  ############################################################
  
  output$distPlot_wins <- renderPlot({
    x <- select(basetable,Country_Name, input$Wins)
    x_df<- data.frame(as.factor(x[[1]]),x[[2]])
    names(x_df)=c("Country","Wins")
    z<-x_df%>%
      group_by(Country)%>%
      summarise(sum_wins=sum(Wins))%>%
      arrange(desc(sum_wins))
    #options(scipen=5)
    barplot(z$sum_wins[1:10],names.arg=z$Country[1:10],las=2)
  })
  
  
  ############################################################
  ####      Output: Bets by Country - Top 10             #####
  ############################################################
  
  output$distPlot_bets <- renderPlot({
    x <- select(basetable,Country_Name, input$Bets)
    x_df<- data.frame(as.factor(x[[1]]),x[[2]])
    names(x_df)=c("Country","Bets")
    z<-x_df%>%
      group_by(Country)%>%
      summarise(sum_bets=sum(Bets))%>%
      arrange(desc(sum_bets))
    #options(scipen=5)
    barplot(z$sum_bets[1:10],names.arg=z$Country[1:10],las=2)
  })
  
  ############################################################
  ####   Output: Wins and Stakes for different Games     #####
  ############################################################
  
  output$winPlot <- renderPlot({
    #subset the basetable for wins
    
    x <- subset(basetable, Gender == input$gender1 & Country_Name == input$country1, select = c(wins_1, wins_2, wins_4, wins_5, wins_6, wins_7, wins_8)) 
    
    game1 <- round(mean(x[[1]]), 1)
    
    game2 <- round(mean(x[[2]]), 1)
    
    game4 <- round(mean(x[[3]]), 1)
    
    game5 <- round(mean(x[[4]]), 1)
    
    game6 <- round(mean(x[[5]]), 1)
    
    game7 <- round(mean(x[[6]]), 1)
    
    game8 <- round(mean(x[[7]]), 1)
    
    game_results1 <- c(game1, game2, game4, game5, game6, game7, game8)
    
    # draw the barplot
    barpos1 <- barplot(c(game1, game2, game4, game5, game6, game7, game8), ylab = "Average_Wins", main = "Average_Wins/Games", names.arg = c("Game 1", "Game 2", "Game 4", "Game 5", "Game 6", "Game 7", "Game 8"))
    text(barpos1, game_results1, labels = game_results1, adj = c(0.5,-0.5) ,cex = 0.8)
  })
  
  output$stakesPlot <- renderPlot({
    #subset the basetable for stakes
    
    x <- subset(basetable, Gender == input$gender1 & Country_Name == input$country1, select = c(stakes_1, stakes_2, stakes_4, stakes_5, stakes_6, stakes_7, stakes_8)) 
    
    game1 <- round(mean(x[[1]]), 1)
    
    game2 <- round(mean(x[[2]]), 1)
    
    game4 <- round(mean(x[[3]]), 1)
    
    game5 <- round(mean(x[[4]]), 1)
    
    game6 <- round(mean(x[[5]]), 1)
    
    game7 <- round(mean(x[[6]]), 1)
    
    game8 <- round(mean(x[[7]]), 1)
    
    game_results2 <- c(game1, game2, game4, game5, game6, game7, game8)
    
    # draw the barplot
    barpos <- barplot(c(game1, game2, game4, game5, game6, game7, game8), ylab = "Average_Stakes", main = "Average_Stakes/Games", names.arg = c("Game 1", "Game 2", "Game 4", "Game 5", "Game 6", "Game 7", "Game 8"))
    text(barpos, game_results2, labels = game_results2, adj = c(0.5,-0.5) ,cex = 0.8)
  })
  
  
  ############################################################
  ####   Output: Frequency of different application       ####
  ############################################################
  
  output$application_boxplot <- renderPlot({
    # subset basetable in order to get the days_played per Application
    x    <- basetable[basetable$Application_Description == input$Application, 44]
    mean <- mean(x$last_played)
    
    
    # draw the boxplot
    boxplot(x)
  })
  output$average <- renderText({
    x    <- basetable[basetable$Application_Description == input$Application, 44]
    average <- round(mean(x$days_played),0)
    paste("mean is equal to", average)
  })
  output$median <- renderText({
    x    <- basetable[basetable$Application_Description == input$Application, 44]
    median_1 <- round(median(x$days_played),0)
    paste("median is equal to", median_1)
  })
  output$minimum <- renderText({
    x    <- basetable[basetable$Application_Description == input$Application, 44]
    minimum <- round(min(x$days_played),0)
    paste("minimum is equal to", minimum)
  })
  output$maximum <- renderText({
    x    <- basetable[basetable$Application_Description == input$Application, 44]
    maximum <- round(max(x$days_played),0)
    paste("maximum is equal to", maximum)
  })
  output$players <- renderText({
    x    <- basetable[basetable$Application_Description == input$Application, 44]
    players <- nrow(x)
    paste("the number of players is equal to", players)
  })
  
  
  ############################################################
  ####      Output: Poker Chips bought vs Chips sold      ####
  ############################################################
  
  output$scatterPlot <- renderPlot({
    # subset table for scatter plot
    temp_table    <- subset(basetable, Country_Name == input$country2 & Gender == input$gender2, select = c(poker_chips_sold, poker_chips_bought)) 
    x <- temp_table[[1]]
    y <- temp_table[[2]]
    # draw the scatterplot chips_bought/chips_sold
    plot(x, y, ylab = "Poker Chips Bought", xlab = "Poker Chips Sold")
  })
  
  output$histogram <- renderPlot({
    # subset table for histogram
    temp_table    <- subset(basetable, Gender == input$gender2 & Country_Name == input$country2 & balance > input$lower & balance < input$upper & balance != 0, select = "balance") 
    x <- temp_table[[1]]
    
    # draw the histogram for the balance
    hist(x, main = "Balance",col = 'darkgray')
  })

  
  
  ############################################################
  ####                Playersprofit per country           ####
  ############################################################
  
  output$profit <- renderPlot({
    # generate bins based on input$bins from ui.R
    profit_temp <- subset(basetable, Country_Name == input$country3 & profit_per_player > -1000 & profit_per_player < 1000, select = "profit_per_player") 
    x <- profit_temp[[1]]
    bins <- seq(-1000,1000,by=100)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', xlim = c(-1000, 1000), xlab = "profit per player", main = "Frequencytable Profit per Player")
  })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

