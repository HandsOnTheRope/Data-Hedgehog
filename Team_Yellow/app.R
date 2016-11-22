library(shiny)
library(shinydashboard)
library(maps)
library(mapproj)
library(dplyr)

#functions for interactive maps
source("MapFinal.R")

#read in data 
loans.file <- read.csv("C:/Users/jdumiak/Desktop/R/loansData_clean.csv", header = TRUE)

#data for histograms 
MI <- read.csv("C:/Users/jdumiak/Documents/MI.csv", header = TRUE)
OH <- read.csv("C:/Users/jdumiak/Documents/ohiodat.csv", header = TRUE)

#average table for plot
avgdata <- read.csv("C:/Users/jdumiak/Documents/state_average_LC.csv", header = TRUE) 

##
ui <- shinyUI(fluidPage(
  #add a title to the UI
  titlePanel("Lendingclub Loans Profile by State"),
  #update the states based on region
  sidebarLayout(
    sidebarPanel(
      wellPanel(htmlOutput("region_selector")),
      wellPanel(htmlOutput("state_selector")),
      #creating image object with hover capabilities--connect in server by using input$plot_hover
      plotOutput("mapeast"),#, hover = hoverOpts(id = "plot_hover", delayType = "throttle"))
      plotOutput("mapwest"),
      plotOutput("mapmountain"),
      plotOutput("mapsouthwest"),
      plotOutput("mapsoutheast"),
      plotOutput("mapmidwest")
      
    ) ,
    #where all the results will go 
    mainPanel(
      textOutput("outtext"),
      br(),
      br(),
      plotOutput("Histogram"),
      br(),
      br(),
      tableOutput("results")
    )
  )
) 
)

server <- shinyServer(function(input, output) {
  locationData <- read.csv("C:/Users/jdumiak/Documents/regions.csv", header = TRUE, stringsAsFactors = FALSE)
  
  
  
  #update drop down menu
  output$region_selector <- renderUI({
    
    selectInput(
      inputId = "region", 
      label = "Region:",
      choices = as.character(unique(locationData$Region)),
      selected = "Northeast")
    
  })
  
  output$state_selector <- renderUI({
    
    available <- locationData[locationData$Region == input$region, "State"]
    
    selectInput(
      inputId = "state", 
      label = "State:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  
  
  #for the interactive map
  output$mapeast <- renderPlot({
    #use input and link output to make a reactive plot
    if (input$region == "Northeast" & !is.null(input$region)){
      northeast_map(loans.file$FICO.Score, "darkgreen", "FICO Score Range")
    } else if(input$region == "West" & !is.null(input$region)) {
      west_map(loans.file$FICO.Score, "darkgreen", "FICO Score Range")
    } else if(input$region == "Mountainwest" & !is.null(input$region)) {
      mountainwest_map(loans.file$FICO.Score, "darkgreen", "FICO Score Range")
    } else if(input$region == "Midwest" & !is.null(input$region)) {
      midwest_map(loans.file$FICO.Score, "darkgreen", "FICO Score Range")
    } else if(input$region == "Southwest" & !is.null(input$region)) {
      southwest_map(loans.file$FICO.Score, "darkgreen", "FICO Score Range")
    } else {
      southeast_map(loans.file$FICO.Score, "darkgreen", "FICO Score Range")
    }
  })
  output$outtext <- renderText({
    if(input$state == "MI"){
      paste("Michigan has a population of 9.9 million, which is equivalent to 3.1% of the U.S. population.
            Michigan represents only 1.8% of Lending Club's total loans, however. Furthemore, Lending Club's
            Michigan borrowers have an average Monthly Income that is significantly lower than the State average
            as a whole. Lending Club should target the higher income population in the Detroit Suburbs and
            Grand Rapids, where they can increase their customer base among the population more likely to receive
            higher grade bonds.
            ")
    } else if (input$state == "OH") {
      paste ("In Ohio, most of Lendingclub's customers have 10+ years of job experience with the average for the
             entire sample size around 7 years. Columbus is a new, booming market considered hip among younger
             generations with the average work experience of 3 years. Lendingclub can target this untapped college
             graduate market in Ohio to gain more business in this region, whether it be through financing student
             loans (debt consolidation) or home ownership.")
    } else if (input$region == "Midwest" & input$state != "OH" & input$state != "MI") {
      paste("The Midwest region is disproportionately under-represented by Lending Club's loan portfolio.
            Only 14% of Lending Club's loans are in the Midwest, but the region makes up approximately 23% of
            the U.S. population. Lending Club should target the Midwest Region as an opportunity for growth.")
    } else if (input$region == "Northeast") {
      paste("The Northeast region is over-represented by Lending Club's loan portfolio. Over 28% of Lending Club's loans are borrowed from individuals in the Northeast, but the region makes up only 18% of the U.S. population. Lending Club should maintain its strengths in this region, but this region is not the top target for growth.")
    } else if (input$region == "Southeast") {
      paste("The Southeast region is under-represented by Lending Club's loan portfolio. 21% of Lending Club's borrowers are located in the Southeast, but the region makes up 26% of the U.S. population. The discrepancy between population and Lending Club's regional portfolio suggests that the Southeast should be identified as a key target area for growth.")
    } else if (input$region == "Mountainwest" & input$state != "OH" & input$state != "MI") {
      paste("The Mountain West is slightly under-represented by Lending Club's loan portfolio. 4% of Lending Club's loans are from borrowers in this region, but the Mountain West 5% of the U.S. population. Since this region is such a small percentage of the overall population, Lending Club should keep its main focus on other key areas.")
    } else if (input$region == "Southwest") {
      paste("The Southwest is accurately represented by Lending Club's loan portfolio. 12% of Lending Club's borrowers are located in this region, which also makes up 12% of the U.S. population. Lending Club is doing a good job with this segment of the population and should maintain its strengths in this region.")
    } else if (input$region == "West") {
      paste("The West is over-represented by Lending Club's loan portfolio. 22% of Lending Club's loans are borrowed from individuals in the West, but the region makes up only 16% of the U.S. population. Lending Club should maintain its strengths in this region, but this area is not a top target for growth.")
    } else {
      paste("Hello")
    }
  })
  
  output$Histogram <- renderPlot ({
    
    if(input$state =="MI") {
      
      h1 <- hist(MI$Monthly.Income, bins=10, prob=TRUE)
      h2 <- hist(MI$Monthly.Income.Fake, bin=10, prob=TRUE)
      plot(h1,col=rgb(0,0,1,0.5),main= "Michigan Average Monthly Income ", 
           ylab= 'Density',xlab = "Monthly Income")
      plot(h2, col=rgb(1,0,0,0.5), add=T, main= "Monthly Income of Michigan Lending Club borrowers vs. Entire State")
      legend("topright", 
             legend = c("MI Lending Club Borrowers","MI State Average"),
             pch=15,
             col=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5))
      )
    }
    else if (input$state == "OH") {
      
      h3 <- hist(OH$Employment.Length.Clean, prob=TRUE)
      h4 <- hist(OH$Columbus.Employment.Length, prob= TRUE) 
      plot(h3, col=rgb(1,0,0,0.5), main="Ohio Average Employment Length", xlab="Employment Length", ylim=c(0, 40))
      plot(h4, add=T, col=rgb(0,0,1,0.5), main= "Employment Length in Columbus versus Lendingclub Ohio Data")
      legend("topright", 
             legend = c("Lendingclub Ohio Data","Columbus Data"),
             pch=15,
             col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))
      )
    }
    
    else {
      
      newdata <- structure(list(
        Northeast = c(28L, 18L), 
        Southeast = c(21L, 26L), 
        Midwest   = c(14L, 23L), 
        Mountain  = c(4L, 5L), 
        Southwest = c(12L, 12L), 
        West      = c(22L, 16L)), 
        .Names = c("Northeast", "Southeast", "Midwest", "Mountain", "Southwest","West" ), 
        class = "data.frame", row.names = c(NA, -2L))
      attach(newdata)
      colours <- c("blue", "red")
      barplot(as.matrix(newdata), main="Comparison of Population and Lending Club Loans by Region", ylab = "Percentage", beside=TRUE, col=colours)
      legend("topright", 
             legend = c("Percentage of LC loans","Percentage of U.S. Population"),
             pch=15,
             col=c("blue","red")
      )
      
    }
    
  })
  
  output$results <- renderTable({
    filtered <-
      avgdata[c("State","Lending_Club_Loans","Avg_Monthly_Income","Avg_Years_of_Experience","Avg_FICO_Score", "Avg_Open_Credit_Lines")] %>%
      filter(State == input$state)
    filtered
  })
  
})

shinyApp(ui = ui, server = server)

