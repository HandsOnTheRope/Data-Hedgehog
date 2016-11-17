library(shiny)
library(ggplot2) 
library(dplyr)
avgdata <- read.csv("state_average_LC.csv", stringsAsFactors = FALSE)
MI <- read.csv("\\Users\\jdessy\\Documents\\R\\Shiny Project\\MI.csv", header = TRUE)
Ohio <- read.csv("\\Users\\jdessy\\Documents\\R\\Shiny Project\\ohiodat.csv", header = TRUE)


ui <- fluidPage(
          titlePanel("Lending Club Growth Opportunity in the Midwest"),
          br(),
         
          sidebarLayout(
          sidebarPanel(
            tags$img(src = 'Midwest.png', height = 285, width = 500 ),
            br(),
            br(),
            br(),
            br(),
            selectInput("stateInput", "State",
                             choices = c("MI", "OH", "MN", "IA", "IN", "IL","MO","ND", "NE","SD","WI")),
            width = 4
                     
                        ),
    mainPanel(
      #textOutput("text1"),
      #br(),
      #br(),
      textOutput("out"),
      br(),
      br(),
      plotOutput("Histogram"),
      br(),
      br(),
      tableOutput("results"), 
      width = 7
    )
    )
  )

server <- function(input, output, session) {
  
  output$out <- renderText({
    if(input$stateInput == "MI"){
      paste("Michigan has a population of 9.9 million, which is equivalent to 3.1% of the U.S. population. 
              Michigan represents only 1.8% of Lending Club's total loans, however. Furthemore, Lending Club's
              Michigan borrowers have an average Monthly Income that is significantly lower than the State average
              as a whole. Lending Club should target the higher income population in the Detroit Suburbs and 
              Grand Rapids, where they can increase their customer base among the population more likely to receive
              higher grade bonds.
            ")
    }
    else if (input$stateInput == "OH") {
      paste ("In Ohio, most of Lendingclub's customers have 10+ years of job experience with the average for the 
             entire sample size around 7 years. Columbus is a new, booming market considered hip among younger 
             generations with the average work experience of 3 years. Lendingclub can target this untapped college 
             graduate market in Ohio to gain more business in this region, whether it be through financing student 
             loans (debt consolidation) or home ownership.")
    }
    
    else {
      paste("The Midwest region is disproportionately under-represented by Lending Club's loan portfolio. 
            Only 14% of Lending Club's loans are in the Midwest, but the region makes up approximately 23% of
            the U.S. population. Lending Club should target the Midwest Region as an opportunity for growth.")
      
    }
  })

output$Histogram <- renderPlot ({
    
    if(input$stateInput =="MI") {
      
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
      else if (input$stateInput == "OH") {
        
        h3 <- hist(Ohio$Employment.Length.Clean, prob=TRUE)
        h4 <- hist(Ohio$Columbus.Employment.Length, prob= TRUE) 
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
      filter(State == input$stateInput)
    filtered
  })
}
shinyApp(ui = ui, server = server)



