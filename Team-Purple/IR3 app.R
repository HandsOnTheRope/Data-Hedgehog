
### MEMORY ##################################################################################################################

#Clear Memory
rm(list = ls()) 
gc()


### LOAD DATA ###############################################################################################################

#Set Directory 
setwd("C:/Users/sampahwa/Documents")

library(shiny)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)

#Read in Data 
loans_data <- read.csv("loansData_Clean.csv", stringsAsFactors = FALSE)
names(loans_data)
attach(loans_data)


### USER INTERFACE ###########################################################################################################

ui <- fluidPage(
  titlePanel("Loans Data App"),
  sidebarPanel(
    sliderInput("Amt_Req","Amount Requested", min = 0, max = 35000, value = c(0,35000)),
    sliderInput("FICO","FICO Score", min = 640, max = 830, value = c(640,830)),
    radioButtons("Length", "Loan Length", choices = c("36 months", "60 months"), selected = "36 months")),
  mainPanel(plotOutput("coolplot2"),
            br(),
            br()
  )
)


### SERVER ###################################################################################################################

server <- function(input,output) {
  output$coolplot1 <- renderPlot({
    ggplot(loans_data,aes(Interest.Rate.Clean)) +
      geom_histogram()
  })
  output$coolplot2 <- renderPlot({
    
    filtered <-
      loans_data %>%
      filter(Amount.Requested >= input$Amt_Req[1],
             Amount.Requested <= input$Amt_Req[2],
             FICO.Score >= input$FICO[1],
             FICO.Score <= input$FICO[2],
             Loan.Length == input$Length)
    ggplot(filtered, aes(Interest.Rate.Clean)) + geom_histogram(fill="#66B2FF", colour = "black") + xlab("Interest Rate") +       ylab("Count") + ggtitle("Interest Rates") + theme(plot.title = element_text(lineheight=4, face="bold"))
  })
}


### RUN APP ##################################################################################################################

shinyApp(ui = ui, server = server)
