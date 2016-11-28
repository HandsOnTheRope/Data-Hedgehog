
### MEMORY ##################################################################################################################

#Clear Memory
rm(list = ls()) 
gc()


### LOAD DATA ###############################################################################################################

#Set Directory 
setwd("C:/Users/sampahwa/Documents")

library(ggplot2)
library(corrplot)
library(Hmisc)

#Read in Data 
loans_data <- read.csv("loansdata_Clean.csv")
names(loans_data)
attach(loans_data)


### DATA CLEANSING ##########################################################################################################

#Convert Categorical Variables to Factors
str(loans_data$Loan.Purpose)
loans_data$Loan.Purpose <- factor(loans_data$Loan.Purpose)

str(loans_data$State)
loans_data$State <- factor(loans_data$State)

str(loans_data$Home.Ownership)
loans_data$Home.Ownership <- factor(loans_data$Home.Ownership)

loans_cor <- loans_data[, c(1,2,7,10,12,13,14,16,17,18,19)]

loans_shiny <- loans_data[, c(1,17,18,19)]

### R SHINY APPLICATION #####################################################################################################

# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)

ui <- fluidPage(
  headerPanel('Lending Club k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(loans_shiny)),
    selectInput('ycol', 'Y Variable', names(loans_shiny),
                selected = names(loans_shiny)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  ),
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  
  selectedData <- reactive({
    loans_shiny[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)


