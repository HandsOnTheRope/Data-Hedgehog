library(shiny)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)
loansData <- read.csv("loansData.csv", stringsAsFactors = FALSE)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

ui <- fluidPage(
  titlePanel("Lending Club Loans Data Dashboard"),
   sidebarPanel(
    #sliderInput("Int1","Interest Rate", min = .05, max = .25, value = c(.05,.25)),
    sliderInput("FIC1","FICO Score", min = 640, max = 830, value = c(640,830)),
    sliderInput("Month1", "Monthly Income", min = 500, max = 30000, value = c(500, 30000)),
    radioButtons("Length1", "Loan Length", choices = c("36 months", "60 months"), selected = "36 months"),
    selectInput("Own1", "Home Ownership", multiple = FALSE, choices = c("ALL", "MORTGAGE", "OWN", "RENT"), selected = "ALL"),
    br(),
    br(),
    br(),
    br(),
    selectInput('xcol', 'X-Axis', choices = c("Amount.Requested", "Amount.Funded.By.Investors", 
                                              "Interest.Rate.Clean", "FICO.Score"), selected = "Interest.Rate.Clean"),
    selectInput('ycol', 'Y-Axis', choices = c("Amount.Requested", "Amount.Funded.By.Investors", 
                                              "Interest.Rate.Clean", "FICO.Score"), selected = "FICO.Score"),
    numericInput('clusters', 'Cluster Count', 3,
                 min = 1, max = 9)
  ),
  
  mainPanel(plotOutput("coolplot2"),
            br(),
            br(),
            plotOutput("coolplot3")
            #tableOutput("results")
            )
)

server <- function(input,output) {
  
  selectedData <- reactive({
    loansData[, c(input$xcol , input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$coolplot3 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  output$coolplot1 <- renderPlot({
    ggplot(loansData,aes(Interest.Rate.Clean)) + geom_histogram()
    })
  output$coolplot2 <- renderPlot({
    if(input$Own1 == "ALL") {
      filtered1 <-
        loansData %>%
        filter(
               #Interest.Rate.Clean >= input$Int1[1],
               #Interest.Rate.Clean <= input$Int1[2],
               FICO.Score >= input$FIC1[1],
               FICO.Score <= input$FIC1[2],
               Monthly.Income >= input$Month1[1],
               Monthly.Income <= input$Month1[2],
               Loan.Length == input$Length1)
      ggplot(filtered1, aes(Interest.Rate.Clean)) + coord_cartesian(xlim = c(0.0,.25)) + scale_x_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) + coord_cartesian(ylim = c(0,250)) + 
        geom_histogram(fill="#66B2FF", colour = "black") + xlab("Interest Rate") + ylab("Count") + 
        theme(plot.title = element_text(lineheight=4, face="bold"))
        # ggtitle("Interest Rates")
      }
    else {
      filtered2 <-
        loansData %>%
        filter(
               #Interest.Rate.Clean >= input$Int1[1],
               #Interest.Rate.Clean <= input$Int1[2],
               FICO.Score >= input$FIC1[1],
               FICO.Score <= input$FIC1[2],
               Monthly.Income >= input$Month1[1],
               Monthly.Income <= input$Month1[2],
               Loan.Length == input$Length1,
               Home.Ownership == input$Own1)
      ggplot(filtered2, aes(Interest.Rate.Clean)) + coord_cartesian(xlim = c(0.0,.25)) + scale_x_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) + coord_cartesian(ylim = c(0,250)) + 
        geom_histogram(fill="#66B2FF", colour = "black") + xlab("Interest Rate") + ylab("Count") + 
        theme(plot.title = element_text(lineheight=4, face="bold"))
    }
  })
}
shinyApp(ui = ui, server = server)