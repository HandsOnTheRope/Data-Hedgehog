library(shiny)
# install.packages('ggplot2')
library(ggplot2)
# install.packages('dplyr')
library(dplyr)
# install.packages('maps')
library(maps)
loansData <- read.csv("loansData.csv", stringsAsFactors = FALSE)

palette(c("lightskyblue", "seagreen1", "salmon", "plum",
          "mediumpurple1", "burlywood", "aquamarine", "seagreen", "royalblue"))


#Adnan---------------------------------------------------------------------
loansDataLong<-loansData %>%
  group_by(State) %>%
  summarise(numLoans=n(),meanFico=mean(FICO.Score,na.rm=T),meanRate=mean(Interest.Rate.Clean,na.rm=T),meanIncome=mean(Monthly.Income,na.rm=T)) %>%
  mutate(freq=numLoans/sum(numLoans))

loansDataLong$state<-tolower(state.name[match(loansDataLong$State,state.abb)])

map.df<-map_data("state") %>% left_join(loansDataLong,by=c("region"="state"))
#Adnan---------------------------------------------------------------------



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
    br(),
    selectInput('xcol', 'X-Axis', choices = c("Amount Requested" = "Amount.Requested", "Amount Funded by Investors" = "Amount.Funded.By.Investors", 
                                              "Interest Rate" = "Interest.Rate.Clean", "FICO Score" = "FICO.Score"), selected = "Interest Rate"),
    selectInput('ycol', 'Y-Axis', choices = c("Amount Requested" = "Amount.Requested", "Amount Funded by Investors" = "Amount.Funded.By.Investors", 
                                              "Interest Rate" = "Interest.Rate.Clean", "FICO Score" = "FICO.Score"), selected = "FICO Score"),
    numericInput('clusters', 'Cluster Count', 3,
                 min = 1, max = 5),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    sliderInput("binwidth",
                "Binwidth:",
                min = .01,
                max = .10,
                value = .025),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    radioButtons("mapVar","Displayed Variable: ",c("Proportion of Loans"="freq","Average FICO"="meanFico",
                                                   "Average Interest Rate"="meanRate","Average Income"="meanIncome")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
   ),
  mainPanel(plotOutput("coolplot2"),
            br(),
            br(),
            plotOutput("coolplot3"),
            br(),
            br(),
            plotOutput("distPlot"),
            br(),
            br(),
            plotOutput("usMap")
            #tableOutput("results")
            )
)

server <- function(input,output) {
  
  
  
  
  
  ###Adnan----------------------------------------------------------
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- loansData$Interest.Rate.Clean 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #bins <- seq(min(x), max(x))
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(loansData,aes(x=x))+geom_histogram(binwidth=input$binwidth,colour="black",fill="lightskyblue")+
      ggtitle("Interest Rate Distribution")+theme(plot.title=element_text(size=20))
    
  })
  
  output$usMap<-renderPlot({
    ggplot(environment = environment())+
      geom_polygon(data=map.df,aes(x=long,y=lat,group=group,fill=get(input$mapVar)),colour="white") + 
      scale_fill_continuous(name=input$mapVar)
  })
  ###Adnan----------------------------------------------------------
  
  
  
  
  
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
      ggplot(filtered1, aes(Interest.Rate.Clean)) + coord_cartesian(xlim = c(0.0,.25)) + 
        scale_x_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) + coord_cartesian(ylim = c(0,250)) + 
        geom_histogram(fill="lightskyblue", colour = "black") + xlab("Interest Rate") + ylab("Count") + 
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
      ggplot(filtered2, aes(Interest.Rate.Clean)) + coord_cartesian(xlim = c(0.0,.25)) + 
        scale_x_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) + coord_cartesian(ylim = c(0,250)) + 
        geom_histogram(fill="lightskyblue", colour = "black") + xlab("Interest Rate") + ylab("Count") + 
        theme(plot.title = element_text(lineheight=4, face="bold"))
    }
  })
}
shinyApp(ui = ui, server = server)