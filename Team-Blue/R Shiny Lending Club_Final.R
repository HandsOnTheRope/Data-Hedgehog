#Lending Club R Shiny Dashboard Creation
#11/15/2016
#Overview: This R Shiny dashboard is intended to visualize the data from Lending Club's Loan Database

#This document contains:
#1.Setting up
#2.UI
#3.Server

####################### SETTING UP #######################
#Set working directory
#setwd("~Your filepath~")

#Imports csv file
  loan.data <-
    read.csv(
      "C:/`YOUR FILE PATH' Trainings/R_Studio/Data/loansData_clean2.csv",
      header = TRUE
    )

#Turn off scientific notation
  options(scipen = 500)

#Download R Shiny Package
  #install.packages("shiny")


#Creating new variable to flag if fully funded loan
  loan.data$Fully.Funded.Flg <- ifelse(loan.data$Amount.Funded.By.Investors == loan.data$Amount.Requested, 1, 0)

# Change categorical variables to factors
  
  loan.data$Loan.Length.Factor <- factor(loan.data$Loan.Length.Clean)
  loan.data$Fico.Category <- factor(loan.data$Fico.Category)
  loan.data$Home.Ownership <- factor(loan.data$Home.Ownership)

  
#Creating new dataframe of variables of interest
    continuous.var.data <- with(loan.data, 
                                data.frame(Amount.Requested, Amount.Funded.By.Investors, Monthly.Income, Open.CREDIT.Lines,
                                            Revolving.CREDIT.Balance,Inquiries.in.the.Last.6.Months,
                                            Interest.Rate.Clean, Loan.Length.Clean, FICO.Score, Fully.Funded.Flg))
    
#Creat new dataframe without missing values (dropping missing variables - may not be the best way to handle)
    continuous.var.data2 <- na.omit(continuous.var.data)

    
#Creating variables/data frames for the bar graph
    
    #Creating unfunded amount variable
      Unfunded.Amount <- loan.data$Amount.Requested - loan.data$Amount.Funded.By.Investors
    
    #Creating flag for unfunded loans
      Unfunded.Loan <- Unfunded.Amount
      for (i in 1:length(Unfunded.Loan)){
        if (Unfunded.Loan[i] > 0) {Unfunded.Loan[i] <- 1} else {Unfunded.Loan[i] <- 0}}
      
    #Creating new dataframe for categorical variables of interest
    loans <- data.frame("Loan Length" = loan.data$Loan.Length.Factor, 
                        "FICO Category" = loan.data$Fico.Category,
                        "Home Ownership" = loan.data$Home.Ownership, 
                        Unfunded.Amount, Unfunded.Loan)

#Execute Libraries    
  library(shiny)
  library(ggplot2)

####################### UI.R #######################

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Lending Club Loan Data Insights"),
  
  navbarPage(
    "Graph Type",
    #Creating navigation bars and labeling them
    
    # About Tab
    tabPanel("About",
             
           mainPanel(
             h1("So you wanted a dashboard with Pie Charts ey?"),
             h1 ("Well here!"),
             h1 ("Have a whole dashboard of Bar Charts and Histograms!")
    )),
    
    
    
    tabPanel("Histogram - Simple",
    
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(
        p("Select variables to display in the histogram at right."),
        #Sidebar instructions
        
        #Creating continuous Variable Select Bar
        selectInput(
          "variable",
          "Select a continuous variable:",
          choices=c("Amount Requested" = 1, "Amount Funded" = 2, "Monthly Income" = 3,
                    "Open Credit Lines" =4, "Revolving Credit Balance" = 5, "Inquiries in the Last Month" = 6,
                    "Interest Rate" = 7, "Loan Length"=8, "Fico Score" = 9) 


        ),
        
        selectInput(
          "data_portion",
          "Select a portion of the loans:",
          choices=c("All Loans" = 3, "Fully Funded" = 1, "Partially Funded" = 0) 
        ),
        
        
        #Creating slider Bar
        sliderInput(
          "bins",
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        ),
        
        #Creating color selection
        radioButtons("color", "Select the color of the histogram", choices=c("Green", "Blue", "Yellow"), selected="Green")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot"))
    )),
    
    tabPanel("Histogram - GGPlot2",    #Creating another Tab
             
             
             # Sidebar with a slider input for the number of bins
             sidebarLayout(
               sidebarPanel(
                 p("Select variables to display in the histogram at right."),
                 #Sidebar instructions
                 
                 #Creating continuous Variable Select Bar
                 selectInput(
                   "variable2",
                   "Select a continuous variable:",
                   choices=c("Amount Requested" = 1, "Amount Funded" = 2, "Monthly Income" = 3,
                             "Open Credit Lines" =4, "Revolving Credit Balance" = 5, "Inquiries in the Last Month" = 6,
                             "Interest Rate" = 7, "Loan Length"=8, "Fico Score" = 9) 
                 ),
                 
                
                 selectInput(
                   "data_portion2",
                   "Select a portion of the loans:",
                   choices=c("All Loans" = 3, "Fully Funded" = 1, "Partially Funded" = 0) 
                 ),
                 
                 
                 #Creating slider Bar
                 sliderInput(
                   "bins2",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30
                 )
                 
                
               ),
               
               # Show a plot of the generated distribution
                   mainPanel(plotOutput("distPlot2"))
             )     
             
             
             
             
    ),
    
    
    tabPanel("Bar Chart",
             
             # Sidebar with a select input for loan category 
             sidebarLayout(
               sidebarPanel(
                 selectInput("category",
                             "Select Loan Category to Explore",
                             choices = colnames(loans[,1:3])),
                 
                 selectInput("type", "Select Loan Type",
                             choices = c("All", "Fully Funded", "Partially Funded"))),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("barchart"))
             ))

             
    )
    
   

    
    
  )





####################### SERVER.R #######################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #OUTPUT FOR TAB 1: HISTOGRAM
  output$distPlot <- renderPlot({
    
    #Renaming inputed values
      colm <- as.numeric(input$variable)
      #cat("Selected Var: ", colm)  #Testing: printing out selected var
      portion <- as.numeric(input$data_portion)
      #cat("portion: ", portion)
    
    #Assigning slected variable and portion of loan data
      if (portion == 3) x <- continuous.var.data2[, colm] else #All data
        x <- continuous.var.data2[continuous.var.data2$Fully.Funded.Flg == portion, colm] #Partial Data
    
    #Assigning bin size from selected
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #Draw the histogram with the specified number of bins
      hist(x,
           breaks = bins,
           col = input$color,
           border = 'white', xlab=names(continuous.var.data2[colm]))
  
    
    
  })
  
  
  #OUTPUT FOR TAB2: HISTOGRAM WITH GGPLOT2
  
  output$distPlot2 <- renderPlot({
    
    #Renaming inputed values
    colm2 <- as.numeric(input$variable2)
    #cat("Selected Var: ", colm)  #Testing: printing out selected var
    portion2 <- as.numeric(input$data_portion2)
    #cat("portion: ", portion)
    
    #Creating new dataframes
      if (portion2 == 3) graph.df <- continuous.var.data2 else
        graph.df <- continuous.var.data2[continuous.var.data2$Fully.Funded.Flg == portion2, ]
    
    #Creating variable of interest
      x2 <- graph.df[ ,colm2]
         
    #Assigning bin size from selected
      bins2 <- seq(min(x2), max(x2), length.out = input$bins2 + 1)
    
    #Changing var Fully.Funded.Flg to factor
      graph.df$Fully.Funded.Flg.Fc <- as.factor(graph.df$Fully.Funded.Flg)
      #Creating variable names
         Fully_Funded = graph.df$Fully.Funded.Flg.Fc       
      #Creating factor labels
         Fully_Funded <- factor(Fully_Funded, levels = c(1,0), labels = c("Fully Funded", "Partially Funded"))
     
    
    #Creating histogram 
      histo_plot2 <- ggplot(data=graph.df, aes(x2)) +
        geom_histogram(breaks= bins2,
                       col= "black",
                       #fill=input$color2) +
                       aes(fill=Fully_Funded)) + #color by variable
        labs(title= "Histogram") +
        labs(x=names(continuous.var.data2[colm2]), y="Frequency")
      
    #Plot
      histo_plot2
     
  })
  
  output$barchart <- renderPlot({
    
    if(input$type == "All"){
      count <- loans
      
    } else if (input$type == "Fully Funded"){
      count <- loans[loans$Unfunded.Loan == 0,]
      
    } else if (input$type == "Partially Funded"){
      count <- loans[loans$Unfunded.Loan == 1,]
      
    }
    
    
    ggplot(data = count, aes_string(input$category)) +
      
      geom_bar(
        fill = "light blue",
        colour = "dark green",
        alpha = .6) +
        labs(title = paste(" Bar Chart of Loans by ", input$category)) +
        labs(x=input$category, y= paste("Count of ", input$type, "Loans"))
        
      
    
  })
  
}



shinyApp(ui = ui, server = server)