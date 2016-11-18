
library(shiny)
library(doBy)
library(ggplot2)

#Create summaryby tables of loan purpose and state
library(readr)
loansData_clean <- read_csv("~/loansData_clean.csv")
loansData_clean <- as.data.frame(loansData_clean)
library(doBy)
purpose_df<-summaryBy(Amount.Funded.By.Investors ~ Loan.Purpose, data = loansData_clean, 
                      FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
state_df<-summaryBy(Amount.Funded.By.Investors ~ State, data = loansData_clean, 
                    FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
colnames(purpose_df) <- c("Purpose", "Count", "Average", "Total")
colnames(state_df) <- c("State", "Count", "Average", "Total")

#----------------------------------------------
###### UI ######
ui <- fluidPage(
  titlePanel("Loan Data by State and Loan Purpose"),
  sidebarPanel(
    selectInput("summaryinput", "Sum, Count, or Average", choices=colnames(purpose_df[,2:4])
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("State", plotOutput("stategraph")),
      tabPanel("Loan Purpose", plotOutput("purposegraph"))
    )
  )
)


#----------------------------------------------
###### SERVER ######
server <- function(input, output){

  
  #State plot
  output$stategraph <- renderPlot({
    
    ggplot(state_df, aes_string(x="State", y=input$summaryinput),
                          label=rownames(state_df)) + xlab("State") + ylab(toString(input$summaryinput)) +
                          ggtitle("Amount Funded by State") + geom_col(aes(fill=State)) 
 
    
    
  })   
  
    
  # Purpose Plot
  output$purposegraph <- renderPlot({

  ggplot(purpose_df, aes_string(x="Purpose", y=input$summaryinput),
                        label=rownames(purpose_df)) + xlab("Purpose") + ylab(toString(input$summaryinput)) +
                        ggtitle("Amount Funded by Loan Purpose") + geom_col(aes(fill=Purpose))


  })

}
#----------------------------------------------
shinyApp(ui=ui, server=server)