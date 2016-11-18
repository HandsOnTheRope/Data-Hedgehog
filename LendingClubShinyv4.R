library(shiny)
library(doBy)
library(ggplot2)
library("RColorBrewer")

setwd("C:/Users/elrivas/Documents/Trainings/R Training")
loandata <- read.csv("loandataclean.csv", header=TRUE)
options(scipen = 999)

# clean Eployment Length

toString(loandata$Employment.Length)
loandata$Employment.Length <- gsub("[^0-9]", "", loandata$Employment.Length)
loandata$Employment.Length <- as.double(loandata$Employment.Length)
# New var
loandata$funded_ratio <- ((loandata$Amount.Funded.By.Investors)/(loandata$Amount.Requested))
# New data
purpose_df<-summaryBy(Amount.Funded.By.Investors ~ Loan.Purpose, data = loandata, 
                      FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
state_df<-summaryBy(Amount.Funded.By.Investors ~ State, data = loandata, 
                    FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
home_owner_df <- summaryBy(Amount.Funded.By.Investors ~ Home.Ownership, data = loandata, 
                           FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
employment_df <- summaryBy(Amount.Funded.By.Investors ~ Employment.Length, data = loandata, 
                           FUN = function(x) { c(c=length(x), m = mean(x), s = sum(x)) })
colnames(purpose_df) <- c("Loan.Purpose", "Count", "Average", "Total")
colnames(state_df) <- c("State", "Count", "Average", "Total") 
colnames(employment_df) <- c("Employment.Length", "Count", "Average", "Total") 
colnames(home_owner_df) <- c("Home.Ownership", "Count", "Average", "Total") 
#----------------------------------------------
###### UI ######
ui <- fluidPage(
  verticalLayout(
    titlePanel("LendingClub Data Explorer"),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot Explorer", plotOutput("comparison_plot"),
                 wellPanel(
                   selectInput("x_input", "Select X Variable", choices=names(loandata[c(1,2,6,9,12,14)])),
                   selectInput("y_input", "Select Y Variable", choices=names(loandata[c(1,2,6,9,12,14)]))
                 )),
        tabPanel("State Explorer", plotOutput("state_plot"),
                 wellPanel(
                   selectInput("sca_input1", "Count, Average, or Total", choices=colnames(state_df[,c(2:4)]))
                 )),
        tabPanel("Loan Purpose Explorer", plotOutput('purpose_plot'),
                 wellPanel(
                   selectInput("sca_input2", "Count, Average, or Total", choices=colnames(purpose_df[,c(2:4)]))
                 )),
        tabPanel("Home Ownership Explorer", plotOutput("ho_plot"),
                 wellPanel(
                   selectInput("sca_input3", "Count, Average, or Total", choices=colnames(home_owner_df[,c(2:4)]))
                 )),
        tabPanel("Employment Explorer", plotOutput("emp_plot"),
                 wellPanel(
                   selectInput("sca_input4", "Count, Average, or Total", choices=colnames(employment_df[,c(2:4)]))
                 ))
      ), style='width: 900px; height: 1000px'), list(tags$head(tags$style("body {background-color: #ade0e6; }")))
  )
)


#----------------------------------------------
###### SERVER ######
server <- function(input, output){
  
  output$comparison_plot <- renderPlot({
    cont_graph <- ggplot(loandata, aes_string(x=input$x_input, y=input$y_input, label = names(loandata))) +
      geom_point()
    print(cont_graph)
  })
  output$state_plot <- renderPlot({
    s <- ggplot(state_df, aes_string(x=state_df$State,y=input$sca_input1, label = rownames(state_df))) + 
      xlab("State") + ylab(toString(input$sca_input1)) + ggtitle("Amount Funded by State") +
      geom_col() + theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    print(s)
  })
  output$purpose_plot <- renderPlot({
    p <- ggplot(purpose_df, aes_string(x=purpose_df$Loan.Purpose, y=input$sca_input2, label = rownames(purpose_df))) +
      xlab("Purpose") + ylab(toString(input$sca_input2)) + ggtitle("Amount Funded by Loan Purpose") +
      geom_col() + theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    print(p)
  })
  output$emp_plot <- renderPlot({
    e <- ggplot(employment_df, aes_string(x=employment_df$Employment.Length, y=input$sca_input4, label=rownames(employment_df))) +
      xlab("Employment Length") + ylab(toString(input$sca_input4)) + ggtitle("Amount Funded by Employment Length") +
      geom_col() + theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    print(e)
  })
  output$ho_plot <- renderPlot({
    h <- ggplot(home_owner_df, aes_string(x=home_owner_df$Home.Ownership, y=input$sca_input3, label=rownames(home_owner_df))) +
      xlab("Home Ownership Status") + ylab(toString(input$sca_input3)) + ggtitle("Amount Funded by Home Ownership Status") +
      geom_col() + theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    print(h)
    
  }
  )
}
  


#----------------------------------------------
shinyApp(ui=ui, server=server)