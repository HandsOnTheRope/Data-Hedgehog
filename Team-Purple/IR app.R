mydata <- data.frame("AR" = 0, "LL"= 0, "FS" = 0, "IR" =0)
mydata$IR <- .6752 + .00000138 * mydata$AR + .001372 * mydata$LL -.0008759 * mydata$FS
mydata

#Library
library("shiny")

# Define UI for regression
ui <- fluidPage(
  pageWithSidebar(
    
    # Application title
    headerPanel("Interest Rate Calculator"),
    
    sidebarPanel(
      numericInput("AR", "Enter Amount Requested:", 0),
      numericInput("LL", "Enter Loan Length:", 0),
      numericInput("FS", "Enter FICO Score:", 0)
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define server logic
server<- (function(input, output) {
  
  newdata <- reactive({
    mydata$AR <- input$AR
    mydata$LL <- input$LL
    mydata$FS <- input$FS
    mydata$IR <- .6752 + .00000138 * input$AR  + .001372 * input$LL -.0008759 * input$FS

    return(mydata)
  })
  
  output$table <- renderTable({
    newdata()
  })
  
})

shinyApp(ui = ui, server = server)