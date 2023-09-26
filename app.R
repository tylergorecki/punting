library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
source("gg_field.R")

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Punt Decision Calculator"),
  
  # Add shinyjs to enable/disable the input fields
  useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(
      # Use numericInput with a min and max value
      numericInput("num1", "Punting Team Yardline:", value = 25, min = 0, max = 99),
      checkboxInput("check1", "Punt Team Side", value = 0),
      numericInput("num2", "Punt Length:", value = 0, min = 0, max = 99),
      checkboxInput("check2", "Out of Bounds", value = 0),
      checkboxInput("check3", "Touchback", value = 0),
      br(),
      actionButton("calculate", "Calculate Product")
    ),
    
    mainPanel(
      h4("Decision Output:"),
      verbatimTextOutput("result"),
      plotOutput("plot1", height = "650px")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    ggplot() +
    gg_field(direction = "vert", buffer = 2)
  })  
  observeEvent(input$calculate, {
    # Get the values from the input fields
    num1 <- input$num1
    num2 <- input$num2
    
    # Calculate the product
    product <- num1 * num2
    
    # Display the result
    output$result <- renderText({
      paste("Product =", product)
    })
  })
  
  # Use shinyjs to disable/enable inputs based on value
  observe({
    if (input$num1 > 4 || input$num1 < 1) {
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("calculate")
    }
  })
  
  observe({
    if (input$num2 > 10 || input$num2 < 1) {
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("calculate")
    }
  })

}

# Run the Shiny app
shinyApp(ui, server)
