library(shiny)
library(shinyjs)

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Punt Decision Calculator"),
  
  # Add shinyjs to enable/disable the input fields
  useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(
      # Use numericInput with a min and max value
      numericInput("num1", "Punting Team Yardline:", value = 1, min = 1, max = 4),
      numericInput("num2", "Ball landing x:", value = 1, min = 1, max = 4),
      numericInput("num3", "Enter Number 3:", value = 1, min = 1, max = 4),
      br(),
      actionButton("calculate", "Calculate Product")
    ),
    
    mainPanel(
      h4("Decision Output:"),
      verbatimTextOutput("result")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$calculate, {
    # Get the values from the input fields
    num1 <- input$num1
    num2 <- input$num2
    num3 <- input$num3
    
    # Calculate the product
    product <- num1 * num2 * num3
    
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
  
  observe({
    if (input$num3 > 4 || input$num3 < 1) {
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("calculate")
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)