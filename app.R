library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(shinyTime)
source("gg_field.R")

nfl_abbrevs <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", 
                 "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "MIA", "MIN", 
                 "NE", "NO", "NYG", "NYJ", "LV", "PHI", "PIT", "LAC", "SF", "SEA", 
                 "LAR", "TB", "TEN", "WAS")

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Punt Decision Calculator"),
  
  # Add shinyjs to enable/disable the input fields
  useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(
      # Use numericInput with a min and max value
      numericInput("punt_yardline", "Punting Team Yardline:", value = 25, min = 0, max = 99),
      numericInput("punt_length", "Punt Length:", value = 0, min = 0, max = 99),
      checkboxInput("outbounds", "Out of Bounds", value = 0),
      checkboxInput("touchback", "Touchback", value = 0),
      checkboxInput("receive_2h_ko", "Receive 2nd Half Kickoff", value = 0),
      selectInput("home_team", "Home Team", nfl_abbrevs),
      selectInput("posteam", "Possession Team", nfl_abbrevs),
      numericInput("score_differential", "Score Differential", value = 0, min = 0, max = 1000),
      selectInput("quarter", "Quarter", c(1,2,3,4,5)),
      timeInput("game_time", "Game Time:", value = hms::as.hms("00:15:00")),
      numericInput("spread_line", "Spread Line", value = 0, min = -1000, max = 1000),
      selectInput("down", "Down", c(1,2,3,4)),
      numericInput("ydstogo", "Yards to Go", value = 10, min = 0, max = 100),
      numericInput("yardline", "Yardline", value = 25, min = 0, max = 50),
      checkboxInput("posteam_yard_side", "Possession Team Side", value = 0),
      numericInput("posteam_timeouts_remaining", "Posession Team Timeouts Remaining", value = 3, min = 0, max = 3),
      numericInput("defteam_timeouts_remaining", "Defensive Team Timeouts Remaining", value = 3, min = 0, max = 3),
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
    
    print(input$receive_2h_ko)
    receive_2h_ko <- ifelse(input$receive_2h_ko == TRUE, 1, 0)

    # convert time to seconds
    half_secs <- 900
    game_secs <- 900
    
    data <- tibble::tibble(
      'receive_2h_ko' = receive_2h_ko, 
      'home_team' = 1, #input$home_team, 
      'posteam' = 1, #input$posteam,
      'score_differential' = input$score_differential, 
      'half_seconds_remaining' = half_secs, 
      'game_seconds_remaining' = game_secs, 
      'spread_line' = input$spread_line,
      'down' = input$down, 
      'ydstogo' = input$ydstogo, 
      'yardline_100' = input$yardline, 
      'posteam_timeouts_remaining' = input$posteam_timeouts_remaining, 
      'defteam_timeouts_remaining' = input$defteam_timeouts_remaining
    )
    
    print('test')
    
    wp <- nflfastR::calculate_win_probability(data) %>% dplyr::select(wp)
    

    # Get the values from the input fields
    num1 <- input$num1
    num2 <- input$num2
    
    # Calculate the product
    product <- num1 * num2
    
    # Display the result
    output$result <- renderText({
      paste("Win Probability =", wp)
    })
  })
  
  # Use shinyjs to disable/enable inputs based on value
  # observe({
  #   if (input$num1 > 4 || input$num1 < 1) {
  #     shinyjs::disable("calculate")
  #   } else {
  #     shinyjs::enable("calculate")
  #   }
  # })
  # 
  # observe({
  #   if (input$num2 > 10 || input$num2 < 0) {
  #     shinyjs::disable("calculate")
  #   } else {
  #     shinyjs::enable("calculate")
  #   }
  # })

}

# Run the Shiny app
shinyApp(ui, server)
