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
  
  splitLayout(
    style = "background-color: $F1F1F1", 
    cellWidths = c("32.5%", "32.5%", "35%"),
    div(
      style = "background-color: #F1F1F1; height: 670px; padding: 5px",
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
      timeInput("game_time", "Game Time:", value = hms::as.hms("00:15:00"))
    ),
    div(
      style = "background-color: #F1F1F1; height: 670px; padding: 5px",
      numericInput("spread_line", "Spread Line", value = 0, min = -1000, max = 1000),
      selectInput("down", "Down", c(1,2,3,4)),
      numericInput("ydstogo", "Yards to Go", value = 10, min = 0, max = 100),
      numericInput("yardline", "Yardline", value = 25, min = 0, max = 100),
      checkboxInput("posteam_yard_side", "Possession Team Side", value = 0),
      numericInput("posteam_timeouts_remaining", "Posession Team Timeouts Remaining", value = 3, min = 0, max = 3),
      numericInput("defteam_timeouts_remaining", "Defensive Team Timeouts Remaining", value = 3, min = 0, max = 3),
      br(),
      actionButton("calculate", "Calculate Product")
    ),
    
    div(
      h4("Decision Output:"),
      verbatimTextOutput("result"),
      plotOutput("plot1", height = "582.5px", width = "500px")
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
    
    receive_2h_ko <- ifelse(input$receive_2h_ko == TRUE, 1, 0)
    
    # Convert time to a numeric value (total seconds)
    game_time <- input$game_time
    minute <- as.numeric(format(game_time, "%M"))
    second <- as.numeric(format(game_time, "%S"))
    
    quarter_time <- minute * 60 + second
    qtr <- as.numeric(input$quarter)
    qtr <- ifelse(qtr == 5, 4, qtr)
    quarters_rem <- 4 - qtr
    
    numeric_time <- quarters_rem * 900 + quarter_time
    
    half_secs <- ifelse(numeric_time > 900, 900, numeric_time)
    game_secs <- numeric_time
    
    # Convert yardline
    yardline <- ifelse(input$posteam_yard_side == 1, input$yardline + 60, input$yardline)
    
    # Get field colors
    team_field <- merge(nflfastR::teams_colors_logos, data.frame(team_abbr = input$home_team))
    
    df <- data.frame(x=c(yardline + 10, input$punt_yardline + 10), y=c(25, 28))
    
    output$plot1 <- renderPlot({
      ggplot(df, aes(x,y)) +
        gg_field(direction = "vert", buffer = 2,
                 field_color = team_field$team_color,
                 sideline_color = team_field$team_color2,
                 endzone_color = team_field$team_color) + 
        geom_point(color = 'yellow', size = 5)
    })
    
    # WIN PROBABILITY FUNCTION
    data_as_played <- tibble::tibble(
      'receive_2h_ko' = as.numeric(input$receive_2h_ko), 
      'home_team' = input$home_team, 
      'posteam' = input$posteam,
      'score_differential' = input$score_differential, 
      'half_seconds_remaining' = half_secs, 
      'game_seconds_remaining' = game_secs, 
      'spread_line' = input$spread_line,
      'down' = as.numeric(input$down), 
      'ydstogo' = as.numeric(input$ydstogo), 
      'yardline_100' = yardline, 
      'posteam_timeouts_remaining' = input$posteam_timeouts_remaining, 
      'defteam_timeouts_remaining' = input$defteam_timeouts_remaining
    )
    
    # as played
    wp <- nflfastR::calculate_win_probability(data_as_played) %>% dplyr::select(wp)
    wp <- paste(round(wp * 100, 2), "%")
    
    data_pred <- tibble::tibble(
      'receive_2h_ko' = as.numeric(input$receive_2h_ko), 
      'home_team' = input$home_team, 
      'posteam' = input$posteam,
      'score_differential' = input$score_differential, 
      'half_seconds_remaining' = half_secs, 
      'game_seconds_remaining' = game_secs, 
      'spread_line' = input$spread_line,
      'down' = as.numeric(input$down), 
      'ydstogo' = as.numeric(input$ydstogo), 
      'yardline_100' = as.numeric(input$punt_yardline) + as.numeric(input$punt_length), 
      'posteam_timeouts_remaining' = input$posteam_timeouts_remaining, 
      'defteam_timeouts_remaining' = input$defteam_timeouts_remaining
    )
    
    # predicted
    wp_pred <- nflfastR::calculate_win_probability(data_pred) %>% dplyr::select(wp)
    wp_pred <- paste(round(wp_pred * 100, 2), "%")
    
    # Display the result
    output$result <- renderText({
      text1 <- paste(input$posteam, "Win Probability - as played =", wp) 
      text2 <- paste(input$posteam, "Win Probability - fair caught (same for now) =", wp_pred)
      
      paste(text1, text2, sep = '\n')
    })
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
