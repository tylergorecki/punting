# This is the Script to Calculate EP & WP For Downed Punts

# Installing Packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggplot2")
install.packages("rlist")

# Loading Libraries
library(dplyr)
library(tidyverse)
library(nflfastR)
library(ggplot2)
library(rlist)

# Loading in CSV Files
downedPunts <- read.csv("downed.csv")

# Loading in 2017-2021 Games for ID Parsing Purposes
schedule <- fast_scraper_schedules(2018:2021)
schedule$old_game_id <- as.numeric(schedule$old_game_id)
pbp <- load_pbp(2018:2021)
pbp$old_game_id <- as.numeric(pbp$old_game_id)

# Running a For Loop to Acquire the Next Play
next_play_df <- data.frame()
actual_play_id = list()
actual_game_id = list()
for (i in 1:nrow(downedPunts)) {
  current_game <- as.numeric(downedPunts$gameId[i])
  current_play <- as.numeric(downedPunts$playId[i])
  row_val <- which(pbp$old_game_id == current_game & pbp$play_id == current_play)
  next_play_df <- rbind(next_play_df, pbp[row_val + 1, ])
  j <- 0
  actual_play_id <- append(actual_play_id, current_play)
  actual_game_id <- append(actual_game_id, current_game)
  j <- j + 1
}
next_play_df$actual_game_id <- actual_game_id
next_play_df$actual_play_id <- actual_play_id

# Loading in Tracking Data to Acquire Ball Landing Position
track2018 <- read.csv("track2018punts.csv")
track2019 <- read.csv("track2019punts.csv")
track2020 <- read.csv("track2020punts.csv")
trackingData <- rbind(track2018, track2019, track2020)

# Parsing Out Where the Punt Landed
trackingData_land <- trackingData %>%
  filter(event == "punt_land", displayName == "football")

# Cleaning Data to Calculate Predicted EP & WP
downedPunts_pred <- next_play_df[, c(3, 1, 8, 12, 22, 26, 4, 60, 15, 16, 54, 55, 73, 93, 373, 374)]
downedPunts_pred$actual_game_id <- as.numeric(downedPunts_pred$actual_game_id)
downedPunts_pred$actual_play_id <- as.numeric(downedPunts_pred$actual_play_id)
downedPunts_pred <- left_join(downedPunts_pred, schedule[, c("season", "spread_line", "old_game_id")],
                              by = c("old_game_id" = "old_game_id"))
downedPunts_unique <- downedPunts[match(unique(downedPunts$gameId), downedPunts$gameId), ]
downedPunts_pred <- left_join(downedPunts_pred, downedPunts_unique[, c("gameId", "Roof", "receive_2h_ko")],
                              by = c("old_game_id" = "gameId"))
downedPunts_pred <- left_join(downedPunts_pred, trackingData_land[, c(2, 3, 16, 17)],
                              by = c("actual_game_id" = "gameId", "actual_play_id" = "playId"))

# Creating a Model to Predict Yard Line of Next Punt
returnedPunts <- read.csv("returned.csv")
returnPred <- lm(kickReturnYardage ~ quarter + Minute + Second + yardsToGo + yardlineNumber +
                   preSnapHomeScore + preSnapVisitorScore + kickLength + 
                   HomeTORemaining + AwayTORemaining + Roof + receive_2h_ko, data = returnedPunts)

downedPunts_returned <- round(predict(returnPred, newdata = downedPunts))
downedPunts_pred$predicted_ret_yards <- downedPunts_returned
downedPunts_pred$pred_yardline100 <- downedPunts_pred$yardline_100 - downedPunts_pred$predicted_ret_yards

# Editing Column Names to Run nflFastR Functions
column_names <- c("gameId", "playId", "posteam", "actual_ydline_100", "down", "ydstogo", 
                  "home_team", "score_differential", "half_seconds_remaining", "game_seconds_remaining",
                  "posteam_timeouts_remaining", "defteam_timeouts_remaining", "actual_ep", "actual_wp",
                  "punt_gameId", "punt_playId", "season", "spread_line", "roof", "receive_2h_ko", 
                  "ball_land_x", "ball_land_y", "pred_ret_yards", "yardline_100")
colnames(downedPunts_pred) <- column_names

# Running nflFastR Functions
downedPunts_pred <- na.omit(downedPunts_pred)
pred_ep <- calculate_expected_points(downedPunts_pred)
downedPunts_pred$pred_ep <- pred_ep$ep
pred_wp <- calculate_win_probability(downedPunts_pred)
downedPunts_pred$pred_wp <- pred_wp$wp

# Writing a CSV File
write.csv(downedPunts_pred, "downed_ep_wp.csv")

# Graphing the Results
downedPunts_pred$ep_diff <- downedPunts_pred$actual_ep - downedPunts_pred$pred_ep
downedPunts_pred$wp_diff <- downedPunts_pred$actual_wp - downedPunts_pred$pred_wp
ep_diff_graph <- downedPunts_pred %>%
  ggplot(aes(x = actual_ep, y = pred_ep, color = ep_diff)) +
  geom_point() +
  guides(color = guide_legend(title = "Actual - Predicted EP")) +
  ggtitle("EP vs. Predicted EP") +
  xlab("Downed Punt EP") +
  ylab("Returned Punt EP")

wp_diff_graph <- downedPunts_pred %>%
  ggplot(aes(x = actual_wp, y = pred_wp, color = wp_diff)) +
  geom_point() +
  guides(color = guide_legend(title = "Actual - Predicted WP")) +
  ggtitle("WP vs. Predicted WP") +
  xlab("Downed Punt WP") +
  ylab("Returned Punt WP")

# Graphing Based Off of Landing Point

