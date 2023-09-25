# sample tracking data to give feel of what dataset looks like
tracking_brief <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/tracking-brief.csv")


events <- tracking_real[tracking_real$event=="punt" | 
                         tracking_real$event=="punt_received" |
                         tracking_real$event=="tackle" |
                         tracking_real$event=="out_of_bounds",]
justball_events <- tracking_real[tracking_real$displayName=="football",][c("x","y","event","playId", "gameId")]

real_ball <- justball_events %>% 
  filter(event != "") %>%
  group_by(gameId, playId) %>%
  select(gameId, playId, x, event)

ball_events <- justball_events %>%
  filter(event=="punt" | event=="tackle" | event=="touchback" | event=="punt_downed" |
           event=="out_of_bounds" | event=="fair_catch" | event=="punt_received" | 
           event=="touchdown" | event=="punt_muffed" | event=="punt_land") %>%
  group_by(gameId, playId) %>%
  select(gameId, playId, x, event)

ballplot <- ggplot(justball_events, aes(event,x, color = playId)) + geom_point()
ballplot

justball_events2369 <- justball_events[justball_events$playId==2369,]

punt_dist <- justball_events2369[justball_events2369$event == "punt",1] -
  justball_events2369[justball_events2369$event == "punt_received",1]

return_dist <- justball_events2369[justball_events2369$event == "tackle",1] -
  justball_events2369[justball_events2369$event == "punt_received",1]

punt_dist_fx <- function(g_id, p_id){
  row_id <- justball_events[justball_events$gameId==g_id & justball_events$playId==p_id,]
  punt <- row_id[row_id$event=="punt",1] - row_id[row_id$event=="punt_received",1]
  abs(punt)
}

punt1 <- sapply(unique(justball_events$playId),punt_dist_fx)
punt1 <- punt1[lapply(punt1, length) > 0]

return_dist_fx <- function(id){
  row_id <- justball_events[justball_events$playId==id,]
  punt <- row_id[row_id$event=="tackle",1] - row_id[row_id$event=="punt_received",1]
  if (length(punt)==0){
    punt <- row_id[row_id$event=="out_of_bounds",1] - row_id[row_id$event=="punt_received",1]
  }
  if (length(punt)==0){
    punt <- row_id[row_id$event=="touchdown",1] - row_id[row_id$event=="punt_received",1]
  }
  abs(punt)
}

ret1 <- sapply(unique(justball_events$playId),return_dist_fx)
ret1 <- ret1[lapply(ret1, length) > 0]

comb1 <- cbind(punt1,ret1)

games <- unique(ball_events$gameId)
plays <- unique(ball_events$playId)

###

# good loop
out <- data.frame()
for (x in unique(ball_events$gameId)) {
  for (y in unique(ball_events$playId)) {
    event_play <- ball_events[(ball_events$gameId==x & ball_events$playId==y),]
    # punt
    punt = event_play[event_play$event=="punt",3][[1]] - event_play[event_play$event=="punt_received",3][[1]]
    if (length(punt)==0) {
      punt <- 0
    }
    # return
    return = event_play[event_play$event=="tackle",3][[1]] - event_play[event_play$event=="punt_received",3][[1]]
    if (length(return)==0){
      return <- event_play[event_play$event=="out_of_bounds",3][[1]] - event_play[event_play$event=="punt_received",3][[1]]
    }
    if (length(return)==0){
      return <- event_play[event_play$event=="touchdown",3][[1]] - event_play[event_play$event=="punt_received",3][[1]]
    }
    
    out[nrow(out)+1,c(1,2)] = c(abs(punt),abs(return))
  }
}
out

row_sub <- apply(out, 1, function(row) all(row != 0))
out_punt_received <- out[row_sub,]

out_punt_received %>% 
  ggplot(aes(V1,V2)) +
  geom_point()

###

for (x in unique(ball_events$gameId)) {
  print("gameId:",x)
  for (y in unique(ball_events$playId)) {
    print("playId:",y)
  }
}

testx <- c(1,2,3)
test <- data.frame()
for (t in testx) {
  test[nrow(test)+1,c(1,2)] = data.frame(1,2)
}
test


ballplot + geom_point() + facet_wrap(~playId)

justball_events %>% 
  group_by(playId)

justball_events36 <- justball_events %>% filter(playId == 36)
justball_events36[2,1] - justball_events36[1,1]
justball_events36[2,1] - justball_events36[4,1]


ball_events2 <- justball_events %>%
  filter(event=="punt" | event=="touchback" | event=="punt_downed" |
           event=="out_of_bounds" | event=="punt_land" | event=="punt_received")

ballplot2 <- ggplot(ball_events2, aes(x,y, color = event)) + geom_point()
ballplot2
abline(v = 10)

################################################################################

# dataset of 50 punts for quicker running
punts50 <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/track50punts.csv")
# remove all data points except for event (only include events to determine punt distance)
punts50 <- punts50 %>% filter(event != "None", displayName == "football")

# good loop second

out2 <- data.frame()
for (x in unique(punts50$gameId)) {
  for (y in unique(punts50$playId)) {
    event_play <- punts50[(punts50$gameId==x & punts50$playId==y),]
    test <- event_play$playDirection[1]=="left"
    out2[nrow(out2)+1,c(1)] = test
  }
}
out2

out2 <- data.frame()
punt_func <- function(datain) {
  out2 <- data.frame()
  # loop through unique games
  for (x in unique(datain$gameId)) {
    # loop through unique plays in each game
    for (y in unique(datain$playId)) {
      # subset data to only look at play y in game x
      event_play <- datain[(datain$gameId==x & datain$playId==y),]
      # calculate length of punt (finding difference in x value)
      punt = event_play[event_play$event=="punt",2] - event_play[event_play$event=="punt_received",2]
      if (length(punt)==0) {
        punt <- 150
      }
      # calculate length of return (again difference in x), depending on 
      return = event_play[event_play$event=="tackle",2] - event_play[event_play$event=="punt_received",2]
      if (length(return)==0){
        return <- event_play[event_play$event=="out_of_bounds",2] - event_play[event_play$event=="punt_received",2]
      }
      if (length(return)==0){
        return <- event_play[event_play$event=="touchdown",2] - event_play[event_play$event=="punt_received",2]
      }
      # add abs of punt and return distances to dataframe
      if (abs(punt) != 150) {
        out2[nrow(out2)+1,c(1,2)] = c(abs(punt),abs(return))
      }
    }
  }
  return(out2)
}
tracking50 <- punt_func(punts50)
tracking50 %>% 
  ggplot(aes(V1,V2)) +
  geom_point()

################################################################################

# full tracking datasets from 2018-20 seasons
tracking_real20 <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/tracking2020.csv")
tracking_real19 <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/tracking2019.csv")
tracking_real18 <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/tracking2018.csv")

# subsetted datasets
tracking2020 <- tracking_real20 %>% filter(event != "None", displayName == "football")
tracking2019 <- tracking_real19 %>% filter(event != "None", displayName == "football")
tracking2018 <- tracking_real18 %>% filter(event != "None", displayName == "football")

# use function to return all punt and return distances for each season
# punt_func_direc BETTER I THINK
# plot20 <- punt_func(tracking2020)
# plot19 <- punt_func(tracking2019)
# plot18 <- punt_func(tracking2018)

# plot punt distance vs return distance, 
plot20 %>% 
  ggplot(aes(V1,V2)) +
  geom_point() + 
  scale_x_continuous(limits = c(40,80))
plot19 %>% 
  ggplot(aes(V1,V2)) +
  geom_point() + 
  scale_x_continuous(limits = c(40,80))
plot18 %>%
  ggplot(aes(V1,V2)) +
  geom_point() + 
  scale_x_continuous(limits = c(40,80))


# use this function instead of just punt_func()
punt_func_direc <- function(datain) {
  out2 <- data.frame()
  for (x in unique(datain$gameId)) {
    for (y in unique(datain$playId)) {
      event_play <- datain[(datain$gameId==x & datain$playId==y),]
      # punt distance
      punt = event_play[event_play$event=="punt",2] - event_play[event_play$event=="punt_received",2]
      if (length(punt)==0) {
        punt <- 0
      }
      # return distances, calculated dependent upon direction of kick
      if (length(event_play$playDirection)!=0) {
        if (event_play$playDirection[1]=="left") {
          return = event_play[event_play$event=="tackle",2] - event_play[event_play$event=="punt_received",2]
          if (length(return)==0){
            return <- event_play[event_play$event=="out_of_bounds",2] - event_play[event_play$event=="punt_received",2]
          }
          if (length(return)==0){
            return <- event_play[event_play$event=="touchdown",2] - event_play[event_play$event=="punt_received",2]
          }
          if (abs(punt) != 0) {
            out2[nrow(out2)+1,c(1,2)] = c(abs(punt),return)
          }
        } else {
          return = event_play[event_play$event=="punt_received",2] - event_play[event_play$event=="tackle",2]
          if (length(return)==0){
            return <- event_play[event_play$event=="punt_received",2] - event_play[event_play$event=="out_of_bounds",2]
          }
          if (length(return)==0){
            return <- event_play[event_play$event=="punt_received",2] - event_play[event_play$event=="touchdown",2]
          }
          if (abs(punt) != 0) {
            out2[nrow(out2)+1,c(1,2)] = c(abs(punt),return)
          }
        }
      }
    }
  }
  return(out2)
}

punts_final <- punt_func_direc(punts50)
punts_final %>% 
  ggplot(aes(V1,V2)) + 
  geom_point()

plot20_direc <- punt_func_direc(tracking2020)
plot19_direc <- punt_func_direc(tracking2019)
plot18_direc <- punt_func_direc(tracking2018)

plot20_direc %>% 
  ggplot(aes(V1,V2)) +
  geom_point() + 
  geom_smooth(method='lm', color = 'red') +
  scale_x_continuous(limits = c(30,90))

plot19_direc %>% 
  ggplot(aes(V1,V2)) +
  geom_point() + 
  geom_smooth(method='lm', color = 'red') +
  scale_x_continuous(limits = c(30,90))

plot18_direc %>%
  ggplot(aes(V1,V2)) +
  geom_point() + 
  geom_smooth(method='lm', color = 'red') +
  scale_x_continuous(limits = c(30,90))

# mod <- lm(V2~V1, data = plot20_direc)
# abline(V2~V1, data = plot20_direc)

# lm_data <- augm ent(mod)
# plot.new()

pbp2020 <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/pbp-2020.csv")
plays <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/plays.csv")
pbp20Timeouts <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/pbp20Timeouts.csv")
pbp19Timeouts <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/pbp19Timeouts.csv")
pbp18Timeouts <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/pbp18Timeouts.csv")

# punts50 <- punts50 %>%
  mutate(HomeScore = 0, AwayScore = 0)

#for (x in seq(1,nrow(punts50))) {
#  game <- punts50[x,'gameId']
#  play <- punts50[x,'playId']
#  punts50[x,'HomeScore'] = plays[(plays$gameId==game & plays$playId==play),'preSnapHomeScore']
#  punts50[x,'AwayScore'] = plays[(plays$gameId==game & plays$playId==play),'preSnapVisitorScore']
#}

tracking_real20 <- tracking_real20 %>%
  mutate(HomeScore = 0, VisitorScore = 0)

for (x in seq(1,nrow(tracking_real20))) {
  game <- tracking_real20[x,'gameId']
  play <- tracking_real20[x,'playId']
  tracking_real20[x,'HomeScore'] = plays[(plays$gameId==game & plays$playId==play),'preSnapHomeScore']
  tracking_real20[x,'VisitorScore'] = plays[(plays$gameId==game & plays$playId==play),'preSnapVisitorScore']
}

event_play <- datain[(datain$gameId==x & datain$playId==y),]
plays_play <- plays[(plays$gameId==x & plays$playId==y),]
event_play$HomeScore <- plays_play$preSnapHomeScore
event_play$AwayScore <- plays_play$preSnapVisitorScore

tracking20_scores <- merge(tracking_real20, plays, by = c('gameId', 'playId'))
tracking19_scores <- merge(tracking_real19, plays, by = c('gameId', 'playId'))
tracking18_scores <- merge(tracking_real18, plays, by = c('gameId', 'playId'))

half2kicks <- plays[plays$quarter == 3 & plays$gameClock == '15:00:00',]

# possession team during kicking plays (all in plays dataset) is the team kicking

halfkicks_team <- half2kicks %>%
  select(gameId, possessionTeam) %>%
  rename(kick_2nd_half = possessionTeam)

plays_2nd_half_kicks <- merge(plays, halfkicks_team, by = 'gameId')
plays_2nd_half_kicks <- plays_2nd_half_kicks %>%
  mutate(possTeam_receives = case_when(
    quarter >= 3 ~ 0, 
    # team receiving during play is kicking second half kickoff
    possessionTeam == kick_2nd_half ~ 1, 
    # team kicking during play is receiving second half kickoff
    possessionTeam != kick_2nd_half ~ 0
  ))



###########################################################################
###########################################################################
###########################################################################

##### Must run

returned <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/returned.csv")
games <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/gamesRSHKO.csv")

returned <- merge(returned, games, by = 'gameId')

returned$EPasplayed <- 0
returned$EPifFC <- 0
returned$WPasplayed <- 0
returned$WPifFC <- 0

returned_head <- as.data.frame(head(returned))
returned_head$EPasplayed <- 0
returned_head$EPifFC <- 0
returned_head$WPasplayed <- 0
returned_head$WPifFC <- 0

##### Testing

#dataTest <- tibble::tibble(
#  'season' = 2018, 
#  'home_team' = 'PHI', 
#  'posteam' = 'PHI',
#  'roof' = 'open', 
#  'half_seconds_remaining' = 1800, 
#  'yardline_100' = 75, 
#  'down' = 1, 
#  'ydstogo' = 10, 
#  'posteam_timeouts_remaining' = 3, 
#  'defteam_timeouts_remaining' = 3
#)

#nflfastR::calculate_expected_points(dataTest) %>%
#  dplyr::select(season, yardline_100, td_prob, ep)

##### EPasplayed

for (i in seq(1, nrow(returned))) {
  
  half_secs <- 0
  if (returned[i, 'quarter'] == 1 | returned[i, 'quarter'] == 3) {
    half_secs <- (15 + returned[i, 'Minute']) * 60 + returned[i, 'Second']
  } else {
    half_secs <- returned[i, 'Minute'] * 60 + returned[i, 'Second']
  }
  
  yardline <- 0
  if (returned[i, 'yardlineNumber'] == (returned[i, 'absoluteYardlineNumber'] - 10)) {
    yardline <- returned[i, 'yardlineNumber'] + returned[i, 'playResult']
  } else {
    yardline <- 110 - returned[i, 'absoluteYardlineNumber'] + returned[i, 'playResult']
  }
  
  posTimeouts <- ''
  defTimeouts <- ''
  if (returned[i, 'homeTeamAbbr'] == returned[i, 'possessionTeam']) {
    posTimeouts <- returned[i, 'HomeTORemaining']
    defTimeouts <- returned[i, 'AwayTORemaining']
  } else {
    posTimeouts <- returned[i, 'AwayTORemaining']
    defTimeouts <- returned[i, 'HomeTORemaining']
  }
  
  data <- tibble::tibble(
    'season' = returned[i, 'season'],
    'home_team' = returned[i, 'homeTeamAbbr'], 
    'posteam' = returned[i, 'possessionTeam'],
    'roof' = returned[i, 'Roof'], 
    'half_seconds_remaining' = half_secs, 
    'yardline_100' = yardline, 
    'down' = 1, 
    'ydstogo' = 10, 
    'posteam_timeouts_remaining' = posTimeouts, 
    'defteam_timeouts_remaining' = defTimeouts
  )
  
  func <- nflfastR::calculate_expected_points(data)
  ep <- func %>% dplyr::select(ep)
  returned[i, 'EPasplayed'] <- ep
  
}

##### EPifFC

for (i in seq(1, nrow(returned))) {
  
  half_secs <- 0
  if (returned[i, 'quarter'] == 1 | returned[i, 'quarter'] == 3) {
    half_secs <- (15 + returned[i, 'Minute']) * 60 + returned[i, 'Second']
  } else {
    half_secs <- returned[i, 'Minute'] * 60 + returned[i, 'Second']
  }
  
  yardline <- 0
  if (returned[i, 'yardlineNumber'] == (returned[i, 'absoluteYardlineNumber'] - 10)) {
    yardline <- returned[i, 'yardlineNumber'] + returned[i, 'kickLength']
  } else {
    yardline <- 110 - returned[i, 'absoluteYardlineNumber'] + returned[i, 'kickLength']
  }
  
  posTimeouts <- ''
  defTimeouts <- ''
  if (returned[i, 'homeTeamAbbr'] == returned[i, 'possessionTeam']) {
    posTimeouts <- returned[i, 'HomeTORemaining']
    defTimeouts <- returned[i, 'AwayTORemaining']
  } else {
    posTimeouts <- returned[i, 'AwayTORemaining']
    defTimeouts <- returned[i, 'HomeTORemaining']
  }
  
  data <- tibble::tibble(
    'season' = returned[i, 'season'],
    'home_team' = returned[i, 'homeTeamAbbr'], 
    'posteam' = returned[i, 'possessionTeam'],
    'roof' = returned[i, 'Roof'], 
    'half_seconds_remaining' = half_secs, 
    'yardline_100' = yardline, 
    'down' = 1, 
    'ydstogo' = 10, 
    'posteam_timeouts_remaining' = posTimeouts, 
    'defteam_timeouts_remaining' = defTimeouts
  )
  
  func <- nflfastR::calculate_expected_points(data)
  ep <- func %>% dplyr::select(ep)
  returned[i, 'EPifFC'] <- ep
  
}

##### Testing

#dataTest2 <- tibble::tibble(
# 'receive_2h_ko' = 0, 
#  'home_team' = 'PHI', 
# 'posteam' = 'PHI',
#  'score_differential' = 0, 
# 'half_seconds_remaining' = 1800, 
#  'game_seconds_remaining' = 3600, 
# 'spread_line' = 2,
#  'down' = 1, 
# 'ydstogo' = 10, 
#  'yardline_100' = 30, 
# 'posteam_timeouts_remaining' = 3, 
#  'defteam_timeouts_remaining' = 3
#)

#nflfastR::calculate_win_probability(dataTest2) %>%
#  dplyr::select(spread_line, wp, vegas_wp)

##### WPasplayed

for (i in seq(1, nrow(returned))) {
  
  half_secs <- 0
  if (returned[i, 'quarter'] == 1) {
    half_secs <- (15 + returned[i, 'Minute']) * 60 + returned[i, 'Second']
    game_secs <- 1800 + half_secs
  } else if (returned[i, 'quarter'] == 2) {
    half_secs <- returned[i, 'Minute'] * 60 + returned[i, 'Second']
    game_secs <- 1800 + half_secs
  } else if (returned[i, 'quarter'] == 3) {
    half_secs <- (15 + returned[i, 'Minute']) * 60 + returned[i, 'Second']
    game_secs <- half_secs
  } else {
    half_secs <- returned[i, 'Minute'] * 60 + returned[i, 'Second']
    game_secs <- half_secs
  }
  
  yardline <- 0
  if (returned[i, 'yardlineNumber'] == (returned[i, 'absoluteYardlineNumber'] - 10)) {
    yardline <- returned[i, 'yardlineNumber'] + returned[i, 'playResult']
  } else {
    yardline <- 110 - returned[i, 'absoluteYardlineNumber'] + returned[i, 'playResult']
  }
  
  posTimeouts <- ''
  defTimeouts <- ''
  score_diff <-
  if (returned[i, 'homeTeamAbbr'] == returned[i, 'possessionTeam']) {
    posTimeouts <- returned[i, 'HomeTORemaining']
    defTimeouts <- returned[i, 'AwayTORemaining']
    score_diff <- returned[i, 'preSnapHomeScore'] - returned[i, 'preSnapVisitorScore']
  } else {
    posTimeouts <- returned[i, 'AwayTORemaining']
    defTimeouts <- returned[i, 'HomeTORemaining']
    score_diff <- returned[i, 'preSnapVisitorScore'] - returned[i, 'preSnapHomeScore']
  }
  
  data <- tibble::tibble(
    'receive_2h_ko' = returned[i, 'receive_2h_ko'], 
    'home_team' = returned[i, 'homeTeamAbbr'], 
    'posteam' = returned[i, 'possessionTeam'],
    'score_differential' = 0, 
    'half_seconds_remaining' = half_secs, 
    'game_seconds_remaining' = game_secs, 
    'spread_line' = 0,
    'down' = 1, 
    'ydstogo' = 10, 
    'yardline_100' = yardline, 
    'posteam_timeouts_remaining' = posTimeouts, 
    'defteam_timeouts_remaining' = defTimeouts
  )
  
  func <- nflfastR::calculate_win_probability(data)
  wp <- func %>% dplyr::select(wp)
  returned[i, 'WPasplayed'] <- wp
  
}

##### WPifFC

for (i in seq(1, nrow(returned))) {
  
  half_secs <- 0
  if (returned[i, 'quarter'] == 1) {
    half_secs <- (15 + returned[i, 'Minute']) * 60 + returned[i, 'Second']
    game_secs <- 1800 + half_secs
  } else if (returned[i, 'quarter'] == 2) {
    half_secs <- returned[i, 'Minute'] * 60 + returned[i, 'Second']
    game_secs <- 1800 + half_secs
  } else if (returned[i, 'quarter'] == 3) {
    half_secs <- (15 + returned[i, 'Minute']) * 60 + returned[i, 'Second']
    game_secs <- half_secs
  } else {
    half_secs <- returned[i, 'Minute'] * 60 + returned[i, 'Second']
    game_secs <- half_secs
  }
  
  yardline <- 0
  if (returned[i, 'yardlineNumber'] == (returned[i, 'absoluteYardlineNumber'] - 10)) {
    yardline <- returned[i, 'yardlineNumber'] + returned[i, 'kickLength']
  } else {
    yardline <- 110 - returned[i, 'absoluteYardlineNumber'] + returned[i, 'kickLength']
  }
  
  posTimeouts <- ''
  defTimeouts <- ''
  score_diff <-
    if (returned[i, 'homeTeamAbbr'] == returned[i, 'possessionTeam']) {
      posTimeouts <- returned[i, 'HomeTORemaining']
      defTimeouts <- returned[i, 'AwayTORemaining']
      score_diff <- returned[i, 'preSnapHomeScore'] - returned[i, 'preSnapVisitorScore']
    } else {
      posTimeouts <- returned[i, 'AwayTORemaining']
      defTimeouts <- returned[i, 'HomeTORemaining']
      score_diff <- returned[i, 'preSnapVisitorScore'] - returned[i, 'preSnapHomeScore']
    }
  
  data <- tibble::tibble(
    'receive_2h_ko' = returned[i, 'receive_2h_ko'], 
    'home_team' = returned[i, 'homeTeamAbbr'], 
    'posteam' = returned[i, 'possessionTeam'],
    'score_differential' = 0, 
    'half_seconds_remaining' = half_secs, 
    'game_seconds_remaining' = game_secs, 
    'spread_line' = 0,
    'down' = 1, 
    'ydstogo' = 10, 
    'yardline_100' = yardline, 
    'posteam_timeouts_remaining' = posTimeouts, 
    'defteam_timeouts_remaining' = defTimeouts
  )
  
  func <- nflfastR::calculate_win_probability(data)
  wp <- func %>% dplyr::select(wp)
  returned[i, 'WPifFC'] <- wp
  
}

View(returned)

# EP comparison
returned %>% ggplot(aes(x = EPasplayed, y = EPifFC)) +
  geom_point()
#abline(a=0,b=1)
  #xlim(-1.25, 6.3) + ylim(-1.25,6.3)

# WP comparison
returned %>% ggplot(aes(x = WPasplayed, y = WPifFC)) +
  geom_point()
  #xlim(.35, .76) + ylim(.35,.75)

write.csv(returned, '/Users/tylergorecki/Desktop/SASL/Fair Catch Project/returned-epwp.csv')

#####################################################################
#####################################################################
#####################################################################

# only have to run this to get ep and wp for returned punts (already ran loops)
returned_catches <- read.csv("/Users/tylergorecki/Desktop/SASL/Fair Catch Project/returned-epwp.csv")

# adding yardline each punt is returned at for further analysis
for (i in seq(1, nrow(returned_catches))) {
  if (returned_catches[i, 'yardlineNumber'] == (returned_catches[i, 'absoluteYardlineNumber'] - 10)) {
    returned_catches[i, 'catchYardline'] <- returned_catches[i, 'yardlineNumber'] + returned_catches[i, 'kickLength']
  } else {
    returned_catches[i, 'catchYardline'] <- 110 - returned_catches[i, 'absoluteYardlineNumber'] + returned_catches[i, 'kickLength']
  }
}

for (i in seq(1, nrow(returned_catches))) {
  if ((returned_catches[i, "catchYardline"] > 90) & (returned_catches[i, "catchYardline"] <= 100)) {
    returned_catches[i, 'catchGroup'] <- '0-10'
  } else if ((returned_catches[i, "catchYardline"] > 80) & (returned_catches[i, "catchYardline"] <= 90)) {
    returned_catches[i, 'catchGroup'] <- '10-20'
  } else if ((returned_catches[i, "catchYardline"] > 70) & (returned_catches[i, "catchYardline"] <= 80)) {
    returned_catches[i, 'catchGroup'] <- '20-30'
  } else if ((returned_catches[i, "catchYardline"] > 60) & (returned_catches[i, "catchYardline"] <= 70)) {
    returned_catches[i, 'catchGroup'] <- '30-40'
  } else if ((returned_catches[i, "catchYardline"] > 50) & (returned_catches[i, "catchYardline"] <= 60)) {
    returned_catches[i, 'catchGroup'] <- '40-50'
  } else {
    returned_catches[i, 'catchGroup'] <- 'opponent half'
  }
}

####################
# Comparisons based on catch location

# EP comparison
returned_catches %>% ggplot(aes(x = EPasplayed, y = EPifFC)) +
  geom_point() + facet_wrap(~catchGroup) + xlim(-1, 6) + ylim(-1,6)

# WP comparison
returned_catches %>% ggplot(aes(x = WPasplayed, y = WPifFC)) +
  geom_point() + facet_wrap(~catchGroup)
  xlim(.35,.75) + ylim(.35,.75)
####################


############
  
# Load the required libraries
library(ggplot2)

# Create a football field background image
field_image <- ggplot() +
  xlim(-100, 100) +
  ylim(-50, 50) +
  theme_void() +
  theme(plot.background = element_rect(fill = "green"), 
        panel.background = element_rect(fill = "green"), 
        plot.margin = margin(0, 0, 0, 0)) + 
  geom_rect(xmin = -100, xmax = 100, ymin = -50, ymax = 50, fill = NA, color = "black", size = 2)

# Overlay black field lines
field_image <- ggplot() +
  geom_segment(aes(x = -110, y = -50, xend = 110, yend = -50), color = "green", size = 280) +
  geom_segment(aes(x = 0, y = -50, xend = 0, yend = 50), color = "black", size = 1) +
  geom_segment(aes(x = -110, y = -50, xend = 110, yend = -50), color = "black", size = 1) +
  geom_segment(aes(x = -110, y = 50, xend = 110, yend = 50), color = "black", size = 1) + 
  geom_segment(aes(x = 110, y = -50, xend = 110, yend = 50), color = "black", size = 1) + 
  geom_segment(aes(x = -110, y = -50, xend = -110, yend = 50), color = "black", size = 1) + 
  geom_segment(aes(x = 100, y = -50, xend = 100, yend = 50), color = "black", size = 1) + 
  geom_segment(aes(x = -100, y = -50, xend = -100, yend = 50), color = "black", size = 1)

print(field_image)

# Create some sample points (replace with your own data)
points_data <- data.frame(x = c(0, 20, -20), y = c(0, 10, -10))

# Overlay points on the field
field_image <- field_image +
  geom_point(data = points_data, aes(x = x, y = y), color = "brown", fill = "brown", size = 4)

# Display the football field with points
print(field_image)
  
  