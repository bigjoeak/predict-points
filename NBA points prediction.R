library(hoopR)
library(tidyverse)
library(zoo)
library(caTools)
library(Metrics)
library(car)

#Get data for multiple seasons. Each season takes about 20 minutes to process
season.23 <- nba_schedule(league_id = "00", season=2023) %>%
  filter(season_type_id==2)
adv.box.23 <- lapply(season.23$game_id, nba_boxscoreadvancedv3)
trad.box.23 <- lapply(season.23$game_id, nba_boxscoretraditionalv3)

season.22 <- nba_schedule(league_id = "00", season=2022) %>%
  filter(season_type_id==2)
adv.box.22 <- lapply(season.22$game_id, nba_boxscoreadvancedv3)
trad.box.22 <- lapply(season.22$game_id, nba_boxscoretraditionalv3)

season.21 <- nba_schedule(league_id = "00", season=2021) %>%
  filter(season_type_id==2)
adv.box.21 <- lapply(season.21$game_id, nba_boxscoreadvancedv3)
trad.box.21 <- lapply(season.21$game_id, nba_boxscoretraditionalv3)

season.20 <- nba_schedule(league_id = "00", season=2020) %>%
  filter(season_type_id==2)
adv.box.20 <- lapply(season.20$game_id, nba_boxscoreadvancedv3)
trad.box.20 <- lapply(season.20$game_id, nba_boxscoretraditionalv3)

season.19 <- nba_schedule(league_id = "00", season=2019) %>% 
  filter(season_type_id==2)
adv.box.19 <- lapply(season.19$game_id, nba_boxscoreadvancedv3)
trad.box.19 <- lapply(season.19$game_id, nba_boxscoretraditionalv3)

#Function to process traditional box scores
process.trad.box <- function(df) {
  list(
    home.player = map_df(df, "home_team_player_traditional"),
    away.player = map_df(df, "away_team_player_traditional"),
    home.team = map_df(df, "home_team_totals_traditional"),
    away.team = map_df(df, "away_team_totals_traditional")
  )
}

#Function to process advanced box scores
process.adv.box <- function(df) {
  list(
    home.player = map_df(df, "home_team_player_advanced"),
    away.player = map_df(df, "away_team_player_advanced"),
    home.team = map_df(df, "home_team_totals_advanced"),
    away.team = map_df(df, "away_team_totals_advanced")
  )
}

adv.box.list <- list(adv.box.19 = adv.box.19, adv.box.20 = adv.box.20, adv.box.21 = adv.box.21, adv.box.22 = adv.box.22, adv.box.23 = adv.box.23)
trad.box.list <- list(trad.box.19 = trad.box.19, trad.box.20 = trad.box.20, trad.box.21 = trad.box.21, trad.box.22 = trad.box.22, trad.box.23 = trad.box.23)

adv.box <- lapply(adv.box.list, process.adv.box)
trad.box <- lapply(trad.box.list, process.trad.box)

#Function to join traditional and advanced box scores
join.box <- function(adv.df, trad.df) {
  adv.df$home.player <- adv.df$home.player %>%
    left_join(select(trad.df$home.player, game_id, person_id, 18:36), by = c("game_id", "person_id"))
  
  adv.df$home.team <- adv.df$home.team %>%
    left_join(select(trad.df$home.team, game_id, team_id, 10:28), by = c("game_id", "team_id"))
  
  adv.df$away.player <- adv.df$away.player %>%
    left_join(select(trad.df$away.player, game_id, person_id, 18:36), by = c("game_id", "person_id"))
  
  adv.df$away.team <- adv.df$away.team %>%
    left_join(select(trad.df$away.team, game_id, team_id, 10:28), by = c("game_id", "team_id"))
  
  return(adv.df)
}

joined.box <- mapply(join.box, adv.box, trad.box, SIMPLIFY = FALSE)


#Function to add indicator for Home/Away, identify opposing team, and clean up minutes format
cleanup <- function(df) {
  df$home.player<- df$home.player %>%
    mutate(Home.Away = 0) %>%
    rename(opponent_id = away_team_id) %>%
    select(-home_team_id) %>%
    mutate(minutes_split = str_split(minutes, ":", simplify = TRUE), minutes_numeric = as.numeric(minutes_split[, 1]) + as.numeric(minutes_split[, 2]) / 60) %>%
    select(-minutes_split)
    
  df$home.player<- df$home.player %>%
      select(-minutes) %>%
      rename(minutes = minutes_numeric) %>%
      replace_na(list(minutes=0))
  
  df$home.team <- df$home.team %>%
    mutate(Home.Away = 0) %>%
    rename(opponent_id = away_team_id) %>%
    select(-home_team_id) %>%
    mutate(minutes_split = str_split(minutes, ":", simplify = TRUE), minutes_numeric = as.numeric(minutes_split[, 1]) + as.numeric(minutes_split[, 2]) / 60) %>%
    select(-minutes_split)
  
  df$home.team<- df$home.team %>%
    select(-minutes) %>%
    rename(minutes = minutes_numeric) %>%
    replace_na(list(minutes=0))
  
  df$away.player <- df$away.player %>%
    mutate(Home.Away = 1) %>%
    rename(opponent_id = home_team_id) %>%
    select(-away_team_id) %>%
    mutate(minutes_split = str_split(minutes, ":", simplify = TRUE), minutes_numeric = as.numeric(minutes_split[, 1]) + as.numeric(minutes_split[, 2]) / 60) %>%
    select(-minutes_split)
  
  df$away.player<- df$away.player %>%
    select(-minutes) %>%
    rename(minutes = minutes_numeric) %>%
    replace_na(list(minutes=0))
  
  df$away.team <- df$away.team %>%
    mutate(Home.Away = 1) %>%
    rename(opponent_id = home_team_id) %>%
    select(-away_team_id) %>%
    mutate(minutes_split = str_split(minutes, ":", simplify = TRUE), minutes_numeric = as.numeric(minutes_split[, 1]) + as.numeric(minutes_split[, 2]) / 60) %>%
    select(-minutes_split)
  
  df$away.team<- df$away.team %>%
    select(-minutes) %>%
    rename(minutes = minutes_numeric) %>%
    replace_na(list(minutes=0))
  
  return(df)
}

clean.box.list <- lapply(joined.box, cleanup)


schedule <- bind_rows(season.18, season.19, season.20, season.21, season.22, season.23)

#Function to combine home and away box score data, calculate rolling 5 game averages, and attach team data to player data
combine.groups <- function(df) {
  df$team.totals <- bind_rows(df$home.team, df$away.team)
  
  df$team.totals <- df$team.totals %>%
    group_by(team_id) %>% 
    arrange(game_id, .by_group = TRUE) %>% 
    relocate(minutes, .after = plus_minus_points) %>%
    mutate(across(estimated_offensive_rating:minutes, ~ round(rollapplyr(.x, width=5, FUN=mean, partial=TRUE),3), .names = "team_5_game_avg_{col}")) %>% 
    mutate(across(starts_with("team_5_game_avg"), ~ lag(.x, n=1))) %>%
    left_join(select(schedule, game_id, game_date), by="game_id") %>% 
    mutate(days_rest = as.numeric((game_date - lag(game_date))-1)) %>%
    mutate(opponent_key = paste(game_id, opponent_id, sep = "-")) %>%
    mutate(team_key = paste(game_id, team_id, sep = "-")) %>%
    ungroup()
  
  df$team.totals <- df$team.totals %>%
    left_join(select(df$team.totals, team_key, 52:93, 95), by=c("opponent_key" = "team_key")) %>%
    rename_with(~ paste0("opp_", gsub("\\.y$", "", .)), .cols = ends_with(".y")) %>%
    rename_with(~gsub("\\.x$", "", .), .col = ends_with(".x"))
  
  
  df$player.totals <- bind_rows(df$home.player, df$away.player)
  
  df$player.totals <- df$player.totals %>%
    group_by(person_id) %>% 
    arrange(game_id, .by_group = TRUE) %>% 
    relocate(minutes, .after = plus_minus_points) %>%
    mutate(across(estimated_offensive_rating:minutes, ~ round(rollapplyr(.x, width=5, FUN=mean, partial=TRUE),3), .names = "player_5_game_avg_{col}")) %>% 
    mutate(across(starts_with("player_5_game_avg"), ~ lag(.x, n=1))) %>%
    left_join(select(schedule, game_id, game_date), by="game_id") %>% 
    mutate(days_rest = as.numeric((game_date - lag(game_date))-1)) %>%
    mutate(opponent_key = paste(game_id, opponent_id, sep = "-")) %>%
    mutate(team_key = paste(game_id, team_id, sep = "-")) %>%
    ungroup() %>%
    left_join(select(df$team.totals, team_key, 98:140), by=c("opponent_key" = "team_key")) %>%
    left_join(select(df$team.totals, team_key, 52:94), by=c("team_key"))
  
  df$home.team <- NULL
  df$away.team <- NULL
  df$home.player <- NULL
  df$away.player <- NULL
  
  return(df)
}

combined.box <- lapply(clean.box.list, combine.groups)

#Function to combine player and team box scores from all seasons
final.process <- function(element) {
  list(
    all.team.totals = map_df(element, "team.totals"),
    all.player.totals = map_df(element, "player.totals")
  )
}

final.box <- final.process(combined.box)

player.data <- final.box$all.player.totals

player.data <- na.omit(player.data)

#Add indicator to mark starters for each game
player.data <- player.data %>%
  mutate(starter = ifelse(position != "", 1, 0)) %>%
  mutate(comment = ifelse(comment != "", 1, 0))

#Filter data to only show players who average at least 10 minutes per game
players.avg.minutes <- player.data %>%
  group_by(person_id) %>%
  summarise(avg_minutes = mean(minutes, na.rm = TRUE)) %>%
  filter(avg_minutes >= 10)

filtered.player.data <- player.data %>%
  inner_join(players.avg.minutes, by = "person_id")

#Predict points using a linear regression model
fit.predict.player <- function(df) {
  set.seed(123)
  split <- sample.split(df$points, SplitRatio = 0.8)
  train <- subset(df, split == TRUE)
  test <- subset(df, split == FALSE)

variables <- train %>%
  select(14,58,60,71:74,77,79:82,45,85,87:89,90,93,96,98,100,102,106,108,115,124,126,138,141,142,153,155,156,179,182,187,190) %>%
  colnames()

formula <- reformulate(variables, response = "points")
model <- lm(formula, data = train)

pred <- predict(model, newdata = test)
pred <- ifelse(pred < 0, 0, pred)

data.frame(
  person.id = test$person_id,
  actual.points = test$points,
  predicted.points = pred
)
}

#Run the model and calculate average RMSE per player and overall RMSE
lm.results <- fit.predict.player(filtered.player.data)

rmse.by.player <- lm.results %>%
  group_by(person.id) %>%
  summarize(rmse = rmse(actual.points, predicted.points))

average.rmse <- mean(rmse.by.player$rmse, na.rm = TRUE)

overall.rmse <- rmse(lm.results$actual.points, lm.results$predicted.points)

print(average.rmse)
print(overall.rmse)

