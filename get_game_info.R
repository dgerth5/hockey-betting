library(hockeyR)
library(lubridate)

games21 <- get_game_ids(2021)
games22 <- get_game_ids(2022)
games23 <- get_game_ids(2023)
games24 <- get_game_ids(2024)

master_games <- bind_rows(games21, games22, games23, games24)
master_games$iso_game_time <- format(as.POSIXct(paste0(master_games$date, master_games$game_time),
                                                format = "%Y-%m-%d %I:%M %p", 
                                                tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ") # time format required for API
master_games$iso_game_time_hour_before <- format(as.POSIXct(paste0(master_games$date, master_games$game_time),
                                                            format = "%Y-%m-%d %I:%M %p", 
                                                            tz = "UTC") - hours(1), "%Y-%m-%dT%H:%M:%SZ") # for getting lines 1 hour before game time

unique_iso_gt <- unique(master_games$iso_game_time_hour_before)
length(unique(master_games$date))


url_formula <- function(key, datetime){
  url <- paste0("https://api.the-odds-api.com/v4/historical/sports/icehockey_nhl/odds?regions=us&markets=totals&oddsFormat=american&apiKey=",key,"&date=", datetime)
}