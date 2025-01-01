library(hockeyR)
library(lubridate)
library(tidyverse)
library(httr)
library(jsonlite)
library(hockeyR)
library(purrr)
library(tidyverse)

games21 <- get_game_ids(2021)
games22 <- get_game_ids(2022)
games23 <- get_game_ids(2023)
games24 <- get_game_ids(2024)

master_games <- bind_rows(games22, games23)
master_games$iso_game_time <- format(as.POSIXct(paste0(master_games$date, master_games$game_time),
                                                format = "%Y-%m-%d %I:%M %p", 
                                                tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ") # time format required for API
master_games$iso_game_time_hour_before <- format(as.POSIXct(paste0(master_games$date, master_games$game_time),
                                                            format = "%Y-%m-%d %I:%M %p", 
                                                            tz = "UTC") - hours(1), "%Y-%m-%dT%H:%M:%SZ") # for getting lines 1 hour before game time

unique_iso_gt <- unique(master_games$iso_game_time_hour_before)


url_formula <- function(key, datetime){
  url <- paste0("https://api.the-odds-api.com/v4/historical/sports/icehockey_nhl/odds?regions=us&markets=totals&oddsFormat=american&apiKey=",key,"&date=", datetime)
}

key <- Sys.getenv("API_KEY")
urls <- url_formula(key, unique_iso_gt)

get_odds_data <- function(url) {
  response <- GET(url)
  data_parsed <- content(response, as = "parsed")
  
  main_timestamp <- data_parsed$timestamp
  games <- data_parsed$data
  
  final_df <- map_dfr(games, function(game) {
    # get all bookmakers
    map_dfr(game$bookmakers, function(bookmaker) {
      # just totals
      totals_market <- keep(bookmaker$markets, ~ .x$key == "totals")
      if (length(totals_market) == 0) return(NULL)
      
      # get outcomes
      outcomes <- totals_market[[1]]$outcomes
      over_info <- keep(outcomes, ~ .x$name == "Over")[[1]]
      under_info <- keep(outcomes, ~ .x$name == "Under")[[1]]
      
      if (is.null(over_info) || is.null(under_info)) return(NULL)
      
      # create df 
      data.frame(
        timestamp = main_timestamp,
        home_team = game$home_team,
        away_team = game$away_team,
        sportsbook = bookmaker$key, 
        over_price = over_info$price,
        over_point = over_info$point,
        under_price = under_info$price,
        under_point = under_info$point
      )
    })
  })
  
  return(final_df)
}

hockey_totals_22_23 <- map_dfr(urls, get_odds_data)

write_csv(hockey_totals_22_23, "hockey_lines_22-23.csv")
