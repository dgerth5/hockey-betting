library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

key <- "INSERT KEY HERE"

url <- paste0("https://api.the-odds-api.com/v4/historical/sports/icehockey_nhl/odds?regions=us&markets=totals&oddsFormat=american&apiKey=",key,"&date=2024-12-19T12:00:00Z")
response <- GET(url)              

data_parsed <- content(response, as = "parsed")

main_timestamp <- data_parsed$timestamp
games <- data_parsed$data

final_df <- map_dfr(games, function(game) {
  
  # just draftkings for now
  dk_info <- keep(game$bookmakers, ~ .x$key == "draftkings")
  if (length(dk_info) == 0) return(NULL)
  
  # just totals
  totals_market <- keep(dk_info[[1]]$markets, ~ .x$key == "totals")
  if (length(totals_market) == 0) return(NULL)
  
  # get outcomes
  outcomes <- totals_market[[1]]$outcomes
  
  # over/under
  over_info <- keep(outcomes, ~ .x$name == "Over")[[1]]
  under_info <- keep(outcomes, ~ .x$name == "Under")[[1]]
  
  df <- data.frame(timestamp = main_timestamp,
                   home_team = game$home_team,
                   away_team = game$away_team,
                   sportsbook = "draftkings", # hardcode for now
                   over_price = over_info$price,
                   over_point = over_info$point,
                   under_price = under_info$price,
                   under_point = under_info$point)
  
})


