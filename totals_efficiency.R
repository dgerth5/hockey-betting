library(hockeyR)
library(tidyverse)
library(readr)

# read game data
games22 <- get_game_ids(2022)
games23 <- get_game_ids(2023)

master_games <- bind_rows(games22, games23) %>%
  mutate(id = paste0(date,"-",home_name,"-",away_name),
         total = home_final_score + away_final_score)

# read betting data
totals_data <- read_csv("hockey_lines_22-23.csv")

# using additive method for devigging
totals_join_df <- totals_data %>%
  filter(sportsbook == "lowvig") %>%
  mutate(over_ip = if_else(over_price < 0, over_price / (over_price - 100), 100 / (over_price + 100)),
         under_ip = if_else(under_price < 0, under_price / (under_price - 100), 100 / (under_price + 100)),
         over_devig = over_ip / (over_ip + under_ip),
         id = paste0(lubridate::date(timestamp),"-",home_team,"-",away_team)) %>%
  distinct(id, .keep_all = TRUE) # not sure why theres 4581 games not 2801


# complete dataframe
join_df <- master_games %>%
  select(date, home_name, away_name, id, total) %>%
  left_join(totals_join_df %>% select(id, over_ip, over_point), by = "id") %>%
  drop_na() %>%
  group_by(over_point) %>%
  summarise(mean_ip = mean(over_ip),
            mean_goals = mean(total),
            sd_goals = sd(total),
            n = n()) %>%
  filter(n > 10)

t5.5 <- join_df2 %>%
  filter(over_point == 7) 

hist(t5.5$total, freq = FALSE, breaks = 10)
lines(dpois(1:12, 7), col = "red")
