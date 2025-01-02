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

# just lowvig for now
# using additive method for devigging
# totals_join_df <- totals_data %>%
#   filter(sportsbook == "lowvig") %>%
#   mutate(over_ip = if_else(over_price < 0, over_price / (over_price - 100), 100 / (over_price + 100)),
#          under_ip = if_else(under_price < 0, under_price / (under_price - 100), 100 / (under_price + 100)),
#          over_devig = over_ip / (over_ip + under_ip),
#          id = paste0(lubridate::date(timestamp),"-",home_team,"-",away_team)) %>%
#   distinct(id, .keep_all = TRUE)

totals_join_df <- totals_data %>%
  mutate(id = paste0(lubridate::date(timestamp),"-",home_team,"-",away_team)) %>%
  group_by(id) %>%
  summarise(mean_goals = mean(over_point, na.rm = TRUE)) %>%
  ungroup()



# # complete dataframe
# join_df <- master_games %>%
#   select(date, home_name, away_name, id, total) %>%
#   left_join(totals_join_df %>% select(id, mean_goals), by = "id") %>% 
#   drop_na() %>%
#   mutate(p_over = if_else(over_devig > 0.5, 1, 0),
#          tot_over = if_else(total > over_point, 1, 0))


join_df <- master_games %>%
  select(date, home_name, away_name, id, total) %>%
  left_join(totals_join_df %>% select(id, mean_goals), by = "id")  %>%
  drop_na()


mod1 <- lm(total ~ mean_goals, data = join_df)
summary(mod1)

mod2 <- glm(tot_over ~ p_over, join_df, family = "binomial")
summary(mod2)
pred_mod2 <- predict(mod2, join_df, type = "response")

library(pROC)
roc <- roc(join_df$tot_over, pred_mod2)
auc(roc)
pROC::plot.roc(roc)
