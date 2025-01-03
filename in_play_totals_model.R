library(hockeyR)
library(tidyverse)
library(Metrics)
library(catboost)

pbp21 <- read_csv("pbp21.csv")
pbp22 <- read_csv("pbp22.csv")
pbp23 <- read_csv("pbp23.csv")
pbp24 <- read_csv("pbp24.csv")

game_lines <- read_csv("hockey_lines_22-23.csv")

clean_df_function <- function(data){
  
  clean_df <- data %>%
    group_by(game_id) %>%
    mutate(rolling_shots = cumsum(event_type %in% c("SHOT", "GOAL"))) %>%
    ungroup() %>%
    mutate(tot_goals = home_final + away_final,
           cur_goals = home_score + away_score,
           goal_dif = abs(home_score - away_score),
           is_ev = if_else(strength_code == "EV", 1, 0),
           # key_faceoff = if_else(zoneCode %in% c("O","D") & event_type == "FACEOFF", 1, 0),
           future_goals = tot_goals - cur_goals) %>%
    dplyr::select(future_goals, is_ev, cur_goals, goal_dif, game_seconds_remaining, rolling_shots) %>%
    # dplyr::select(future_goals, is_ev, key_faceoff, cur_goals, game_seconds_remaining) %>%
    #   filter(game_seconds_remaining <1200) %>% # trying just 3rd period
    #    filter(game_seconds_remaining <2400) %>%
    #   filter(game_seconds_remaining >0) %>% # remove OT
    drop_na()
  
  clean_df$is_ev <- as.factor(clean_df$is_ev)
  
  return(clean_df)
  
}

master_pbp <- bind_rows(clean_df_function(pbp21), 
                        clean_df_function(pbp22), 
                        clean_df_function(pbp23))

test_df <- clean_df_function(pbp24)

mod1 <- lm(future_goals ~ is_ev + cur_goals + goal_dif + game_seconds_remaining, 
           data = master_pbp)
mod2 <- lm(future_goals ~ rolling_shots + is_ev + cur_goals + goal_dif + game_seconds_remaining, 
           data = master_pbp)
mod3 <- bam(future_goals ~ s(rolling_shots) + is_ev + s(cur_goals) + s(goal_dif) + game_seconds_remaining, 
            data = master_pbp)
pred <- predict(mod3, test_df)

rmse(test_df$future_goals, pred) # 1.02
sd(test_df$future_goals) # rmse below sd so there is some signal


#### test how odds help ####

clean_game_lines <- game_lines %>%
  mutate(id = paste0(lubridate::date(timestamp),"-",home_team,"-",away_team),
         over_ip = if_else(over_price < 0, over_price / (over_price - 100), 100 / (over_price + 100)),
         under_ip = if_else(under_price < 0, under_price / (under_price - 100), 100 / (under_price + 100)),
         over_devig = over_ip / (over_ip + under_ip))%>%
  filter(sportsbook == "draftkings") %>%
  distinct(id, .keep_all = TRUE)

clean_df_function2 <- function(data){
  
  clean_df <- data %>%
    group_by(game_id) %>%
    mutate(rolling_shots = cumsum(event_type %in% c("SHOT", "GOAL"))) %>%
    ungroup() %>%
    mutate(tot_goals = home_final + away_final,
           cur_goals = home_score + away_score,
           goal_dif = abs(home_score - away_score),
           is_ev = if_else(strength_code == "EV", 1, 0),
           id = paste0(game_date, "-", home_name, "-", away_name),
           # key_faceoff = if_else(zoneCode %in% c("O","D") & event_type == "FACEOFF", 1, 0),
           future_goals = tot_goals - cur_goals) %>%
    dplyr::select(id, future_goals, is_ev, cur_goals, goal_dif, game_seconds_remaining, rolling_shots) %>%
    # dplyr::select(future_goals, is_ev, key_faceoff, cur_goals, game_seconds_remaining) %>%
    #   filter(game_seconds_remaining <1200) %>% # trying just 3rd period
    #    filter(game_seconds_remaining 2400) %>%
    #   filter(game_seconds_remaining >0) %>% # remove OT
    drop_na()
  
  clean_df$is_ev <- as.factor(clean_df$is_ev)
  
  return(clean_df)
  
}

master2 <- bind_rows(clean_df_function2(pbp22),
                     clean_df_function2(pbp23)) %>%
  left_join(clean_game_lines, by = "id") %>%
  drop_na()

mod3 <- lm(future_goals ~ rolling_shots + is_ev + cur_goals + goal_dif + game_seconds_remaining , 
           data = master2)
mod4 <- mgcv::bam(future_goals ~  s(rolling_shots) + is_ev + s(cur_goals) + s(goal_dif) + game_seconds_remaining + s(over_point, over_ip), 
           data = master2)

pred <- predict(mod3, master2)
pred2 <- predict(mod4, master2)

rmse(master2$future_goals, pred2)

anova(mod3,mod4)

summary(mod4)
