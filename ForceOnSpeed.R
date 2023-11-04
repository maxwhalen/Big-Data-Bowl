{
  library(tidyverse) 
  library(devtools)
  library(dplyr)
  library(gganimate)
  library(ggfootball)
  library(ggforce)
  library(ggplot2)
  library(ggrepel)
  library(readr)
  setwd("/Users/maxwhalen/Documents/GitHub/Big-Data-Bowl")
  
  # * load helper functions ----
  source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
  source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")
}

{
  plays <- read_csv("plays.csv") %>% mutate(newId = paste0(gameId, playId))
  tackles_data <- read_csv("tackles.csv") %>% mutate(newId = paste0(gameId, playId))
  players <- read_csv("players.csv")
  games <- read_csv("games.csv")
  pbp_22 <- load_pbp(seasons = 2022) %>% mutate(newId = paste0(old_game_id, play_id))
  ftn_22 <- load_ftn_charting(seasons = 2022)
  
  # Filter to get the plays you want
  pbp_22 %>% 
    filter(rush_attempt == 1) %>% 
    mutate(newId = paste0(old_game_id, play_id)) %>% 
    pull(newId)-> run_plays
  
  tracking <- tibble()
  
  for(i in 1:9){
    temp <- data.table::fread(paste0("tracking_week_", i,".csv"))
    tracking <- bind_rows(tracking, temp) %>% 
      mutate(newId = paste0(gameId, playId))
  }
}

{
  tracking %>% 
    filter(event == "tackle") %>% 
    left_join(tackles_data, by = c("newId", "nflId")) %>% 
    left_join(players %>% select(nflId, weight), by = c("nflId")) %>% 
    filter(tackle == 1 | assist == 1) %>% 
    mutate(force = weight * s) %>% 
    group_by(newId) %>% 
    summarise(total_force = sum(force)) -> force_on_tackles
  
  plays %>% 
    left_join(force_on_tackles) %>% 
    left_join(games) %>% 
    group_by(ballCarrierDisplayName, ballCarrierId, gameId, week) %>% 
    summarise(
      carries = n(),
      cumulative_force = sum(total_force, na.rm = T),
      avg_force =  mean(total_force, na.rm = T),
      ) %>% 
    left_join(players %>% select(displayName, weight), 
              by = c("ballCarrierDisplayName" = "displayName"), 
              relationship = "many-to-many") %>% 
    mutate(adj_force = cumulative_force / weight) %>% 
    select(-weight) %>% 
    arrange(week) %>% 
    group_by(ballCarrierDisplayName) %>% 
    mutate(rolling_force = roll::roll_sum(adj_force, width = 8, min_obs = 1)) -> rolling_force
}

# nothing to be gathered from injury reports
{
  injuries <- load_injuries(seasons = 2022) %>% filter(week <= 8)
  
  injuries %>% 
    select(season, team, week, full_name, report_status) %>% 
    mutate(last_report = lag(report_status)) %>% 
    filter(!last_report %in% c("Out", "Questionable")) %>% 
    distinct() %>% 
    drop_na() %>% 
    count(full_name) %>% 
    arrange(-n) -> injury_report
}

{
  tracking %>% 
    group_by(nflId, gameId) %>% 
    summarise(max_speed = max(s, na.rm = T),
              mean_speed = mean(s, na.rm = T),
              median_speed = median(s, na.rm = T)) %>% 
    left_join(rolling_force %>% select(nflId = ballCarrierId, gameId, rolling_force)) %>% 
    group_by(nflId) %>% 
    mutate(week = row_number()) %>% 
    drop_na(ballCarrierDisplayName) %>% 
    mutate(
      rolling_max_speed = roll::roll_max(max_speed, width = 3, min_obs = 1),
      rolling_mean_speed = roll::roll_mean(max_speed, width = 3, min_obs = 1),
      rolling_med_speed = roll::roll_median(max_speed, width = 3, min_obs = 1)
      ) -> synthesized_rolling_data
  
  synthesized_rolling_data %>% 
    ggplot(aes(x = rolling_force, y = rolling_max_speed)) +
    geom_point() + 
    geom_smooth(method = "lm") +
    ggthemes::theme_clean()
}