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
  tackles_data <- read_csv("tackles.csv")  %>% mutate(newId = paste0(old_game_id, play_id))
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
