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
  tackles_data <- read_csv("tackles.csv")
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
  # Attach player alignment where possible
  alignment_mapping <- read_csv("alignment_mapping.csv")
  
  # Filtering for non-special teams plays
  tracking %>% 
    filter(newId %in% run_plays) %>% 
    left_join(alignment_mapping %>% mutate(newId = as.character(newId)), by = join_by(nflId, displayName, newId),
              relationship = "many-to-many") %>% 
    left_join(plays %>% select(newId, ballCarrierId, defensiveTeam), by = c("newId"), 
              relationship = "many-to-many") -> run_tracking
  
}

{
  ## Looking to measure instincts by first taking a look at distance closed on ball until the give
  
  # First find mesh point, so only using traditional handoff plays
  run_tracking %>% 
    filter(event == "handoff" & nflId == ballCarrierId) %>% 
    select(newId, mesh_pt_x = x, mesh_pt_y = y) -> mesh_point
  
  # Finding ground gained
  run_tracking %>% 
    filter(ballCarrierId == nflId | club == defensiveTeam) %>% 
    left_join(mesh_point, by = c("newId"),relationship = "many-to-many") %>% 
    filter(event %in% c("ball_snap", "handoff"))  %>%
    group_by(newId, nflId) %>% 
    mutate(
      dist_to_mesh = sqrt((x - mesh_pt_x)^2 + (y - mesh_pt_y)^2),
      old_dist_to_mesh = lag(dist_to_mesh),
      ground_gained = old_dist_to_mesh - dist_to_mesh,
      pos_read = if_else(ground_gained > 0, 1, 0),
      neg_read = if_else(ground_gained <= 0, 1, 0)
      ) %>% 
    drop_na(old_dist_to_mesh) %>% 
    distinct() -> snap_to_mesh_data
}

{
  players %>% select(nflId, player = displayName) -> players_mapping
  
  snap_to_mesh_data %>% 
    left_join(players_mapping, by = c("nflId")) %>% 
    mutate(
      tech = if_else(tech %in% c("0", "1"), "0-1", tech),
      tech = if_else(tech %in% c("2", "3", "4"), "2-4", tech),
      tech = if_else(tech %in% c("5", "6", "7"), "5-7", tech),
      tech = if_else(tech %in% c("60", "70"), "In Coverage", tech),
      tech = if_else(tech %in% c("00", "10", "20", "30", "40", "50"), "Off Ball LB", tech)
    ) %>% 
    group_by(tech, nflId, player) %>% 
    summarise(
      snaps = n(),
      pos_read_pct = mean(pos_read, na.rm = T),
      neg_read_pct = mean(neg_read, na.rm = T),
      avg_ground_gained = mean(ground_gained, na.rm = T)
      ) %>% 
    filter(snaps > 50) -> summarised_instinct_data
  
  summarised_instinct_data %>% 
    group_by(tech) %>% 
    slice_max(avg_ground_gained, n=3)
}

{
  summarised_instinct_data %>% 
    left_join(summarised_gtd %>% 
                select(nflId, tech, success_run_stop_pct = tackle_pct),
              by = c("nflId", "tech")) %>% 
    filter(tech == "Interior Off Ball") %>% 
    ggplot(aes(x = avg_ground_gained, y = success_run_stop_pct)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggthemes::theme_clean()
}
