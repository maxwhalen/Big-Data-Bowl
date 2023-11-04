{
  library(tidyverse) 
  library(devtools)
  library(deldir)
  library(dplyr)
  library(gganimate)
  library(ggfootball)
  library(ggforce)
  library(ggplot2)
  library(ggrepel)
  library(igraph)
  library(readr)
  library(ngscleanR)
  library(nflverse)
  library(gt)
  library(gtExtras)
  library(patchwork)
  setwd("/Users/maxwhalen/Documents/GitHub/Big-Data-Bowl")
  
  # * load helper functions ----
  source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
  source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")
}

{
  ########################### HELPER FUNCTIONS #################################
  
  # Way too much code repetition, but working on it here
  
  # First, finding whether a player is being blocked or not
  find_engagement <- function() {
    engaged_values <- c()
    
    for(j in def_players){
      train_data %>% 
        filter(nflId == j) %>% 
        left_join(off_no_ball_carrier, by = c("newId")) %>% 
        mutate(
          off_distance_to_ball = sqrt((off_x - ball_carrier_data$x)^2 + (off_y - ball_carrier_data$y)^2), 
          distance_to_off = sqrt((off_x - def_x)^2 + (off_y - def_y)^2)
        ) %>% 
        filter(off_distance_to_ball * 1.05 < def_distance_to_ball) -> off_locs
      
      engaged_value <- if_else(min(off_locs$distance_to_off) < 2, "E", "F") 
      
      engaged_values <- c(engaged_values, engaged_value)
    }
  }
}

################################## LOAD DATA ###################################

{
  plays <- read_csv("plays.csv") %>% mutate(newId = paste0(gameId, playId))
  tackles_data <- read_csv("tackles.csv")  %>% mutate(newId = paste0(gameId, playId))
  players <- read_csv("players.csv")
  games <- read_csv("games.csv")
  pbp_22 <- load_pbp(seasons = 2022) %>% mutate(newId = paste0(old_game_id, play_id))
  ftn_22 <- load_ftn_charting(seasons = 2022)
  rosters_22 <- load_rosters(seasons = 2022)
  
  # Filter to get the plays you want
  pbp_22 %>% 
    filter(rush_attempt == 1) %>% 
    mutate(newId = paste0(old_game_id, play_id)) %>% 
    pull(newId) %>% 
    unique() -> run_plays
  
  pbp_22 %>% 
    filter(complete_pass == 1) %>% 
    mutate(newId = paste0(old_game_id, play_id)) %>% 
    pull(newId) %>% 
    unique() -> pass_plays
  
  tracking <- tibble()
  for(i in 1:9){
    temp <- data.table::fread(paste0("tracking_week_", i,".csv"))
    tracking <- bind_rows(tracking, temp) %>% 
      mutate(newId = paste0(gameId, playId)) 
  }
  
  tracking <- tracking %>% rotate_to_ltr()
}

{
  # Attach player alignment where possible
  alignment_mapping <- read_csv("alignment_mapping.csv")
  
  tracking %>% 
    left_join(alignment_mapping %>% mutate(newId = as.character(newId)), by = join_by(nflId, displayName, newId),
              relationship = "many-to-many") %>% 
    left_join(plays %>% select(newId, ballCarrierId, defensiveTeam, passResult), by = c("newId"), 
              relationship = "many-to-many") -> joined_tracking
  
  joined_tracking %>% 
    filter(newId %in% run_plays) -> run_tracking
  
  joined_tracking %>% 
    filter(newId %in% pass_plays) -> pass_tracking
  
}

############################# FIND VORONOI AREAS ###############################

{
  run_tracking_temp <- tibble()
  index <- 0
  unique_plays <- unique(run_tracking$newId)
  num_plays <- length(unique_plays)
  
  for(i in unique_plays) {
    
    run_tracking %>% 
      filter(newId == i & club != "football" & 
               (defensiveTeam == club | ballCarrierId == nflId)) %>% 
      cut_plays() -> play_data
      
      for(j in play_data$frameId){
        play_data %>% 
          filter(frameId == j) -> frame_data
        
        ball_carrier_data <- frame_data[frame_data$ballCarrierId == frame_data$nflId, c("x", "y", "club")] 
        
        frame_data <- frame_data %>% 
          mutate(distance_to_ball = ifelse(ballCarrierId != nflId, 
                                           sqrt((x - ball_carrier_data$x)^2 + (y - ball_carrier_data$y)^2), 0))
        
        # Extract x and y coordinates 
        x <- frame_data$x
        y <- frame_data$y
        
        if(length(x*y) > length(unique(x*y))){
          x = x + runif(length(x), min=-0.1, max=0.1)
          y = y + runif(length(y), min=-0.1, max=0.1)
        }
        
        # Create Voronoi tesselation
        x_min <- frame_data %>% filter(frame_data$nflId == frame_data$ballCarrierId) %>% pull(x) %>% min()
        vor <- deldir(x, y, xlim = c(x_min - 5, 110), ylim = c(0, 53))

        # Get Voronoi polygons
        polygons <- tile.list(vor)
        
        # Calculate area of each polygon 
        areas <- sapply(polygons, function(p) abs(geometry::polyarea(p$x, p$y)))
        
        # Add areas to original dataframe
        frame_data$area <- areas
        
        run_tracking_temp <- bind_rows(run_tracking_temp, frame_data)
      }
    
    index = index + 1
    print(paste0("Progress: ", round(index/num_plays, digits = 5) *100, "%"))
  }
  
  write_csv(run_tracking_temp, "mod_run_tracking")
}

{
  tackles_data %>% 
    filter(!(tackle == 0 & assist == 1 & pff_missedTackle == 0)) %>% 
    mutate(tackle_value = if_else(pff_missedTackle == 1, -1, 1)) %>% 
    left_join(run_tracking_temp %>% 
                filter(grepl("contact", event)) %>% 
                select(area, distance_to_ball, newId, nflId, frameId), 
              by = c("newId", "nflId")) %>% 
    distinct() %>% 
    drop_na() %>% 
    group_by(nflId) %>% 
    summarise(
      tackles = sum(tackle),
      total_tackle_leverage = sum(area*tackle_value)
    ) %>% 
    left_join(players %>% select(displayName, nflId)) %>% 
    arrange(-total_tackle_leverage)
}

{
  id <- sample(run_tracking$newId)
  
  play <- run_tracking %>% filter(newId == 20220911002491)
  
  plot_play(df_track_og = play, frame = 38)
}

############################### VISUALIZE VORONOI ##############################


{
  run_tracking %>% 
    filter(newId == sample(run_plays, 1) & grepl("contact", event)) %>% 
    mutate(
      ballCarrier = if_else(ballCarrierId == nflId, 1, 0),
      frameId = frameId - 20
    ) -> play_level_data
  
  play_id <- min(play_level_data$newId, na.rm = T)
  frame_id <- min(play_level_data$frameId, na.rm = T)
  
  play_level_data %>% 
    left_join(teams_colors_logos %>% select(club = team_abbr, team_color, team_color2)) %>% 
    filter(frameId == frame_id & club != "football") -> voronoi_data
  
  # Extract x and y coordinates 
  x <- voronoi_data$x
  y <- voronoi_data$y
  
  # Create Voronoi tesselation
  vor <- deldir(x, y)
  
  # Get Voronoi polygons
  polygons <- tile.list(vor)
  
  # Calculate area of each polygon 
  areas <- sapply(polygons, function(p) abs(geometry::polyarea(p$x, p$y)))
  
  # Add areas to original dataframe
  voronoi_data$area <- areas
  
}

{ 
  dot_size <- 7
  
  v_plot_data <- voronoi_data %>% 
    filter(ballCarrier == 1 | defensiveTeam == club)
  
  voronoi_data %>% 
    mutate(y = y - 10) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point(data = voronoi_data, 
               aes(x, y),
               color = if_else(voronoi_data$ballCarrier == 1, "#825736", voronoi_data$team_color),
               shape =  19,
               size = dot_size
    ) +
    geom_point(data = voronoi_data, 
               aes(x, y),
               color = voronoi_data$team_color2,
               shape =  1,
               stroke = dot_size*0.25,
               size = dot_size*0.95
    ) +
    geom_text(
      data = voronoi_data,
      mapping = aes(x = x, y = y, label = jerseyNumber),
      color = "white",
      size = 1/2 * dot_size
    ) +
    geom_voronoi_segment(data = voronoi_data, 
                         aes(x, y)) +
    ggthemes::theme_clean() +
    #coord_flip() +
    labs(
      x = "Absolute Yardline",
      y = ""
    ) +
    theme(
      axis.ticks.y = element_blank(),
    )
  
}