
{
    library(gt)
    library(gtExtras)
    library(ggforce)
    library(tidyverse)
    library(deldir)
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