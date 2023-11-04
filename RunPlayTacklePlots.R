# Plotting Functions
{
  library(nflverse)
  library(tidyverse)
}

{
  # Adjust location to center football and reset depth (mostly for pre-snap alignment)
  adjust_locs <- function(df, median_y = 27){
    df %>% 
      group_by(playId, gameId) %>%
      mutate(x = abs(x - x[displayName == "football"]),
             y_middle = median_y,
             y_adjust = y_middle - y,
             y = y - y_adjust)
  }
  
  my_theme <- function(gg){
    theme(
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.line.x.bottom = element_blank(),
      axis.line.y.left = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      line = element_blank(),
      text = element_text(family = "PT Mono")
    )
  }
}

# Load Data
{
  tracking <- tibble()
  
  for(i in 1:9){
    temp <- data.table::fread(paste0("tracking_week_", i, ".csv"))
    tracking <- bind_rows(tracking, temp)
  }
  
  tracking <- tracking %>% mutate(newId = paste0(gameId, playId)) # Unique playIds (I may be missing something)
  games <- data.table::fread("games.csv")
  players <- data.table::fread("players.csv")
  tackles <- data.table::fread("tackles.csv")
  plays <- data.table::fread("plays.csv")
  
  runPlays <- plays %>% mutate(newId = paste0(gameId,playId)) %>% filter(is.na(passLength)) %>% pull(newId) 
}

# Plot Pre-Snap Defensive Alignment
defensive_alignment_heat_map <- function(tracking, defTeam = NA){
  if(is.na(defTeam)){
    myTitle <- "NFL Defensive Pre-Snap Alignment Heat Map"
  } else{
    myTitle <- paste0(defTeam, " Defensive Pre-Snap Alignment Heat Map")
  }
  
  tracking %>% 
    filter(newId %in% runPlays) %>% 
    filter(frameId == 1) %>% 
    left_join(plays, by = c("gameId", "playId")) %>% 
    filter(club == defensiveTeam | displayName == "football") -> frame_1_data
  
  frame_1_data %>% 
    group_by(playId, gameId) %>%
    mutate(
      x = abs(x - x[displayName == "football"]),
      y_middle = 27,
      y_adjust = y_middle - y,
      y = y - y_adjust
    ) -> adjusted_data
  
  adjusted_data %>% 
    mutate(x = round(x, digits = 2),
           y = round(y, digits = 2)) -> plot_data
  
  if(!is.na(defTeam)){
    plot_data %>% 
      filter(defensiveTeam == defTeam) -> plot_data
  }
  
  plot_data %>% 
    ggplot(aes(x = x, y = y)) +
    stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE) + 
    coord_flip() +
    xlim(0,11) +
    ylim(-15, 70) +
    labs(title = myTitle) +
    scale_fill_gradient(low = "forestgreen", high = "#D50A0A")+
    guides(legend = "none") +
    my_theme()
}

{
  # Plot for NFL
  defensive_alignment_heat_map(tracking) +
    geom_vline(xintercept = 0, color = "blue", size = 1) +
    geom_vline(xintercept = 10, color = "yellow", size = 1) +
    geom_vline(xintercept = 5, color = "white", size = 1)
  
  # Plot For PHI Eagles
  defensive_alignment_heat_map(tracking, "PHI") +
    geom_vline(xintercept = 0, color = "blue", size = 1) +
    geom_vline(xintercept = 10, color = "yellow", size = 1) +
    geom_vline(xintercept = 5, color = "white", size = 1)
}

{
  # Multiple Player Tackle Maps, players in form = c("[displayName]" = "[plot color]")
  tackle_map <- function(players){
    tracking %>% 
      filter(newId %in% runPlays) %>% 
      adjust_locs() %>% 
      ungroup() %>% 
      filter(event == "tackle" & displayName %in% names(players)) %>% 
      left_join(tackles, by = c("gameId", "playId", "nflId")) %>% 
      filter(tackle == 1 | assist == 1) %>% 
      mutate(tackle_type = if_else(tackle == 1, "solo", "assist")) -> tackle_loc_data
    
    ggplot(tackle_loc_data, aes(x=x, y=y, color = displayName)) +
      coord_flip() +
      geom_rect(aes(xmin = if_else(min(x) < -1, min(x), -1), 
                    xmax = if_else(max(x) > 12, max(x), 12), 
                    ymin = min(y) - 2, ymax = max(y) + 2), 
                fill = "forestgreen", color = "white", alpha = 0.3) + 
      geom_segment(aes(x = 0, xend = 0, y = min(y) - 2, yend = max(y) + 2), color = "blue", size = 1.5) +
      geom_segment(aes(x = 5, xend = 5, y = min(y) - 2, yend = max(y) + 2), color = "white", size = 1.5) +
      geom_segment(aes(x = 10, xend = 10, y = min(y) - 2, yend = max(y) + 2), color = "yellow", size = 1.5) +
      geom_point(size = 3) +
      labs(x = "", y = "", title = "Tackle Locations") +
      my_theme() +
      scale_color_manual(values = players)
  }
}

{
  # Eagles ILB Tackle Map
  tackle_map(c("T.J. Edwards" = "#004C54", "Kyzir White" = "#ACC0C6"))
}

{
  # Single Player (displayName) tackle density map
  tackle_density <- function(player){
    tracking %>% 
      filter(newId %in% runPlays) %>% 
      adjust_locs() %>% 
      ungroup() %>% 
      filter(event == "tackle" & displayName == player) %>% 
      left_join(tackles, by = c("gameId", "playId", "nflId")) %>% 
      filter(tackle == 1 | assist == 1) %>% 
      mutate(tackle_type = if_else(tackle == 1, "solo", "assist")) -> tackle_loc_data
    
    ggplot(tackle_loc_data, aes(x=x, y=y)) +
      coord_flip() +
      stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
      labs(x = "", y = "", 
           title = paste0(player, " Tackle Density Map")
      ) +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.line.y.left = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        line = element_blank(),
        text = element_text(family = "PT Mono")
      ) +
      xlim(-1, 11) +
      ylim(-21,75) +
      scale_fill_gradient(low = "forestgreen", high = "#D50A0A") 
  }
}

{
  # Density Map Examples
  tackle_density("Roquan Smith") +
    geom_vline(xintercept = 0, color = "blue", size = 1) +
    geom_vline(xintercept = 10, color = "yellow", size = 1) +
    geom_vline(xintercept = 5, color = "white", size = 1)
  
  tackle_density("Foyesade Oluokun") +
    geom_vline(xintercept = 0, color = "blue", size = 1) +
    geom_vline(xintercept = 10, color = "yellow", size = 1) +
    geom_vline(xintercept = 5, color = "white", size = 1)
}
