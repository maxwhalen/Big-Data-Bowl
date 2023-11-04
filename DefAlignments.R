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
  plays <- read_csv("plays.csv")
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
  # Filtering for non-special teams plays
  tracking %>% 
    filter(newId %in% run_plays) -> run_tracking
  
}

{
  # Helper functions
  adjust_locs <- function(df, median_y = 26.5){
    df %>% 
      group_by(newId) %>%
      mutate(x = abs(x - x[displayName == "football"]),
             y_middle = median_y,
             y_adjust = y_middle - y,
             y = y - y_adjust)
  }
}

{
  # This is weird and unintuitive. Better solutions welcome, I'm tired
  gaps <- c("C", "G", "T", "TE", "A GAP", "B GAP", "C GAP", "C GAP", "D GAP")
  techniques <- c(0, 2, 4, 6, 1, 3, 5, 5, 7)
  gap_tech_mapping <- bind_cols(gaps, techniques) %>% rename(loc = 1, tech = 2)
  
  run_tracking %>% 
    group_by(newId) %>% 
    mutate(event_lag = lag(event)) %>% 
    filter(grepl("snap", event)) %>% 
    left_join(players %>% select(nflId, position), by = c("nflId")) %>% 
    filter(position %in% gaps) -> play_data
}

{
  all_plays_dl_locs <- tibble()
  index = 0
  all_plays <- unique(play_data$newId)
  
  for(play in all_plays){
    index = index + 1
    print(paste0("Progress: ", round(index/length(all_plays), digits = 3) *100, "%"))
    
    play_ol <- play_data %>% 
      filter(newId == play) %>% 
      select(newId, position, x, y)
    
    play_ol %>% 
      filter(position != "TE") %>% 
      arrange(y) %>% 
      mutate(
        order = row_number(),
        position = if_else(order == 2 | order == 4, "G", position),
        position = if_else(order == 3, "C", position)
      ) %>% 
      bind_rows(play_ol %>% filter(position == "TE") %>% mutate(order = 6)) %>% 
      arrange(y) %>% 
      mutate(
        lead_y = lead(y),
        lag_y = lag(y),
        lead_diff = abs(y - lead_y),
        lag_diff = abs(y - lag_y),
        lag_diff = if_else(is.na(lag_diff), lead_diff, lag_diff),
        lead_diff = if_else(is.na(lead_diff), lag_diff, lead_diff)
        ) %>%
      filter(!(lead_diff > 2.2 | lag_diff > 2.2)) -> linemen
    
    #TODO Detect whether TE is close to linemen
    
    ol_order <- linemen$order
    max_order <- max(ol_order)
    min_order <- min(ol_order)
    
    all_gaps <- tibble()
    
    for(i in ol_order){
      linemen %>% 
        filter(order == i) -> lineman_row
    
      if(i == min_order){
        bind_rows(
          lineman_row %>% mutate(y = y - 0.9),
          lineman_row,
          lineman_row %>% mutate(y = y + 0.85) 
        ) -> temp
      } else {
        bind_rows(
          lineman_row,
          lineman_row %>% mutate(y = y + 0.85) 
        )  -> temp
      }
      
      all_gaps <- bind_rows(all_gaps, temp)
    }
    
    all_gaps %>% 
      mutate(
        pos_lag = lag(position),
        pos_lead = lead(position),
        order = row_number(),
        position = if_else(order %% 2 == 1 & (order < floor(nrow(all_gaps)/2)), paste0(pos_lag, " GAP"), position),
        position = if_else(order %% 2 == 1 & (order > floor(nrow(all_gaps)/2)), paste0(pos_lead, " GAP"), position),
        position = if_else(order == 1, paste0(pos_lead, " EDGE"), position),
        position = if_else(order == nrow(all_gaps), paste0(pos_lag, " EDGE"), position)
        ) %>% 
      select(-c(order, pos_lag, pos_lead)) %>% 
      left_join(gap_tech_mapping, by = c("position" = "loc")) -> final_newId_dl_locs
    
    all_plays_dl_locs <- bind_rows(all_plays_dl_locs, final_newId_dl_locs)
    
  }
}

{
  dline <- c("NT", "DT", "DE", "OLB", "ILB", "MLB")
  
  run_tracking %>% 
    group_by(newId) %>% 
    mutate(event_lag = lag(event)) %>% 
    filter(grepl("snap", event_lag)) %>% 
    left_join(players %>% select(nflId, position), by = c("nflId")) %>% 
    filter(position %in% dline) -> dline_players
  
  dline_players %>% 
    inner_join(all_plays_dl_locs %>% rename(x_gap = x, y_gap = y), 
               by = c("newId" = "newId"), relationship = "many-to-many") %>% 
    drop_na(tech) %>% 
    mutate(x_dist = abs(x_gap - x),
           y_dist = abs(y_gap - y)) %>% 
    group_by(newId, nflId, displayName) %>% 
    slice_min(y_dist) %>%
    mutate(tech = if_else(x_dist > 3.25 | (position.x %in% c("MLB", "ILB") & tech < 10),
                          paste0(as.character(tech), '0'), as.character(tech))) %>% 
    # Make sure the players are on the line and not split out
    filter(x_dist < 8 & y_dist <= 3.1) -> tracking_dl_tech_data
  
}

{
  teams_colors_logos %>% select(team_abbr, team_logo_espn) -> logos_mapping
  
  logos_dict <- as.list(setNames(logos_mapping$team_logo_espn, logos_mapping$team_abbr))
  
  tracking_dl_tech_data %>% 
    ungroup() %>% 
    left_join(tackles_data %>% 
                mutate(newId = paste0(gameId, playId)),
              by = c("newId", "nflId")) %>% 
    mutate(
           tech = if_else(tech %in% c("0", "1"), "0-1", tech),
           tech = if_else(tech %in% c("2", "3", "4"), "2-4", tech),
           tech = if_else(tech %in% c("5", "6", "7"), "5-7", tech),
           tech = if_else(tech %in% c("50", "60", "70"), "Outside Off Ball", tech),
           tech = if_else(tech %in% c("00", "10", "20", "30", "40"), "Interior Off Ball", tech)
           ) -> grouped_techs
  
  grouped_techs %>% 
    left_join(pbp_22 %>% select(newId, success)) %>% 
    group_by(tech, club, nflId, displayName) %>% 
    mutate(success_tackle = if_else(tackle == 1, 1, 0)) %>% 
    summarise(snaps = n(),
              tackles = sum(success_tackle, na.rm = T)) %>% 
    mutate(tackle_pct = tackles/snaps,
           club = logos_dict[club]) %>% 
    ungroup() %>% 
    filter(snaps > 50) -> summarised_gtd
  
  summarised_gtd %>% 
    group_by(tech) %>% 
    slice_max(snaps, n = 2) %>% 
    select(-nflId) -> gtd
  
  gtd %>% 
    group_by(tech) %>% 
    gt() %>% 
    gt_theme_538() %>% 
    fmt_percent(tackle_pct) %>% 
    gt_img_rows(columns = club) %>% 
    data_color( # Update cell colors...
      columns = c(tackle_pct), # ...for mean_len column
      colors = scales::col_numeric(
        palette = colorspace::diverge_hcl(n = 9, palette = "Blue-Red 3") %>% rev(), # Overboard colors! 
        domain = c(0,0.21) # Column scale endpoints
      )
    ) %>% 
    cols_label(club = "",
               displayName = "Player",
               tackle_pct = "Tackle %") %>% 
    gt::tab_header(title = "Stop Percentage by Defensive Front Techniques",
                   subtitle = "Data is given only for run plays in weeks 1-8 of the '22 Season") %>% 
    gt::tab_footnote("Data: @nflfastR + Big Data Bowl | Table: @maxstats3") -> gt
  
  gt %>% 
    gtsave("Graphics/SuccessfulRunStopsByTechniques.png")
}

{
  tracking_dl_tech_data %>% 
    ungroup() %>% 
    select(displayName, nflId, newId, tech) %>% 
    distinct() -> tech_mapping
  
  write_csv(tech_mapping, "alignment_mapping.csv")
  
  run_tracking %>% 
    left_join(tech_mapping) %>% 
    group_by(newId) %>% 
    mutate(lag_event = lag(event)) %>% 
    filter(grepl("snap", lag_event)) %>% 
    left_join(players %>% select(nflId, position), by = c("nflId")) %>% 
    group_by(newId, nflId) %>% 
    slice_max(tech) %>% 
    filter(position %in% c(gaps, dline) | displayName == "football") -> tracking_w_alignment
}

{
  sample_play <- sample(unique(tracking_w_alignment$newId), 1)
  
  x_low <- tracking_w_alignment %>% 
    filter(newId == sample_play) %>% 
    pull(x) %>% min() - 5
  
  x_high <- tracking_w_alignment %>% 
    filter(newId == sample_play) %>% 
    pull(x) %>% max() + 5
  
  
  tracking_w_alignment %>% 
    filter(newId == sample_play) %>% 
    ggplot(aes(x = x, y = y, color = club)) +
    xlim(x_low, x_high) +
    geom_point(size = 6) +
    ggrepel::geom_text_repel(aes(label = tech), box.padding = 1) +
    ggthemes::theme_clean() +
    coord_flip() +
    scale_color_manual(values = 
                         c("football" = "#825736", "BUF" = "#C60C30", "TEN" = "#4B92DB"))
}
