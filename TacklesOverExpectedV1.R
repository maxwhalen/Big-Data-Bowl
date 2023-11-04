{
  library(tidyverse) 
  library(devtools)
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
    pull(newId)-> run_plays
  
  pbp_22 %>% 
    filter(complete_pass == 1) %>% 
    mutate(newId = paste0(old_game_id, play_id)) %>% 
    pull(newId) -> pass_plays
  
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

################################ RUN PLAYS #####################################

{
  ## Looking to measure settled version of the play by first taking a look at distance closed on ball
  
  # First find mesh point, so only using traditional handoff plays
  run_tracking %>% 
    filter(event == "handoff" & nflId == ballCarrierId) %>% 
    select(newId, mesh_pt_x = x, mesh_pt_y = y) -> mesh_point
  
  # Finding ground gained
  run_tracking %>% 
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
    drop_na(old_dist_to_mesh) %>% # Drops ball_snap plays
    distinct() -> snap_to_mesh_data
}

###################### CALCULATING FOR ALL RUN PLAYS ###########################

{
  index <- 0
  unique_plays <- unique(snap_to_mesh_data$newId)
  num_plays <- length(unique_plays)
  run_train <- tibble()
  
  snap_to_mesh_data %>% 
    mutate(ballCarrier = if_else(ballCarrierId == nflId, 1, 0)) %>% 
    select(newId, frameId, nflId, displayName, club, x , y, s, a, o, jerseyNumber,
           tech, ground_gained, ballCarrier) %>% 
    mutate(tech = if_else(is.na(tech), "secondary", tech)) %>% # TODO Define Corner v Safety
    left_join(tackles_data %>% select(-c(gameId, playId)), 
              by = c("nflId", "newId")) %>% 
    mutate_all(~ifelse(is.na(.) & ballCarrier != 1, 0, .)) -> run_loop_data
  
  for(i in unique_plays) {
    run_loop_data %>% 
      filter(newId == i) -> play_level_data
    
    ball_carrier_data <- play_level_data[play_level_data$ballCarrier == 1, c("x", "y", "club")] %>% drop_na()
    
    play_level_data %>% 
      filter(ballCarrier == 1 | !(club %in% ball_carrier_data$club) | club == "football") %>%
      mutate(distance_to_ball = ifelse(ballCarrier == 0, 
                                       sqrt((x - ball_carrier_data$x)^2 + (y - ball_carrier_data$y)^2), 0),
             ball_x = max(ball_carrier_data$x, na.rm = T),
             ball_y = max(ball_carrier_data$y, na.rm = T)) %>% # TODO FIX THIS
      ungroup() %>% 
      compute_o_diff() %>% 
      filter(club != "football") %>% 
      mutate(
        o_to_ball = if_else(o_to_ball <= 180, o_to_ball, 360 - o_to_ball),
        orientation_score = 1 - (o_to_ball/180),
        s = if_else(s < 0, 0, s),
        distance_score = distance_to_ball^-1,
        position_score = distance_score*orientation_score # TODO Verify distance and speed are on same scale
      ) %>% 
      drop_na(tackle) %>% 
      select(newId, frameId, nflId, displayName, ballCarrier, def_x = x, def_y = y, 
             def_o = o, s, a, o_to_ball, def_distance_to_ball = distance_to_ball,
             tech, tackle, assist, orientation_score, distance_score, position_score) -> train_data
    
    # Find whether the player was obstructed or not
    train_data %>% filter(ballCarrier == 0) %>% pull(nflId) -> def_players
    
    play_level_data %>% 
      filter(!nflId %in% train_data$nflId & !is.na(nflId)) %>% 
      select(off_nflId = nflId, jerseyNumber, off_x = x, off_y = y, off_o = o) -> off_no_ball_carrier
    
    engaged_values <- c()
    
    for(j in def_players){
      train_data %>% 
        filter(nflId == j) %>% 
        left_join(off_no_ball_carrier, by = c("newId")) %>% 
        mutate(
          off_distance_to_ball = sqrt((off_x - ball_carrier_data$x)^2 + (off_y - ball_carrier_data$y)^2), 
          distance_to_off = sqrt((off_x - def_x)^2 + (off_y - def_y)^2)
        ) %>% 
        filter(off_distance_to_ball * 1.1 < def_distance_to_ball) -> off_locs
      
      engaged_value <- if_else(min(off_locs$distance_to_off) < 2, "E", "F") 
      
      engaged_values <- c(engaged_values, engaged_value)
    }
    
    train_data$engaged <- engaged_values
    
    run_train <- bind_rows(run_train, train_data)
    
    index = index + 1
    print(paste0("Progress: ", round(index/num_plays, digits = 3) *100, "%"))
  }
}

############################## FITTING MODEL ###################################

{
  run_train %>% 
    mutate(position_score = if_else(position_score == Inf, 10, position_score)) %>% # TODO Handle better
    drop_na(position_score, tackle, newId) -> run_data
  
  # TODO Evaluate Collinearity in tech and position, really just need to handle players' at depth.
  # Likely better handled by accounting for potential obstructions than another way of evaluating position
  summary(model <- glm(tackle ~ orientation_score:distance_score, data = run_data))
}

{
  # Model Findings
  run_data$exp_tackle <- model$fitted.values
  run_data$tackle_oe <- model$residuals
  
  run_data %>% 
    group_by(displayName) %>% 
    summarise(
      run_snaps = n(),
      run_tackle_snap = mean(tackle, na.rm = T),
      run_total_tackles = sum(tackle, na.rm = T),
      run_tackle_oe_snap = mean(tackle_oe, na.rm = T),
      run_total_tackles_oe = sum(tackle_oe, na.rm = T)
    ) %>% 
    arrange(-run_tackle_oe_snap) %>% 
    filter(run_snaps > 75) -> run_tackles_oe_table
  
  run_data %>%
    left_join(pbp_22 %>% select(week, newId), by = c("newId")) %>% 
    mutate(week = if_else(week > 4, "Weeks 5-8", "Weeks 1-4")) %>% 
    group_by(displayName, week)%>% 
    summarise(
      run_snaps = n(),
      run_tackle_snap = mean(tackle, na.rm = T),
      run_total_tackles = sum(tackle, na.rm = T),
      run_tackle_oe_snap = mean(tackle_oe, na.rm = T),
      run_total_tackles_oe = sum(tackle_oe, na.rm = T)
    ) %>% 
    arrange(-run_tackle_oe_snap) %>% 
    filter(run_snaps > 50) -> run_tackles_oe_table_wk
}

################################## PASS PLAYS ##################################

{
  ## Looking to measure settled version of the play by first taking a look at distance closed on ball
  
  # First find mesh point, so only using traditional handoff plays
  pass_tracking %>% 
    filter(event == "pass_outcome_caught" & nflId == ballCarrierId) %>% 
    select(newId, catch_pt_x = x, catch_pt_y = y) -> catch_point
  
  # Finding ground gained
  pass_tracking %>% 
    left_join(catch_point, by = c("newId"), relationship = "many-to-many") %>% 
    mutate(
      event = if_else(event == "autoevent_passforward", "pass_forward", event),
      event = if_else(frameId == 1, "first_frame", event)
    ) %>% 
    filter(event %in% c("first_frame", "pass_outcome_caught"))  %>% #TODO How do we handle OOB
    group_by(newId, nflId) %>% 
    mutate(
      dist_to_catch = sqrt((x - catch_pt_x)^2 + (y - catch_pt_x)^2),
      old_dist_to_catch = lag(dist_to_catch),
      ground_gained = old_dist_to_catch - dist_to_catch,
      pos_read = if_else(ground_gained > 0, 1, 0),
      neg_read = if_else(ground_gained <= 0, 1, 0)
    ) %>% 
    drop_na(old_dist_to_catch) %>% # Drops ball_snap plays
    distinct() -> snap_to_catch_data
}

##################### CALCULATING FOR ALL PASS PLAYS ###########################

{
  index <- 0
  unique_pass_plays <- unique(snap_to_catch_data$newId)
  num_pass_plays <- length(unique_plays)
  pass_train <- tibble()
  
  snap_to_catch_data %>% 
    mutate(ballCarrier = if_else(ballCarrierId == nflId, 1, 0)) %>% 
    select(newId, frameId, nflId, displayName, club, x , y, s, a, o, jerseyNumber,
           tech, ground_gained, ballCarrier) %>% 
    mutate(tech = if_else(is.na(tech), "secondary", tech)) %>% # TODO Define Corner v Safety
    left_join(tackles_data %>% select(-c(gameId, playId)), 
              by = c("nflId", "newId")) %>% 
    mutate_all(~ifelse(is.na(.) & ballCarrier != 1, 0, .)) -> pass_loop_data
  
  for(i in unique_pass_plays) {
    pass_loop_data %>% 
      filter(newId == i) -> play_level_data
    
    ball_carrier_data <- play_level_data[play_level_data$ballCarrier == 1, c("x", "y")] %>% drop_na()
    
    play_level_data %>% 
      filter(ballCarrier == 1 | !(club %in% ball_carrier_data$club) | club == "football") %>%
      mutate(distance_to_ball = ifelse(ballCarrier == 0, 
                                       sqrt((x - ball_carrier_data$x)^2 + (y - ball_carrier_data$y)^2), 0),
             ball_x = max(ball_carrier_data$x, na.rm = T),
             ball_y = max(ball_carrier_data$y, na.rm = T)) %>% # TODO FIX THIS
      ungroup() %>% 
      compute_o_diff() %>% 
      filter(club != "football") %>% 
      mutate(
        o_to_ball = if_else(o_to_ball <= 180, o_to_ball, 360 - o_to_ball),
        orientation_score = 1 - (o_to_ball/180),
        s = if_else(s < 0, 0, s),
        distance_score = distance_to_ball ^ -1,
        position_score = distance_score*orientation_score # TODO Verify distance and speed are on same scale
      ) %>% 
      drop_na(tackle)  %>% 
      select(newId, frameId, nflId, displayName, ballCarrier, def_x = x, def_y = y, 
             def_o = o, s, a, o_to_ball, def_distance_to_ball = distance_to_ball,
             tech, tackle, assist, orientation_score, distance_score, position_score) -> train_data
    
    # Find whether the player was obstructed or not
    train_data %>% filter(ballCarrier == 0) %>% pull(nflId) -> def_players
    
    play_level_data %>% 
      filter(!nflId %in% train_data$nflId & !is.na(nflId)) %>% 
      select(off_nflId = nflId, jerseyNumber, off_x = x, off_y = y, off_o = o) -> off_no_ball_carrier
    
    engaged_values <- c()
    
    for(j in def_players){
      train_data %>% 
        filter(nflId == j) %>% 
        left_join(off_no_ball_carrier, by = c("newId")) %>% 
        mutate(
          off_distance_to_ball = sqrt((off_x - ball_carrier_data$x)^2 + (off_y - ball_carrier_data$y)^2), 
          distance_to_off = sqrt((off_x - def_x)^2 + (off_y - def_y)^2)
        ) %>% 
        filter(off_distance_to_ball * 1.1 < def_distance_to_ball) -> off_locs
      
      engaged_value <- if_else(min(off_locs$distance_to_off) < 2, "E", "F") 
      
      engaged_values <- c(engaged_values, engaged_value)
    }
    
    train_data$engaged <- engaged_values 
    
    pass_train <- bind_rows(pass_train, train_data)
    
    index = index + 1
    print(paste0("Progress: ", round(index/num_pass_plays, digits = 3) *100, "%"))
  }
}

############################## FITTING MODEL ###################################

{
  pass_train %>% 
    mutate(position_score = if_else(position_score == Inf, 12, position_score)) %>% # TODO Handle better
    drop_na(position_score, tackle, newId) -> pass_data
  
  # Likely better handled by accounting for potential obstructions than another way of evaluating position
  summary(pass_model <- glm(tackle ~ orientation_score:distance_score, data = pass_data))
}

{
  # Model Findings
  pass_data$exp_tackle <- pass_model$fitted.values
  pass_data$tackle_oe <- pass_model$residuals
  
  pass_data %>% 
    left_join(pbp_22 %>% select(week, newId), by = c("newId")) %>% 
    group_by(displayName) %>% 
    summarise(
      pass_snaps = n(),
      pass_tackle_snap = mean(tackle, na.rm = T),
      pass_total_tackles = sum(tackle, na.rm = T),
      pass_tackle_oe_snap = mean(tackle_oe, na.rm = T),
      pass_total_tackles_oe = sum(tackle_oe, na.rm = T)
    ) %>% 
    arrange(-pass_tackle_oe_snap) %>% 
    filter(pass_snaps > 75) -> pass_tackles_oe_table
  
  pass_data %>% 
    left_join(pbp_22 %>% select(week, newId), by = c("newId")) %>% 
    mutate(week = if_else(week > 4, "Weeks 5-8", "Weeks 1-4")) %>% 
    group_by(displayName, week) %>% 
    summarise(
      pass_snaps = n(),
      pass_tackle_snap = mean(tackle, na.rm = T),
      pass_total_tackles = sum(tackle, na.rm = T),
      pass_tackle_oe_snap = mean(tackle_oe, na.rm = T),
      pass_total_tackles_oe = sum(tackle_oe, na.rm = T)
    ) %>% 
    arrange(-pass_tackle_oe_snap) %>% 
    filter(pass_snaps > 50) -> pass_tackles_oe_table_wk
}

############################ OVERALL MODEL RESULTS #############################

{
  run_tackles_oe_table %>% 
    left_join(pass_tackles_oe_table, by = c("displayName")) %>% 
    mutate(
      total_snaps = run_snaps + pass_snaps,
      avg_tackle_oe_snap = (pass_tackle_oe_snap * (pass_snaps/total_snaps)) + (run_tackle_oe_snap * (run_snaps/total_snaps)),
      total_tackles_oe = run_total_tackles_oe + pass_total_tackles_oe
    ) %>% 
    arrange(-avg_tackle_oe_snap) -> overall_model_results
  
  run_tackles_oe_table_wk %>% 
    left_join(pass_tackles_oe_table_wk, by = c("displayName", "week")) %>% 
    mutate(
      total_snaps = run_snaps + pass_snaps,
      avg_tackle_oe_snap = (pass_tackle_oe_snap * (pass_snaps/total_snaps)) + (run_tackle_oe_snap * (run_snaps/total_snaps)),
      avg_tackle_snap = (pass_tackle_snap * (pass_snaps/total_snaps)) + (run_tackle_snap * (run_snaps/total_snaps)),
      total_tackles_oe = run_total_tackles_oe + pass_total_tackles_oe
    ) %>% 
    arrange(-avg_tackle_oe_snap) -> overall_model_results_test
  
  # Predictiveness
  overall_model_results_test %>% 
    filter(week == "Weeks 1-4") %>% 
    left_join(overall_model_results_test %>% 
                filter(week == "Weeks 5-8"),
              by = c("displayName")) %>% 
    drop_na(contains("avg_tackle_oe_snap")) %>% 
    ggplot(aes(x = avg_tackle_oe_snap.x, y = avg_tackle_snap.y)) +
    geom_point() +
    ggthemes::theme_clean() +
    geom_smooth(method = "lm", se = F) +
    geom_text(aes(x = max(avg_tackle_oe_snap.x), y = min(avg_tackle_snap.y),
                  label = paste("R^2 =", round(summary(lm(avg_tackle_snap.y ~ avg_tackle_oe_snap.x))$r.squared, 3)),
                  hjust = 1, vjust = -1)) +
    labs(
      x = "Weeks 1-4 Tackles per Snap OE",
      y = "Weeks 5-8 Tackles per Snap OE",
    )
  
  # Tackles/Snap Predictiveness/Stability
  overall_model_results_test %>% 
    filter(week == "Weeks 1-4") %>% 
    left_join(overall_model_results_test %>% 
                filter(week == "Weeks 5-8"),
              by = c("displayName")) %>% 
    drop_na(contains("avg_tackle_oe_snap")) %>% 
    ggplot(aes(x = avg_tackle_snap.x, y = avg_tackle_snap.y)) +
    geom_point() +
    ggthemes::theme_clean() +
    geom_smooth(method = "lm", se = F) +
    geom_text(aes(x = max(avg_tackle_snap.x), y = min(avg_tackle_snap.y),
                  label = paste("R^2 =", round(summary(lm(avg_tackle_snap.y ~ avg_tackle_snap.x))$r.squared, 3)),
                  hjust = 1, vjust = -1))  +
    labs(
      x = "Weeks 1-4 Tackles per Snap",
      y = "Weeks 5-8 Tackles per Snap",
    )
  
  # Tackles OE Stability
  overall_model_results_test %>% 
    filter(week == "Weeks 1-4") %>% 
    left_join(overall_model_results_test %>% 
                filter(week == "Weeks 5-8"),
              by = c("displayName")) %>% 
    drop_na(contains("avg_tackle_oe_snap")) %>% 
    ggplot(aes(x = avg_tackle_oe_snap.x, y = avg_tackle_oe_snap.y)) +
    geom_point() +
    ggthemes::theme_clean() +
    geom_smooth(method = "lm", se = F) +
    geom_text(aes(x = max(avg_tackle_oe_snap.x), y = min(avg_tackle_oe_snap.y),
                  label = paste("R^2 =", round(summary(lm(avg_tackle_oe_snap.y ~ avg_tackle_oe_snap.x))$r.squared, 3)),
                  hjust = 1, vjust = -1))  +
    labs(
      x = "Weeks 1-4 Tackles per Snap",
      y = "Weeks 5-8 Tackles per Snap",
    )
}

########################### OVERALL RESULTS VIZ ################################

{
  overall_model_results %>% 
    left_join(rosters_22 %>% select(full_name, position, headshot_url, team), 
              by = c("displayName" = "full_name")) %>% 
    left_join(teams_colors_logos %>% select(team_abbr, logo = team_logo_espn), 
              by = c("team" = "team_abbr")) %>% 
    filter(!position %in% c("QB", "RB", "WR", "TE", "C", "OT", "OG")) %>% 
    ungroup() %>% 
    mutate(
      headshot_url = if_else(is.na(headshot_url), logo, headshot_url),
      overall_rank = row_number()
    ) %>% 
    #filter(team == "JAX") %>% 
    select(overall_rank, displayName, headshot_url, total_snaps, run_tackle_oe_snap, pass_tackle_oe_snap, avg_tackle_oe_snap) %>% 
    drop_na() %>% 
    tail(20) %>% 
    arrange(-avg_tackle_oe_snap) -> table_data
  
  table_data %>% 
    gt() %>% 
    fmt_percent(columns =  c(run_tackle_oe_snap, pass_tackle_oe_snap, avg_tackle_oe_snap)) %>% 
    cols_label(
      overall_rank = "Rank",
      displayName = "Player",
      total_snaps = "Snaps",
      run_tackle_oe_snap = "RUN", 
      pass_tackle_oe_snap = "PASS", 
      avg_tackle_oe_snap = "CUMULATIVE",
      headshot_url = ""
    ) %>% 
    gt_theme_538() %>% 
    data_color( # Update cell colors...
      columns = c(pass_tackle_oe_snap), # ...for mean_len column
      colors = scales::col_numeric(
        palette = c("#bfcbdb", "#cbd6e4", "#d7e1ee", "white" ,"#df8879", "#c86558", "#b04238"), 
        domain = c(min(overall_model_results$pass_tackle_oe_snap, na.rm = T),
                   max(overall_model_results$pass_tackle_oe_snap, na.rm = T)) # Column scale endpoints
      )
    ) %>% 
    data_color( # Update cell colors...
      columns = c(run_tackle_oe_snap), # ...for mean_len column
      colors = scales::col_numeric(
        palette = c("#bfcbdb", "#cbd6e4", "#d7e1ee", "white", "#ff9991","#df8879", "#c86558", "#b04238"), 
        domain = c(min(overall_model_results$run_tackle_oe_snap, na.rm = T),
                   max(overall_model_results$run_tackle_oe_snap, na.rm = T)) # Column scale endpoints
      )
    )  %>% 
    data_color( # Update cell colors...
      columns = c(avg_tackle_oe_snap), # ...for mean_len column
      colors = scales::col_numeric(
        palette = c("#bfcbdb", "#cbd6e4", "#d7e1ee", "white", "#ff9991" ,"#df8879", "#c86558", "#b04238"), 
        domain = c(min(overall_model_results$avg_tackle_oe_snap, na.rm = T),
                   max(overall_model_results$avg_tackle_oe_snap, na.rm = T)) # Column scale endpoints
      )
    ) %>% 
    gt_img_rows(columns = headshot_url, height = 40) %>% 
    tab_header(
      title = "Tackles Over Expectation Leaders",
      subtitle = "Minimum 200 Snaps ('22 Weeks 1 - 8)"
    )
}

################################ PASS VIZ ######################################

{
  pass_data %>% 
    filter(tackle == 0 & assist == 0) %>% 
    slice_max(exp_tackle) -> max_exp_tkl
}

{
  pass_tracking %>% 
    filter(newId == max_exp_tkl$newId[1]) -> play_data
  
  p <- plot_play(df_track_og = play_data, frame = max_exp_tkl$frameId[1], animated = F, pbp = pbp_22, zoom = F)
  
  # Set the zoom-in limits using coord_cartesian
  p_zoomed <- plot_play(df_track_og = play_data, frame =  max_exp_tkl$frameId[1], animated = F, pbp = pbp_22, zoom = T)
}

{
  {
    data %>% 
      left_join(pbp_22 %>% select(newId, success, yards_gained), by = c("newId")) %>% 
      filter(success == 0 & yards_gained < 5) %>% 
      slice_max(tackle_oe, n = 3) -> min_exp_tkl
  }
  
  {
    i <- 1
    
    run_tracking %>% 
      filter(newId == min_exp_tkl$newId[i]) -> play_data
    
    p <- plot_play(df_track_og = play_data, frame = min_exp_tkl$frameId[i], animated = F, pbp = pbp_22, zoom = F)
    
    # Set the zoom-in limits using coord_cartesian
    p_zoomed <- plot_play(df_track_og = play_data, frame =  min_exp_tkl$frameId[i], animated = F, pbp = pbp_22, zoom = T)
  }
}

################################# RUN VIZ ######################################

{
  run_data %>% 
    filter(tackle == 0 & assist == 0) %>% 
    slice_max(exp_tackle) -> max_exp_tkl
}

{
  run_tracking %>% 
    filter(newId == max_exp_tkl$newId[1]) -> play_data
  
  p <- plot_play(df_track_og = play_data, frame = max_exp_tkl$frameId[1], animated = F, pbp = pbp_22, zoom = F)
  
  # Set the zoom-in limits using coord_cartesian
  p_zoomed <- plot_play(df_track_og = play_data, frame =  max_exp_tkl$frameId[1], animated = F, pbp = pbp_22, zoom = T)
}

{
  {
    run_data %>% 
      left_join(pbp_22 %>% select(newId, success, yards_gained), by = c("newId")) %>% 
      filter(success == 0 & yards_gained < 5) %>% 
      slice_max(tackle_oe, n = 3) -> min_exp_tkl
  }
  
  {
    i <- 1
    play_id <- 20220908001406
    frame_id <- 19 #min_exp_tkl$frameId[i]
    
    run_tracking %>% 
      filter(newId == play_id) -> play_data
    
    p <- plot_play(df_track_og = play_data, frame = min_exp_tkl$frameId[i], animated = F, pbp = pbp_22, zoom = F)
    
    # Set the zoom-in limits using coord_cartesian
    p_zoomed <- plot_play(df_track_og = play_data, frame =  frame_id, animated = F, pbp = pbp_22, zoom = T)
  }
}
