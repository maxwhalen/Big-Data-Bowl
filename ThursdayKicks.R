{
  library(nflverse)
}

{
  pbp <- load_pbp(seasons = 2017:2023)
}

{
  pbp %>% 
    mutate(day_of_week = weekdays(as.Date(game_date)),
           day_of_week = if_else(day_of_week %in% c("Saturday", "Sunday"), "Weekend", day_of_week)) %>% 
    group_by(game_id, day_of_week) %>% 
    summarise(fga = sum(field_goal_attempt, na.rm = T)) %>% 
    group_by(day_of_week) %>% 
    summarise(avg_fga = mean(fga, na.rm = T)) %>% 
    filter(day_of_week %in% c("Monday", "Thursday", "Weekend"))
}