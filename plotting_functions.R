{
  library(tidyverse) 
  library(devtools)
  library(dplyr)
  library(gganimate)
  library(ggfootball)
  library(ggforce)
  library(ggplot2)
  library(ggrepel)
  library(ggshadow)
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

#' Plot a play
#'
#' @description Plot or animate a play.
#' @param df_track A df of tracking data from one play.
#' @param orientation Show lines representing where player is facing (default = T).
#' @param dot_size Size of player dots (default = 6).
#' @param segment_length Length of orientation segment lines (default = 2.5).
#' @param segment_size Width of orientation segment lines (default = 1.5).
#' @param numbers Show player jersey numbers (default = T).
#' @param animated Whether play is animated, rather than a still frame (default = T).
#' @param animated_h If animated, height of animated image (default = 4).
#' @param animated_w If animated, width of animated image (default = 8).
#' @param animated_res If animated, resolution of animated image (default = 200).
#' @param frame frameId to plot (default = NULL, ie plot all provided frames).
#' @param zoom zoom to play area (default = FALSE, ie plot shows full field).
#'  @param highlight highlight a column attribute (default = FALSE, colmust be named 'highlight_col').
#' @export
plot_play <- function(
    df_track_og,
    pbp,
    orientation = TRUE,
    dot_size = 6,
    segment_length = 2.5,
    segment_size = 1.5,
    numbers = TRUE,
    animated = FALSE,
    animated_h = 4,
    animated_w = 8,
    animated_res = 200,
    frame = NULL,
    zoom = TRUE,
    highlight = FALSE,
    voronoi = FALSE
) {
  
  df_track_og %>% 
    left_join(
      pbp_22 %>% select(newId, nflfastr_game_id = game_id, down, qtr, ydstogo, desc), 
      by = c("newId")
    ) %>% 
    left_join(teams_colors_logos %>% select(club = team_abbr, team_color, team_color2)) %>%
    mutate(defense = if_else(club == defensiveTeam, 1, 0)) -> df_track
  
  caption <- glue::glue("{df_track$nflfastr_game_id[1]} {df_track$down[1]}&{df_track$ydstogo[1]}: Q{df_track$qtr[1]} {df_track$desc[1]}")
  
  if (!is.null(frame)) {
    
    df_track_filtered <- df_track %>% filter(frameId == frame) %>% distinct()
    
  }
  
  fig <- ggfootball() 
  
  print((df_track_filtered$highlight_col / sum(df_track_filtered$highlight_col)))
  
  if (highlight) { #TODO MAKE WORK
    fig <- fig +
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = "red",
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   18, 19
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   0, (dot_size * 10) * (df_track_filtered$highlight_col / sum(df_track_filtered$highlight_col))
                 ),
                 alpha = 1 * (df_track_filtered$highlight_col / sum(df_track_filtered$highlight_col))
      ) + 
      geom_point(data = df_track_filtered, 
                 aes(x, y),
                 color = "orangered",
                 shape = ifelse(
                   df_track_filtered$club == "football",
                   18, 19
                 ),
                 size = ifelse(
                   df_track_filtered$club == "football",
                   0, (dot_size * 15) * (df_track_filtered$highlight_col / sum(df_track_filtered$highlight_col))
                 ),
                 alpha = 0.7 * (df_track_filtered$highlight_col / sum(df_track_filtered$highlight_col))
      ) 
  } 
  
  fig <- fig +
    geom_point(data = df_track_filtered, 
               aes(x, y),
               color = if_else(df_track_filtered$club == "football", "#825736", df_track_filtered$team_color),
               shape = ifelse(
                 df_track_filtered$club == "football",
                 18, 19
               ),
               size = ifelse(
                 df_track_filtered$club == "football",
                 dot_size*0.5, dot_size
               )
    ) +
    geom_point(data = df_track_filtered, 
               aes(x, y),
               color = if_else(df_track_filtered$club == "football", "white", df_track_filtered$team_color2),
               shape = ifelse(
                 df_track_filtered$club == "football",
                 5, 1
               ),
               size = ifelse(
                 df_track_filtered$club == "football",
                 dot_size*0.51 , dot_size
               )
    ) +
    labs(
      caption = caption
    ) +
    theme(
      plot.title = element_blank(),
      plot.margin = margin(.1, 0, .5, 0, "cm"),
      plot.caption = element_text(size = 8)
    )
  
  if (orientation == TRUE & "o" %in% names(df_track_filtered)) {
    
    fig <- fig +
      # orientation lines
      geom_segment(
        data = df_track_filtered,
        aes(x, y, xend = x + segment_length * o_x, yend = y + segment_length * o_y),
        color = df_track_filtered$team_color, 
        linewidth = segment_size
      )
    
  }
  
  if (numbers) {
    
    fig <- fig +
      geom_text(
        data = df_track_filtered,
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = ifelse(df_track_filtered$defense == 1, 
                        df_track_filtered$team_color2, 
                        "white"),
                        size = 2
      )
    
  }
  
  if(zoom) {
    fig <- fig + 
      coord_cartesian(xlim = c(min(df_track_filtered$x) - 1, max(df_track_filtered$x) + 1), 
                      ylim = c(min(df_track_filtered$y) - 3, max(df_track_filtered$y) + 3))
  }
  
  if(voronoi) {
    fig <- fig +
      geom_voronoi_tile(
        data = df_track_filtered,
        aes(x = x, y = y, alpha = 0.75),
        
      )
  }
  
  if (animated) {
    
    # if (animated_output == "mp4") {
    #   renderer <- gganimate::gifski_renderer()
    # } else {
    #   renderer <- gganimate::av_renderer()
    # }
    #
    fig <- fig +
      gganimate::transition_time(df_track_filtered$frameId)
    
    fig <- gganimate::animate(
      fig,
      # renderer = renderer,
      height = animated_h, width = animated_w, units = "in",
      res = animated_res,
      nframes = n_distinct(df_track_filtered$frameId),
      start_pause = 6,
      end_pause = 4
    )
    
  }
  
  return(fig)
  
}


# helper function to not make every table have so many lines of code
make_table <- function(df) {
  df %>%
    gt::gt() %>%
    gt::tab_style(
      style = gt::cell_text(color = "black", weight = "bold"),
      locations = list(
        gt::cells_column_labels(dplyr::everything())
      )
    ) %>%
    gt::tab_options(
      row_group.border.top.width = gt::px(3),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.top.color = "black",
      table.border.top.width = gt::px(1),
      table.border.bottom.color = "white",
      table.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = gt::px(2),
      row.striping.background_color = '#FFFFFF',
      row.striping.include_table_body = TRUE,
      table.background.color = '#F2F2F2',
      data_row.padding = gt::px(2),
      table.font.size = gt::px(16L)
    ) %>%
    return()
}

############################ HELPER FUNCTIONS ##################################
interpolate_color <- function(value, low_color = "forestgreen", high_color = "red") {
  # Validate input value
  value <- pmax(0, pmin(1, value))
  
  # Create a function that interpolates between two colors
  color_interp <- colorRampPalette(c(low_color, high_color))
  
  # Interpolate the color based on the input value
  interpolated_color <- color_interp(value)
  
  return(interpolated_color)
}

