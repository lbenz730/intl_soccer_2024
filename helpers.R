### Dictionary of Team Codes
team_codes <- function(df) {
  teams <- sort(unique(c(df$home_team, df$away_team)))
  codes <- 1:length(teams)
  names(codes) <- teams
  
  return(codes)
}

### Generate Goal Expectations Given Teams + Location
goal_expectations <- function(team_1, team_2, location) {
  if(is.na(team_1)) {
    return(list('lambda_1' = NA,
                'lambda_2' = NA))
  }
  ### Team 1 Offensive/Defensive Ratings
  alpha_1 <- filter(df_ratings, team == team_1) %>% pull(alpha)
  delta_1 <- filter(df_ratings, team == team_1) %>% pull(delta)
  
  ### Team 2 Offensive/Defensive Ratings
  alpha_2 <- filter(df_ratings, team == team_2) %>% pull(alpha)
  delta_2 <- filter(df_ratings, team == team_2) %>% pull(delta) 
  
  ### Location Adjustments
  loc_1 <- case_when(team_1 == location ~ home_field,
                     team_2 == location ~ 0,
                     T ~ neutral_field)
  
  loc_2 <- case_when(team_1 == location ~ 0,
                     team_2 == location ~ home_field,
                     T ~ neutral_field)
  
  ### Goal Expectations
  lambda_1 <- exp(mu + alpha_1 + delta_2 + loc_1)
  lambda_2 <- exp(mu + alpha_2 + delta_1 + loc_2)
  
  return(list('lambda_1' = lambda_1, 
              'lambda_2' = lambda_2))
  
}

adorn_xg <- function(df) {
  df_xg <- future_pmap_dfr(list('team_1' = df$team1,
                                'team_2' = df$team2,
                                'location' = df$location),
                           ~{as_tibble(goal_expectations(..1, ..2, ..3))}) 
  return(bind_cols(df, df_xg))
}

### Simulate Group Stage
sim_group_stage <- function(df_group_stage, euro = T) {
  ### Sim Each Game
  ix1 <- is.na(df_group_stage$team1_score)
  ix2 <- is.na(df_group_stage$team2_score)
  df_group_stage$team1_score[ix1] <- 
    rpois(sum(ix1), lambda = df_group_stage$lambda_1[ix1])
  df_group_stage$team2_score[ix2] <- 
    rpois(sum(ix2), lambda = df_group_stage$lambda_2[ix2])
  
  ### Aggregate Results
  df_results <- 
    bind_rows(
      select(df_group_stage, sim_id, 'team' = team1, 'opp' = team2, 'team_score' = team1_score, 'opp_score' = team2_score, group),
      select(df_group_stage, sim_id, 'team' = team2, 'opp' = team1, 'team_score' = team2_score, 'opp_score' = team1_score, group)
    )
  
  standings <-
    df_results %>% 
    group_by(sim_id, group, team) %>% 
    summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
              'goal_diff' = sum(team_score - opp_score),
              'goals_scored' = sum(team_score),
              'goals_allowed' = sum(opp_score)) %>% 
    ungroup() %>% 
    arrange(sim_id, group, desc(points), desc(goal_diff), desc(goals_scored)) %>% 
    group_by(sim_id, group) %>% 
    mutate('place' = 1:n()) %>% 
    ungroup()
  
  ### Tiebreakers
  standings <-
    future_map2_dfr(standings %>% 
                      group_by(sim_id) %>% 
                      group_split(),
                    df_results %>% 
                      group_by(sim_id) %>% 
                      group_split(),
                    ~group_tiebreak(.x, .y, euro),
                    .id = 'sim_id', 
                    .progress = T) %>%
    mutate('sim_id' = as.numeric(sim_id)) %>% 
    arrange(sim_id, group, place) 
  
  
  ### 3rd Place Team Advance
  if(euro) {
    third_place <- 
      standings %>% 
      group_by(sim_id) %>% 
      filter(place == 3) %>% 
      arrange(desc(points), desc(goal_diff), desc(goals_scored)) %>% 
      slice(1:4) %>% 
      ungroup()
    
    ### Teams To Advance
    standings <- 
      standings %>% 
      left_join(select(third_place, sim_id, team) %>% mutate('progress_T' = T)) %>% 
      mutate('progress' = case_when(place < 3 ~ T,
                                    place == 3 & progress_T ~ T,
                                    T ~ F))
  }
  
  if(euro) {
    write_parquet(standings, 'predictions/euro_sim_standings.parquet')
    write_parquet(df_results, 'predictions/euro_sim_game_results.parquet')
  } else {
    write_parquet(standings, 'predictions/copa_sim_standings.parquet')
    write_parquet(df_results, 'predictions/copa_sim_game_results.parquet')
  }
  
  return(standings)
  
}

group_tiebreak <- function(standings, df_results, euro = T) {
  options(dplyr.summarise.inform = F)
  
  max_g <- ifelse(euro, 6, 4)
  
  df_final <- NULL
  
  for(g in LETTERS[1:max_g]) {
    group_standings <- filter(standings, group == g)
    group_results <- filter(df_results, group == g)
    
    if(group_standings$points[1] != group_standings$points[4]) {
      if(group_standings$points[1] == group_standings$points[3]) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[1:3], opp %in% group_standings$team[1:3]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:3, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[1:3] <- ix
      } 
      if((group_standings$points[1] != group_standings$points[3]) & 
         (group_standings$points[1] == group_standings$points[2])) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[1:2], opp %in% group_standings$team[1:2]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[1:2] <- ix
      } 
      if(group_standings$points[2] == group_standings$points[4]) {
        tiebreak_order <- 
          group_results %>% 
          filter(team %in% group_standings$team[2:4], opp %in% group_standings$team[2:4]) %>% 
          group_by(group, team) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored)) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:3, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[2:4] <- ix
      }
      if((group_standings$points[2] != group_standings$points[4]) &
         (group_standings$points[1] != group_standings$points[3]) &
         (group_standings$points[2] == group_standings$points[3])) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[2:3], opp %in% group_standings$team[2:3]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[2:3] <- ix
      }
      if((group_standings$points[2] != group_standings$points[4]) &
         (group_standings$points[3] == group_standings$points[4])) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[3:4], opp %in% group_standings$team[3:4]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[3:4] <- ix
      }
    }
    df_final <- bind_rows(df_final, group_standings)
  }
  
  
  return(df_final)
}

build_knockout_bracket <- function(group_stage_results, euro = T) {
  
  a <- filter(group_stage_results, group == 'A') %>% pull(team)
  b <- filter(group_stage_results, group == 'B') %>% pull(team)
  c <- filter(group_stage_results, group == 'C') %>% pull(team)
  d <- filter(group_stage_results, group == 'D') %>% pull(team)
  if(euro) {
    third_place_groups <- 
      group_stage_results %>% 
      filter(progress, place == 3) %>% 
      pull(group) %>% 
      paste(collapse = '')
    
    e <- filter(group_stage_results, group == 'E') %>% pull(team)
    f <- filter(group_stage_results, group == 'F') %>% pull(team)
    
    ### Third Place Teams
    t1 <- case_when(third_place_groups == 'ABCD' ~ a[3],
                    third_place_groups == 'ABCE' ~ a[3],
                    third_place_groups == 'ABCF' ~ a[3],
                    third_place_groups == 'ABDE' ~ d[3],
                    third_place_groups == 'ABDF' ~ d[3],
                    third_place_groups == 'ABEF' ~ e[3],
                    third_place_groups == 'ACDE' ~ e[3],
                    third_place_groups == 'ACDF' ~ f[3],
                    third_place_groups == 'ACEF' ~ e[3],
                    third_place_groups == 'ADEF' ~ e[3],
                    third_place_groups == 'BCDE' ~ e[3],
                    third_place_groups == 'BCDF' ~ f[3],
                    third_place_groups == 'BCEF' ~ f[3],
                    third_place_groups == 'BDEF' ~ f[3],
                    third_place_groups == 'CDEF' ~ f[3])
    
    t2 <- case_when(third_place_groups == 'ABCD' ~ d[3],
                    third_place_groups == 'ABCE' ~ e[3],
                    third_place_groups == 'ABCF' ~ f[3],
                    third_place_groups == 'ABDE' ~ e[3],
                    third_place_groups == 'ABDF' ~ f[3],
                    third_place_groups == 'ABEF' ~ f[3],
                    third_place_groups == 'ACDE' ~ d[3],
                    third_place_groups == 'ACDF' ~ d[3],
                    third_place_groups == 'ACEF' ~ f[3],
                    third_place_groups == 'ADEF' ~ f[3],
                    third_place_groups == 'BCDE' ~ d[3],
                    third_place_groups == 'BCDF' ~ d[3],
                    third_place_groups == 'BCEF' ~ e[3],
                    third_place_groups == 'BDEF' ~ e[3],
                    third_place_groups == 'CDEF' ~ e[3])
    
    t3 <- case_when(third_place_groups == 'ABCD' ~ b[3],
                    third_place_groups == 'ABCE' ~ b[3],
                    third_place_groups == 'ABCF' ~ b[3],
                    third_place_groups == 'ABDE' ~ a[3],
                    third_place_groups == 'ABDF' ~ a[3],
                    third_place_groups == 'ABEF' ~ b[3],
                    third_place_groups == 'ACDE' ~ c[3],
                    third_place_groups == 'ACDF' ~ c[3],
                    third_place_groups == 'ACEF' ~ c[3],
                    third_place_groups == 'ADEF' ~ d[3],
                    third_place_groups == 'BCDE' ~ b[3],
                    third_place_groups == 'BCDF' ~ c[3],
                    third_place_groups == 'BCEF' ~ c[3],
                    third_place_groups == 'BDEF' ~ d[3],
                    third_place_groups == 'CDEF' ~ d[3])
    
    t4 <- case_when(third_place_groups == 'ABCD' ~ c[3],
                    third_place_groups == 'ABCE' ~ c[3],
                    third_place_groups == 'ABCF' ~ c[3],
                    third_place_groups == 'ABDE' ~ b[3],
                    third_place_groups == 'ABDF' ~ b[3],
                    third_place_groups == 'ABEF' ~ a[3],
                    third_place_groups == 'ACDE' ~ a[3],
                    third_place_groups == 'ACDF' ~ a[3],
                    third_place_groups == 'ACEF' ~ a[3],
                    third_place_groups == 'ADEF' ~ a[3],
                    third_place_groups == 'BCDE' ~ c[3],
                    third_place_groups == 'BCDF' ~ b[3],
                    third_place_groups == 'BCEF' ~ b[3],
                    third_place_groups == 'BDEF' ~ b[3],
                    third_place_groups == 'CDEF' ~ c[3])
    
    return(tibble('team1' = c(b[1], a[1], f[1], d[2], e[1], d[1], c[1], a[2]),
                  'team2' = c(t1, c[2], t4, e[2], t3, f[2], t2, b[2])))
    
  } else {
    return(tibble('team1' = c(a[1], b[1], c[1], d[1]),
                  'team2' = c(b[2], a[2], d[2], c[2])))
    
  }
  
}

### Simulate KO Round Games
sim_ko_round <- function(df) {
  
  lambdas_1 <- df$lambda_1[is.na(df$team1_score)]
  lambdas_2 <- df$lambda_2[is.na(df$team2_score)]
  
  n <- length(lambdas_1)
  if(n > 0) {
    goals_1 <- rpois(n, lambdas_1)
    goals_2 <- rpois(n, lambdas_2)
    
    ### In event of tie, assume scoring rate of 1/3 for extra time
    tie_ix <- goals_1 == goals_2
    if(sum(tie_ix) > 0) {
      goals_1[tie_ix] <- goals_1[tie_ix] + rpois(sum(tie_ix), lambdas_1[tie_ix]/3)
      goals_2[tie_ix] <- goals_2[tie_ix] + rpois(sum(tie_ix), lambdas_2[tie_ix]/3)
      
      ## If Still Tied, Flip Coin For Shoot Out (increase/decrease goals_1 w/ p = 0.5)
      tie_ix <- goals_1 == goals_2
      if(sum(tie_ix) > 0) {
        goals_1[tie_ix] <- goals_1[tie_ix] + sample(c(0.1, -0.1), size = sum(tie_ix), replace = T)
      }
    }
    df$team1_score[is.na(df$team1_score)] <- goals_1
    df$team2_score[is.na(df$team2_score)] <- goals_2
  }  
  
  
  
  return(df)
}

### W/D/L given expectations
match_probs <- function(lambda_1, lambda_2) {
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(list('win' = win_prob, 'draw' = tie_prob, 'loss' = loss_prob))
}


match_probs_ko <- function(lambda_1, lambda_2) {
  regulation <- match_probs(lambda_1, lambda_2)
  extra_time <- match_probs(lambda_1/3, lambda_2/3)
  
  win <- regulation$win + regulation$draw * extra_time$win + 0.5 * regulation$draw * extra_time$draw 
  loss <- regulation$loss + regulation$draw * extra_time$loss + 0.5 * regulation$draw * extra_time$draw 
  
  return(list('win_ko' = win, 'loss_ko' = loss))
}

### Custom ggplot theme
theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 24, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  plot.subtitle = element_text(size = 20, hjust = 0.5),
                  strip.text = element_text(size = 14, hjust = 0.5),
                  legend.position = "none")
)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.2*a", channel = "alpha")
}
