### Copa America 2024 Simulations
library(tidyverse)
library(furrr)
library(arrow)
options(dplyr.summarise.inform = F)
plan(multisession(workers = 12))
source('helpers.R')

### Simulation Parameters
n_sims <- 10000
set.seed(12345)
run_date <- case_when(lubridate::hour(Sys.time()) <= 10 ~as.Date(Sys.Date()-1),
                      T ~ as.Date(Sys.Date() ))


### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

### Read in Ratings and Schedule
df_ratings <- read_csv('predictions/ratings.csv')
schedule <- 
  read_csv('data/copa2024_schedule.csv') %>% 
  mutate('team1_score' = ifelse(date > run_date, NA, team1_score),
         'team2_score' = ifelse(date > run_date, NA, team2_score)) %>% 
  mutate('team1_score' = case_when(is.na(shootout_winner) ~ team1_score,
                                   shootout_winner == team1 ~ 0.1 + team1_score,
                                   shootout_winner == team2 ~ -0.1 + team1_score))
### Expected Score for Each Game
schedule <- adorn_xg(schedule)

### Simulate Group Stage
df_group_stage <- filter(schedule, !is.na(group))
if(any(is.na(schedule$team1_score[1:24]))) {
  dfs_group_stage <- 
    map_dfr(1:n_sims, ~df_group_stage, .id = 'sim_id') %>% 
    mutate('sim_id' = as.numeric(sim_id))
  
  group_stage_results <-
    sim_group_stage(dfs_group_stage, euro = F) %>%
    mutate('progress' = place < 3)
  
  ### Knockout Round Brackets
  knockout_brackets <- 
    future_map_dfr(group_stage_results %>% 
                     group_by(sim_id) %>% 
                     group_split(),
                   ~build_knockout_bracket(.x, euro = F),
                   .id = 'sim_id') %>% 
    mutate('sim_id' = as.numeric(sim_id))
  
}  else {
  knockout_brackets <-
    future_map_dfr(1:n_sims, ~filter(schedule,  str_detect(ko_round, 'QF')), .id = 'sim_id')  %>% 
    mutate('sim_id' = as.numeric(sim_id))
  gsr <- sim_group_stage(df_group_stage %>% mutate('sim_id' = 1), euro = F) %>% select(-sim_id)
  group_stage_results <- 
    map_dfr(1:n_sims, ~mutate(gsr, 'sim_id' = .x)) %>% 
    mutate('progress' = place < 3)
}

df_qf <- schedule %>% filter(ko_round == 'QF')
df_sf <- schedule %>% filter(ko_round == 'SF')
df_f <- schedule %>% filter(ko_round == 'FINAL')

### QF
knockout_brackets <- 
  future_map_dfr(1:n_sims, ~df_qf, .id = 'sim_id') %>% 
  mutate('sim_id' = as.numeric(sim_id)) %>% 
  bind_cols(knockout_brackets %>% select('tm1' = team1, 'tm2' = team2)) %>% 
  mutate('team1' = ifelse(is.na(team1), tm1, team1),
         'team2' = ifelse(is.na(team2), tm2, team2)) %>% 
  select(-lambda_1, -lambda_2, -tm1, -tm2) %>% 
  adorn_xg(.)

qf_results <- 
  sim_ko_round(knockout_brackets) %>% 
  mutate('winner' = ifelse(team1_score > team2_score, team1, team2))

### SF
knockout_brackets <-
  future_map_dfr(1:n_sims, ~df_sf, .id = 'sim_id') %>% 
  mutate('sim_id' = as.numeric(sim_id))  %>% 
  bind_cols(
    qf_results %>% 
      group_by(sim_id) %>% 
      reframe('tm1' = winner[c(1,3)],
              'tm2' = winner[c(2,4)]) %>% 
      ungroup() %>% 
      select(tm1, tm2)
  ) %>% 
  mutate('team1' = ifelse(is.na(team1), tm1, team1),
         'team2' = ifelse(is.na(team2), tm2, team2)) %>% 
  select(-lambda_1, -lambda_2, -tm1, -tm2) %>% 
  adorn_xg(.)

sf_results <-
  sim_ko_round(knockout_brackets) %>% 
  mutate('winner' = ifelse(team1_score > team2_score, team1, team2))

### Finals
knockout_brackets <-
  future_map_dfr(1:n_sims, ~df_f, .id = 'sim_id') %>% 
  mutate('sim_id' = as.numeric(sim_id))  %>% 
  bind_cols(
    sf_results %>% 
      group_by(sim_id) %>% 
      reframe('tm1' = winner[c(1)],
              'tm2' = winner[c(2)]) %>% 
      ungroup() %>% 
      select(tm1, tm2)
  ) %>% 
  mutate('team1' = ifelse(is.na(team1), tm1, team1),
         'team2' = ifelse(is.na(team2), tm2, team2)) %>% 
  select(-lambda_1, -lambda_2, -tm1, -tm2) %>% 
  adorn_xg(.)

finals_results <- 
  sim_ko_round(knockout_brackets) %>% 
  mutate('winner' = ifelse(team1_score > team2_score, team1, team2))

### Aggregate Results
sf_teams <- sf_results %>% pivot_longer(c('team1', 'team2')) %>% pull(value)
final_teams <- finals_results %>% pivot_longer(c('team1', 'team2')) %>% pull(value)
winners <- finals_results %>% pull(winner)

df_stats <-
  group_stage_results %>% 
  group_by(team, group) %>% 
  summarise('mean_pts' = mean(points),
            'mean_gd' = mean(goal_diff),
            'qf' = mean(progress),
            'sf' = sum(team == sf_teams)/n_sims,
            'finals' = sum(team == final_teams)/n_sims,
            'champ' = sum(team == winners)/n_sims) %>% 
  ungroup() 

### Save Results
write_csv(df_stats, 'predictions/copa_sim_results.csv')

### Track History
history <- 
  read_csv('predictions/copa_history.csv') %>% 
  filter(date != run_date) %>% 
  bind_rows(df_stats %>% mutate('date' = run_date)) %>% 
  arrange(date)
write_csv(history, 'predictions/copa_history.csv')

