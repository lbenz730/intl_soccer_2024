### Euro 2024 Game Predictions
library(tidyverse)
options(dplyr.summarise.inform = F)
source('helpers.R')
library(gt)
library(furrr)

### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

### Read in Ratings and Schedule
df_ratings <- read_csv('predictions/ratings.csv')
euro_schedule <- read_csv('data/euro2024_schedule.csv')
copa_schedule <- read_csv('data/copa2024_schedule.csv')

### Expected Score for Each Game
euro_preds  <- adorn_xg(euro_schedule)
copa_preds  <- adorn_xg(copa_schedule)

### WLD Probs
euro_preds <- bind_cols(euro_preds, map2_dfr(euro_preds$lambda_1, euro_preds$lambda_2, ~as_tibble(match_probs(.x,.y))))
euro_preds_old <- read_csv('predictions/euro_game_predictions.csv')
euro_preds_old <- 
  euro_preds_old %>% 
  filter(date < Sys.Date()) %>% 
  select(lambda_1, lambda_2, win, draw, loss)
euro_preds[euro_preds$date < Sys.Date(), c('lambda_1', 'lambda_2', 'win', 'draw', 'loss')] <- 
  euro_preds_old

euro_preds <- bind_cols(euro_preds, map2_dfr(euro_preds$lambda_1, euro_preds$lambda_2, ~as_tibble(match_probs_ko(.x,.y))))
write_csv(euro_preds, 'predictions/euro_game_predictions.csv')


copa_preds <- bind_cols(copa_preds, map2_dfr(copa_preds$lambda_1, copa_preds$lambda_2, ~as_tibble(match_probs(.x,.y))))
copa_preds_old <- read_csv('predictions/copa_game_predictions.csv')
copa_preds_old <- 
  copa_preds_old %>% 
  filter(date < Sys.Date()) %>% 
  select(lambda_1, lambda_2, win, draw, loss)
copa_preds[copa_preds$date < Sys.Date(), c('lambda_1', 'lambda_2', 'win', 'draw', 'loss')] <- 
  copa_preds_old

copa_preds <- bind_cols(copa_preds, map2_dfr(copa_preds$lambda_1, copa_preds$lambda_2, ~as_tibble(match_probs_ko(.x,.y))))
write_csv(copa_preds, 'predictions/copa_game_predictions.csv')


### Graphics
df_preds <- 
  euro_preds %>% 
  filter(!is.na(group)) %>% 
  arrange(date) %>% 
  mutate('logo1' = paste0('flags/', team1, '.png')) %>% 
  mutate('logo2' = paste0('flags/', team2, '.png'))  %>% 
  select(date, team1, logo1, team2, logo2, group, lambda_1, lambda_2, win, draw, loss)

### Graphics
df_preds_ko <- 
  euro_preds %>% 
  filter(!is.na(ko_round)) %>% 
  arrange(date) %>% 
  mutate('logo1' = paste0('flags/', team1, '.png')) %>% 
  mutate('logo2' = paste0('flags/', team2, '.png'))  %>% 
  select(date, team1, logo1, team2, logo2, ko_round, lambda_1, lambda_2, win, loss, draw, win_ko, loss_ko)

### Graphics
df_preds_c <- 
  copa_preds %>% 
  filter(!is.na(group)) %>% 
  arrange(date) %>% 
  mutate('logo1' = paste0('flags/', team1, '.png')) %>% 
  mutate('logo2' = paste0('flags/', team2, '.png'))  %>% 
  select(date, team1, logo1, team2, logo2, group, lambda_1, lambda_2, win, draw, loss)

### Graphics
df_preds_ko_c <- 
  copa_preds %>% 
  filter(!is.na(ko_round)) %>% 
  arrange(date) %>% 
  mutate('logo1' = paste0('flags/', team1, '.png')) %>% 
  mutate('logo2' = paste0('flags/', team2, '.png'))  %>% 
  select(date, team1, logo1, team2, logo2, ko_round, lambda_1, lambda_2, win, loss, draw, win_ko, loss_ko)


t1 <-
  df_preds %>% 
  slice(1:12) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Euro Cup 2024 Game Predictions**'),
    subtitle = md('**Matchweek 1**'),
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtExtras::gtsave_extra(t1, 'figures/euro/matchweek1.png')

t2 <-
  df_preds %>% 
  slice(13:24) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Euro Cup 2024 Game Predictions**'),
    subtitle = md('**Matchweek 2**'),
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtExtras::gtsave_extra(t2, 'figures/euro/matchweek2.png')



t3 <-
  df_preds %>% 
  slice(25:36) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Euro Cup 2024 Game Predictions**'),
    subtitle = md('**Matchweek 3**'),
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtExtras::gtsave_extra(t3, 'figures/euro/matchweek3.png')

t1c <-
  df_preds_c %>% 
  slice(1:8) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_c[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Copa América 2024 Game Predictions**'),
    subtitle = md('**Matchweek 1**'),
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtExtras::gtsave_extra(t1c, 'figures/copa/matchweek1.png')

t2c <-
  df_preds_c %>% 
  slice(9:16) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_c[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Copa América 2024 Game Predictions**'),
    subtitle = md('**Matchweek 2**'),
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtExtras::gtsave_extra(t2c, 'figures/copa/matchweek2.png')



t3c <-
  df_preds_c %>% 
  slice(17:24) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_c[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Copa América 2024 Game Predictions**'),
    subtitle = md('**Matchweek 3**'),
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtExtras::gtsave_extra(t3c, 'figures/copa/matchweek3.png')



t4 <-
  df_preds_ko %>%
  filter(str_detect(ko_round, 'R\\d+')) %>%
  select(-ko_round) %>%
  gt() %>%
  
  cols_align('center') %>%
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>%
  fmt_percent(columns = c(win_ko, loss_ko, win, loss, draw), decimals = 0, sep_mark = '') %>%
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_ko[, c('lambda_1', 'lambda_2')], na.rm = T))) %>%
  data_color(columns = c(win_ko, loss_ko, win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(logo2, lambda_2, draw)
      )
    )
  ) %>%
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>%
  tab_spanner(label = 'Regulation Result', columns = c('win', 'loss', 'draw')) %>%
  tab_spanner(label = 'To Advance', columns = c('win_ko', 'loss_ko')) %>%
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  text_transform(
    locations = cells_body(columns = "logo2"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    win = 'Team 1 Win', 
    loss = 'Team 2 Win',
    draw = 'Extra Time',
    'lambda_1' = 'Team 1',
    'lambda_2' = 'Team 2',
    'win_ko' = 'Team 1',
    'loss_ko' = 'Team 2'
    
  ) %>%
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Euro Cup 2024 Game Predictions**'),
    subtitle = md('**Round of 16**'),
  ) %>%
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )

gtExtras::gtsave_extra(t4, 'figures/euro/r16_preds.png')


t5 <-
  df_preds_ko %>%
  filter(str_detect(ko_round, 'QF')) %>%
  select(-ko_round) %>%
  gt() %>%
  
  cols_align('center') %>%
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>%
  fmt_percent(columns = c(win_ko, loss_ko, win, loss, draw), decimals = 0, sep_mark = '') %>%
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_ko[, c('lambda_1', 'lambda_2')], na.rm = T))) %>%
  data_color(columns = c(win_ko, loss_ko, win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(logo2, lambda_2, draw)
      )
    )
  ) %>%
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>%
  tab_spanner(label = 'Regulation Result', columns = c('win', 'loss', 'draw')) %>%
  tab_spanner(label = 'To Advance', columns = c('win_ko', 'loss_ko')) %>%
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  text_transform(
    locations = cells_body(columns = "logo2"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    win = 'Team 1 Win', 
    loss = 'Team 2 Win',
    draw = 'Extra Time',
    'lambda_1' = 'Team 1',
    'lambda_2' = 'Team 2',
    'win_ko' = 'Team 1',
    'loss_ko' = 'Team 2'
    
  ) %>%
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Euro Cup 2024 Game Predictions**'),
    subtitle = md('**Quarterfinals**'),
  ) %>%
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )

gtExtras::gtsave_extra(t5, 'figures/euro/qf_preds.png')


t4c <-
  df_preds_ko_c %>%
  filter(str_detect(ko_round, 'QF')) %>%
  select(-ko_round) %>%
  gt() %>%
  
  cols_align('center') %>%
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>%
  fmt_percent(columns = c(win_ko, loss_ko, win, loss, draw), decimals = 0, sep_mark = '') %>%
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_ko[, c('lambda_1', 'lambda_2')], na.rm = T))) %>%
  data_color(columns = c(win_ko, loss_ko, win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(logo2, lambda_2, draw)
      )
    )
  ) %>%
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>%
  tab_spanner(label = 'Regulation Result', columns = c('win', 'loss', 'draw')) %>%
  tab_spanner(label = 'To Advance', columns = c('win_ko', 'loss_ko')) %>%
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  text_transform(
    locations = cells_body(columns = "logo2"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    win = 'Team 1 Win', 
    loss = 'Team 2 Win',
    draw = 'Extra Time',
    'lambda_1' = 'Team 1',
    'lambda_2' = 'Team 2',
    'win_ko' = 'Team 1',
    'loss_ko' = 'Team 2'
    
  ) %>%
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**Copa América 2024 Game Predictions**'),
    subtitle = md('**Quarterfinals**'),
  ) %>%
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )

gtExtras::gtsave_extra(t4c, 'figures/copa/qf_preds.png')