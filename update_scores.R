library(XML)
library(RCurl)
library(tidyverse)

get_scores <- function(date, league) {
  date_ <- gsub('-', '', date)
  url <- paste0('https://www.espn.com/soccer/fixtures/_/date/', date_, '/league/', league)
  scores <- readHTMLTable(getURL(url))[[1]]
  penalties_ix <- 1 + which(str_detect(scores$result, 'FT-Pens'))
  penalties_winners <- gsub('\\s+win.*', '', scores$match[penalties_ix])
  df <-
    tibble('date' = as.Date(date_, '%Y%m%d'),
           'team1_score' = map_dbl(str_extract_all(scores[,2], '\\d+'), ~as.numeric(.x[1]) ),
           'team2_score' = map_dbl(str_extract_all(scores[,2], '\\d+'), ~as.numeric(.x[2]) ),
           'team1' = gsub('.*\\s+.\\s+..', '', gsub( '\\s[A-Z]+\\d+.*$', '', scores[,1])),
           'team2' = gsub('.*\\s+.\\s+..', '', gsub( '\\s[A-Z]+$', '', scores[,2])),
           'shootout_winner' = NA) %>% 
    slice(setdiff(1:nrow(.), penalties_ix))
  
  if(length(penalties_ix) > 0) {
    df$shootout_winner[penalties_ix-1] <- penalties_winners
  }
  
  df <- bind_rows(df, select(df, date,
                             'team2' = team1, 'team1' = team2, 
                             'team1_score' = team2_score, 'team2_score' = team1_score, 
                             shootout_winner))
  
  return(df)
  
}

### Euro 2024 ###
### Read In Schedule
schedule <- 
  read_csv('data/euro2024_schedule.csv') %>% 
  mutate('date' = as.Date(date, '%m/%d/%y'))

### Get Scores for Tournament
scores <- map_dfr(seq.Date(as.Date('2024-06-14'), max(as.Date('2024-06-14'), Sys.Date()), 1), ~get_scores(.x, 'uefa.euro'))

### Update Scores
schedule <- 
  schedule %>% 
  select(-contains('score'),
         -contains('shootout_winner')) %>% 
  left_join(scores, by = c("date", "team1", "team2"))

### Save Results
write_csv(schedule, 'data/euro2024_schedule.csv')

### Copa America 2024 ###
### Read In Schedule
schedule <- 
  read_csv('data/copa2024_schedule.csv') %>% 
  mutate('date' = as.Date(date, '%m/%d/%y'))

### Get Scores for Tournament
scores <- map_dfr(seq.Date(as.Date('2024-06-20'), max(as.Date('2024-06-20'), Sys.Date()), 1), ~get_scores(.x, 'conmebol.america'))

### Update Scores
schedule <- 
  schedule %>% 
  select(-contains('score'),
         -contains('shootout_winner')) %>% 
  left_join(scores, by = c("date", "team1", "team2"))

### Save Results
write_csv(schedule, 'data/copa2024_schedule.csv')

