library(XML)
library(RCurl)
library(tidyverse)

get_scores <- function(date, league, schedule) {
  date_ <- gsub('-', '', date)
  url <- paste0('https://www.espn.com/soccer/fixtures/_/date/', date_, '/league/', league)
  scores <- readHTMLTable(getURL(url))[[1]]
  
  penalties_ix <-  which(str_detect(scores$V3, 'FT-Pens'))
  
  df <-
    tibble('date' = as.Date(date_, '%Y%m%d'),
           'team1_score' = map_dbl(str_extract_all(scores[,2], '\\d+'), ~as.numeric(.x[1]) ),
           'team2_score' = map_dbl(str_extract_all(scores[,2], '\\d+'), ~as.numeric(.x[2]) ),
           'team1' = gsub('.*\\s+.\\s+..', '', gsub( '\\s[A-Z]+\\d+.*$', '', scores[,1])),
           'team2' = gsub('.*\\s+.\\s+..', '', gsub( '\\s[A-Z]+$', '', scores[,2])),
           'shootout_winner' = NA) 
  
  if(length(penalties_ix) > 0) {
    tms <- unique(c(schedule$team1, schedule$team2))
    tms <- tms[!is.na(tms)]
    penalties_winners <- 
      map_chr(scores$V1[penalties_ix], function(x) {
        tms[map_lgl(tms, ~{
          grepl(paste(.x, '(win|advance) \\d+-\\d+ on penalties'), x) |
            grepl(paste(.x, '(win|advance) \\d+-\\d+ on penalties'), x)
        })]
      })
    df$shootout_winner[penalties_ix] <- penalties_winners
    
    df$team1[penalties_ix] <- 
      gsub('\\s+(win|advance) \\d+-\\d+ on penalties', '', df$team1[penalties_ix])
    
    for(t in tms) {
      df$team1[penalties_ix] <- 
        map_chr(df$team1[penalties_ix], ~ifelse(strsplit(.x, t)[[1]][1] == '', t, .x))
    }
    
    
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
scores <- map_dfr(seq.Date(as.Date('2024-06-14'), max(as.Date('2024-06-14'), Sys.Date()), 1), ~get_scores(.x, 'uefa.euro', schedule))

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
scores <- map_dfr(seq.Date(as.Date('2024-06-20'), max(as.Date('2024-06-20'), Sys.Date()), 1), ~get_scores(.x, 'conmebol.america', schedule))

### Update Scores
schedule <- 
  schedule %>% 
  select(-contains('score'),
         -contains('shootout_winner')) %>% 
  left_join(scores, by = c("date", "team1", "team2"))

### Save Results
write_csv(schedule, 'data/copa2024_schedule.csv')

