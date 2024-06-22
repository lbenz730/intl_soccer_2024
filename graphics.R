library(tidyverse)
library(ggimage)
library(arrow)
source('helpers.R')


### Load in Data
euro_history <- 
  read_csv('predictions/euro_history.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png'),
         'eliminated' = (finals == 0))
euro_stats <- 
  read_csv('predictions/euro_sim_results.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png')) 
euro_schedule <- 
  read_csv('data/euro2024_schedule.csv')

copa_history <- 
  read_csv('predictions/copa_history.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png'),
         'eliminated' = (finals == 0))
copa_stats <- 
  read_csv('predictions/copa_sim_results.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png')) 
copa_schedule <- 
  read_csv('data/copa2024_schedule.csv')


euro_elim_round <- 
  euro_stats %>% 
  mutate('group_stage' = 1) %>% 
  mutate('exp_elim_round' = champ * 6 + finals * 5 + sf * 4 + qf * 3 + r16 * 2 + group_stage) %>% 
  pivot_longer(c('group_stage', 'r16', 'qf', 'sf', 'finals', 'champ'),
               names_to = 'round',
               values_to = 'prob') %>% 
  group_by(team) %>% 
  mutate('elim_prob' = prob - lead(prob, default = 0)) %>% 
  ungroup() %>% 
  mutate('team' = fct_reorder(team, exp_elim_round),
         'round' = fct_rev(fct_relevel(round, 'group_stage', 'r16', 'qf', 'sf', 'finals', 'champ')))

ggplot(euro_elim_round, aes(x = team, y = elim_prob)) + 
  geom_col(aes(fill = round), col = 'black', lwd = 0.1) + 
  scale_fill_discrete(labels = rev(c('Group Stage', 'R16', 'QF', 'SF', 'Finals', 'Champ'))) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_image(data = distinct(euro_elim_round, team, logo, exp_elim_round), 
             aes(x = team, y = -0.03, image = logo)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_blank()) + 
  labs(x = 'Country',
       y = 'Probability',
       title = 'Distribution of Elimination Round',
       subtitle = 'Euro 2024',
       fill = 'Elimination Round')
ggsave('figures/euro/exp_elim.png', height = 12/1.2, width = 16/1.2)

ggplot(euro_history, aes(x = date, y = r16)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Knockout Round',
       title = 'Euro 2024',
       subtitle = 'Knockout Round Chances Over Time')
ggsave('figures/euro/r16.png', height = 12/1.2, width = 16/1.2)

ggplot(euro_history, aes(x = date, y = qf)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Quarterfinals',
       title = 'Euro 2024',
       subtitle = 'QF Chances Over Time')
ggsave('figures/euro/qf.png', height = 12/1.2, width = 16/1.2)

ggplot(euro_history, aes(x = date, y = sf)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Semifinals',
       title = 'Euro 2024',
       subtitle = 'SF Chances Over Time')

ggsave('figures/euro/sf.png', height = 12/1.2, width = 16/1.2)

ggplot(euro_history, aes(x = date, y = finals)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Finals',
       title = 'Euro 2024',
       subtitle = 'Finals Chances Over Time')

ggsave('figures/euro/finals.png', height = 12/1.2, width = 16/1.2)


ggplot(euro_history, aes(x = date, y = champ)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Winning Tournament',
       title = 'Euro 2024',
       subtitle = 'Title Chances Over Time')

ggsave('figures/euro/champ.png', height = 12/1.2, width = 16/1.2)


third_place <-
  read_csv('predictions/euro_third_place.csv') %>%
  group_by(sim_id) %>%
  slice(4) %>%
  mutate('goal_diff_chr' =
           case_when(goal_diff <= -4 ~ '< -3',
                     goal_diff >= 4 ~ '> 3',
                     T ~ as.character(goal_diff)
           )) %>%
  group_by(points, goal_diff_chr) %>%
  summarise('pct' = n()/max(sim_id)) %>%
  ungroup()

ggplot(third_place, aes(x = as.character(points), y = pct, fill = fct_relevel(goal_diff_chr, '< -3', '-3', '-2', '-1', '0', '1', '2', '3', '> 3'))) +
  geom_col(color = 'black')  +
  labs(x = 'Points',
       y = 'Frequency',
       title = 'Distribution of Points + Goal Differential of 4th Best 3rd Place Team',
       fill = 'Goal Differential') +
  theme(legend.position = 'bottom') +
  scale_y_continuous(labels = scales::percent)
ggsave('figures/euro/last_thrid_place.png', height = 9/1.2, width = 16/1.2)


read_csv('predictions/euro_third_place.csv') %>% 
  mutate('goal_diff_chr' =
           case_when(goal_diff <= -4 ~ -4,
                     goal_diff >= 4 ~ 4,
                     T ~ goal_diff
           )) %>%
  # mutate('goal_diff_chr' = fct_relevel(goal_diff_chr, '< -3', '-3', '-2', '-1', '0', '1', '2', '3', '> 3')) %>%
  group_by(points, goal_diff_chr)  %>% 
  summarise('advance' = mean(progress)) %>% 
  ungroup() %>% 
  ggplot(aes(x = points, y = goal_diff_chr)) + 
  geom_tile(aes(fill = advance), color = 'black', lwd = 1) + 
  geom_label(aes(label =  paste0(sprintf('%0.1f', 100 * advance), '%'))) + 
  scale_x_continuous(breaks = 1:6, labels = 1:6) +
  scale_y_continuous(breaks = seq(-4, 4, 1), labels = c('< -3', '-3', '-2', '-1', '0', '1', '2', '3', '> 3')) + 
  scale_fill_viridis_c(option = 'B', labels = scales::percent) + 
  theme(legend.position = 'bottom') + 
  labs(x = 'Points',
       y = 'Goal Differential',
       fill = 'Knockout %',
       title = 'Probability of Reaching Knockout Round\nFor Third Place Team',
       subtitle = 'Euro 2024')
ggsave('figures/euro/thrid_place.png', height = 9/1.2, width = 16/1.2)




copa_elim_round <- 
  copa_stats %>% 
  mutate('group_stage' = 1) %>% 
  mutate('exp_elim_round' = champ * 6 + finals * 5 + sf * 4 + qf * 3 + 2 * group_stage) %>% 
  pivot_longer(c('group_stage', 'qf', 'sf', 'finals', 'champ'),
               names_to = 'round',
               values_to = 'prob') %>% 
  group_by(team) %>% 
  mutate('elim_prob' = prob - lead(prob, default = 0)) %>% 
  ungroup() %>% 
  mutate('team' = fct_reorder(team, exp_elim_round),
         'round' = fct_rev(fct_relevel(round, 'group_stage', 'qf', 'sf', 'finals', 'champ')))

ggplot(copa_elim_round, aes(x = team, y = elim_prob)) + 
  geom_col(aes(fill = round), col = 'black', lwd = 0.1) + 
  scale_fill_discrete(labels = rev(c('Group Stage', 'QF', 'SF', 'Finals', 'Champ'))) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_image(data = distinct(copa_elim_round, team, logo, exp_elim_round), 
             aes(x = team, y = -0.03, image = logo)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_blank()) + 
  labs(x = 'Country',
       y = 'Probability',
       title = 'Distribution of Elimination Round',
       subtitle = 'Copa América 2024',
       fill = 'Elimination Round')
ggsave('figures/copa/exp_elim.png', height = 12/1.2, width = 16/1.2)

ggplot(copa_history, aes(x = date, y = qf)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Knockout Round',
       title = 'Copa América 2024',
       subtitle = 'Knockout Round Chances Over Time')
ggsave('figures/copa/qf.png', height = 12/1.2, width = 16/1.2)


ggplot(copa_history, aes(x = date, y = sf)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Semifinals',
       title = 'Copa América 2024',
       subtitle = 'SF Chances Over Time')

ggsave('figures/copa/sf.png', height = 12/1.2, width = 16/1.2)

ggplot(copa_history, aes(x = date, y = finals)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Finals',
       title = 'Copa América 2024',
       subtitle = 'Finals Chances Over Time')

ggsave('figures/copa/finals.png', height = 12/1.2, width = 16/1.2)


ggplot(copa_history, aes(x = date, y = champ)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Winning Tournament',
       title = 'Copa América 2024',
       subtitle = 'Title Chances Over Time')

ggsave('figures/copa/champ.png', height = 12/1.2, width = 16/1.2)


match3_plot <- function(group_, score_max, colors, euro = T) {
  if(euro) {
    df_standings <- read_parquet('predictions/euro_sim_standings.parquet')
    df_results <- read_parquet('predictions/euro_sim_game_results.parquet')
  }
  
  df_scores <-
    df_results %>% 
    filter(group == group_) %>% 
    group_by(sim_id) %>% 
    slice(5:6) %>% 
    ungroup()
  
  df_advance <-
    df_standings %>% 
    filter(group == group_)
  
  
  
  levels2 <- 
    c(paste(pmin(df_scores$team[1:2], df_scores$opp[1:2]), 'Win'),
      'Draw',
      paste(pmax(df_scores$team[1:2], df_scores$opp[1:2]), 'Win'))
  
  df_summary <-
    df_scores %>%
    mutate('match' = case_when(team < opp ~ paste(team, '-', opp),
                               team > opp ~ paste(opp, '-', team))) %>%
    mutate('result' = case_when(team_score > opp_score ~ paste(team, team_score, '-', opp_score, opp),
                                team_score < opp_score ~ paste(opp, opp_score, '-', team_score, team),
                                team_score == opp_score & team < opp ~ paste(team, team_score, '-', opp_score, opp),
                                team_score == opp_score & team > opp ~ paste(opp, opp_score, '-', team_score, team))) %>%
    mutate('label' = case_when(team_score > opp_score ~ paste0("<img src = 'flags/", team, ".png' width = '12'/>", team_score, ' - ', opp_score,  "<img src = 'flags/", opp, ".png' width = '12'/>"),
                               team_score < opp_score ~ paste0("<img src = 'flags/", opp, ".png' width = '12'/>", opp_score, ' - ', team_score,  "<img src = 'flags/", team, ".png' width = '12'/>"),
                               team_score == opp_score & team < opp ~ paste0("<img src = 'flags/", team, ".png' width = '12'/>", team_score, ' - ', opp_score,  "<img src = 'flags/", opp, ".png' width = '12'/>"),
                               team_score == opp_score & team > opp ~ paste0("<img src = 'flags/", opp, ".png' width = '12'/>", opp_score, ' - ', team_score,  "<img src = 'flags/", team, ".png' width = '12'/>")),
           'label2' = case_when(team_score == opp_score ~ 'Draw', 
                                team_score > opp_score ~ paste(team, 'Win'),
                                team_score < opp_score ~ paste(opp, 'Win'))) %>%
    mutate('margin' = team_score - opp_score,
           'winning_score' = pmax(team_score, opp_score),
           'losing_score' = pmin(team_score, opp_score)) %>%
    select(-team, -opp) %>%
    left_join(df_advance) %>%
    group_by(match, team, result, margin, winning_score, losing_score, label, label2) %>%
    summarise('n' = n(),
              'place_1' = mean(place == 1),
              'place_2' = mean(place == 2),
              'p_advance' = mean(progress)) %>%
    filter(winning_score <= score_max) %>%
    ungroup() %>%
    arrange(match, margin, -winning_score, losing_score) %>%
    mutate('result' = factor(result, levels = unique(result))) %>%
    mutate('label' = factor(label, levels = unique(label))) %>% 
    mutate('label2' = fct_relevel(label2, levels2))
  
  
  
  ggplot(df_summary, aes(x = label, y = p_advance, group = team)) +
    facet_wrap(match~label2, scales = 'free_x', nrow = 2) +
    geom_col(aes(fill = team), position = 'dodge') +
    geom_text(aes(label = paste0(round(100 * p_advance), '%')),
              size = 3,
              vjust = -0.5,
              position = position_dodge(width = 0.99)) +
    scale_y_continuous(limits = c(0, 1.05), labels = scales::percent) +
    scale_fill_manual(values = colors) +
    theme(axis.text.x = ggtext::element_markdown(),
          legend.position = 'bottom') +
    labs(title = 'Probability of Reaching Knockout Round',
         subtitle = paste('Group', group_),
         x = '',
         y = 'Probability of Reaching Knockout Round',
         fill = '')
  
  
  if(!dir.exists('figures/euro/match3')) {
    dir.create('figures/euro/match3') 
    dir.create('figures/copa/match3') 
  }
  
  if(euro) {
    ggsave(paste0('figures/euro/match3/group_', group_, '_match3.png'), height = 9, width = 16)
  } else {
    ggsave(paste0('figures/copa/match3/group_', group_, '_match3.png'), height = 9, width = 16)
  }
}

match3_plot('A', score_max = 2, c('#FFCC00', '#477050', '#005EB8', '#DA291C'))
match3_plot('B', score_max = 2, c('#DA291C', '#71C5E8', '#008C45', '#AA151B'))
match3_plot('C', score_max = 2, c('#C8102E', '#94bfac', '#0C4076', '#FF0000'))
match3_plot('D', score_max = 2, c('#EF3340', '#002654', '#F36C21', '#DC143C'))
match3_plot('E', score_max = 2, c('#C8102E', '#FCD116', '#0B4EA2', '#0057B7'))
match3_plot('F', score_max = 2, c('#11457E', '#DA291C', '#046A38', '#C8102E'))
