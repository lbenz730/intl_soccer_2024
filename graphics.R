library(tidyverse)
library(ggimage)
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
