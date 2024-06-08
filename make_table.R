library(tidyverse)
library(gt)

make_table <- function(Group = 'all', tourney = 'euro') {
  if(Group == 'all') {
    
    if(tourney == 'euro') {
      df_stats <- 
        read_csv('predictions/ratings.csv') %>% select(team, alpha, delta, net_rating) %>% 
        inner_join(  read_csv('predictions/euro_sim_results.csv')) %>% 
        arrange(group, desc(round(champ)), desc(round(finals)),
                desc(sf), desc(qf), desc(r16)) %>% 
        mutate('logo' = paste0('flags/', team, '.png')) %>% 
        select(team, logo, group, everything()) 
      
      df <- group_by(df_stats, group)
      
      title <- md('<img src="flags/UEFA_Euro_2024_Logo.svg.png" style="height:140px;">')
      subtitle <- md('**EURO 2024 Simulations**')
      
      df1 <- 
        df %>% 
        filter(group <= 'C')  
      names(df1) <- paste0(names(df1), '_1')
      
      df2 <- 
        df %>% 
        filter(group > 'C')  
      names(df2) <- paste0(names(df2), '_2')
      
      
      
      
      
      bind_cols(df1, df2) %>% 
        ungroup() %>% 
        # group_by(paste(group_1, group_2)) %>% 
        gt() %>% 
        
        ### Round Numbers
        fmt_number(columns = c(alpha_1, delta_1, net_rating_1, mean_pts_1, mean_gd_1), decimals = 2, sep_mark = '') %>% 
        fmt_number(columns = c(alpha_2, delta_2, net_rating_2, mean_pts_2, mean_gd_2), decimals = 2, sep_mark = '') %>% 
        fmt_percent(columns = c(r16_1, qf_1, sf_1, finals_1, champ_1), decimals = 0, sep_mark = '') %>% 
        fmt_percent(columns = c(r16_2, qf_2, sf_2, finals_2, champ_2), decimals = 0, sep_mark = '') %>% 
        
        ### Align Columns
        cols_align(align = "center") %>% 
        
        ### Colors
        data_color(columns = c(mean_pts_1, mean_pts_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>% 
        data_color(columns = c(mean_gd_1, mean_gd_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>% 
        data_color(columns = c(r16_2, qf_2, sf_2, finals_2, champ_2, r16_1, qf_1, sf_1, finals_1, champ_1),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
        data_color(columns = c(alpha_1, alpha_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
        data_color(columns = c(net_rating_1, net_rating_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
        data_color(columns = c(delta_1, delta_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
        
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
              sides = "bottom",
              color = "black",
              weight = px(3)
            )
          ),
          locations = list(
            cells_body(rows = c(4, 8, 12))
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
              columns = c(group_1, net_rating_1, mean_gd_1, champ_1, 
                          group_2, net_rating_2, mean_gd_2)
            )
          )
        ) %>% 
        
        tab_spanner(label = 'Ratings', columns = c('alpha_1', 'delta_1', 'net_rating_1'), id = '1') %>% 
        tab_spanner(label = 'Ratings', columns = c('alpha_2', 'delta_2', 'net_rating_2'), id = '2') %>% 
        tab_spanner(label = 'Group Stage', columns = c('mean_pts_1', 'mean_gd_1'), id = '3') %>% 
        tab_spanner(label = 'Group Stage', columns = c('mean_pts_2', 'mean_gd_2'), id = '4') %>% 
        tab_spanner(label = 'Knockout Round', columns = c('r16_1', 'qf_1', 'sf_1',  'finals_1', 'champ_1'), id = '5') %>% 
        tab_spanner(label = 'Knockout Round', columns = c('r16_2', 'qf_2', 'sf_2',  'finals_2', 'champ_2'), id = '6')  %>% 
        
        ### Logos
        text_transform(
          locations = cells_body(columns = c(logo_1, logo_2)), 
          fn = function(x) {
            local_image(
              filename = ifelse(is.na(x), '--', as.character(x)),
              height = 30
            ) 
          }
        ) %>% 
        
        ### Names
        cols_label(
          team_1 = '',
          logo_1 = '',
          group_1 = 'Group',
          alpha_1 = 'Offense',
          delta_1 = 'Defense',
          net_rating_1 = 'Overall',
          mean_pts_1 = 'Points',
          mean_gd_1 = 'GD',
          r16_1 = 'R16',
          qf_1 = 'QF',
          sf_1 = 'SF',
          finals_1 = 'Finals',
          champ_1 = 'Champ',
          
          team_2 = '',
          logo_2 = '',
          group_2 = 'Group',
          alpha_2 = 'Offense',
          delta_2 = 'Defense',
          net_rating_2 = 'Overall',
          mean_pts_2 = 'Points',
          mean_gd_2 = 'GD',
          r16_2 = 'R16',
          qf_2 = 'QF',
          sf_2 = 'SF',
          finals_2 = 'Finals',
          champ_2 = 'Champ'
          
        ) %>% 
        tab_source_note("Luke Benz (@recspecs730)") %>%
        tab_source_note("Ratings = Change in Log Goal Expectations") %>%
        tab_source_note("Based on 10,000 Simulations") %>%
        tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
        tab_header(
          title = title,
          subtitle = subtitle
        ) %>% 
        tab_options(column_labels.font.size = 20,
                    row_group.font.weight = 'bold',
                    heading.title.font.size = 40,
                    heading.subtitle.font.size = 30,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold',
                    column_labels.font.weight = 'bold'
        )
    } else {
      
      df_stats <- 
        read_csv('predictions/ratings.csv') %>% select(team, alpha, delta, net_rating) %>% 
        inner_join(  read_csv('predictions/copa_sim_results.csv')) %>% 
        arrange(group, desc(round(champ)), desc(round(finals)),
                desc(sf), desc(qf)) %>% 
        mutate('logo' = paste0('flags/', team, '.png')) %>% 
        select(team, logo, group, everything()) 
      
      df <- group_by(df_stats, group)
      title <- md('<img src="flags/2024_Copa_América_logo.svg.png" style="height:140px;">')
      subtitle <- md(paste0('**Copa América 2024 Simulations**'))
      
      
      df1 <- 
        df %>% 
        filter(group <= 'B')  
      names(df1) <- paste0(names(df1), '_1')
      
      df2 <- 
        df %>% 
        filter(group > 'B')  
      names(df2) <- paste0(names(df2), '_2')
      
      bind_cols(df1, df2) %>% 
        ungroup() %>% 
        # group_by(paste(group_1, group_2)) %>% 
        gt() %>% 
        
        ### Round Numbers
        fmt_number(columns = c(alpha_1, delta_1, net_rating_1, mean_pts_1, mean_gd_1), decimals = 2, sep_mark = '') %>% 
        fmt_number(columns = c(alpha_2, delta_2, net_rating_2, mean_pts_2, mean_gd_2), decimals = 2, sep_mark = '') %>% 
        fmt_percent(columns = c(qf_1, sf_1, finals_1, champ_1), decimals = 0, sep_mark = '') %>% 
        fmt_percent(columns = c(qf_2, sf_2, finals_2, champ_2), decimals = 0, sep_mark = '') %>% 
        
        ### Align Columns
        cols_align(align = "center") %>% 
        
        ### Colors
        data_color(columns = c(mean_pts_1, mean_pts_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>% 
        data_color(columns = c(mean_gd_1, mean_gd_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>% 
        data_color(columns = c(qf_2, sf_2, finals_2, champ_2, qf_1, sf_1, finals_1, champ_1),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
        data_color(columns = c(alpha_1, alpha_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
        data_color(columns = c(net_rating_1, net_rating_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
        data_color(columns = c(delta_1, delta_2),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
        
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
              sides = "bottom",
              color = "black",
              weight = px(3)
            )
          ),
          locations = list(
            cells_body(rows = c(4, 8))
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
              columns = c(group_1, net_rating_1, mean_gd_1, champ_1, 
                          group_2, net_rating_2, mean_gd_2)
            )
          )
        ) %>% 
        
        tab_spanner(label = 'Ratings', columns = c('alpha_1', 'delta_1', 'net_rating_1'), id = '1') %>% 
        tab_spanner(label = 'Ratings', columns = c('alpha_2', 'delta_2', 'net_rating_2'), id = '2') %>% 
        tab_spanner(label = 'Group Stage', columns = c('mean_pts_1', 'mean_gd_1'), id = '3') %>% 
        tab_spanner(label = 'Group Stage', columns = c('mean_pts_2', 'mean_gd_2'), id = '4') %>% 
        tab_spanner(label = 'Knockout Round', columns = c('qf_1', 'sf_1',  'finals_1', 'champ_1'), id = '5') %>% 
        tab_spanner(label = 'Knockout Round', columns = c('qf_2', 'sf_2',  'finals_2', 'champ_2'), id = '6')  %>% 
        
        ### Logos
        text_transform(
          locations = cells_body(columns = c(logo_1, logo_2)), 
          fn = function(x) {
            local_image(
              filename = ifelse(is.na(x), '--', as.character(x)),
              height = 30
            ) 
          }
        ) %>% 
        
        ### Names
        cols_label(
          team_1 = '',
          logo_1 = '',
          group_1 = 'Group',
          alpha_1 = 'Offense',
          delta_1 = 'Defense',
          net_rating_1 = 'Overall',
          mean_pts_1 = 'Points',
          mean_gd_1 = 'GD',
          qf_1 = 'QF',
          sf_1 = 'SF',
          finals_1 = 'Finals',
          champ_1 = 'Champ',
          
          team_2 = '',
          logo_2 = '',
          group_2 = 'Group',
          alpha_2 = 'Offense',
          delta_2 = 'Defense',
          net_rating_2 = 'Overall',
          mean_pts_2 = 'Points',
          mean_gd_2 = 'GD',
          qf_2 = 'QF',
          sf_2 = 'SF',
          finals_2 = 'Finals',
          champ_2 = 'Champ'
          
        ) %>% 
        tab_source_note("Luke Benz (@recspecs730)") %>%
        tab_source_note("Ratings = Change in Log Goal Expectations") %>%
        tab_source_note("Based on 10,000 Simulations") %>%
        tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
        tab_header(
          title = title,
          subtitle = subtitle
        ) %>% 
        tab_options(column_labels.font.size = 20,
                    row_group.font.weight = 'bold',
                    heading.title.font.size = 40,
                    heading.subtitle.font.size = 30,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold',
                    column_labels.font.weight = 'bold'
        )
      
    }
    
    
    
  } else {
    if(tourney == 'euro') {
      df_stats <- 
        read_csv('predictions/ratings.csv') %>% select(team, alpha, delta, net_rating) %>% 
        inner_join(  read_csv('predictions/euro_sim_results.csv')) %>% 
        arrange(group, desc(round(champ)), desc(round(finals)),
                desc(sf), desc(qf), desc(r16)) %>% 
        mutate('logo' = paste0('flags/', team, '.png')) %>% 
        select(team, logo, group, everything()) 
      
      df <- group_by(df_stats, group)
      title <- md('<img src="flags/UEFA_Euro_2024_Logo.svg.png" style="height:140px;">')
      subtitle <- md(paste0('**EURO 2024 Simulations: Group ', Group, '**'))
      
      df %>% 
        filter(group == Group) %>% 
        gt() %>% 
        
        ### Round Numbers
        fmt_number(columns = c(alpha, delta, net_rating, mean_pts, mean_gd), decimals = 2, sep_mark = '') %>% 
        fmt_percent(columns = c(r16, qf, sf, finals, champ), decimals = 0, sep_mark = '') %>% 
        
        ### Align Columns
        cols_align(align = "center") %>% 
        
        ### Colors
        data_color(columns = c(mean_pts),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>% 
        data_color(columns = c(mean_gd),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>% 
        data_color(columns = c(r16, qf, sf, finals, champ),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
        data_color(columns = c(alpha),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
        data_color(columns = c(net_rating),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
        data_color(columns = c(delta),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
        
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
              columns = c(net_rating, mean_gd)
            )
          )
        ) %>% 
        
        
        tab_style(
          style = list(
            cell_borders(
              sides = "left",
              color = "black",
              weight = px(3)
            )
          ),
          locations = list(
            cells_body(
              columns = c(alpha)
            )
          )
        ) %>% 
        
        tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>% 
        tab_spanner(label = 'Group Stage', columns = c('mean_pts', 'mean_gd')) %>% 
        tab_spanner(label = 'Knockout Round', columns = c('r16', 'qf', 'sf',  'finals', 'champ')) %>% 
        
        ### Logos
        text_transform(
          locations = cells_body(columns = "logo"), 
          fn = function(x) map_chr(x, ~{
            local_image(filename =  as.character(.x), height = 30)
          })
        ) %>% 
        
        ### Names
        cols_label(
          team = '',
          logo = '',
          group = 'Group',
          alpha = 'Offense',
          delta = 'Defense',
          net_rating = 'Overall',
          mean_pts = 'Points',
          mean_gd = 'GD',
          r16 = 'R16',
          qf = 'QF',
          sf = 'SF',
          finals = 'Finals',
          champ = 'Champ'
          
        ) %>% 
        tab_source_note("Luke Benz (@recspecs730)") %>%
        tab_source_note("Ratings = Change in Log Goal Expectations") %>%
        tab_source_note("Based on 10,000 Simulations") %>%
        tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
        tab_header(
          title = title,
          subtitle = subtitle
        ) %>% 
        tab_options(column_labels.font.size = 20,
                    row_group.font.weight = 'bold',
                    row_group.font.size = 20,
                    heading.title.font.size = 40,
                    heading.subtitle.font.size = 30,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold',
                    column_labels.font.weight = 'bold'
        )
    } else {
      df_stats <- 
        read_csv('predictions/ratings.csv') %>% select(team, alpha, delta, net_rating) %>% 
        inner_join(  read_csv('predictions/copa_sim_results.csv')) %>% 
        arrange(group, desc(round(champ)), desc(round(finals)),
                desc(sf), desc(qf)) %>% 
        mutate('logo' = paste0('flags/', team, '.png')) %>% 
        select(team, logo, group, everything()) 
      
      df <- group_by(df_stats, group)
      title <- md('<img src="flags/2024_Copa_América_logo.svg.png" style="height:140px;">')
      subtitle <- md(paste0('**Copa América 2024 Simulations: Group ', Group, '**'))
      df %>% 
        filter(group == Group) %>% 
        gt() %>% 
        
        ### Round Numbers
        fmt_number(columns = c(alpha, delta, net_rating, mean_pts, mean_gd), decimals = 2, sep_mark = '') %>% 
        fmt_percent(columns = c(qf, sf, finals, champ), decimals = 0, sep_mark = '') %>% 
        
        ### Align Columns
        cols_align(align = "center") %>% 
        
        ### Colors
        data_color(columns = c(mean_pts),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>% 
        data_color(columns = c(mean_gd),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>% 
        data_color(columns = c(qf, sf, finals, champ),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
        data_color(columns = c(alpha),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
        data_color(columns = c(net_rating),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
        data_color(columns = c(delta),
                   colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
        
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
              columns = c(net_rating, mean_gd)
            )
          )
        ) %>% 
        
        
        tab_style(
          style = list(
            cell_borders(
              sides = "left",
              color = "black",
              weight = px(3)
            )
          ),
          locations = list(
            cells_body(
              columns = c(alpha)
            )
          )
        ) %>% 
        
        tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>% 
        tab_spanner(label = 'Group Stage', columns = c('mean_pts', 'mean_gd')) %>% 
        tab_spanner(label = 'Knockout Round', columns = c( 'qf', 'sf',  'finals', 'champ')) %>% 
        
        ### Logos
        text_transform(
          locations = cells_body(columns = "logo"), 
          fn = function(x) map_chr(x, ~{
            local_image(filename =  as.character(.x), height = 30)
          })
        ) %>% 
        
        ### Names
        cols_label(
          team = '',
          logo = '',
          group = 'Group',
          alpha = 'Offense',
          delta = 'Defense',
          net_rating = 'Overall',
          mean_pts = 'Points',
          mean_gd = 'GD',
          qf = 'QF',
          sf = 'SF',
          finals = 'Finals',
          champ = 'Champ'
          
        ) %>% 
        tab_source_note("Luke Benz (@recspecs730)") %>%
        tab_source_note("Ratings = Change in Log Goal Expectations") %>%
        tab_source_note("Based on 10,000 Simulations") %>%
        tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
        tab_header(
          title = title,
          subtitle = subtitle
        ) %>% 
        tab_options(column_labels.font.size = 20,
                    row_group.font.weight = 'bold',
                    row_group.font.size = 20,
                    heading.title.font.size = 40,
                    heading.subtitle.font.size = 30,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold',
                    column_labels.font.weight = 'bold'
        )
      
    }
    
  }
  
}


table <- make_table('all', 'euro')
gtExtras::gtsave_extra(table, filename = 'figures/euro/euro_2024.png', vwidth = 1800)
map(LETTERS[1:6], ~gtExtras::gtsave_extra(make_table(Group = .x, tourney = 'euro'), filename = paste0('figures/euro/', .x, '.png')))
table <- make_table('all', 'copa')
gtExtras::gtsave_extra(table, filename = 'figures/copa/copa_2024.png', vwidth = 1800)
map(LETTERS[1:4], ~gtExtras::gtsave_extra(make_table(Group = .x, tourney = 'copa'), filename = paste0('figures/copa/', .x, '.png')))



