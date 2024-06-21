### Update Scores
source('update_scores.R')

### Re-Fit Model After Each Round --> new preds
if(as.character(Sys.Date()) %in% c('2024-06-18')) {
  source('fit_model.R')
  source('game_preds.R')
}

### Run Simulations
source('euro_sim.R')
if(Sys.Date() > '2024-06-19') {
  source('copa_sim.R')
}

### Make Tables
source('make_table.R')

### Make Graphics
source('graphics.R')

