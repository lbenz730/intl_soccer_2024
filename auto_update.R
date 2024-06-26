args <- commandArgs(trailingOnly=TRUE)
print(args)
### Update Scores
source('update_scores.R')

## Re-Fit Model After Each Round --> new preds
if(as.character(Sys.Date()) %in% c('2024-06-18', '2024-06-22', '2024-06-26')) {
  source('fit_model.R')
  source('game_preds.R')
}

### Run Simulations
if('euro' %in% args) {
  source('euro_sim.R')
}
if('copa' %in% args) {
  source('copa_sim.R')
}

### Make Tables
source('make_table.R')

### Make Graphics
source('graphics.R')

