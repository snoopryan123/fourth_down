
ttswd = getwd()
setwd("..")
source("00_main.R")
setwd(ttswd)
WP = if(!exists("WP")) FALSE else WP ### default to expected points models

################
### Datasets ###
################

### check seasonal differences
# data_full %>% group_by(season) %>% summarise(count=n())
# data_full %>% ggplot() + facet_wrap(~season) + geom_histogram(aes(x=oq_op_0_sum), fill="black")

if (!WP) { ### for expected points models, keep epochs together
  all_epochs = unique(data_full$epoch)
  set.seed(99) # Aaron Donald!
  HOLD_OUT_EPOCHS = sort(sample(all_epochs, size = round(0.5*length(all_epochs)), replace = FALSE))
  remaining_epochs = setdiff(all_epochs, HOLD_OUT_EPOCHS)
  VAL_EPOCHS = sort(sample(remaining_epochs, size = round(0.5*length(remaining_epochs)), replace = FALSE)) ### for xgb param tuning
  # length(HOLD_OUT_EPOCHS) + length(remaining_epochs) == length(all_epochs)
  
  test_set = data_full %>% filter(epoch %in% HOLD_OUT_EPOCHS)
  train_set = data_full %>% filter(!(epoch %in% HOLD_OUT_EPOCHS))
} else { ### for win probability models, keep games together
  all_games = unique(data_full$game_id)
  set.seed(99) # Aaron Donald!
  HOLD_OUT_GAMES = sort(sample(all_games, size = round(0.5*length(all_games)), replace = FALSE))
  remaining_games = setdiff(all_games, HOLD_OUT_GAMES)
  VAL_GAMES = sort(sample(remaining_games, size = round(0.5*length(remaining_games)), replace = FALSE)) ### for xgb param tuning
  # length(HOLD_OUT_GAMES) + length(remaining_games) == length(all_games)
  
  test_set = data_full %>% filter(game_id %in% HOLD_OUT_GAMES)
  train_set = data_full %>% filter(!(game_id %in% HOLD_OUT_GAMES))
}

### check
print(paste("there are", dim(data_full)[1], "plays, with", 
            dim(train_set)[1], "training plays and", 
            dim(test_set)[1], "testing plays. It is",
            dim(train_set)[1] + dim(test_set)[1] == dim(data_full)[1], 
            "that they sum up properly."))

### dataset of first downs!
train_set_110 = train_set %>% filter(down == 1 & (ydstogo == 10 | yardline_100 <= 10) )
test_set_110 = test_set %>% filter(down == 1 & (ydstogo == 10 | yardline_100 <= 10) )

### check
print(paste("there are", dim(data_full_110)[1], "1st down plays, with", 
            dim(train_set_110)[1], "training plays and", 
            dim(test_set_110)[1], "testing plays. It is",
            dim(train_set_110)[1]+ dim(test_set_110)[1] == dim(data_full_110)[1], 
            "that they sum up properly."))








