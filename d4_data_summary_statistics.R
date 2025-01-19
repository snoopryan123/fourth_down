
source("00_main.R") 

###########################################
### # plays  ###
###########################################

# num first downs and num plays
data_full_WP %>%
  group_by(down) %>%
  summarise(
    count = n()
  ) %>%
  mutate(p = count/sum(count), total=sum(count))

### num games
length(unique(data_full$game_id))

# on fourth down, decision breakdown
all_fourth_downs %>%
  group_by(decision_actual) %>%
  summarise(count = n()) %>%
  mutate(p = count/sum(count))
dim(all_fourth_downs)
dim(ALL_fourth_downs)

# num fourth downs vs. num first downs
sum(data_full_WP$down1)/sum(data_full_WP$down4)

### WP variables
names(data_full_WP)

