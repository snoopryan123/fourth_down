
source("0_clean_lm.R")

WP = if(!exists("WP")) TRUE else WP ### default to expected points models

########################
####### LOAD DATA ######
########################

### full dataset
data_full_0 <- read_csv("data7b.csv") 

### for some reason, the pointspread from nflFastR is flipped, so flip it back!
data_full_0 <- data_full_0 %>% mutate(posteam_spread = -posteam_spread)

### standardize some covariates 
# std <- function(x, mu, sigma) { (x-mu) / (2*sigma) }
std <- function(x) { (x-mean(x,.na.rm=TRUE)) / (2*sd(x,na.rm=TRUE)) }

### standardizing the columns as-is 
data_full_0A = 
  data_full_0 %>% 
  mutate(
    posteam_coach = ifelse(home == 1, home_coach, away_coach),
      
    posteam_spread_std = std(posteam_spread),
    kq_0_sum_std = std(kq0_sum), 
    pq_0_sum_std =  std(pq0_sum), 
    kq_0_dt_sum_std = std(kq0_dt_sum),
    pq_0_dt_sum_std = std(pq0_dt_sum),
    
    # oq_op_0_sum = std(oq_op_0_sum),
    oq_ot_0_total_sum = std(oq_ot_0_total_sum),
    oq_rot_0_total_sum = std(oq_rot_0_total_sum),
    qbq_ot_0_sum = std(qbq_ot_0_sum),
    dq_dt_0_combined_sum = std(dq_dt_0_combined_sum),
    dq_dt_0_againstRun_sum = std(dq_dt_0_againstRun_sum),
    dq_dt_0_againstPass_sum = std(dq_dt_0_againstPass_sum),
    dq_dt_0_total_sum = std(dq_dt_0_total_sum),
    oq_dt_0_sum = std(oq_dt_0_sum),
    oq_rdt_0_sum = std(oq_rdt_0_sum),
    qbq_dt_0_sum = std(qbq_dt_0_sum),
    dq_ot_0_sum = std(dq_ot_0_sum),
    dq_ot_0_againstRun_sum = std(dq_ot_0_againstRun_sum),
    dq_ot_0_againstPass_sum = std(dq_ot_0_againstPass_sum),
    
    # oq_op_0_sum_time = oq_op_0_sum * exp(-4 * .data$elapsed_share),
    dq_dt_0_combined_sum_time = dq_dt_0_combined_sum * exp(-4 * .data$elapsed_share),
    # oq_op_0_sum_time = oq_op_0_sum * exp(-4 * .data$elapsed_share),
    # dq_dt_0_combined_sum_time = dq_dt_0_combined_sum * exp(-4 * .data$elapsed_share),
    oq_dt_0_sum_time = oq_dt_0_sum * exp(-4 * .data$elapsed_share),
    dq_ot_0_sum_time = dq_ot_0_sum * exp(-4 * .data$elapsed_share),
    
    ### for WP models
    e_score_diff = ep00 + score_differential,
    eScoreTimeRatio = (e_score_diff)/(game_seconds_remaining + 1),
    scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
    utm = as.numeric(half_seconds_remaining <= 120),
    not_utm = 1-utm,
    gtg = (yardline_100 <= 10),
  
    half_sec_rem_std = 1-(half_seconds_remaining/1800), ### in [0,1] where 0 is start and 1 is end.
    
    ### features for Lock and Nettleton WP
    AdjustedScore_LN = score_differential / sqrt(game_seconds_remaining + 1),
    total_score = posteam_score + defteam_score
  )  %>%
  ### fix era numeric column
  mutate(era_A = case_when(era0==1~0, era1==1~1, era2==1~2, era3==1~3, era4==1~4, TRUE~NA_real_)) %>%
  mutate(era_B = case_when(era0==1~0, era1==1~1, era2==1~2, era3==1|era4==1~3, TRUE~NA_real_)) %>%
  ### drive_id, the unique index of all drives in the dataset
  left_join(
    data_full_0 %>% distinct(game_id, drive) %>% mutate(drive_id = 1:n())
  )

###
data_full_AA = data_full_0A %>%
  ### remember, we used era0 and era1 to construct WP0 and EPA0, so remove them
  filter(era0 != 1 & era1 != 1) %>%
  ### this analysis was originally completed using data <= 2021
  filter(season <= 2021) 

### datasets for WP
data_full_WP = data_full_AA
data_full_110_WP = data_full_WP %>% filter(down == 1 & (ydstogo == 10 | yardline_100 <= 10) )

data_full = data_full_WP
data_full_110 = data_full_110_WP

# ### datasets for EP
# data_full_A = data_full_AA %>% drop_na(pass_or_rush)
# omega = 0.15
# data_full_EP = data_full_A %>% filter(omega <= wp0 & wp0 <= 1-omega)
# data_full_110_EP = data_full_EP %>% filter(down == 1 & (ydstogo == 10 | yardline_100 <= 10) )
# 
# if (WP) {
#   data_full = data_full_WP
#   data_full_110 = data_full_110_WP
# } else { # EP
#   data_full = data_full_EP
#   data_full_110 = data_full_110_EP
# }

print(paste("there are",  length(unique(data_full$game_id)), "games in our dataset"))

##########################################
### Get FG, PUNT, and CONVERT Datasets ###
##########################################

fg_df = 
  data_full_WP %>%
  filter(field_goal_attempt == 1) %>%
  select(row_idx, field_goal_attempt, fg_made, yardline_100, posteam, season, week, kicker_name, kq_0_sum_std) %>%
  filter(yardline_100 <= 50) 
tail(fg_df)

punt_df = 
  data_full_WP %>%
  filter(punt_attempt == 1) %>%
  select(row_idx, yardline_100, next_ydl, posteam, season, week, punter_name, pq_0_sum_std) %>%
  filter(yardline_100 >= 30)
tail(punt_df)

go_df = 
  data_full_WP %>%
  filter(down == 3 | down == 4) %>%
  drop_na(pass_or_rush) %>%
  select(row_idx, season, posteam_spread, qbq_ot_0_sum, oq_rot_0_total_sum, dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum, yardline_100, down, pass_or_rush, ydstogo, yards_gained) %>%
  mutate(convert = as.numeric(yards_gained >= ydstogo))
go_df

#### dataframe of all fourth down decisions
all_fourth_downs = 
  data_full_WP %>%
  filter(down==4) %>%
  mutate(
  decision_actual = case_when(
    !is.na(kicker_player_name) ~ "FG",
    !is.na(punter_player_name) ~ "Punt",
    TRUE ~ "Go"
  )) %>%
  mutate(
    label_decision_actual = case_when(
      decision_actual == "Go" ~ 0,
      decision_actual == "FG" ~ 1,
      decision_actual == "Punt" ~ 2
    )
  )
all_fourth_downs$fgp = as.numeric(all_fourth_downs$decision_actual == "FG")
all_fourth_downs$puntp = as.numeric(all_fourth_downs$decision_actual == "Punt")
tail(
  all_fourth_downs %>% select(
    row_idx, posteam, season, posteam_coach, decision_actual,label_decision_actual,
    # kicker_player_name, punter_player_name,
    yardline_100, ydstogo, down, score_differential, posteam_spread, game_seconds_remaining,
    fgp, puntp
  )
)

