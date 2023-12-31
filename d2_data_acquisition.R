
### purpose of this file: create `data_model_data1a.csv`

#####################
### nflFastR Data ###
#####################

library(tidyverse)
# library(nflfastR)
theme_set(theme_bw())
theme_update(text = element_text(size=16))
theme_update(plot.title = element_text(hjust = 0.5))

# # some helper files are in these
source("d0_data2h_add_nflscrapr_mutations.R") # source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("d0_data2h_add_ep_wp.R") # source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")

# from remote
# pbp_data_A <- readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/cal_data.rds?raw=true"))
# write_csv(pbp_data_A, "data_nflFastR_pbp.csv")
# from local
pbp_data_1 <- read_csv('data_nflFastR_pbp_1999_2022.csv')  

pbp_data_A = pbp_data_1

########################################################

# add label (for EP), pts_next_score, Total_W_Scaled
{
  model_data <- pbp_data_A %>%
    # in 'R/helper_add_nflscrapr_mutations.R'
    make_model_mutations() %>%
    mutate(
      label = case_when(
        Next_Score_Half == "Touchdown" ~ 0,
        Next_Score_Half == "Opp_Touchdown" ~ 1,
        Next_Score_Half == "Field_Goal" ~ 2,
        Next_Score_Half == "Opp_Field_Goal" ~ 3,
        Next_Score_Half == "Safety" ~ 4,
        Next_Score_Half == "Opp_Safety" ~ 5,
        Next_Score_Half == "No_Score" ~ 6
      ),
      label = as.factor(label),
      pts_next_score = case_when(
        Next_Score_Half == "Touchdown" ~ 7,
        Next_Score_Half == "Opp_Touchdown" ~ -7,
        Next_Score_Half == "Field_Goal" ~ 3,
        Next_Score_Half == "Opp_Field_Goal" ~ -3,
        Next_Score_Half == "Safety" ~ 2,
        Next_Score_Half == "Opp_Safety" ~ -2,
        Next_Score_Half == "No_Score" ~ 0
      ),
      # use nflscrapR weights
      Drive_Score_Dist = Drive_Score_Half - drive,
      Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
        (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
      ScoreDiff_W = (max(abs(score_differential), na.rm = T) - abs(score_differential)) /
        (max(abs(score_differential), na.rm = T) - min(abs(score_differential), na.rm = T)),
      Total_W = Drive_Score_Dist_W + ScoreDiff_W,
      Total_W_Scaled = (Total_W - min(Total_W, na.rm = T)) /
        (max(Total_W, na.rm = T) - min(Total_W, na.rm = T))
    ) %>%
    filter(
      !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
      !is.na(yardline_100)
    )
}

# add {pts_of_play, not_score, pos_changes_w} for Romer Instrumental Vars method
### pos_changes_w === 1 if team with possession on play i still has possession on play i+1, else -1
{
  model_data01 <- model_data %>%
    group_by(game_id, Drive_Score_Half) %>%
    mutate(not_score = 1-as.numeric(row_number()==n())) %>%
    ungroup() %>%
    mutate(pts_of_play = ifelse(not_score==0, pts_next_score, 0)) %>%
    group_by(game_id, Drive_Score_Half) %>%
    mutate(pos_changes_w = replace_na(ifelse(pts_next_score == lead(pts_next_score), 1, -1), 0)) %>%
    ungroup()
}

# add win probability columns
### label_win, receive_2h_ko, spread_time, home, Diff_Time_Ratio
# https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
{
  #FIXME
  model_data1 <- model_data01 %>%
    mutate(
      # label data with whether possession team ended up winning
      # note that NA result and ties dealt with later
      label_win = case_when(
        result > 0 & posteam == home_team ~ 1,
        result < 0 & posteam == away_team ~ 1,
        TRUE ~ 0
      ),
      label_win = as.factor(label_win),
      # create home indicator used in model
      home = ifelse(posteam == home_team, 1, 0)
    ) %>%
    # creates Diff_Time_Ratio and spread_time and receive_2h_ko
    prepare_wp_data() %>%
    # don't deal with NA, just drop
    filter(
      !is.na(down),
      !is.na(game_seconds_remaining),
      !is.na(yardline_100),
      !is.na(score_differential),
      # remove overtime plays
      qtr <= 4,
      !is.na(result),
      !is.na(posteam),
      # remove all games that result in a tie at the end of overtime
      result != 0
    )
}

### Check that Epoch is Drive_Score_Half ###
### Check that not_score, pts_of_play, pos_changes_w are right ###
# View(model_data1 %>% filter(game_id == "2014_01_BUF_CHI") %>%
#        select(game_id, qtr, half_seconds_remaining, posteam, defteam,
#               yardline_100,drive, desc, Next_Score_Half,
#               Drive_Score_Half, pts_next_score, not_score, pts_of_play, pos_changes_w))

# View(model_data1 %>% select(game_id, qtr, half_seconds_remaining, posteam, defteam,
#                            yardline_100,drive,Next_Score_Half,
#                            Drive_Score_Half,Drive_Score_Dist,
#                            Drive_Score_Dist_W,ScoreDiff_W,Total_W,Total_W_Scaled ))

######################################
#####  complete dataset creation #####
######################################

# idk why this is all necessary for xgb but it is
model_data1a <- model_data1 %>%
  mutate(
    label = as.numeric(label),
    label = label - 1,
    label_win = as.numeric(label_win),
    label_win = label_win - 1
  )

### check that Drive_Score_Half is indeed epoch
data.frame(
  model_data1 %>% 
    select(game_id, posteam, defteam, Drive_Score_Half, pts_of_play, pts_next_score, score_differential)
)

### create EPOCH (Drive_Score_Half)
model_data1a = model_data1a %>% 
  group_by(game_id, Drive_Score_Half) %>%
  mutate(epoch = cur_group_id()) %>%
  ungroup() 

# epochs (Drive_Score_Half's) which begin in the 1st and 3rd quarters only
drive_score_starts_in_q13 = model_data1a %>% 
  filter(qtr %in% c(1,3)) %>% 
  select(epoch) %>% 
  distinct() %>% 
  mutate(drive_score_starts_in_q13 = TRUE)

model_data1a <- model_data1a %>% 
  left_join(drive_score_starts_in_q13) %>% 
  mutate(drive_score_starts_in_q13 = replace_na(drive_score_starts_in_q13, FALSE)) 

# View(model_data1a %>% filter(row_number() <= 300))

#################################################################################
#################################################################################

######################################
#### simple win probability model ####
######################################

#### create a simple WP model, called WP0,
#### trained only on data from era0 and era1 (1999-2005), 
#### to avoid data-bleed.

lr.wp0 = glm(label_win ~ score_differential*game_seconds_remaining, family="binomial",
             data= (model_data1a %>% filter(era0==1 | era1==1)) )

# lr.wp0 = glm(label_win ~ bs(score_differential, df=3) +  
#                          I((score_differential)/(game_seconds_remaining + 1)) + 
#                          bs(half_seconds_remaining, df=3) +
#                          I((game_seconds_remaining <= 120)*(posteam_timeouts_remaining==0)),
#              family="binomial",
#              data= (model_data1a %>% filter(era0==1 | era1==1)) )

p2_tib = bind_rows(
  tibble(score_differential=seq(-30,30,by=1), 
         half_seconds_remaining=1800, game_seconds_remaining=3600, gsr="3600", posteam_timeouts_remaining=3),
  tibble(score_differential=seq(-30,30,by=1), 
         half_seconds_remaining=900, game_seconds_remaining=2700, gsr="2700", posteam_timeouts_remaining=3),
  tibble(score_differential=seq(-30,30,by=1), 
         half_seconds_remaining=1800, game_seconds_remaining=1800, gsr="1800", posteam_timeouts_remaining=3),
  tibble(score_differential=seq(-30,30,by=1), 
         half_seconds_remaining=900, game_seconds_remaining=900, gsr="900", posteam_timeouts_remaining=3),
  tibble(score_differential=seq(-30,30,by=1), 
         half_seconds_remaining=200, game_seconds_remaining=200, gsr="200", posteam_timeouts_remaining=3)
)
p2_tib$wp = predict(lr.wp0, p2_tib, type="response")
p2_tib$gsr = factor(p2_tib$gsr, levels=c("3600", "2700", "1800", "900", "200") )

plot_wp2 = p2_tib %>%
  ggplot(aes(x=score_differential, y=wp, color=gsr)) +
  geom_line(size=1) +
  xlab("score differential") + ylab("win probability") +
  scale_colour_manual(values= scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=5)),
                      name="game\nseconds\nremaining")
plot_wp2
ggsave("plot_wp0.png", plot_wp2, width=8, height=6)

model_data1a$wp0 = predict(lr.wp0, model_data1a, type="response")

#####################
##### save data #####
#####################

write_csv(model_data1a, "data1a.csv")
 
