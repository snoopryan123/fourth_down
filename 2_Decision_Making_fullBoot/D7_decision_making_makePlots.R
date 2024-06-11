
### LOAD 4TH DOWN PLOTTING DATA, MODELS, AND FUNCTIONS
source("D6_loadModelsAndData.R")
source("D3_decision_making_functions.R")
options(warn=-1)
###options(warn=0)

###############################
### Find Good Example Plays ###
###############################

fourth_downs_df_2022 = ALL_fourth_downs %>% filter(season %in% 2022)
ddf_2022 = get_all_decision_making(fourth_downs_df_2022, wp=TRUE, SE=TRUE, coachBaseline=FALSE, bind_w_plays=TRUE) 
ddf_2022_A = 
  ddf_2022 %>% 
  rowwise() %>%
  mutate(wp = max(v_convert, V_fg, V_punt)) %>%
  ungroup() %>%
  filter(ydstogo <= 10) %>%
  select(row_idx, decision_intensity, prop_decision, wp) 
ddf_2022_A

### example plays: looks good but is uncertain
ddf_2022_A %>% filter(decision_intensity > 0.03) %>% arrange(prop_decision)

### example plays: looks nebulous but is certain
# dfex2 = ddf_2022_A %>% filter(wpcutoff < wp & wp < 1-wpcutoff) %>% filter(prop_decision > 0.9) %>% arrange(decision_intensity)
dfex2 = ddf_2022_A %>% filter(prop_decision > 0.9) %>% arrange(decision_intensity)
dfex2 
wpcutoff = 0.1
dfex2a = dfex2 %>% filter(decision_intensity < 0.01) %>% filter(wpcutoff < wp & wp < 1-wpcutoff)
dfex2a

# ddf_2022_A %>% filter(wpcutoff < wp & wp < 1-wpcutoff) %>% filter(prop_decision > 0.95) %>% arrange(decision_intensity)
# View(ddf_2022_A %>% filter(wpcutoff < wp & wp < 1-wpcutoff) %>% filter(prop_decision > 0.95) %>% arrange(decision_intensity))

###########################
### Example Plays (OLD) ###
###########################

{
  # ### find some example plays
  # ex_plays_full = data_full_0A %>% filter(down==4) %>%
  #   filter(half_seconds_remaining >= 1300) %>%
  #   filter(season >= 2012) %>%
  #   select(row_idx, game_id, week, posteam, defteam, season, oq_ot_0_total_sum, dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum, oq_dt_0_sum, dq_ot_0_againstPass_sum dq_ot_0_againstRun_sum)
  # data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum <= -0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum <= -0.5))
  # data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum >= 0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum >= 0.5))
  # data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum >= 0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum <= -0.5))
  # data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum <= -0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum >= 0.5))
  # data.frame(ex_plays_full %>% filter(abs(oq_ot_0_total_sum) <= 0.2 & abs(oq_dt_0_sum) <= 0.2 & week >= 10))
  
  # ###
  # explays1 = data_full_0A %>% filter(down==4) %>% filter(season >= 2012) %>%
  #   select(row_idx, game_id, week, posteam, defteam, season, posteam_spread, score_differential, game_seconds_remaining)
  # View(explays1)
  
  # play_idxs = c(531987, 660986, 767858, 775775, 765904, 710161, 760684, 721265, 733206, 643380)
  # play_idxs = c(531987, 660986, 767858, 775775) #, 765904, 710161, 760684, 721265, 733206, 643380)
  # play_idxs = c(531987, 660986) 
  # play_idxs = c(531987, 660986, 767858, 775775, 709544, 684381, 833737, 796009, 855454, 858240, 788893)
  
  # play_idxs = c(531987, 720008, 744330,    713189, 594436, 843142, 831546,    815661, 632706, 656302, 766604,    554262, 621001, 799918, 842155)
  # play_idxs = c(885738, 877222, 875978, 895095) 
  # play_idxs = c(876754, )
  # play_idxs = c(867213, 859866, 888428, 877789)
  
  # 888598 860533 880711 883540 865345 863419 892421 860973, 898019 878240 893775 865772)
  
  # play_idxs = c(877205, 877797, 878865, 883589, 888428, 888822)
}

#####################
### Example Plays ###
#####################

play_idxs = c(885617, 890744, 
              883589, 888428,
              877425, 859774
              )

plays = bind_rows(
  data_full_0A %>% filter(row_idx %in% play_idxs) %>% mutate(preset_play=FALSE)
)
plays = plays %>% arrange(factor(row_idx, levels = play_idxs))
plays

############################
### Manual Example Plays ###
############################

# colts patriots week 10 2023 EX PLAY 1: (ydl=17, ytg=3, sd=-4, ps=1.5, gsr=1421, ts=10)
manual_play_1 = plays[1,]
manual_play_1 = 
  manual_play_1 %>%
  mutate(
    preset_play=T,
    yardline_100 = 17, ydstogo = 3, score_differential = -4, posteam_spread = 1.5, 
    game_seconds_remaining = 1421, era_A=4, total_score=10,
    posteam_timeouts_remaining=3, defteam_timeouts_remaining=3, receive_2h_ko=T,
    kq_0_sum_std=0, pq_0_sum_std=0,
    qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0,
    kicker_player_name="some kicker", punter_player_name=NA, ### they kicked a FG
    #################################################################
    half = ifelse(game_seconds_remaining <= 1800, 2, 1), 
    qtr = ifelse(game_seconds_remaining<=900,4,
                 ifelse(game_seconds_remaining<=1800,3,
                        ifelse(game_seconds_remaining<=2700,2,1))),
    half_seconds_remaining = ifelse(half == 1, game_seconds_remaining-1800, game_seconds_remaining),
    scoreTimeRatio=compute_scoreTimeRatio(score_differential, game_seconds_remaining)
  ) 

# eagles 49ers NFC championship 2023 EX PLAY 2:  (ydl=35, ytg=3, sd=0, ps=-2.5, gsr=3328)
manual_play_2 = plays[1,]
manual_play_2 = 
  manual_play_2 %>%
  mutate(
    preset_play=T,
    yardline_100 = 35, ydstogo = 3, score_differential = 0, posteam_spread = -2.5, 
    game_seconds_remaining = 3328, era_A=4, total_score=0,
    posteam_timeouts_remaining=3, defteam_timeouts_remaining=3, receive_2h_ko=F,
    kq_0_sum_std=0, pq_0_sum_std=0,
    qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0,
    kicker_player_name=NA, punter_player_name=NA, ### they went for it
    #################################################################
    half = ifelse(game_seconds_remaining <= 1800, 2, 1), 
    qtr = ifelse(game_seconds_remaining<=900,4,
                 ifelse(game_seconds_remaining<=1800,3,
                        ifelse(game_seconds_remaining<=2700,2,1))),
    half_seconds_remaining = ifelse(half == 1, game_seconds_remaining-1800, game_seconds_remaining),
    scoreTimeRatio=compute_scoreTimeRatio(score_differential, game_seconds_remaining)
  ) 

plays = bind_rows(plays, manual_play_1, manual_play_2)
plays

########################################################################
grid_to_plot = tibble(i = 1:nrow(plays), wp=TRUE, preset_plays=plays$preset_play)
grid_to_plot

####################################
# for (j in 1:6) {
# for (j in c(3,4,6,8,7)) {
for (j in 1:nrow(grid_to_plot)) {
  
  ############## play description ############## 
  i = grid_to_plot[j,]$i
  og_method=FALSE
  wp = grid_to_plot[j,]$wp
  print(paste0("iter ", j, " of ", nrow(grid_to_plot), "; i = ", i))
  play = plays[i,]
  preset_play_i = play$preset_play
  
  if (play$down == 4) { # actual 4th down 
    play = play %>% mutate(
      decision_actual = case_when(
        !is.na(kicker_player_name) ~ "FG",
        !is.na(punter_player_name) ~ "Punt",
        TRUE ~ "Go"
      )) 
  }
  
  confounders_dataset_i = play %>% select(
    season, posteam, home_team, roof, posteam_timeouts_remaining, defteam_timeouts_remaining, 
    half_seconds_remaining, half_sec_rem_std, era_A, receive_2h_ko, spread_time, home, game_seconds_remaining, 
    Diff_Time_Ratio, score_differential, elapsed_share, total_score
  )
  
  qbqot_i = play$qbq_ot_0_sum
  oqrot_i = play$oq_rot_0_total_sum
  dqpdt_i = play$dq_dt_0_againstPass_sum 
  dqrdt_i = play$dq_dt_0_againstRun_sum 
  qbqdt_i = play$qbq_dt_0_sum
  oqrdt_i = play$oq_rdt_0_sum
  dqpot_i = play$dq_ot_0_againstPass_sum 
  dqrot_i = play$dq_ot_0_againstRun_sum 
  ps_i = play$posteam_spread
  kicker_i = play$kicker_name
  kq_i = play$kq_0_sum_std
  punter_i = play$punter_name
  pq_i = play$pq_0_sum_std
  qb_i = play$qb_name
  
  play_desc_i = paste0(play$down, "th down and ", play$ydstogo, " yards to go at the ", play$yardline_100, " yardline \n",
                       "this play features ", play$posteam, " on offense vs. ", play$defteam, 
                       " \n the spread is ", ps_i,
                       ". \n The ", play$posteam, " kicker ", kicker_i, " has a kicker quality of ", round(kq_i,3),
                       ". \n The ", play$posteam, " punter ", punter_i, " has a punter quality of ", round(pq_i,3),
                       
                       ". \n ", qb_i, " has a QB quality of ", round(qbqot_i, 3),
                       ", \n ", play$posteam, " has an offensive non-QB quality of ", round(oqrot_i,3),
                       ", \n a def. quality against the pass of ", round(dqpot_i,3),
                       ", \n and a def. quality against the run of ", round(dqrot_i,3), ". ",
                       
                       ", \n The defensive team is ", play$defteam, 
                       ", \n their QB is ", play$qb_name_dt, " who has a QB quality of ", round(qbqdt_i,3),
                       ", \n a non-QB off. quality of ", round(oqrdt_i,3),
                       " \n  a def. quality against the pass of ", round(dqpdt_i,3), 
                       " \n and a def. quality against the run of ", round(dqrdt_i,3), ". \n",
                       " \n game_id ", play$game_id,
                       " \n row_idx ", play$row_idx 
                       )
  print(play_desc_i)
  print(confounders_dataset_i)
  
  play_str = paste(play_desc_i, "\n ", paste0(colnames(confounders_dataset_i)," ",as.character(confounders_dataset_i[1,]), collapse="\n"))
  if (!preset_play_i) {
    textPlot(paste0("plots_decisions/", "plot_decisions_play_", i, "desc"), play_str)
  }
  
  ############## make plots ##############
  # ddf_i = get_full_decision_making(kq=kq_i, pq=pq_i, pspread=ps_i,
  #                                  qbqot=qbqot_i, oqrot=oqrot_i, dqpdt=dqpdt_i, dqrdt=dqrdt_i,
  #                                  qbqdt=qbqdt_i, oqrdt=oqrdt_i, dqpot=dqpot_i, dqrot=dqrot_i,
  #                                  confounders_dataset_i, wp=wp, og_method=og_method, SE=SE)
  
  
  ### non-SE PLOTS
  ddf_i = get_full_decision_making(play_df=play, wp=wp, og_method=og_method, SE=FALSE, coachBaseline=play$down==4)
  plot_prefix = paste0("plot_decisions_play_", i, "_wp", wp, "_og", og_method, "_se", FALSE, "_")
  heatmap_i = plot_4thDownHeatmap(ddf_i, wp=wp, og_method=og_method, SE=FALSE, ydl=play$yardline_100, ytg=play$ydstogo)
  ggsave(paste0("plots_decisions/", plot_prefix, ".png"), heatmap_i, width=9, height=7)

  ### SE PLOTS
  ddf_ise = get_full_decision_making(play_df=play, wp=wp, og_method=og_method, SE=TRUE, coachBaseline=play$down==4)
  plot_prefix_se = paste0("plot_decisions_play_", i, "_wp", wp, "_og", og_method, "_se", TRUE, "_")
  heatmap_ise = plot_4thDownHeatmap(ddf_ise, wp=wp, og_method=og_method, SE=TRUE, ydl=play$yardline_100, ytg=play$ydstogo)
  ggsave(paste0("plots_decisions/", plot_prefix_se, ".png"), heatmap_ise, width=9, height=7)
  
  decision_df_i = get_decision(play$yardline_100, play$ydstogo, ddf_ise, include_uncertainty=TRUE)
  aaa = plot_gt_4th_down_summary(play, ddf_ise, decision_df=decision_df_i, SE=TRUE, wp=wp)
  gtsave(aaa,  paste0("plots_decisions/", plot_prefix_se, "summary", ".png"))

}

