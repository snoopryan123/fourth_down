
#########################################
### Example Decision Making Dataframe ###
#########################################

generate_V_dataset <- function(kq, pq, pspread, qbqot, oqrot, dqpdt, dqrdt, qbqdt, oqrdt, dqpot, dqrot, confounders_dataset) {
  df = tibble()
  MAX_YTG = 15 #10
  for (ydstogo in 1:MAX_YTG) {
    dfy = tibble(
      yardline_100 = ydstogo:99,
      ydstogo = ydstogo,
      qbq_ot_0_sum = qbqot,
      oq_rot_0_total_sum = oqrot,
      dq_dt_0_againstPass_sum = dqpdt,
      dq_dt_0_againstRun_sum = dqrdt,
      qbq_dt_0_sum = qbqdt,
      oq_rdt_0_sum = oqrdt,
      dq_ot_0_againstPass_sum = dqpot,
      dq_ot_0_againstRun_sum = dqrot,
      kq_0_sum_std = kq,
      pq_0_sum_std = pq,
      posteam_spread = pspread
    )
    df = bind_rows(df, dfy)
  }
  ### crudely, join the confounders dataset row to each row of DF
  df$key = 1
  confounders_dataset$key = 1
  df = df %>% left_join(confounders_dataset) %>% select(-key)
  return(df)
}

# ex2_confounders_dataset = tibble(
#   season=2015, posteam="LAR", home_team="LAR", roof="dome",
#   posteam_timeouts_remaining=3, defteam_timeouts_remaining=3,
#   # half_seconds_remaining = 120, half_sec_rem_std = 1-(120/1800),
#   half_seconds_remaining = 1800, half_sec_rem_std = 0, game_seconds_remaining = 3600,
#   # half_seconds_remaining = 900, half_sec_rem_std = 1/2,
#   score_differential = 0, scoreTimeRatio = 0, total_score = 0, receive_2h_ko=1, home=1, elapsed_share=0,
#   era_A = 3
# )
# ex2 = generate_V_dataset(kq=0, pq=0, pspread=0, qbqot=0, oqrot=0, dqpdt=0, dqrdt=0, qbqdt=0, oqrdt=0, dqpot=0, dqrot=0, ex2_confounders_dataset)
# ex2
## View(ex2)

###########################
### First Down EP Model ###
###########################

calculate_V1 <- function(dataset, model_fit, og_method=FALSE, wp) {
  ### calculate the value of having a FIRST DOWN (wth 10 ydstogo, or goal) at the yardlines in DATASET
  dataset = dataset %>% mutate(
    down = 1,
    ydstogo = ifelse(yardline_100 <= 10, yardline_100, 10)
  )

  ### WP
  if (wp) {
    wp_ = predict_probs_xgb(
       model_fit, 
       dataset, 
       xgb_features = get(paste0(V1_model_name_WP, "_features")), 
       wp=TRUE
    )
    return(wp_)
  }
  
  ### EP
  if (og_method) {
    # og_method = "nflFastR"
    og_method = "Burke"
    if (og_method == "Burke") { # Burke 2009 method
      # ep = predict_lm(Burke_model_fit, dataset)
      ep = predict_lm(V1_model_og, dataset)
    } else if (og_method == "nflFastR") { # nflFastR method 
      ep = nflfastR::calculate_expected_points(dataset)$ep
    }
  } else { # EP, our method
    if (V1_model_type_EP == "MLR") {
      ep = predict_mlr_ep(model_fit, dataset, "")$pred
    } else if (V1_model_type_EP == "XGB") {
      ep = predict_ep_xgb(
        model_fit, 
        dataset, 
        xgb_features = get(paste0(V1_model_name_EP, "_features")), 
        model_name = V1_model_name_EP
      )$pred
    }
  }
  ep = ifelse(dataset$yardline_100 <= 0, 7, ifelse(dataset$yardline_100 >= 100, -2, ep))
  return(ep)
}

# ### check
# V1_model_main = V1_wp_model_obs
# calculate_V1(ex2, V1_model_main, wp=T)

###################################
### Field Goal Probablity Model ###
###################################

P_FG <- function(fg_model_fit, dataset, og_method=FALSE) {
  ### calculate the probability of making a FIELD GOAL at the yardlines in DATASET
  dataset = dataset %>% select(yardline_100, kq_0_sum_std)
  if (!og_method) {
    predict(fg_model_fit, dataset, type="response")
  } else { ### original decision making ignores team quality
    predict(fg_model_og, dataset, type="response")
  }
}

# ### check
# P_FG(fg_model, ex2)
# P_FG(fg_model, ex2, og_method=TRUE)

##########################################
### Punt: Expected Next Yardline Model ###
##########################################

E_punt <- function(punt_model_fit, dataset, og_method=FALSE) {
  ### calculate the EXPECTED NEXT YARDLINE of a PUNT at the yardlines in DATASET
  dataset = dataset %>% select(yardline_100, pq_0_sum_std)
  if (!og_method) {
    predict(punt_model_fit, dataset)
  } else { ### original decision making ignores team quality
    predict(punt_model_og, dataset)
  }
}

### check
# E_punt(punt_model, ex2)
# E_punt(punt_model, ex2, og_method=TRUE)

###################################
### Conversion Probablity Model ###
###################################

P_convert <- function(go_model_fit, dataset, og_method=FALSE) {
  ### calculate the probability of converting a FOURTH DOWN GO ATTEMPT at the ydstogo's in DATASET
  dataset = dataset %>% 
    select(ydstogo, yardline_100, qbq_ot_0_sum, oq_rot_0_total_sum, dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum) %>%
    mutate(down = 4) 
  if (!og_method) {
    # predict(go_model_fit, xgb.DMatrix(as.matrix(dataset))) ### catalytic xgboost
    predict(go_model_fit, dataset, type="response") ### logistic regression
  } else { ### original decision making ignores team quality
    predict(go_model_og, dataset, type="response") ### logistic regression
  }
}

# ### check
# P_convert(go_model, ex2)
# P_convert(go_model, ex2, og_method=TRUE)

E_go_outcome_given_success <- function(go_Eoutcome_success_model_fit, dataset) {
  ### calculate the expected outcome (yards gained) given a successful conversion attempt
  dataset = dataset %>% 
    select(ydstogo, yardline_100, qbq_ot_0_sum, oq_rot_0_total_sum, dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum) %>%
    mutate(down = 4) 
  predict(go_Eoutcome_success_model_fit, dataset) ### linear regression
}

E_go_outcome_given_failure <- function(go_Eoutcome_failure_model_fit, dataset) {
  ### calculate the expected outcome (yards gained) given a failed conversion attempt
  dataset = dataset %>% 
    select(ydstogo, yardline_100, qbq_ot_0_sum, oq_rot_0_total_sum, dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum) %>%
    mutate(down = 4) 
  predict(go_Eoutcome_failure_model_fit, dataset) ### linear regression
}

####################################
### Extra Point Probablity Model ###
####################################

P_ExtraPoint <- function(fg_model_fit, dataset, og_method=FALSE) {
  ### calculate the probability of making an EXTRA POINT given the kicker qualities in DATASET
  dataset = dataset %>% mutate(yardline_100 = 15)
  P_FG(fg_model_fit, dataset, og_method=og_method)
}

# ### check
# P_ExtraPoint(fg_model, ex2[1,])

###########################################
### 2 Point Conversion Probablity Model ###
###########################################

P_2PtConversion <- function(go_model_fit, dataset, og_method=FALSE) {
  ### calculate the probability of converting a FOURTH DOWN GO ATTEMPT at the ydstogo's in DATASET
  dataset = dataset %>% mutate(yardline_100 = 2, ydstogo=2, down = 4)
  P_convert(go_model_fit, dataset, og_method=og_method)
}

# ### check
# P_2PtConversion(go_model, ex2)

#############################
### Value of Going For It ###
#############################

field_position_switcherooni <- function(dataset, next_ydl_opp, next_score_diff_opp) {
  ### imagine turning it over and the other team getting the ball
  ### switch the dataset to be from the other team's perspective, if they have possession
  dataset_1a = dataset %>% mutate(
    yardline_100 = next_ydl_opp, #####
    score_differential = next_score_diff_opp,
    ### temp
    qbqot = qbq_ot_0_sum,
    oqrot = oq_rot_0_total_sum,
    qbqdt = qbq_dt_0_sum,
    oqrdt = oq_rdt_0_sum,
    dqpdt = dq_dt_0_againstPass_sum,
    dqrdt = dq_dt_0_againstRun_sum,
    dqpot = dq_ot_0_againstPass_sum,
    dqrot = dq_ot_0_againstRun_sum,
    ### switch TQ's
    qbq_ot_0_sum = qbqdt,
    oq_rot_0_total_sum = oqrdt,
    qbq_dt_0_sum = qbqot,
    oq_rdt_0_sum = oqrot,
    dq_dt_0_againstPass_sum = dqpot,
    dq_dt_0_againstRun_sum = dqrot,
    dq_ot_0_againstPass_sum = dqpdt,
    dq_ot_0_againstRun_sum = dqrdt,
    posteam_spread = -posteam_spread,
    ### switch other stuff
    receive_2h_ko = !receive_2h_ko,
    home = !home,
    ### switch timeouts
    temp = defteam_timeouts_remaining,
    defteam_timeouts_remaining = posteam_timeouts_remaining,
    posteam_timeouts_remaining = temp
  ) %>% 
    mutate(
      ### switch vars which are derived from other vars
      scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
      Diff_Time_Ratio = score_differential / (exp(-4*elapsed_share)),
      spread_time = posteam_spread * exp(-4*elapsed_share),
    ) %>%
    select(-c(qbqot,oqrot,dqpdt,dqrdt,qbqdt,oqrdt,dqpot,dqrot))
  return(dataset_1a)
}

V_go <- function(dataset, go_model_fit, go_Eoutcome_success_model_fit, go_Eoutcome_failure_model_fit, 
                 model_fit, og_method = FALSE, wp, custom_conv_prob=NULL) {
  ### calculate the value of GOING FOR IT at the (yardlines, ydstogo's) in DATASET
  
  ### probability you convert
  pc = unname(P_convert(go_model_fit, dataset, og_method=og_method))
  if (!is.null(custom_conv_prob)) { ### use custom conversion probability
    pc = rep(custom_conv_prob, length(pc))
  } 
  
  ### calculate the expected outcome (yards gained) given a successful conversion attempt
  E_delta_given_success = unname(E_go_outcome_given_success(go_Eoutcome_success_model_fit, dataset))
  E_delta_given_failure = unname(E_go_outcome_given_failure(go_Eoutcome_failure_model_fit, dataset))
  
  # browser()
  
  ### value of a first down if you convert
  dataset_1a = dataset %>% mutate(
    #### yardline_100 = yardline_100 - ydstogo ### assuming that a successful conversion results in gaining exactly ydstogo yards
    yardline_100 = yardline_100 - E_delta_given_success
  )
  v_convert = calculate_V1(dataset_1a, model_fit, og_method = og_method, wp=wp)
  
  ### value of a first down for the other team if you don't convert
  dataset_1b = field_position_switcherooni(
    dataset, 
    #### next_ydl_opp = 100 - dataset$yardline_100, ### assuming that a failed conversion results in gaining exactly 0 yards
    next_ydl_opp = 100 - (dataset$yardline_100 - E_delta_given_failure), 
    next_score_diff_opp = -dataset$score_differential
  )
  v_dont_convert = calculate_V1(dataset_1b, model_fit, og_method = og_method, wp=wp)
  if (wp) {
    v_dont_convert = 1 - v_dont_convert
  } else { ### ep
    v_dont_convert = -v_dont_convert
  }
  
  ### Value & sd of Value, if make this decision
  value = pc*v_convert + (1-pc)*v_dont_convert
  sd_value = sqrt( pc*(1-pc)*(v_convert - v_dont_convert)^2 )

  list(v_convert=v_convert, v_dont_convert=v_dont_convert, p_convert = pc, wp = value, sd_value=sd_value)
}

# ### check
# V_go(ex2, go_model_obs, go_Eoutcome_success_model_obs, go_Eoutcome_failure_model_obs, V1_wp_model_obs, wp=T)

#####################
### Value of a FG ###
#####################

V_FG <- function(dataset, fg_model_fit, model_fit, og_method = FALSE, wp) {
  ### calculate the value of kicking a FIELD GOAL at the yardlines in DATASET

  ### probability you make the field goal
  # pfg = suppressWarnings( P_FG(yardline_100, kq) )
  pfg = P_FG(fg_model_fit, dataset, og_method=og_method)
  pfg = unname(pfg)
  
  ### value if you make the field goal
  if (wp) {
    dataset_1a = field_position_switcherooni(
      dataset, 
      next_ydl_opp = 75, # touchback
      next_score_diff_opp = -dataset$score_differential - 3
    )
    v_make_fg = 1 - calculate_V1(dataset_1a, model_fit, og_method = og_method, wp=wp)
  } else { ### ep
    v_make_fg = 3
  }
  
  # browser()
  # dataset_1a$v_make_fg = v_make_fg
  # dataset_1a$sd_og = dataset$score_differential
  # dataset_1a$ydl_og = dataset$yardline_100
  # dataset_1a %>% select(ydl_og, yardline_100, game_seconds_remaining, score_differential, posteam_spread, v_make_fg) %>% arrange(game_seconds_remaining)
  
  ### value of a first down for the other team if you miss the field goal
  dataset_1b = field_position_switcherooni(
    dataset, 
    # next_ydl_opp = 100 - dataset$yardline_100, 
    next_ydl_opp = pmin(80, 100 - (dataset$yardline_100 + 7)),
    next_score_diff_opp = -dataset$score_differential
  )
  v_miss_fg = calculate_V1(dataset_1b, model_fit, og_method = og_method, wp=wp)
  if (wp) {
    v_miss_fg = 1 - v_miss_fg
  } else { ### ep
    v_miss_fg = -v_miss_fg
  }
  
  ### Value & sd of Value, if make this decision
  value = pfg*v_make_fg + (1-pfg)*v_miss_fg
  sd_value = sqrt( pfg*(1-pfg)*(v_make_fg - v_miss_fg)^2 )
  
  list(pfg = pfg, v_make_fg=v_make_fg, v_miss_fg=v_miss_fg, wp=value, sd_value=sd_value)
}

# ### check
# V_FG(ex2, V1_model_main, og_method = FALSE)
# V_FG(ex2, V1_model_main, og_method = TRUE)

#######################
### Value of a PUNT ###
#######################

V_punt <- function(dataset, punt_model_fit, model_fit, og_method = FALSE, wp) {
  ### calculate the value of PUNTING at the yardlines in DATASET

  ### value of a first down for the other team at the expected next yardline if you punt
  ### no punting within the 30 yardline (the punt model can't even fit this), so just assume touchback if punt
  # v1a = ifelse(dataset$yardline_100 >= 30, v1a, Inf)
  next_ydl_opp_1a = ifelse(
    dataset$yardline_100 >= 30,
    E_punt(punt_model_fit, dataset, og_method=og_method), 
    75 # touchback
  )
  dataset_1a = field_position_switcherooni(
    dataset, 
    # next_ydl_opp = 75, # touchback 
    next_ydl_opp = next_ydl_opp_1a,
    next_score_diff_opp = -dataset$score_differential
  )
  v1a = calculate_V1(dataset_1a, model_fit, og_method = og_method, wp=wp)
  
  # browser()
  # dataset_1a$v1a = v1a
  # dataset_1a$ydl_og = dataset$yardline_100
  # dataset_1a %>% select(ydl_og, yardline_100, game_seconds_remaining, score_differential, posteam_spread, v1a) %>% arrange(game_seconds_remaining)
  # 
  # dataset_1ab = dataset_1a %>% select(game_seconds_remaining, yardline_100, score_differential, scoreTimeRatio, posteam_spread)
  # v1ab = calculate_V1(dataset_1ab, model_fit, og_method = og_method, wp=wp)
  # dataset_1ab$v1ab = v1ab
  # dataset_1ab$ydl_og = dataset$yardline_100
  # dataset_1ab %>% arrange(game_seconds_remaining)
  
  if (wp) {
    return(1 - v1a)
  } else { ### ep
    return(-v1a)
  }
}

# ### check
# V_punt(ex2, V1_model_main, og_method = FALSE)
# V_punt(ex2, V1_model_main, og_method = TRUE)

#######################################
### Value of an Extra Point Attempt ###
#######################################

V_ExtraPoint <- function(dataset, fg_model_fit, model_fit, og_method = FALSE, wp, custom_xp_prob=NULL) {
  ### calculate the value of attempting an EXTRA POINT given covariates in DATASET
  
  ### probability you make the extra point
  p_xp = P_ExtraPoint(fg_model_fit, dataset, og_method=og_method)
  p_xp = unname(p_xp)
  
  if (!is.null(custom_xp_prob)) {
    ### use custom extra point probability
    p_xp = rep(custom_xp_prob, length(p_xp))
  } 
  
  ### value if you make the extra point
  if (wp) {
    dataset_1a = field_position_switcherooni(
      dataset, 
      next_ydl_opp = 75, # touchback
      next_score_diff_opp = -dataset$score_differential - 1 # scored a point
    )
    v_make_xp = 1 - calculate_V1(dataset_1a, model_fit, og_method = og_method, wp=wp)
  } else { ### ep
    v_make_xp = 1
  }
  
  ### value if you miss the extra point
  if (wp) {
    dataset_1b = field_position_switcherooni(
      dataset, 
      next_ydl_opp = 75, # touchback
      next_score_diff_opp = -dataset$score_differential # didn't score a point
    )
    v_miss_xp = 1 - calculate_V1(dataset_1b, model_fit, og_method = og_method, wp=wp)
  } else { ### ep
    v_miss_xp = 0
  }
  
  ### Value & sd of Value, if make this decision
  value = p_xp*v_make_xp + (1-p_xp)*v_miss_xp
  sd_value = sqrt( p_xp*(1-p_xp)*(v_make_xp - v_miss_xp)^2 )
  
  list(p_xp = p_xp, v_make_xp=v_make_xp, v_miss_xp=v_miss_xp, wp=value, sd_value=sd_value)
}

# ### check
# V_ExtraPoint(ex2, V1_model_main)

#############################################
### Value of a 2 Point Conversion Attempt ###
#############################################

V_2PtConversion <- function(dataset, go_model_fit, model_fit, og_method = FALSE, wp, custom_conv_prob=NULL) {
  ### calculate the value of attempting a 2 POINT CONVERSION given the covariates in DATASET
  
  ### probability you convert
  pc = P_2PtConversion(go_model_fit, dataset, og_method=og_method)
  pc = unname(pc)

  if (!is.null(custom_conv_prob)) {
    ### use custom conversion probability
    pc = rep(custom_conv_prob, length(pc))
  } 
  
  ### value if you convert
  if (wp) {
    dataset_1a = field_position_switcherooni(
      dataset, 
      next_ydl_opp = 75, # touchback
      next_score_diff_opp = -dataset$score_differential - 2 # scored 2 points
    )
    v_convert = 1 - calculate_V1(dataset_1a, model_fit, og_method=og_method, wp=wp)
  } else { ### ep
    v_convert = 2
  }
  
  ### value if you don't convert
  if (wp) {
    dataset_1b = field_position_switcherooni(
      dataset, 
      next_ydl_opp = 75, # touchback
      next_score_diff_opp = -dataset$score_differential # didn't score
    )
    v_dont_convert = 1 - calculate_V1(dataset_1b, model_fit, og_method=og_method, wp=wp)
  } else { ### ep
    v_dont_convert = 0
  }
  
  ### Value & sd of Value, if make this decision
  value = pc*v_convert + (1-pc)*v_dont_convert
  sd_value = sqrt( pc*(1-pc)*(v_convert - v_dont_convert)^2 )
  
  list(v_convert=v_convert, v_dont_convert=v_dont_convert, p_convert = pc, wp = value, sd_value=sd_value)
}

# ### check
# V_2PtConversion(ex2, V1_model_main)

#############################################
### Compute the Value of Go, FG, and PUNT ###
#############################################

calculate_Vs <- function(dataset, model_fit, fg_model_fit, punt_model_fit, go_model_fit, 
                         go_Eoutcome_success_model_fit, go_Eoutcome_failure_model_fit, og_method = FALSE, wp, custom_conv_prob=NULL) {
  Vgo_lst = V_go(dataset, go_model_fit, go_Eoutcome_success_model_fit, go_Eoutcome_failure_model_fit, model_fit, og_method=og_method, wp=wp, custom_conv_prob=custom_conv_prob)
  Vgo = Vgo_lst$wp
  Vfg_lst = V_FG(dataset, fg_model_fit, model_fit, og_method=og_method, wp=wp)
  Vfg = Vfg_lst$wp
  Vpunt = V_punt(dataset, punt_model_fit, model_fit, og_method=og_method, wp=wp)

  # ### restrict the range
  # Vfg = ifelse(dataset$yardline_100 > 43, NA_real_, Vfg) ###FIXME
  # Vpunt = ifelse(dataset$yardline_100 < 25, NA_real_, Vpunt)
  
  ### decision
  V_scores = tibble(
    Vgo_scores = replace_na(Vgo, -Inf),
    Vfg_scores = replace_na(Vfg, -Inf),
    Vpunt_scores = replace_na(Vpunt, -Inf),
  ) %>% mutate(
    maxcol = max.col(.[,1:3]),
    mincol = max.col(-.[,1:3]),
    secondMaxCol = 1+2+3-maxcol-mincol,
    decision = case_when(maxcol == 1 ~ "Go", maxcol == 2 ~ "FG", maxcol == 3 ~ "Punt", TRUE ~ NA_character_),
    decision_2ndBest = case_when(secondMaxCol == 1 ~ "Go", secondMaxCol == 2 ~ "FG", secondMaxCol == 3 ~ "Punt", TRUE ~ NA_character_),
    decision_3rdBest = case_when(mincol == 1 ~ "Go", mincol == 2 ~ "FG", mincol == 3 ~ "Punt", TRUE ~ NA_character_)
  ) %>% 
  ungroup() %>%
  mutate(
    V_bestDecision = case_when(decision == "Go" ~ Vgo_scores, decision == "FG" ~ Vfg_scores, decision == "Punt" ~ Vpunt_scores, TRUE ~ NA_real_),
    V_2ndbestDecision = case_when(decision_2ndBest == "Go" ~ Vgo_scores, decision_2ndBest == "FG" ~ Vfg_scores, decision_2ndBest == "Punt" ~ Vpunt_scores, TRUE ~ NA_real_),
    V_3rdbestDecision = case_when(decision_3rdBest == "Go" ~ Vgo_scores, decision_3rdBest == "FG" ~ Vfg_scores, decision_3rdBest == "Punt" ~ Vpunt_scores, TRUE ~ NA_real_),
    decision_intensity = V_bestDecision - V_2ndbestDecision
  )
  V_scores
  
  tibble(
     V_go = Vgo, V_fg = Vfg, V_punt = Vpunt, 
     decision = V_scores$decision, decision_2ndBest = V_scores$decision_2ndBest, decision_3rdBest = V_scores$decision_3rdBest, 
     decision_intensity = V_scores$decision_intensity,
     p_convert = Vgo_lst$p_convert, v_convert = Vgo_lst$v_convert, v_dont_convert = Vgo_lst$v_dont_convert, sd_v_convert = Vgo_lst$sd_value,
     p_fg = Vfg_lst$pfg, v_make_fg = Vfg_lst$v_make_fg, v_miss_fg = Vfg_lst$v_miss_fg, sd_v_fg = Vfg_lst$sd_value,
  )
}

# ### check
# calculate_Vs(ex2, V1_model_main, fg_model, punt_model, go_model, og_method = FALSE)
# calculate_Vs(ex2, V1_model_main, og_method = TRUE)

##################################################################
### Compute the Value of Go, FG, and PUNT using our Bootstrap  ###
##################################################################

calculate_Vs_bootstrap <- function(dataset, wp, b_max=B, custom_conv_prob=NULL, alpha=0.05) {
  # browser()

  Vs_all = tibble()
  for (b in 1:b_max) {
    print(paste0("evaluate bootstrap b = ", b, " of B = ", b_max))
    ### b^th bootstrapped model fits
    if (wp) {
      V1_wp_model_fit_b = V1_wp_model_fitList_boot[[b]]
    } else {
      #FIXME
      model_fit_b = V1_model_fitList_boot[[b]]
    }
    fg_model_fit_b = fg_model_fitList_boot[[b]]
    punt_model_fit_b = punt_model_fitList_boot[[b]]
    go_model_fit_b = go_model_fitList_boot[[b]]
    go_Eoutcome_success_model_fit_b = go_Eoutcome_success_model_fitList_boot[[b]]
    go_Eoutcome_failure_model_fit_b = go_Eoutcome_failure_model_fitList_boot[[b]]
  
    Vs_b = calculate_Vs(dataset, if (wp) V1_wp_model_fit_b else model_fit_b, fg_model_fit_b, punt_model_fit_b, go_model_fit_b, 
                        go_Eoutcome_success_model_fit_b, go_Eoutcome_failure_model_fit_b, og_method=FALSE, wp=wp, custom_conv_prob=custom_conv_prob)
    Vs_b$b = b
    Vs_b = Vs_b %>% mutate(i = 1:n())
    Vs_all = bind_rows(Vs_all, Vs_b)
  }
  
  ### bootstrapped 4th down decisions!
  Vs_all0 = Vs_all %>% ###filter(i==1) %>%
    group_by(i) %>%
    mutate(
      decision_pointEstimate = decision[1],
      # decision_intensity_pointEstimate = decision_intensity[1],
      num_go = sum(decision == "Go"),
      num_fg = sum(decision == "FG"),
      num_punt = sum(decision == "Punt"),
      boot_prop_go = num_go/n(),
      boot_prop_fg = num_fg/n(),
      boot_prop_punt = num_punt/n(),
    ) %>%
    relocate(decision_pointEstimate, .before=decision) %>%
    rowwise() %>%
    mutate(
      V_gain = case_when(
        decision_pointEstimate == "Go" ~ V_go - max(V_fg, V_punt),
        decision_pointEstimate == "FG" ~ V_fg - max(V_go, V_punt),
        decision_pointEstimate == "Punt" ~ V_punt - max(V_go, V_fg),
      )
    ) %>%
    relocate(V_gain, .after=decision_intensity) %>%
    group_by(i) %>%
    # relocate(decision_intensity_pointEstimate, .after=decision_intensity) %>%
    mutate(
      num_boot_best = num_go*(decision=="Go") + num_fg*(decision=="FG") + num_punt*(decision=="Punt"),
      num_boot_2ndbest = num_go*(decision_2ndBest=="Go") + num_fg*(decision_2ndBest=="FG") + num_punt*(decision_2ndBest=="Punt"),
      num_boot_3rdbest = num_go*(decision_3rdBest=="Go") + num_fg*(decision_3rdBest=="FG") + num_punt*(decision_3rdBest=="Punt"),
      prop_decision = num_boot_best/n(),
      prop_decision_2nd_Best = num_boot_2ndbest/n(),
      prop_decision_3rd_Best = num_boot_3rdbest/n(),
      prop_intensity = prop_decision - prop_decision_2nd_Best
    ) %>% ungroup() 
  Vs_all0
  
  ### get CI from boostrap samples
  Vs_all2 = Vs_all0 %>% group_by(i) %>% 
    summarise(
      V_go = quantile(V_go, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_fg = quantile(V_fg, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_punt = quantile(V_punt, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_decision_intensity = quantile(decision_intensity, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_gain = quantile(V_gain, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      ### V_decision_intensity_pointEstimate = quantile(decision_intensity_pointEstimate, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
    ) %>% 
    mutate(
      V_go_names = paste0("V_go", c("_L", "_M", "_U")),
      V_fg_names = paste0("V_fg", c("_L", "_M", "_U")),
      V_punt_names = paste0("V_punt", c("_L", "_M", "_U")),
      V_decision_intensity_names = paste0("decision_intensity", c("_L", "_M", "_U")),
      V_gain_names = paste0("V_gain", c("_L", "_M", "_U")),
      # V_go_names = paste0("V_go", c("_L", "", "_U")),
      # V_fg_names = paste0("V_fg", c("_L", "", "_U")),
      # V_punt_names = paste0("V_punt", c("_L", "", "_U"))
    ) %>% ungroup()
  Vs_all2
  V_go_SE = Vs_all2 %>% pivot_wider(names_from = V_go_names, values_from = V_go, id_cols=i) %>% ungroup()
  V_fg_SE = Vs_all2 %>% pivot_wider(names_from = V_fg_names, values_from = V_fg, id_cols=i) %>% ungroup()
  V_punt_SE = Vs_all2 %>% pivot_wider(names_from = V_punt_names, values_from = V_punt, id_cols=i) %>% ungroup()
  V_decision_intensity_SE = Vs_all2 %>% pivot_wider(names_from = V_decision_intensity_names, values_from = V_decision_intensity, id_cols=i) %>% ungroup()
  V_gain_SE = Vs_all2 %>% pivot_wider(names_from = V_gain_names, values_from = V_gain, id_cols=i) %>% ungroup()
  Vs_all3 = V_go_SE %>% left_join(V_fg_SE) %>% left_join(V_punt_SE) %>% left_join(V_decision_intensity_SE) %>% left_join(V_gain_SE)
  Vs_all3
  # Vs_all4 = left_join(Vs_all0, Vs_all3, by="i")
  Vs_all4 = left_join(Vs_all0, Vs_all3, by="i")
  Vs_all4
  # Vs_all5 = Vs_all4 %>% filter(b == 1) %>% ### keep only the model from observed data
  #   relocate(V_go, .before = V_go_M) %>% relocate(V_fg, .before = V_fg_M) %>% relocate(V_punt, .before = V_punt_M)
  Vs_all5 = Vs_all4 %>% filter(b == 1) ### 
  Vs_all5
  Vs_all6 = Vs_all5 %>% 
    select(-c(V_go_M, V_fg_M, V_punt_M)) %>%
    relocate(V_go, .after= V_go_L) %>% relocate(V_fg, .after= V_fg_L) %>% relocate(V_punt, .after= V_punt_L) 
  Vs_all6
  
  # return(Vs_all6)
  list(Vs_all = Vs_all0, Vs = Vs_all6)
}

# ### check
# calculate_Vs_bootstrap(ex2)

#######################################################
### Compute the Value of ExtraPoint & 2PtConversion ###
#######################################################

calculate_Vs_xp2pc <- function(dataset, model_fit, fg_model_fit, go_model_fit, og_method=FALSE, wp, custom_conv_prob=NULL, custom_xp_prob=NULL) {
  V_xp_lst = V_ExtraPoint(dataset, fg_model_fit, model_fit, og_method, wp, custom_xp_prob)
  V_xp = V_xp_lst$wp
  V_2pc_lst = V_2PtConversion(dataset, go_model_fit, model_fit, og_method, wp, custom_conv_prob)
  V_2pc = V_2pc_lst$wp
  
  ### decision
  V_scores = tibble(
    V2pc_scores = replace_na(V_2pc, -Inf),
    Vxp_scores = replace_na(V_xp, -Inf),
  ) %>% mutate(
    decision = ifelse(V2pc_scores > Vxp_scores, "2pc", "xp"),
    decision_2ndBest = ifelse(decision != "2pc", "2pc", "xp")
  ) %>% 
    ungroup() %>%
    mutate(
      V_bestDecision = case_when(decision == "2pc" ~ V2pc_scores, decision == "xp" ~ Vxp_scores, TRUE ~ NA_real_),
      V_2ndbestDecision = case_when(decision_2ndBest == "2pc" ~ V2pc_scores, decision_2ndBest == "xp" ~ Vxp_scores, TRUE ~ NA_real_),
      decision_intensity = V_bestDecision - V_2ndbestDecision
    )
  V_scores
  
  tibble(
    V_2pc = V_2pc, V_xp = V_xp, 
    decision = V_scores$decision, decision_2ndBest = V_scores$decision_2ndBest,
    decision_intensity = V_scores$decision_intensity,
    p_convert = V_2pc_lst$p_convert, v_convert = V_2pc_lst$v_convert, v_dont_convert = V_2pc_lst$v_dont_convert, sd_v_convert = V_2pc_lst$sd_value,
    p_xp = V_xp_lst$p_xp, v_make_xp = V_xp_lst$v_make_xp, v_miss_xp = V_xp_lst$v_miss_xp, sd_v_xp = V_xp_lst$sd_value,
  )
}

calculate_Vs_xp2pc_bootstrap <- function(dataset, wp, b_max=B, custom_conv_prob=NULL, custom_xp_prob=NULL) {
  Vs_all = tibble()
  for (b in 1:b_max) {
    print(paste0("evaluate bootstrap b = ", b, " of B = ", b_max))
    ### b^th bootstrapped model fits
    if (wp) {
      V1_wp_model_fit_b = V1_wp_model_fitList_boot[[b]]
    } else {
      #FIXME
      model_fit_b = V1_model_fitList_boot[[b]]
    }
    fg_model_fit_b = fg_model_fitList_boot[[b]]
    punt_model_fit_b = punt_model_fitList_boot[[b]]
    go_model_fit_b = go_model_fitList_boot[[b]]
    
    Vs_b = calculate_Vs_xp2pc(dataset, if (wp) V1_wp_model_fit_b else model_fit_b, fg_model_fit_b, go_model_fit_b, og_method=FALSE, 
                              wp=wp, custom_conv_prob=custom_conv_prob, custom_xp_prob=custom_xp_prob) 
    Vs_b$b = b
    Vs_b = Vs_b %>% mutate(i = 1:n())
    Vs_all = bind_rows(Vs_all, Vs_b)
  }
  
  ### bootstrapped 4th down decisions!
  Vs_all0 = Vs_all %>% ###filter(i==1) %>%
    group_by(i) %>%
    mutate(
      decision_pointEstimate = decision[1],
      # decision_intensity_pointEstimate = decision_intensity[1],
      num_xp = sum(decision == "xp"),
      num_2pc = sum(decision == "2pc"),
      boot_prop_xp = num_xp/n(),
      boot_prop_2pc = num_2pc/n(),
    ) %>%
    relocate(decision_pointEstimate, .before=decision) %>%
    rowwise() %>%
    mutate(
      V_gain = case_when(
        decision_pointEstimate == "2pc" ~ V_2pc - V_xp,
        decision_pointEstimate == "xp" ~ V_xp -V_2pc,
      )
    ) %>%
    relocate(V_gain, .after=decision_intensity) %>%
    group_by(i) %>%
    # relocate(decision_intensity_pointEstimate, .after=decision_intensity) %>%
    mutate(
      num_boot_best = num_2pc*(decision=="2pc") + num_xp*(decision=="xp"),
      num_boot_2ndbest = num_2pc*(decision_2ndBest=="2pc") + num_xp*(decision_2ndBest=="xp"),
      prop_decision = num_boot_best/n(),
      prop_decision_2nd_Best = num_boot_2ndbest/n(),
      prop_intensity = prop_decision - prop_decision_2nd_Best
    ) %>% ungroup() 
  Vs_all0
  
  ### get CI from boostrap samples
  Vs_all2 = 
    Vs_all0 %>% 
    group_by(i) %>% 
    summarise(
      V_2pc = quantile(V_2pc, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_xp = quantile(V_xp, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_decision_intensity = quantile(decision_intensity, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      V_gain = quantile(V_gain, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
      ### V_decision_intensity_pointEstimate = quantile(decision_intensity_pointEstimate, probs=c(alpha, 0.5, 1-alpha), na.rm=TRUE),
    ) %>% 
    mutate(
      V_2pc_names = paste0("V_2pc", c("_L", "_M", "_U")),
      V_xp_names = paste0("V_xp", c("_L", "_M", "_U")),
      V_decision_intensity_names = paste0("decision_intensity", c("_L", "_M", "_U")),
      V_gain_names = paste0("V_gain", c("_L", "_M", "_U")),
    ) %>% ungroup()
  Vs_all2
  V_2pc_SE = Vs_all2 %>% pivot_wider(names_from = V_2pc_names, values_from = V_2pc, id_cols=i) %>% ungroup()
  V_xp_SE = Vs_all2 %>% pivot_wider(names_from = V_xp_names, values_from = V_xp, id_cols=i) %>% ungroup()
  V_decision_intensity_SE = Vs_all2 %>% pivot_wider(names_from = V_decision_intensity_names, values_from = V_decision_intensity, id_cols=i) %>% ungroup()
  V_gain_SE = Vs_all2 %>% pivot_wider(names_from = V_gain_names, values_from = V_gain, id_cols=i) %>% ungroup()
  Vs_all3 = V_2pc_SE %>% left_join(V_xp_SE) %>% left_join(V_decision_intensity_SE) %>% left_join(V_gain_SE)
  Vs_all3
  # Vs_all4 = left_join(Vs_all0, Vs_all3, by="i")
  Vs_all4 = left_join(Vs_all0, Vs_all3, by="i")
  Vs_all4
  # Vs_all5 = Vs_all4 %>% filter(b == 1) %>% ### keep only the model from observed data
  #   relocate(V_2pc, .before = V_2pc_M) %>% relocate(V_xp, .before = V_xp_M) %>% relocate(V_punt, .before = V_punt_M)
  Vs_all5 = Vs_all4 %>% filter(b == 1) ### 
  Vs_all5
  Vs_all6 = Vs_all5 %>% 
    select(-c(V_2pc_M, V_xp_M)) %>%
    relocate(V_2pc, .after= V_2pc_L) %>% relocate(V_xp, .after= V_xp_L)
  Vs_all6
  
  return(Vs_all6)
}

#################################################
### coachs' baseline decision model functions ###
#################################################

###
get_coach_decision_freqs <- function(fourth_down_dataset, coach_model) {
  ### XGBoost predictions
  p = pred_xgb_coach(coach_model, fourth_down_dataset)

  # p_fg_raw = pred_xgb_coach_fg(coach_model_fg, fourth_down_dataset)
  # p_punt_raw = pred_xgb_coach_punt(coach_model_punt, fourth_down_dataset)
  ### logistic regression predictions
  # p_fg_raw = predict(coach_model_fg, fourth_down_dataset, type="link")
  # p_punt_raw = predict(coach_model_punt, fourth_down_dataset, type="link")
  # p_fg_raw = exp(p_fg_raw)
  # p_punt_raw = exp(p_punt_raw)
  # p_raw = 1 + p_fg_raw + p_punt_raw
  # result = matrix(data = c(1/p_raw,  p_fg_raw/p_raw,  p_punt_raw/p_raw), ncol=3, byrow=FALSE)
  # colnames(result) = c("C_Go", "C_FG", "C_Punt")
  # result = as_tibble(result)
  
  result = as_tibble(p)
  result = result %>% 
    rowwise() %>%
    mutate(decision_coach_usual = case_when(
      C_Go == max(C_Go, C_FG, C_Punt) ~ "Go",
      C_FG == max(C_Go, C_FG, C_Punt) ~ "FG",
      C_Punt == max(C_Go, C_FG, C_Punt) ~ "Punt",
    )) %>%
    ungroup()
  result
}

###
get_coach_freqs_plot_df <- function(game_seconds_remaining, score_differential, coach_model) {
  plot_df = tibble()
  for (ytg in 1:15) {
    plot_df_ =  tibble(
      yardline_100 = 1:99,
      ydstogo = ytg,
      score_differential = score_differential,
      game_seconds_remaining = game_seconds_remaining, 
      posteam_spread = 0, 
      era_A=4
    )
    plot_df = bind_rows(plot_df, plot_df_)
  }
  ddf_coach = plot_df %>%
    mutate(get_coach_decision_freqs(., coach_model)) %>%
    rowwise() %>%
    mutate(decision = case_when(
      C_Go == max(C_Go, C_FG, C_Punt) ~ "Go",
      C_FG == max(C_Go, C_FG, C_Punt) ~ "FG",
      C_Punt == max(C_Go, C_FG, C_Punt) ~ "Punt",
    )) %>%
    mutate(decision_intensity = case_when(
      decision == "Go" ~ C_Go,
      decision == "FG" ~ C_FG,
      decision == "Punt" ~ C_Punt,
    )) 
  ddf_coach
}

############################
### Full Decision Making ###
############################

get_all_decision_making <- function(plays_df, wp, og_method=FALSE, SE=FALSE, b_max=B, 
                                    coachBaseline=FALSE, custom_conv_prob=NULL, bind_w_plays=TRUE) {
  # browser()
  
  if (SE) { ### decision making with bootstrapped standard errors
    Vs_lst = calculate_Vs_bootstrap(plays_df, wp=wp, b_max=b_max, custom_conv_prob=custom_conv_prob)
    Vs = Vs_lst$Vs
    Vs_all = Vs_lst$Vs_all
  } else {
    Vs = calculate_Vs(plays_df, if (wp) V1_wp_model_obs else V1_model_obs, fg_model_obs, punt_model_obs, go_model_obs, 
                      go_Eoutcome_success_model_obs, go_Eoutcome_failure_model_obs, og_method=og_method, wp=wp, custom_conv_prob=custom_conv_prob)
  }
  
  if (bind_w_plays) {
    result = bind_cols(Vs, plays_df)
    if (SE) {
      Vs_all = bind_cols(
        Vs_all, 
        do.call("rbind", replicate(nrow(Vs_all)/nrow(plays_df), plays_df, simplify = FALSE))
      )
    }
  } else {
    result = Vs
  }
  
  if (coachBaseline) {
    result = result %>%
      mutate(get_coach_decision_freqs(., coach_model)) %>%
      mutate(V_baseline = C_Go*V_go + C_FG*V_fg + C_Punt*V_punt)
    
    if ("decision_actual" %in% colnames(result)) {
      result = result %>%
        rowwise() %>%
        mutate(V_actual = case_when(
          decision_actual == "Go" ~ V_go,
          decision_actual == "FG" ~ V_fg,
          decision_actual == "Punt" ~ V_punt,
        )) %>%
        ungroup() %>%
        mutate(V_added = V_actual - V_baseline)
    }
 
  }
  
  # return(result)
  if (SE) {
    list(Vs=result, Vs_all=Vs_all)
  } else {
    result
  }
}

get_POST_TD_decision_making <- function(plays_df, wp, og_method=FALSE, SE=FALSE, b_max=B, 
                                        custom_conv_prob=NULL, custom_xp_prob=NULL) {
  if (SE) { ### decision making with bootstrapped standard errors
    Vs = calculate_Vs_xp2pc_bootstrap(plays_df, wp=wp, b_max=b_max, custom_conv_prob=custom_conv_prob, custom_xp_prob=custom_xp_prob)
  } else {
    Vs = calculate_Vs_xp2pc(plays_df, if (wp) V1_wp_model_obs else V1_model_obs, fg_model_obs, go_model_obs, og_method, wp, custom_conv_prob, custom_xp_prob)
  }
  result = bind_cols(Vs, plays_df)
  return(result)
}

get_full_decision_making <- function(play_df, wp, og_method=FALSE, SE=FALSE, b_max=B, coachBaseline=FALSE, 
                                     custom_conv_prob=NULL, custom_xp_prob=NULL, post_TD=FALSE) {
  
  kq = play_df$kq_0_sum_std
  pq = play_df$pq_0_sum_std
  pspread = play_df$posteam_spread
  qbqot = play_df$qbq_ot_0_sum
  oqrot = play_df$oq_rot_0_total_sum
  dqpdt = play_df$dq_dt_0_againstPass_sum 
  dqrdt = play_df$dq_dt_0_againstRun_sum 
  
  qbqdt = play_df$qbq_dt_0_sum
  oqrdt = play_df$oq_rdt_0_sum
  dqpot = play_df$dq_ot_0_againstPass_sum 
  dqrot = play_df$dq_ot_0_againstRun_sum 

  confounders_dataset = play_df %>% select(
    score_differential, scoreTimeRatio, game_seconds_remaining, home, receive_2h_ko,
    posteam_timeouts_remaining, defteam_timeouts_remaining,
    half_seconds_remaining, half_sec_rem_std, half, utm, era_A, 
    elapsed_share, spread_time, Diff_Time_Ratio, total_score
  )
  
  dataset_V = generate_V_dataset(kq, pq, pspread, qbqot, oqrot, dqpdt, dqrdt, qbqdt, oqrdt, dqpot, dqrot, confounders_dataset)
  if (!post_TD) {
    ### FOURTH DOWN
    if (coachBaseline) {
      dataset_V$decision_actual = play_df$decision_actual
    }
    get_all_decision_making(dataset_V, wp, og_method=og_method, SE=SE, b_max=b_max, coachBaseline=coachBaseline, custom_conv_prob=custom_conv_prob) 
  } else {
    ### POST-TD
    get_POST_TD_decision_making(dataset_V[1,], wp, og_method, SE, b_max, custom_conv_prob, custom_xp_prob) 
  }
}

# ### check
# A = get_full_decision_making(kq=0, oqot=0, dqdt=0, oqdt=0, dqot=0, ex2_confounders_dataset,
#                              V1_model_main, og_method=FALSE, SE=FALSE)
# A
  
############
### plot ###
############ decision_intensity_0   prop_decision_A

plot_4thDownHeatmap <- function(decision_df, wp, og_method=FALSE, title=TRUE, legend_pos="right", 
                                SE=FALSE, coach=FALSE, ydl=NA, ytg=NA, dec_conf_str=FALSE) {
  
  decision_df = decision_df %>% filter(ydstogo <= 10)
  
  # colrs <- c("Go" = "forestgreen", "FG" = "goldenrod2", "Punt" = "firebrick")
  colrs <- c("Go" = "darkgreen", "FG" = "darkgoldenrod", "Punt" = "darkred")
  # num_brks = 3
  
  fill_var = if (SE) "prop_decision" else "decision_intensity"
  
  # V_title = if (wp) "win probability" else "expected points"
  
  if (coach) {
    V_title = paste0(" baseline coach decision making frequencies\n with ",
                     decision_df$game_seconds_remaining[1], " sec. to go and score diff. = ", decision_df$score_differential[1])
    wp = TRUE
  } else if (SE) {
    # V_title = paste0(" proportion of bootstrapped ", if (wp) "WP" else "EP", "\n models making that decision")
    if (dec_conf_str) {
      V_title = paste0("decision confidence")
    } else {
      # V_title = paste0(" proportion of bootstrapped\n", "models making that decision")
      V_title = paste0(" prop. bootstrapped models making that decision")
    }
  } else {
    # V_title = paste0(" ", if (wp) "win probability" else "expected points", " added by\n making that decision")
    V_title = paste0(" ", if (wp) "win probability" else "expected points", " added by making that decision")
  }
  
  if (SE) {
    limits_b = c(0,1)
    values_b = rescale(c(0,0.5,1))
    colors_go=c("white","white",colrs["Go"])
    colors_fg=c("white","white",colrs["FG"])
    colors_punt=c("white","white",colrs["Punt"])
    breaks_b = seq(0.5,1,by=0.1)
  } else {
    max_val = max(0.05, max(decision_df$decision_intensity))
    # limits_b = c(0,NA)
    # limits_b = c(0,0.05)
    limits_b = c(0,max_val)
    values_b = rescale(c(0,0.05,max_val))
    colors_go = c("white",colrs["Go"],colrs["Go"])
    colors_fg = c("white",colrs["FG"],colrs["FG"])
    colors_punt = c("white",colrs["Punt"],colrs["Punt"])
    break_val = 0.01
    breaks_b = seq(0,0.05,by=break_val)
    # breaks_b = seq(0,max_val,by=break_val)
  }
  
  # if (SE) { browser() }
  # decision_df %>%
  #   ggplot(aes(x = yardline_100, y = ydstogo)) +
  #   geom_tile(data = . %>% filter(decision == "FG"), aes(fill = .data[[fill_var]])) +
  #   scale_fill_gradientn(colors = colors_fg, values = values_b, limits = limits_b, breaks=breaks_b) +
  #   guides(fill = guide_legend(title=" FG",label.hjust = 0.5, label.vjust = 1, order = 1))
  
  p =
    decision_df %>%
    ggplot(aes(x = yardline_100, y = ydstogo)) +
    geom_tile(data = . %>% filter(decision == "Go"), aes(fill = .data[[fill_var]])) +
    scale_fill_gradientn(colors = colors_go, values = values_b, limits = limits_b, breaks=breaks_b) +
    guides(fill = guide_legend(title=" Go",label.hjust = 0.5, label.vjust = 1, order = 1)) +
    new_scale_fill() +
    geom_tile(data = . %>% filter(decision == "FG"), aes(fill = .data[[fill_var]])) +
    scale_fill_gradientn(colors = colors_fg, values = values_b, limits = limits_b, breaks=breaks_b) +
    guides(fill = guide_legend(title=" FG",label.hjust = 0.5, label.vjust = 1, order = 1)) +
    new_scale_fill() +
    geom_tile(data = . %>% filter(decision == "Punt"), aes(fill = .data[[fill_var]])) +
    scale_fill_gradientn(colors = colors_punt, values = values_b, limits = limits_b, breaks=breaks_b) +
    guides(fill = guide_legend(title=" Punt",label.hjust = 0.5, label.vjust = 1, order = 1)) +
    geom_point(x=ydl, y=ytg, size=3, stroke=2, color="magenta", fill="white", shape=21)
  p
  
  if (title) {
    p = p + labs(title=V_title)
  }
  p
  
  # if (title) {
  #   # p = p + labs(title = paste0(paste0("Fourth Down Decision Making using ", if (wp) "Win Probability" else "Expected Points"))) 
  #   p = p + labs(title = paste0(paste0(" fourth down decision making \n using ", V_title))) 
  # }
  
  if (SE) {
    p = p + scale_alpha_continuous(
      name = " proportion\n of\n bootstrapped\n models\n making\n that\n decision", 
      # breaks=seq(0,1,by=0.1)
    )
  } else {
    p = p + scale_alpha_continuous(
      name = paste0(" ", V_title, "\n added if", "\n make that", "\n decision"), 
    )
  }
  
  p = p +
    # xlab("yardline") + 
    xlab("yards to opponent endzone") + 
    ylab("yards to go") +
    scale_y_continuous(breaks=seq(1,10,by=1), minor_breaks = seq(1,10,by=1)) +
    scale_x_continuous(breaks=seq(10,90,by=10)) +
    theme(
      axis.title = element_text(size=30),
      axis.text = element_text(size=25),
    ) 
  
  if (legend_pos == "right") {
    p = p + theme(legend.direction = "vertical", legend.box = "vertical", legend.position="right") 
  } else if (legend_pos == "bottom") {
    p = p + theme(legend.direction = "horizontal", legend.box = "horizontal", legend.position="bottom") 
  } else if (legend_pos == "none") {
    p = p + guides(alpha = "none")
  } else if (legend_pos == "separate") {
    # browser()
    p = p + theme(
      legend.key.size = unit(1, 'cm'), #change legend key size
      legend.key.height = unit(1/2, 'cm'), #change legend key height
      legend.key.width = unit(1/2, 'cm'), #change legend key width
      legend.title = element_text(size=12), #change legend title font size
      legend.text = element_text(size=9) #change legend text font size
    ) 
    p
    legend = ggdraw(cowplot::get_legend(p))
    p = p + theme(legend.position="none") 
    return(list(plot = p, legend = legend))
  } 
  
  return(p)
}

textPlot <- function(plotname, string){
  # par(mar=c(0,0,0,0))
  # # pdf(paste0(plotname, ".pdf"))
  # png(paste0(plotname, ".png"))
  # plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  # text(x = 0.5, y = 0.5, paste(string), cex = 1, col = "black", family="serif", font=2, adj=0.5)
  # dev.off()
  strplot = ggplot() +
    annotate("text", x = 4, y = 25, size = 3, label = string) +
    theme_classic() + theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
    ) 
  ggsave(paste0(plotname, ".png"), strplot)
}

#########################################
### plot the V_go, V_fg, V_punt lines ###
#########################################

plot_Vs <- function(ddf, ytg, SE=FALSE, include_og=FALSE, wp=FALSE) {
  ddf_iytg = ddf %>% filter(ydstogo == ytg)
  ddf_iytg = ddf_iytg %>% mutate(V_punt = ifelse(yardline_100 <= 30, NA, V_punt))
  if (SE) {
    ddf_iytg = ddf_iytg %>% mutate(V_punt_L = ifelse(yardline_100 <= 30, NA, V_punt_L))
    ddf_iytg = ddf_iytg %>% mutate(V_punt_U = ifelse(yardline_100 <= 30, NA, V_punt_U))
  }
  
  plot_title = paste0(ytg, " yards to go")
  
  breaks=seq(-7,7,by=1)
  ylab_ = "expected points"
  if (wp) {
    y_max = 1
    y_min = 0
    breaks = seq(-1,1,by=0.2)
    ylab_ = "win probability"
  } else if (SE) { ### bootstrapped standard errors
    y_max = max(c(ddf_iytg$V_fg_U, ddf_iytg$V_go_U, ddf_iytg$V_punt_U), na.rm=TRUE)+0.2
    y_min = min(c(ddf_iytg$V_fg_U, ddf_iytg$V_go_U, ddf_iytg$V_punt_U), na.rm=TRUE)
  } else {
    y_max = max(c(ddf_iytg$V_fg, ddf_iytg$V_go, ddf_iytg$V_punt), na.rm=TRUE)#+0.2
    y_min = min(c(ddf_iytg$V_fg, ddf_iytg$V_go, ddf_iytg$V_punt), na.rm=TRUE)
  }
  plot_decision = ddf_iytg %>%
    ggplot(aes(x=yardline_100)) +
    geom_line(aes(y = V_punt, color="Punt"), size=1) +
    geom_line(aes(y = V_fg, color="FG"), size=1) +
    geom_line(aes(y = V_go, color="Go"), size=1) +
    scale_color_manual(name="", values=c("Punt"="firebrick", "Go"="forestgreen", "FG"="goldenrod2")) +
    labs(title=plot_title) +
    # xlab("yardline") + 
    xlab("yards to opponent endzone") + 
    ylab(ylab_) +
    scale_x_continuous(breaks=seq(0,100,by=10), limits=c(0,100)) +
    scale_y_continuous(breaks=breaks, 
                       limits=c(y_min,y_max)
                       # limits=c(y_min-1,y_max+1.5)
    )
  plot_decision
  
  if (SE) { ### bootstrapped standard errors
    plot_decision = plot_decision + 
      geom_line(aes(y = V_punt_L), linetype="dashed", color="firebrick") +
      geom_line(aes(y = V_punt_U), linetype="dashed", color="firebrick") +
      geom_line(aes(y = V_fg_L), linetype="dashed", color="goldenrod2") +
      geom_line(aes(y = V_fg_U), linetype="dashed", color="goldenrod2") +
      geom_line(aes(y = V_go_L), linetype="dashed", color="forestgreen") +
      geom_line(aes(y = V_go_U), linetype="dashed", color="forestgreen")
  } else {
    
  }
  
  if (include_og) { ### include decision making of the original method
    plot_decision = plot_decision +
      geom_line(aes(y = V_PUNT_og), color="darkred", size=1) +
      geom_line(aes(y = V_FG_og), color="goldenrod4", size=1) +
      geom_line(aes(y = V_GO_og), size=1, color="darkgreen")
  }
  plot_decision
  
  # plot_decision = plot_decision +
  #   annotation_custom(
  #     grobTree(textGrob("Right Decision Making", x=0.05,  y=0.955, hjust=0,
  #                       gp=gpar(col="black", fontsize=12,fontface="italic")))
  #   ) +
  #   annotation_custom(
  #     grobTree(textGrob( paste0("Previous ", "Decision Making"),  ##"Naive Decision Making",
  #                        x=0.05,  y=0.865, hjust=0, gp=gpar(col="black", fontsize=12,fontface="italic")))
  #   )
  # plot_decision
  # 
  # get_color <- function(x) {
  #   sapply(x, FUN = function(j) {
  #     case_when(
  #       j == "FG" ~ "orange2",
  #       j == "Go" ~ "seagreen3",
  #       j == "Punt" ~ "firebrick3",
  #       TRUE ~ "gray70"
  #     )
  #   })
  # }
  # 
  # ddf_iytg$decision_right_color = get_color(ddf_iytg$decision)
  # ddf_iytg$decision_prev_color = get_color1(ddf_iytg$decision_prev)
  # plot_decision = plot_decision + geom_segment(aes(x=yardline_100, xend=yardline_100, yend=y_max+1.5-0.3, y=y_max+1.5-0.6),
  #                                              color = ex1a$decision_right_color, size=2.5)
  # plot_decision = plot_decision + geom_segment(aes(x=yardline_100, xend=yardline_100, yend=y_max+1.5-1.1, y=y_max+1.5-1.4),
  #                                              color = ex1a$decision_prev_color, size=2.5)
  
  return(plot_decision)
}

# plot_Vs(ddf_i, ytg=4)

plot_V_vs_ydstogo <- function(ddf, ytg, ydl, SE=FALSE, include_og=FALSE, wp=FALSE) {
  ddf_iytg = ddf %>% filter(yardline_100 == ydl)
  
  plot_title = paste0(ydl, " yardline")
  
  breaks=seq(-7,7,by=1)
  ylab_ = "expected points"
  if (wp) {
    y_max = 1
    y_min = 0
    breaks = seq(-1,1,by=0.02)
    ylab_ = "win probability"
  } else if (SE) { ### bootstrapped standard errors
    y_max = max(c(ddf_iytg$V_fg_U, ddf_iytg$V_go_U, ddf_iytg$V_punt_U), na.rm=TRUE)+0.2
    y_min = min(c(ddf_iytg$V_fg_U, ddf_iytg$V_go_U, ddf_iytg$V_punt_U), na.rm=TRUE)
  } else {
    y_max = max(c(ddf_iytg$V_fg, ddf_iytg$V_go, ddf_iytg$V_punt), na.rm=TRUE)#+0.2
    y_min = min(c(ddf_iytg$V_fg, ddf_iytg$V_go, ddf_iytg$V_punt), na.rm=TRUE)
  }
  # max_wp= max(
  #   (ddf_iytg %>% filter(ydstogo==ytg))$V_go,
  #   (ddf_iytg %>% filter(ydstogo==ytg))$V_punt,
  #   (ddf_iytg %>% filter(ydstogo==ytg))$V_fg
  # )
  plot_decision = ddf_iytg %>%
    ggplot(aes(x=ydstogo)) +
    geom_line(aes(y = V_punt), color="firebrick", size=1) +
    geom_line(aes(y = V_fg), size=1, color="goldenrod2") +
    geom_line(aes(y = V_go), size=1, color="forestgreen") +
    # geom_point(x=ytg, y = max_wp, size=3, stroke=2, color="magenta", fill="white", shape=21) +
    geom_vline(xintercept=ytg, linetype="dotted") +
    labs(title=plot_title) +
    xlab("yards to go") + ylab(ylab_) +
    # scale_y_continuous(breaks=breaks,
    #                    # limits=c(y_min,y_max)
    # ) +
    scale_x_continuous(breaks=seq(1,10,by=1), limits=c(1,10)) 
  plot_decision
  
  if (SE) { ### bootstrapped standard errors
    plot_decision = plot_decision + 
      geom_line(aes(y = V_punt_L), linetype="dashed", color="firebrick") +
      geom_line(aes(y = V_punt_U), linetype="dashed", color="firebrick") +
      geom_line(aes(y = V_fg_L), linetype="dashed", color="goldenrod2") +
      geom_line(aes(y = V_fg_U), linetype="dashed", color="goldenrod2") +
      geom_line(aes(y = V_go_L), linetype="dashed", color="forestgreen") +
      geom_line(aes(y = V_go_U), linetype="dashed", color="forestgreen")
  } else {
    
  }
  
  if (include_og) { ### include decision making of the original method
    plot_decision = plot_decision +
      geom_line(aes(y = V_PUNT_og), color="darkred", size=1) +
      geom_line(aes(y = V_FG_og), color="goldenrod4", size=1) +
      geom_line(aes(y = V_GO_og), size=1, color="darkgreen")
  }
  plot_decision
  
  return(plot_decision)
}

plot_4th_down_lookahead <- function(ddf, ydl, SE=FALSE, boot_plot=FALSE, wp=FALSE, dec_conf_str=FALSE) {
  ytg_ = ifelse(ydl < 10, ydl, 10)
  ddf_lookahead = ddf %>% 
    # filter(ydstogo)
    filter(ydl-9 <= yardline_100 & yardline_100 <= ydl+5) %>%
    arrange(-yardline_100) %>%
    filter(ydl - ytg_ == yardline_100 - ydstogo)
  ddf_lookahead = ddf_lookahead %>% mutate(V_punt = ifelse(yardline_100 <= 30, NA, V_punt))
  ddf_lookahead
  
  if (SE) {
    ddf_lookahead = ddf_lookahead %>% mutate(V_punt_L = ifelse(yardline_100 <= 30, NA, V_punt_L))
    ddf_lookahead = ddf_lookahead %>% mutate(V_punt_U = ifelse(yardline_100 <= 30, NA, V_punt_U))
  }
  
  plot_title = paste0("4th Down Look-Ahead\n", "1st Down at ", ydl)
  
  if (boot_plot) {
    boot_perc_str = if (dec_conf_str) "decision confidence" else "bootstrap proportion"
    plot_decision = ddf_lookahead %>%
      ggplot(aes(x=ydstogo)) +
      geom_line(aes(y = boot_prop_punt, color="Punt"), size=1) +
      geom_line(aes(y = boot_prop_fg, color="FG"), size=1) +
      geom_line(aes(y = boot_prop_go, color="Go"), size=1) +
      scale_color_manual(name="", values=c("Punt"="firebrick", "Go"="forestgreen", "FG"="goldenrod2")) +
      labs(title=plot_title) +
      xlab("yards to go") + 
      # ylab("bootstrap proportion") +
      ylab(boot_perc_str) +
      scale_y_continuous(breaks=seq(0,1,by=0.1), limits=c(0,1)) +
      scale_x_continuous(
        breaks=seq(1,15,by=2),
        sec.axis = sec_axis(~.x+ydl-ytg_, breaks = seq(ydl-100,ydl+100,by=5), 
                            # name="yardline"
                            name = "yards to opponent endzone"
                            )
      ) 
    plot_decision
    return(plot_decision)
  }

  ####################
  breaks=seq(-7,7,by=1)
  ylab_ = "expected points"
  if (wp) {
    y_max = 1
    y_min = 0
    breaks = seq(-1,1,by=0.02)
    ylab_ = "win probability"
  } else if (SE) { ### bootstrapped standard errors
    y_max = max(c(ddf_lookahead$V_fg_U, ddf_lookahead$V_go_U, ddf_lookahead$V_punt_U), na.rm=TRUE)+0.2
    y_min = min(c(ddf_lookahead$V_fg_U, ddf_lookahead$V_go_U, ddf_lookahead$V_punt_U), na.rm=TRUE)
  } else {
    y_max = max(c(ddf_lookahead$V_fg, ddf_lookahead$V_go, ddf_lookahead$V_punt), na.rm=TRUE)#+0.2
    y_min = min(c(ddf_lookahead$V_fg, ddf_lookahead$V_go, ddf_lookahead$V_punt), na.rm=TRUE)
  }
  # browser()
  plot_decision = ddf_lookahead %>%
    ggplot(aes(x=ydstogo)) +
    geom_line(data=.%>%drop_na(V_punt), aes(y = V_punt, color="Punt"), size=1, na.rm = TRUE) +
    geom_line(aes(y = V_fg, color="FG"), size=1) +
    geom_line(aes(y = V_go, color="Go"), size=1) +
    scale_color_manual(name="", values=c("Punt"="firebrick", "Go"="forestgreen", "FG"="goldenrod2")) +
    labs(title=plot_title) +
    xlab("yards to go") + ylab(ylab_) +
    scale_x_continuous(
      breaks=seq(1,15,by=2),
      sec.axis = sec_axis(~.x+ydl-ytg_, breaks = seq(ydl-100,ydl+100,by=5), 
                          name = "yards to opponent endzone"
                          # name="yardline"
                          )
    ) 
  plot_decision
 
  if (SE) { ### bootstrapped standard errors
    plot_decision = plot_decision +
      geom_line(data=.%>%drop_na(V_punt), aes(y = V_punt_L), linetype="dashed", color="firebrick") +
      geom_line(data=.%>%drop_na(V_punt), aes(y = V_punt_U), linetype="dashed", color="firebrick") +
      geom_line(aes(y = V_fg_L), linetype="dashed", color="goldenrod2") +
      geom_line(aes(y = V_fg_U), linetype="dashed", color="goldenrod2") +
      geom_line(aes(y = V_go_L), linetype="dashed", color="forestgreen") +
      geom_line(aes(y = V_go_U), linetype="dashed", color="forestgreen")
  }
  plot_decision
  
  return(plot_decision)
}

################
### decision ###
################

get_decision <- function(ydl, ytg, decision_df, include_uncertainty=FALSE) {
  df_ = decision_df %>% filter(yardline_100 == ydl & ydstogo == ytg)
  result_ = list(decision = df_$decision)
  if (include_uncertainty) {
    result_$decision_intensity = df_$decision_intensity
    result_$decision_confidence = df_$prop_decision
    result_$decision_V_gain_L = df_$V_gain_L
    result_$decision_V_gain_U = df_$V_gain_U
  } else {
    result_$decision_intensity = df_$decision_intensity
  }
  return(result_)
}

plot_gt_4th_down_summary <- function(play_df, ddf, decision_df=NULL, SE=FALSE, wp=TRUE, dec_conf_str=FALSE) {
  a1 = paste0(
    if (play_df$score_differential == 0) "Tied" else {
      paste0(abs(play_df$score_differential), " points ", if (play_df$score_differential < 0) "down" else "up")
    },
    ", 4th & ", play_df$ydstogo, ", ", play_df$yardline_100, " yards to opponent endzone"
  )
  a1
  
  play_df$qtr
  qtr_sec_rem = play_df$game_seconds_remaining - (4-play_df$qtr)*900
  qtr_min_rem = floor(qtr_sec_rem / 60)
  qtr_sec_str_rem = qtr_sec_rem %% 60
  qtr_sec_str_rem = if (nchar(qtr_sec_str_rem) == 1) paste0("0", qtr_sec_str_rem) else qtr_sec_str_rem
  a2 = paste0(
    "Qtr ", play_df$qtr, ", ", qtr_min_rem, ":", qtr_sec_str_rem, " | ", 
    "Timeouts: Off ", play_df$posteam_timeouts_remaining, ", Def ", play_df$defteam_timeouts_remaining, " | ",
    "Point Spread: ", play_df$posteam_spread
  )
  a2
  
  ddf_i_i = ddf %>% filter(yardline_100==play_df$yardline_100 & ydstogo==play_df$ydstogo)
  a3 = tibble("decision" = c("Go for it", "Field goal", "Punt"))
  wpstr = if (wp) "WP" else "EP"
  
  ### numeric values in the chart
  # a3[[wpstr]] = c(percent_me(ddf_i_i$V_go), percent_me(ddf_i_i$V_fg), percent_me(ddf_i_i$V_punt))
  a3[[wpstr]] = c(ddf_i_i$V_go, ddf_i_i$V_fg, ddf_i_i$V_punt)
  # boot_perc_str = "boot %"
  boot_perc_str = if (dec_conf_str) "decision\nconfidence" else "boot %"
  if (SE) {
    a3[[boot_perc_str]] = c(ddf_i_i$boot_prop_go, ddf_i_i$boot_prop_fg, ddf_i_i$boot_prop_punt)
  }
  a3[["success prob"]] = c(ddf_i_i$p_convert, ddf_i_i$p_fg, NA)
  a3[[paste0(wpstr, " if fail")]] = c(ddf_i_i$v_dont_convert, ddf_i_i$v_miss_fg, NA)
  a3[[paste0(wpstr, " if succeed")]] = c(ddf_i_i$v_convert, ddf_i_i$v_make_fg, NA)
  a3[[paste0("SD of ", wpstr)]] = c(ddf_i_i$sd_v_convert, ddf_i_i$sd_v_fg, NA)
  a3[["baseline coach %"]] = c(ddf_i_i$C_Go, ddf_i_i$C_FG, ddf_i_i$C_Punt)
  # a3 = if (play_df$yardline_100 <= 40) a3 %>% filter(decision != "Punt") else a3
  # a3 = if (play_df$yardline_100 >= 51) a3 %>% filter(decision != "Field goal") else a3
  a3 = a3 %>% round_df(digits=3)
  a3 = a3 %>% arrange(-!!sym(wpstr))
  
  ### if WP, turn to percentages; do the colored columns LATER
  percent_on = TRUE
  percent_me = function(x) if (percent_on & wp) percent(x, digits=1) else x
  a3[["success prob"]] = percent_me(a3[["success prob"]])
  a3[[paste0(wpstr, " if fail")]] = percent_me(a3[[paste0(wpstr, " if fail")]])
  a3[[paste0(wpstr, " if succeed")]] = percent_me(a3[[paste0(wpstr, " if succeed")]])
  a3[[paste0("SD of ", wpstr)]] = percent_me(a3[[paste0("SD of ", wpstr)]])
  a3[["baseline coach %"]] = percent_me(a3[["baseline coach %"]])
  
  ### CI in WP gain
  if (SE) {
    # v_gain_ci_str = paste0(
    #   "[", round(decision_df$decision_wp_L*100,3), "%, ", round(decision_df$decision_wp_U*100,3), "%]"
    # )
    v_gain_ci_L = round(decision_df$decision_V_gain_L, 5)
    v_gain_ci_U = round(decision_df$decision_V_gain_U, 5)
    v_gain_ci_L = percent_me(v_gain_ci_L)
    v_gain_ci_U = percent_me(v_gain_ci_U)
    v_gain_ci_str = paste0(
      "[", v_gain_ci_L, ", ", v_gain_ci_U, "]"
    )
    a3[[paste0("90% ", wpstr, " gain CI")]] = c(v_gain_ci_str, rep("", nrow(a3)-1))
    a3 = a3 %>% relocate(!!paste0("90% ", wpstr, " gain CI"), .before=boot_perc_str)
  }
  a3
  
  
  # output$decision_wp_CI = renderText({ 
  #   paste0("decision adds wp CI: [", paste0(round(decision_wp$decision_wp_L*100,3), "%, ", round(decision_wp$decision_wp_U*100,3), "%]", " win probility") )
  # })
  
  library(gt)
  library(paletteer)
  
  aaa = gt(a3) %>% tab_header(
    title=a1,
    subtitle=a2
  ) %>% data_color(
    columns = c(wpstr),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = c("ggsci::blue_material")
      ) %>% as.character(),
      domain = range(a3[[wpstr]]) + c(-0.01, 0.01)
      # domain = NULL
      # domain = if (wp) c(0,1) else NULL
    )
  ) 
  if (SE) {
    aaa = aaa %>%
      data_color(
        columns = c(boot_perc_str),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(
            palette = c("ggsci::orange_material")
          ) %>% as.character(),
          # domain = NULL
          domain = c(0,1)
        )
      )
  }
  
  ### percent_me the Colored Columns
  aaa[["_data"]][[wpstr]] = percent_me(aaa[["_data"]][[wpstr]])
  if (SE) {
    aaa[["_data"]][[boot_perc_str]] = percent_me(aaa[["_data"]][[boot_perc_str]])
  }
  
  # browser() 
  ### final table formatting
  aaa = aaa %>% tab_options(
    table.width = pct(100)
  ) %>% sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text=""
  )
  
  return(aaa)
}

plot_gt_post_TD_summary <- function(play_df, ddf, decision_df=NULL, SE=FALSE, wp=TRUE, dec_conf_str=FALSE) {
  
  a1 = paste0(
    "Just scored a TD. ",
    if (play_df$score_differential < 0) "Down " else if (play_df$score_differential > 0) "Up " else "Tied Up", 
    if (play_df$score_differential!=0) abs(play_df$score_differential), 
    "."
  )
  a1
  
  play_df$qtr
  qtr_sec_rem = play_df$game_seconds_remaining - (4-play_df$qtr)*900
  qtr_min_rem = floor(qtr_sec_rem / 60)
  qtr_sec_str_rem = qtr_sec_rem %% 60
  qtr_sec_str_rem = if (nchar(qtr_sec_str_rem) == 1) paste0("0", qtr_sec_str_rem) else qtr_sec_str_rem
  a2 = paste0(
    "Qtr ", play_df$qtr, ", ", qtr_min_rem, ":", qtr_sec_str_rem, " | ", 
    "Timeouts: Off ", play_df$posteam_timeouts_remaining, ", Def ", play_df$defteam_timeouts_remaining, " | ",
    "Point Spread: ", play_df$posteam_spread
  )
  a2
  
  a3 = tibble("decision" = c("Extra Point Attempt", "2 Point Conversion Attempt"))
  wpstr = if (wp) "WP" else "EP"
  
  ### numeric values in the chart
  # a3[[wpstr]] = c(percent_me(ddf_i_i$V_go), percent_me(ddf_i_i$V_fg), percent_me(ddf_i_i$V_punt))
  a3[[wpstr]] = c(ddf$V_xp, ddf$V_2pc)
  # boot_perc_str = "boot %"
  boot_perc_str = if (dec_conf_str) "decision\nconfidence" else "boot %"
  if (SE) {
    a3[[boot_perc_str]] = c(ddf$boot_prop_xp, ddf$boot_prop_2pc)
  }
  a3[["success prob"]] = c(ddf$p_xp, ddf$p_convert)
  a3[[paste0(wpstr, " if fail")]] = c(ddf$v_miss_xp, ddf$v_dont_convert)
  a3[[paste0(wpstr, " if succeed")]] = c(ddf$v_make_xp, ddf$v_convert)
  a3[[paste0("SD of ", wpstr)]] = c( ddf$sd_v_xp, ddf$sd_v_convert)
  a3 = a3 %>% round_df(digits=3)
  a3 = a3 %>% arrange(-!!sym(wpstr))
  
  ### if WP, turn to percentages; do the colored columns LATER
  percent_on = TRUE
  percent_me = function(x) if (percent_on & wp) percent(x, accuracy=0.1) else x
  a3[["success prob"]] = percent_me(a3[["success prob"]])
  a3[[paste0(wpstr, " if fail")]] = percent_me(a3[[paste0(wpstr, " if fail")]])
  a3[[paste0(wpstr, " if succeed")]] = percent_me(a3[[paste0(wpstr, " if succeed")]])
  a3[[paste0("SD of ", wpstr)]] = percent_me(a3[[paste0("SD of ", wpstr)]])

  ### CI in WP gain
  if (SE) {
    v_gain_ci_L = round(decision_df$decision_V_gain_L, 5)
    v_gain_ci_U = round(decision_df$decision_V_gain_U, 5)
    v_gain_ci_L = percent_me(v_gain_ci_L)
    v_gain_ci_U = percent_me(v_gain_ci_U)
    v_gain_ci_str = paste0(
      "[", v_gain_ci_L, ", ", v_gain_ci_U, "]"
    )
    a3[[paste0("90% ", wpstr, " gain CI")]] = c(v_gain_ci_str, rep("", nrow(a3)-1))
    a3 = a3 %>% relocate(!!paste0("90% ", wpstr, " gain CI"), .before=boot_perc_str)
  }
  a3
  
  library(gt)
  library(paletteer)
  
  aaa = gt(a3) %>% tab_header(
    title=a1,
    subtitle=a2
  ) %>% data_color(
    columns = c(wpstr),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = c("ggsci::blue_material")
      ) %>% as.character(),
      domain = range(a3[[wpstr]]) + c(-0.01, 0.01)
      # domain = NULL
      # domain = if (wp) c(0,1) else NULL
    )
  ) 
  if (SE) {
    aaa = aaa %>%
      data_color(
        columns = c(boot_perc_str),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(
            palette = c("ggsci::orange_material")
          ) %>% as.character(),
          # domain = NULL
          domain = c(0,1)
        )
      )
  }
  
  ### percent_me the Colored Columns
  aaa[["_data"]][[wpstr]] = percent_me(aaa[["_data"]][[wpstr]])
  if (SE) {
    aaa[["_data"]][[boot_perc_str]] = percent_me(aaa[["_data"]][[boot_perc_str]])
  }
  
  ### final table formatting
  aaa = aaa %>% tab_options(
    table.width = pct(85)
  ) %>% sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text=""
  )
  
  return(aaa)
}

get_post_TD_decision <- function(decision_df, include_uncertainty=FALSE) {
  df_ = decision_df 
  result_ = list(decision = df_$decision)
  if (include_uncertainty) {
    result_$decision_intensity = df_$decision_intensity
    result_$decision_confidence = df_$prop_decision
    result_$decision_V_gain_L = df_$V_gain_L
    result_$decision_V_gain_U = df_$V_gain_U
  } else {
    result_$decision_intensity = df_$decision_intensity
  }
  return(result_)
}

plot_4th_down_boot_dist <- function(play_df, ddf, wp=TRUE) {
  ddf_i_i = ddf %>% filter(yardline_100==play_df$yardline_100 & ydstogo==play_df$ydstogo)
  wpstr = if (wp) "WP" else "EP"
  
  ### bootstrapped distribution of the effect size (decision intensity)
  # V_title = paste0(" ", if (wp) "win probability" else "expected points", " added\nby making that decision")
  V_title = paste0("estimated ", if (wp) "win probability" else "expected points", " added ")
  effect_size_i_i = (ddf_i_i %>% filter(b==1))$V_gain
  # browser()
  
  ddf_i_i %>%
    ggplot(aes(x=V_gain)) +
    geom_histogram(bins=50, fill = "black") +
    geom_vline(xintercept = 0, linetype="dashed", color="gray60", linewidth=2) +
    geom_vline(xintercept = effect_size_i_i, linetype="solid", color="dodgerblue2", linewidth=2) +
    xlab(V_title) +
    theme(
      axis.title = element_text(size=27.5),
      axis.text = element_text(size=20),
      legend.title = element_text(size=27.5),
      legend.text = element_text(size=20),
      plot.margin = unit(c(0.05, 0.25, 0.05, 0.05),"inches")
    ) 
}


