
##############
### Models ###
##############

### BEST VALUE OF A FIRST DOWN MODEL
fit_V1_model_best <- function(model_name, model_type, dataset) {
  if (model_type == "MLR") {
    model_fit = clean_lm(get(paste0("fit_", model_name))(dataset))
  } else if (model_type == "XGB") {
    model_fit = train_xgb(
      xgb_features = get(paste0(model_name, "_features")), 
      train_set = dataset, 
      params = get(paste0(model_name, "_params")), 
      nrounds = get(paste0(model_name, "_nrounds")),  
      catalytic= get(paste0(model_name, "_catalytic")), 
      Regression=str_detect(model_name, "_R_"), 
      BoundedRegression=str_detect(model_name, "_BR_"), 
      wp=str_detect(model_name, "_wp_"), 
      catalytic_model_name = if (str_detect(model_name, "catalytic")) get(paste0(model_name, "_prior_name")) else FALSE,
      weight_by_epoch=str_detect(model_name, "_wbe_"), 
      weight_by_game=str_detect(model_name, "_wbg_"), 
    )
  } else {
    stop("need to implement...")
  }
  return(model_fit)
}

### BEST FIELD GOAL PROBABILITY MODEL
fit_fgp_model_best <- function(fg_data) {
  ### impute fake missed field goals for yardlines beyond 50 (which has never been made before)
  fg_data_1 = fg_data %>% bind_rows(tibble(
    fg_made = 0, kq_0_sum_std = 0,
    yardline_100 = sample(51:99, size=500, replace=TRUE)
  ))
  
  fit = glm(fg_made ~  bs(yardline_100, df=5) + kq_0_sum_std, data = fg_data_1, family="binomial")
  clean_lm(fit)
}

### BEST PUNT EXPECTED NEXT YARDLINE MODEL
fit_punt_eny_model_best <- function(punt_data) {
  fit = lm(next_ydl ~ bs(yardline_100, df=4) + pq_0_sum_std + pq_0_sum_std:yardline_100, data = punt_data)
  clean_lm(fit)
}

### BEST CONVERSION PROBABILITY LOGISTIC REGRESSION MODEL
fit_convp_model_best <- function(go_dataset) {
  fit = glm(convert ~ 
              (down==4):bs(log(ydstogo+1),4,intercept = FALSE) +
              qbq_ot_0_sum + oq_rot_0_total_sum + dq_dt_0_againstPass_sum + dq_dt_0_againstRun_sum
            ,data=go_dataset, family="binomial") 
  clean_lm(fit)
}

### CONVERSION EXPECTED OUTCOME GIVEN SUCCESSFUL CONVERSION
fit_go_exp_outcome_model_best <- function(go_dataset, success=TRUE) {
  if (success) {
    go_dataset_filtered = go_dataset %>% filter(convert==1)
  } else {
    go_dataset_filtered = go_dataset %>% filter(convert==0)
  }
  if (success) {
    fit = lm(yards_gained ~
               (down==4):bs(log(ydstogo),4,intercept = FALSE) +
               as.numeric(ydstogo==1):bs(yardline_100, df=3) +
               as.numeric(ydstogo!=1):bs(yardline_100, df=4,intercept = FALSE) +
               qbq_ot_0_sum + oq_rot_0_total_sum + dq_dt_0_againstPass_sum + dq_dt_0_againstRun_sum
             ,data = go_dataset_filtered)
  } else {
    fit = lm(yards_gained ~
               (down==4):log(ydstogo+1) +
               qbq_ot_0_sum + oq_rot_0_total_sum + dq_dt_0_againstPass_sum + dq_dt_0_againstRun_sum
             ,data = go_dataset_filtered)
  }
  fit
  clean_lm(fit)
}

### COACHS' BASELINE DECISION MODEL
fit_coach_model_best <- function(fourth_down_dataset) {
  fit_xgb_coach(fourth_down_dataset, params_xgb_coach) 
}

###########################
### Bootstrap Functions ###
###########################

get_randomized_clustered_bootstrap_dataset <- function(dataset, phi) {
  ### randomized cluster bootstrap
  
  ### sample clusters (GAME or EPOCH, given by `group_name`) with replacement
  all_group_ids = sort(unique(dataset[["game_id"]]))
  num_resample_cb = round(length(all_group_ids)*phi)
  group_ids_boot = tibble(
    g = sort(sample(all_group_ids, size=num_resample_cb, replace=TRUE))
  ) %>% mutate(ii = 1:n()) 
  group_ids_boot[["game_id"]] = group_ids_boot$g
  group_ids_boot = group_ids_boot %>% select(-g)
  df_cb = left_join(group_ids_boot, dataset)
  
  ### within each cluster (GAME or EPOCH), sample DRIVES with replacement
  df_rcb = df_cb %>% select(game_id, ii, drive_id) %>% distinct()
  df_rcb_1 = 
    df_rcb %>%
    group_by(ii) %>%
    sample_n(size = n(), replace = TRUE) %>%
    arrange(game_id,ii,drive_id) %>%
    ungroup() 
  df_rcb_2 = df_rcb_1 %>% left_join(dataset)
  
  # ### within each cluster (GAME or EPOCH), sample PLAYS (ROWS) with replacement
  # df_rcb = df_cb %>% select(all_of(group_name), ii, row_idx)
  # df_rcb_1 =
  #   df_rcb %>%
  #   group_by(ii) %>%
  #   sample_n(size = n(), replace = TRUE) %>%
  #   arrange(all_of(group_name),ii,row_idx) %>%
  #   ungroup() 
  # df_rcb_2 = df_rcb_1 %>% left_join(dataset)
  
  return(df_rcb_2)
}

get_iid_bootstrap_dataset <- function(dataset) {
  row_idxs = 1:nrow(dataset)
  resampled_idxs = sort(sample(row_idxs, size=nrow(dataset), replace=TRUE))
  dataset_boot = dataset[resampled_idxs, ]
  dataset_boot
}


