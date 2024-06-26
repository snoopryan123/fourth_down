
args <- commandArgs(trailingOnly = TRUE)
m = as.numeric(args[1]) ### sim index
source("sim_bias_variance_main.R")

#######################################################
### fit the XGBoost models for each parameter combo ###
#######################################################

for (i in 1:nrow(df_bvsim)) {
  N = df_bvsim$N[i]
  K = df_bvsim$K[i]
  G = df_bvsim$G[i]
  zeta = df_bvsim$zeta[i]
  bvsimidx = df_bvsim$bv_sim_idx[i]
  
  sim_str = get_param_combo_str(zeta,G,N,K,m)
  print(sim_str)
  
  ### generate training dataset
  set.seed(23748 + m*143)
  df_train = simulate_football_season(G,N,K)
  print(df_train)
  
  ### hold-out half of the games in the training dataset for parameter tuning validation
  all_game_idxs = 1:G
  set.seed(9375689 + m*2947)
  val_game_idxs = sort(sample(all_game_idxs, size=round(G*0.5), replace=FALSE))
  train_game_idxs = setdiff(all_game_idxs, val_game_idxs)
  train_game_idxs_OG = all_game_idxs
  val_df = df_train %>% filter(g %in% val_game_idxs)
  train_df = df_train %>% filter(g %in% train_game_idxs)
  train_df_OG = df_train
  
  ### tune XGBoost & load XGBoost tuned parameters 
  params_filename = paste0("xgb_params/", "xgb_params_", sim_str, ".yaml")
  if (RETUNE_XGB | !file.exists(params_filename)) {
    tune_xgboost(train_df, val_df, params_filename)
  } 
  params = list.load(params_filename)
  
  ### fit XGBoost
  xgb_fit = fit_xgb(params, train_df_OG, nrounds=params$nrounds) 
  
  ### save the XGBoost model
  xgb_filename = paste0("xgb_models/", "xgb_", sim_str, ".xgb")
  xgb.save(xgb_fit, xgb_filename) 
}

