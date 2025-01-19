
########################
### Helper Functions ###
########################

map_n77_to_01 <- function(x) {
  ### input vector x includes points of the next score, with values from -7 to 7
  (x + 7)/14
}

map_01_to_n77 <- function(y) {
  ### input vector yincludes points of the next score, already mapped into 0 to 1
  14*y - 7
}

get_xgb_train_DMatrix <- function(xgb_features, train_set, params, w=FALSE, catalytic=FALSE, 
                                  Regression=FALSE, BoundedRegression=FALSE, wp=FALSE, catalytic_model_name="") {
  
  ### add fake catalytic data to xgboost
  if (catalytic) {
    ### get catalytic train set.  Catalytic params (M,tau) are included in `params`
    if (catalytic_model_name == "") { stop("FIXME:", "should specify the catalytic model in train_xgb") }
    catalytic_model_type = case_when(
      str_detect(catalytic_model_name, "mlr") ~ "MLR",
      str_detect(catalytic_model_name, "gam") ~ "GAM",
      str_detect(catalytic_model_name, "lm") ~ "OLS",
    )
    catalytic_outcome = if (BoundedRegression | Regression | wp) "numeric" else "categorical"
    train_set = get_catalytic_set(params$M, params$tau, train_set, catalytic_model_type, 
                                  catalytic_model_name, catalytic_outcome, params$U, params$MU)
    params = within(params, rm(M))
    params = within(params, rm(tau))
    w = TRUE ### catalytic xgboost must be weighted
  }
  
  ### row-weights for xgboost
  if (!w) {
    xgb_weights = rep(1, nrow(train_set))
  } else {
    xgb_weights = train_set$w
  }
  
  ### create response column (label) for xgboost
  if (Regression) { ### numeric expected points 
    train_labels_xgb = train_set$pts_next_score
  } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
    train_labels_xgb = map_n77_to_01( train_set$pts_next_score )
  }
  else if (wp) { ### win probability {0,1}
    train_labels_xgb = train_set$label_win
  } else { ### categorical expected points {-7,-3,...,3,7}
    train_labels_xgb = train_set$label
  }
  
  # browser()
  
  ### get the xgboost training matrix
  train_features_xgb = train_set %>% select(all_of(xgb_features))
  train_set_xgbDM = xgboost::xgb.DMatrix(
    model.matrix(~ . + 0, data = train_features_xgb), 
    label = train_labels_xgb,
    weight = xgb_weights,
  )
  
  return(train_set_xgbDM)
}

train_xgb <- function(xgb_features, train_set, params, nrounds, watchSet=FALSE, 
                      w=FALSE, catalytic=FALSE, Regression=FALSE, BoundedRegression=FALSE, wp=FALSE, catalytic_model_name="", 
                      param_tuning=FALSE, print_every_n=50, weight_by_epoch=FALSE, weight_by_game=FALSE) {
  
  if (weight_by_epoch) {
    train_set = train_set %>% group_by(epoch) %>% mutate(w = 1/n()) %>% ungroup()
    w = TRUE
  } else if (weight_by_game) {
    train_set = train_set %>% group_by(game_id) %>% mutate(w = 1/n()) %>% ungroup()
    w = TRUE
  } else {
    train_set = train_set %>% mutate(w = 1) 
  }
  
  train_set_xgbDM =  get_xgb_train_DMatrix(
    xgb_features, train_set, params, w=w, catalytic=catalytic, 
    Regression=Regression, BoundedRegression=BoundedRegression, wp=wp, catalytic_model_name=catalytic_model_name
  ) 
    
  if (is.data.frame(watchSet)) { ### validation set evaluation
    val_features_xgb = watchSet %>% select(all_of(xgb_features))
    
    ### create watchlist (val set) response column (label) for xgboost
    if (Regression) { ### numeric expected points 
      val_labels_xgb = watchSet$pts_next_score
    } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
      val_labels_xgb = map_n77_to_01( watchSet$pts_next_score )
    }
    else if (wp) { ### win probability {0,1}
      val_labels_xgb = watchSet$label_win
    } else { ### categorical expected points {-7,-3,...,3,7}
      val_labels_xgb = watchSet$label
    }
    
    val_set_xgbDM = xgboost::xgb.DMatrix(
      model.matrix(~ . + 0, data = val_features_xgb),
      label = val_labels_xgb
    )
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  } else { ### just train set for watchlist
    watchlist <- list(train=train_set_xgbDM)
  }
  
  set.seed(34362649) #########################
  if (param_tuning) { ### if tuning parameters
    xgb <- xgb.train( 
      data = train_set_xgbDM, 
      watchlist = watchlist,
      params = params, 
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = print_every_n,
      verbose = 2
    )
  } else {
    xgb <- xgb.train( 
      data = train_set_xgbDM, 
      watchlist = watchlist,
      params = params, 
      nrounds = nrounds, 
      print_every_n = print_every_n,
      verbose = 2
    )
  }
  return(xgb)
}

predict_probs_xgb <-  function(xgb, test_set, xgb_features, wp=FALSE) {
  test_features_xgb =  test_set %>% select(all_of(xgb_features))
  test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
  xgb_pred = predict(xgb, test_set_xgbDM)
  if (!wp) { ### expected points model
    pred_matrix = matrix(xgb_pred, ncol=7, nrow=length(xgb_pred)/7, byrow=TRUE)
    colnames(pred_matrix) = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
    return(pred_matrix)
  } else { ### win probability model
    return(xgb_pred)
  }
}

predict_ep_xgb <- function(xgb, test_set, xgb_features, model_name, Regression=FALSE, BoundedRegression=FALSE) {
  ### get expected points predictions
  if (Regression) { ### numeric expected points 
    test_features_xgb =  test_set %>% select(all_of(xgb_features))
    test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
    pred_ep = predict(xgb, test_set_xgbDM)
  } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
    test_features_xgb =  test_set %>% select(all_of(xgb_features))
    test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
    pred_ep = map_01_to_n77(predict(xgb, test_set_xgbDM))
  } else { ### categorical expected points {-7,-3,...,3,7}
    pred_cg = predict_probs_xgb(xgb, test_set, xgb_features)
    pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(-7) + pred_cg[,3]*(3) + pred_cg[,4]*(-3) + pred_cg[,5]*(2) + pred_cg[,6]*(-2) + pred_cg[,7]*(0)
  }
  return( tibble(pred = pred_ep,  model=model_name) )
}

load_params <- function(target_model_name) {
  params = list.load(paste0("param_tuning_results_FINAL/", target_model_name, ".yaml"))
  nrounds = params$nrounds
  catalytic = params$CATALYTIC
  suppressWarnings({ params = within(params, rm(target_model_name)) })
  suppressWarnings({ params = within(params, rm(model_name)) })
  suppressWarnings({ params = within(params, rm(CATALYTIC)) })
  suppressWarnings({ params = within(params, rm(phi)) })
  suppressWarnings({ params = within(params, rm(test_loss)) })
  suppressWarnings({ params = within(params, rm(nrounds)) })
  params = lapply(params, function(x) { ifelse(x == "NA", NA, x) }) # replace "NA" with NA
  return(list(params, nrounds, catalytic))
}

######################################################################
######################################################################
######################################################################

#########################################################
### XGBoost (Monotonic) WP Models, fit on First Downs ###
#########################################################

##############################
xgb_wp_110_7_model_name = "xgb_wp_110_7"
xgb_wp_110_7_features = c(
  "score_differential", "game_seconds_remaining", "posteam_spread", "yardline_100",
  "scoreTimeRatio", # "AdjustedScore_LN", 
  "total_score", "receive_2h_ko", "posteam_timeouts_remaining",  "defteam_timeouts_remaining"
)
xgb_wp_110_7_monotonicities = c(1,0,-1,-1,1,0,0,1,-1)
xgb_wp_110_7_params = load_params(xgb_wp_110_7_model_name)[[1]]
xgb_wp_110_7_nrounds = load_params(xgb_wp_110_7_model_name)[[2]]
xgb_wp_110_7_catalytic = load_params(xgb_wp_110_7_model_name)[[3]]

#######################################################
### XGBoost (Monotonic) WP Models, fit on All Downs ###
#######################################################

##############################
xgb_wp_1_model_name = "xgb_wp_1"
xgb_wp_1_features = c("score_differential", "game_seconds_remaining", "yardline_100", "down", "ydstogo", "posteam_spread", "scoreTimeRatio")
xgb_wp_1_monotonicities = c(1,0,-1,-1,-1,-1,1)
xgb_wp_1_params = load_params(xgb_wp_1_model_name)[[1]]
xgb_wp_1_nrounds = load_params(xgb_wp_1_model_name)[[2]]
xgb_wp_1_catalytic = load_params(xgb_wp_1_model_name)[[3]]

#######################################################
### Baldwin nflFastR: XGBoost (Monotonic) WP Models ###
#######################################################

##############################
xgb_wp_Baldwin_1_model_name = "xgb_wp_Baldwin_1"
xgb_wp_Baldwin_1_features = c(
  "receive_2h_ko", "spread_time", "home", "half_seconds_remaining", "game_seconds_remaining",
  "Diff_Time_Ratio", "score_differential", "down", "ydstogo", "yardline_100", "posteam_timeouts_remaining", "defteam_timeouts_remaining"
)
xgb_wp_Baldwin_1_monotonicities = c(0, -1, 0, 0, 0,  1, 1, -1, -1, -1, 1, -1)
xgb_wp_Baldwin_1_params = load_params(xgb_wp_Baldwin_1_model_name)[[1]]
xgb_wp_Baldwin_1_nrounds = load_params(xgb_wp_Baldwin_1_model_name)[[2]]
xgb_wp_Baldwin_1_catalytic = load_params(xgb_wp_Baldwin_1_model_name)[[3]]

# ##############################
# ### https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
# xgb_wp_Baldwin_model_name = "xgb_wp_Baldwin"
# xgb_wp_Baldwin_features = c(
#   "receive_2h_ko", "spread_time", "home", "half_seconds_remaining", "game_seconds_remaining",
#   "Diff_Time_Ratio", "score_differential", "down", "ydstogo", "yardline_100", "posteam_timeouts_remaining", "defteam_timeouts_remaining"
# )
# xgb_wp_Baldwin_monotonicities = c(0, -1, 0, 0, 0,  1, 1, -1, -1, -1, 1, -1) ### spread is back to the right sign in our dataset
# # determined by Baldwin's parameter tuning
# xgb_wp_Baldwin_params <-  list(booster = "gbtree",
#                                objective = "binary:logistic",
#                                eval_metric = c("logloss"),
#                                ###########################
#                                eta = 0.03336242,
#                                gamma = 0.001755908,
#                                max_depth = 7,
#                                subsample = 0.5283268,
#                                colsample_bytree = 0.5,
#                                min_child_weight = 4,
#                                monotone_constraints = xgb_wp_Baldwin_monotonicities
# )
# xgb_wp_Baldwin_nrounds = 588
# xgb_wp_Baldwin_catalytic = FALSE


