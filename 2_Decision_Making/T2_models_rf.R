
########################
### Helper Functions ###
########################

train_rf <- function(rf_features, train_set, params, wp=TRUE, reg=TRUE, w=FALSE, catalytic=FALSE) {
  if (catalytic) {
    ### get catalytic train set.  Catalytic params (M,tau) are included in `params`
    train_set = get_catalytic_set(params$M, params$tau, train_set)
    w = TRUE
  }
  
  if (!w) { # unweighted
    train_set$w = 1
  } 
  
  if (reg) { # random forest REGRESSION
    if (wp) {
      y = train_set$label_win
    } else { # EP
      y = train_set$pts_next_score
    }
  } else { # random forest CLASSIFICATION
    if (wp) {
      y = factor(train_set$label_win)
    } else { # EP
      y = factor(train_set$label)
    }
  }
  
  # Fit the Random Forest
  set.seed(120)  # Setting seed
  rf <- randomForest(
    x = train_set %>% select(all_of(rf_features)),
    y = y,
    weights = train_set$w,
    #####xtest = xtest, ytest = ytest,
    mtry = params$mtry,
    nodesize = params$nodesize,
    ntree = params$ntree,
    do.trace=10, 
    keep.forest=TRUE,
  )
  return(rf)
}

predict_probs_rf <-  function(rf, test_set, rf_features) {
  rf_pred = predict(rf, newdata = test_set %>% select(all_of(rf_features)), type='prob')
  colnames(rf_pred) = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
  rf_pred
}

predict_ep_rf <- function(rf, test_set, rf_features, model_name) {
  pred_cg = predict_probs_rf(rf, test_set, rf_features)
  pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(-7) + pred_cg[,3]*(3) + pred_cg[,4]*(-3) + pred_cg[,5]*(2) + pred_cg[,6]*(-2) + pred_cg[,7]*(0)
  return( tibble(pred = pred_ep,  model=model_name) )
}

##########################################
### Random Forest Regression EP Models ###
##########################################

#################################
rf_R_wp_LockNettleton_model_name = "rf_R_wp_LockNettleton"
rf_R_wp_LockNettleton_features = c(
  "down", "score_differential", "game_seconds_remaining",
  "AdjustedScore_LN", "total_score",
  "posteam_spread", "posteam_timeouts_remaining",  "defteam_timeouts_remaining",  
  "yardline_100",  "ydstogo" #####,"era_A",
)
rf_R_wp_LockNettleton_params = list(ntree = 500, mtry = 2, nodesize=200) # determined from the paper (LN 2014)




