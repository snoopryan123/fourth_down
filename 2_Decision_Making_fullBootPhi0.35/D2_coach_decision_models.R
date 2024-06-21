
######################
### XGBoost Models ###
######################

### xgboost model
xgb_coach_features = c("yardline_100", "ydstogo", "score_differential", "game_seconds_remaining", "posteam_spread", "era_A")

get_xgb_coach_xgbDmat <- function(df, w=FALSE, fake_data=NULL) {
  if (!is.null(fake_data)) {
    ### combine fake data with real data
    df1 = df %>% select(all_of(xgb_coach_features), w, fgp)
    fake_data
    df2 = bind_rows(df1, fake_data)
  } else {
    df2 = df
  }
  if ("label_decision_actual" %in% colnames(df2)) {
    xgbdm = xgb.DMatrix(
      model.matrix(~ . + 0, data = df2 %>% select(all_of(xgb_coach_features))), 
      label = df2$label_decision_actual,
      # label = rep(c(0,1,2), nrow(df2))[1:nrow(df2)],
      weight = if (w) df2$w else rep(1,nrow(df2))
    )
  } else {
    xgbdm = xgb.DMatrix(
      model.matrix(~ . + 0, data = df2 %>% select(all_of(xgb_coach_features))), 
      weight = if (w) df2$w else rep(1,nrow(df2))
    )
  }
  return(xgbdm)
}

fit_xgb_coach <- function(data_train, params, data_test=NULL, w=FALSE, param_tuning=FALSE, catalytic=FALSE) {

  if (catalytic) {
    w = TRUE
    data_train$w = rep(1, nrow(data_train))
    ### cat params
    M = params$M
    tau = params$tau 
    ### fake data
    fake_data = generate_fake_data_coach(M, tau, data_train)
    ### 
    xgb_coach_trainMat = get_xgb_coach_xgbDmat(data_train, w=w, fake_data=fake_data)
    params = within(params, rm(M))
    params = within(params, rm(tau))
  } else {
    xgb_coach_trainMat = get_xgb_coach_xgbDmat(data_train, w=w)
  }

  if (!is.null(data_test)) {
    xgb_coach_testMat = get_xgb_coach_xgbDmat(data_test)
    watchlist = list(train=xgb_coach_trainMat, test=xgb_coach_testMat)
  } else  {
    watchlist = list(train=xgb_coach_trainMat)
  }
  
  if (!param_tuning) {
    nrounds = params$nrounds
    params = within(params, rm(nrounds))
    xgb_coach_model <- xgb.train( 
      data = xgb_coach_trainMat, 
      watchlist = watchlist,
      params = params,
      nrounds = nrounds,
      print_every_n = 50
    )
  } else { ### parameter tuning
    fold1 = sort(sample(1:nrow(data_train), replace=FALSE, size=0.5*nrow(data_train)))
    fold2 = setdiff(1:nrow(data_train), fold1)
    folds = list(Fold1 = fold1, Fold2 = fold2)
    # do the cross validation
    xgb_coach_model <- xgboost::xgb.cv(
      data = xgb_coach_trainMat,
      params = params,
      folds = folds,
      metrics = list("mlogloss"),
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = 50
    )
  }
  
  return(xgb_coach_model)
}

pred_xgb_coach <- function(xgb, df) {
  df_xgbDM =  get_xgb_coach_xgbDmat(df)
  xgb_pred = predict(xgb, df_xgbDM)
  pred_matrix = matrix(xgb_pred, ncol=3, nrow=length(xgb_pred)/3, byrow=TRUE)
  colnames(pred_matrix) = c("C_Go", "C_FG", "C_Punt")
  return(pred_matrix)
}

if (file.exists("param_tuning_results_FINAL/xgb_coach_params.yaml")) {
  params_xgb_coach = list.load("param_tuning_results_FINAL/xgb_coach_params.yaml")
}


