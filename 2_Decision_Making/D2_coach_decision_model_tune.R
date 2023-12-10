
################
filewd = getwd()
setwd("..")
source("00_main.R")
setwd(filewd)
################

source("D2_coach_decision_models.R")
train_df = all_fourth_downs

########################################
### XGBoost Model Tuning and Testing ###
########################################

### Ben Baldwin's param tuning from https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
RETUNE_XGB = FALSE
if (RETUNE_XGB) {
  ###############################################################
  set.seed(30) ### Todd
  grid_size = 40
  xgb_coach_param_grid = grid_latin_hypercube(
    #################
    dials::loss_reduction(),
    dials::min_n(),
    dials::finalize(dials::mtry(), train_df), # this finalize thing is because mtry depends on # of columns in data
    dials::tree_depth(),
    dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
    sample_size = dials::sample_prop(),
    #################
    # dials::mtry(range = c(round(length(train_df) * 0.75), length(train_df))),
    # dials::learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
    # dials::loss_reduction(range=c(-1.5, 0), trans = scales::log10_trans()),
    # dials::tree_depth(range=c(3,6)),
    # dials::min_n(range=c(6,13)),
    # sample_size = dials::sample_prop(range = c(0.7, 0.95)),
    #################
    size = grid_size
  ) %>% mutate(
    mtry = mtry / length(train_df),
    # monotone_constraints = "(0, 0, 0, 0, 0, 0)"
  ) %>% rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )
  xgb_coach_param_grid
  # function to perform xgb.cv for a given row in a hyperparameter grid
  get_row <- function(row) {
    params <-
      list(
        booster = "gbtree",
        objective = "multi:softprob",
        eval_metric = c("mlogloss"),
        # num_class = 3+1,
        num_class = 3,
        eta = row$eta,
        gamma = row$gamma,
        subsample = row$subsample,
        colsample_bytree = row$colsample_bytree,
        max_depth = row$max_depth,
        min_child_weight = row$min_child_weight
      )
    xgb_cv_model <- fit_xgb_coach(train_df, params, param_tuning=TRUE) 
    # bundle up the results together for returning
    output <- params
    output$iter <- xgb_cv_model$best_iteration
    output$logloss <- xgb_cv_model$evaluation_log[output$iter]$test_mlogloss_mean
    row_result <- bind_rows(output)
    return(row_result)
  }
  # get results
  results = map_df(1:nrow(xgb_coach_param_grid), function(x) {
    print(paste0("row ", x)); return(get_row(xgb_coach_param_grid %>% dplyr::slice(x)))
  })
  # visualize param tuning
  results %>%
    dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
    tidyr::pivot_longer(
      eta:min_child_weight,
      values_to = "value",
      names_to = "parameter"
    ) %>%
    ggplot(aes(value, logloss, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "logloss") +
    theme_minimal()
  # re-tune, with better param range based on these plots...
  # Collect best parameters
  results %>% arrange(logloss) %>% select(eta, subsample, colsample_bytree, max_depth, logloss, min_child_weight, iter)
  best_model <- results %>% arrange(logloss) %>% slice_head()
  best_model
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    # objective = "binary:logistic",
    eval_metric = c("mlogloss"),
    num_class=3,
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    # monotone_constraints = best_model$monotone_constraints,
    nrounds = best_model$iter
  )
  params
  list.save(params, "param_tuning_results_FINAL/xgb_coach_params.yaml")
  ###############################################################
}



