
##################
WP = TRUE
source("T1_train_test_split.R") 
source("T2_models_rf.R")
source("T2_models_xgb.R")
##################

print(""); print(xgb_wp_110_7_model_name); print("");
xgb_wp_110_7 = train_xgb(xgb_wp_110_7_features, train_set_110, xgb_wp_110_7_params, xgb_wp_110_7_nrounds, watchSet=test_set_110, catalytic=xgb_wp_110_7_catalytic, wp=TRUE)
logloss_xgb_wp_110_7 = last(xgb_wp_110_7$evaluation_log$validation_logloss)

print(""); print(xgb_wp_1_model_name); print("");
xgb_wp_1 = train_xgb(xgb_wp_1_features, train_set, xgb_wp_1_params, xgb_wp_1_nrounds, watchSet=test_set_110, catalytic=xgb_wp_1_catalytic, wp=TRUE)
logloss_xgb_wp_1 = last(xgb_wp_1$evaluation_log$validation_logloss)

print(""); print(xgb_wp_Baldwin_1_model_name); print("");
xgb_wp_Baldwin_1 = train_xgb(xgb_wp_Baldwin_1_features, train_set, xgb_wp_Baldwin_1_params, xgb_wp_Baldwin_1_nrounds, watchSet=test_set_110, catalytic=xgb_wp_Baldwin_1_catalytic, wp=TRUE)
logloss_xgb_wp_Baldwin_1 = last(xgb_wp_Baldwin_1$evaluation_log$validation_logloss)

# print(""); print(xgb_wp_Baldwin_model_name); print("");
# xgb_wp_Baldwin = train_xgb(xgb_wp_Baldwin_features, train_set, xgb_wp_Baldwin_params, xgb_wp_Baldwin_nrounds, watchSet=test_set_110, catalytic=xgb_wp_Baldwin_catalytic, wp=TRUE)
# logloss_xgb_wp_Baldwin = last(xgb_wp_Baldwin$evaluation_log$validation_logloss)

print(""); print(rf_R_wp_LockNettleton_model_name); print("");
rf_R_wp_LockNettleton = train_rf(rf_R_wp_LockNettleton_features, train_set, rf_R_wp_LockNettleton_params, wp=TRUE, reg=TRUE)
pred_rf_R_wp_LockNettleton = predict(rf_R_wp_LockNettleton, test_set %>% select(all_of(rf_R_wp_LockNettleton_features)))
logloss_rf_R_wp_LockNettleton = LOGLOSS(test_set$label_win, pred)

################################################################################

results_wp = tibble(
  model = c(
    xgb_wp_110_7_model_name,
    xgb_wp_1_model_name,
    xgb_wp_Baldwin_1_model_name,
    # xgb_wp_Baldwin_model_name, 
    rf_R_wp_LockNettleton_model_name
  ),
  logloss = c(
    logloss_xgb_wp_110_7,
    logloss_xgb_wp_1,
    logloss_xgb_wp_Baldwin_1,
    # logloss_xgb_wp_Baldwin, 
    logloss_rf_R_wp_LockNettleton
  )
) %>% arrange(logloss)
print(data.frame(results_wp))

write_csv(results_wp, paste0("T3_test_results_wp.csv"))







