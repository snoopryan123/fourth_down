
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
pred_rf_R_wp_LockNettleton = predict(rf_R_wp_LockNettleton, test_set_110 %>% select(all_of(rf_R_wp_LockNettleton_features)))
logloss_rf_R_wp_LockNettleton = LOGLOSS(test_set_110$label_win, pred_rf_R_wp_LockNettleton)

### pre-game point spread
pointSpreadGame_model_name = "pre-game point spread logistic regression"
df_pointSpreadGame = 
  train_set %>%
  distinct(game_id,posteam,home,posteam_spread,label_win) 
df_pointSpreadGame
m_ps = glm(label_win ~ posteam_spread, data = df_pointSpreadGame %>% filter(home==1), family="binomial")
pred_pointSpreadGame_testSet = test_set_110 %>% mutate(p = predict(m_ps,.,type="response"))
logloss_pointSpreadGame = LOGLOSS(test_set_110$label_win, pred_pointSpreadGame_testSet$p)
logloss_pointSpreadGame

### home team baseline
baseline_model_name = "home team"
pred_baseline_perGame = 
  train_set %>%
  distinct(game_id,posteam,home,label_win) %>%
  group_by(home) %>%
  reframe(p = mean(label_win), n = n()) %>%
  ungroup()
pred_baseline_perGame
pred_baseline_testSet = test_set_110 %>% left_join(pred_baseline_perGame %>% select(-n))
logloss_baseline = LOGLOSS(test_set_110$label_win, pred_baseline_testSet$p)

################################################################################

results_wp = tibble(
  model = c(
    xgb_wp_110_7_model_name,
    xgb_wp_1_model_name,
    xgb_wp_Baldwin_1_model_name,
    # xgb_wp_Baldwin_model_name, 
    rf_R_wp_LockNettleton_model_name,
    pointSpreadGame_model_name,
    baseline_model_name,
    "fair coin"
  ),
  logloss = c(
    logloss_xgb_wp_110_7,
    logloss_xgb_wp_1,
    logloss_xgb_wp_Baldwin_1,
    # logloss_xgb_wp_Baldwin, 
    logloss_rf_R_wp_LockNettleton,
    logloss_pointSpreadGame,
    logloss_baseline,
    -log(0.5)
  )
) %>% arrange(logloss) %>% 
  mutate(
    L = round(logloss,3), 
    RIE = round(-(logloss - -log(0.5))/(-log(0.5))*100, 2) 
  )
print(data.frame(results_wp))

write_csv(results_wp, paste0("T3_test_results_wp.csv"))







