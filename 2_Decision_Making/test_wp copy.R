
##################
WP = TRUE
filewd = getwd()
setwd("..")
source("1_train_test_split.R") 
setwd(filewd)
source("models.R")
##################

##########################################################
### Fit the Tree Classification Win Probability Models ### 
##########################################################

#######################################################
print(""); print(rf_R_wp_LockNettleton_model_name); print("");
rf_R_wp_LockNettleton = train_rf(rf_R_wp_LockNettleton_features, train_set, rf_R_wp_LockNettleton_params, wp=TRUE, reg=TRUE)
pred_rf_R_wp_LockNettleton = tibble(
  pred = predict(rf_R_wp_LockNettleton, test_set %>% select(all_of(rf_R_wp_LockNettleton_features))),
  model = rf_R_wp_LockNettleton_model_name
)

########################################################################################

preds = bind_rows(
  pred_rf_R_wp_LockNettleton
)

results = preds %>% 
  group_by(model) %>%
  summarise(
    logloss = LOGLOSS(test_set$label_win, pred)
  ) %>% arrange(logloss) 
print(results)

write_csv(results, paste0("test_results_wp.csv"))

