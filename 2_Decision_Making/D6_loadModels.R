
### num boot models to load 
B = 100 # justified by the stability analysis

### load XGBoost files
source("T2_models_xgb.R")
source("D2_coach_decision_models.R")

### load models
V1_model_name_WP = xgb_wp_110_7_model_name
V1_wp_model_fitList_boot <- list()
for (b in 1:B) {
  print(paste0("loading boostrap b = ", b, " of B = ", B))
  ### bootstrapped models
  V1_wp_model_fitList_boot[[b]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_WP, "_b", b), ".rds"))
}
V1_wp_model_obs = V1_wp_model_fitList_boot[[1]]
fg_model_obs = load_lm(paste0("fitted_models/", "fg_model", "_b", 1, ".rds"))
punt_model_obs = load_lm(paste0("fitted_models/", "punt_model", "_b", 1, ".rds"))
go_model_obs = load_lm(paste0("fitted_models/", "go_model", "_b", 1, ".rds"))
coach_model = xgb.load(paste0("fitted_models/", "coach_model.rds")) 



