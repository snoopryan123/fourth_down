
### load bootstrap hyperparams
# args <- commandArgs(trailingOnly = TRUE)
# b = as.numeric(args[1])
# B = 500
phi = 0.5 # this is `phi`, justified by the simulation study
B = 100 # justified by the stability analysis

### load data
filewd = getwd()
setwd("..")
source("00_main.R")
setwd(filewd)

### load XGBoost files
source("T2_models_xgb.R")

### these variables are required for `model_fitting_functions`
model_name = xgb_wp_110_7_model_name
model_type = if (str_detect(model_name, "xgb")) "XGB"
WP = str_detect(model_name, "_wp_")
if (!WP) {
  stop(paste0("only WP=TRUE is supported."))
}
data_110 = str_detect(model_name, "110")
dataset_str = paste0("data_full_", if (data_110) "110" else "", "_", if (WP) "WP" else "EP")
print(dataset_str)
DATASET = get(dataset_str)
source("D1_model_fitting_functions.R")

### fit and save Coach Model ###
source("D2_coach_decision_models.R")
coach_model_fit = fit_coach_model_best(all_fourth_downs)
xgb.save(coach_model_fit, paste0("fitted_models/coach_model.rds"))

### fit B bootstrapped WP models
for (b in 1:B) {
  print(paste0("**** bootstrap ", ", b=",b,"/B=",B," ****"))
  
  ### get the b^th bootstrap re-sampled datasets
  if (b==1) { ### actual data
    dataset_wp_b = DATASET
    dataset_fg_b = fg_df
    dataset_punt_b = punt_df
    dataset_go_b = go_df
  } else { ### bootstrapped data
    set.seed(3493 + b*299)
    dataset_wp_b = get_randomized_clustered_bootstrap_dataset(DATASET, wp=WP, phi=phi) 
    dataset_fg_b = get_iid_bootstrap_dataset(fg_df)
    dataset_punt_b = get_iid_bootstrap_dataset(punt_df)
    dataset_go_b = get_iid_bootstrap_dataset(go_df)
  }
  
  ### fit the b^th bootstrapped models
  ### fit the b^th WP model
  V1_model_b = fit_V1_model_best(model_name, model_type, dataset_wp_b)
  model_wp_filename = paste0("fitted_models/", model_name, "_b", b, ".rds")
  xgb.save(V1_model_b, model_wp_filename) 
  ### fit and save the b^th FG Model ###
  fg_model_fit = fit_fgp_model_best(dataset_fg_b)
  save_lm(fg_model_fit, paste0("fitted_models/", "fg_model", "_b", b, ".rds"))
  ### fit and save the b^th Punt Model ###
  punt_model_fit = fit_punt_eny_model_best(dataset_punt_b)
  save_lm(punt_model_fit, paste0("fitted_models/", "punt_model", "_b", b, ".rds"))
  ### fit and save the b^th Convert Model ###
  go_model_fit = fit_convp_model_best(dataset_go_b)
  save_lm(go_model_fit, paste0("fitted_models/", "go_model", "_b", b, ".rds"))
}


