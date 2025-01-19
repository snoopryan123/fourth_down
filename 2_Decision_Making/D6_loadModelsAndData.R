
### num boot models to load 
B = 101 # justified by the stability analysis

### phi (for fractional bootstrap)
# if (!exists("phi")) { phi = 0.5 } #FIXME
# GET PHI FROM ANOTHER FILE!

### load XGBoost files
source("T2_models_xgb.R")
source("D2_coach_decision_models.R")

### load data
V1_model_name_WP = xgb_wp_110_7_model_name
if (!exists("LOADDATA")) { LOADDATA = TRUE }
if (LOADDATA) {
  filewd = getwd()
  setwd("..")
  source("00_main.R")
  setwd(filewd)
  
  data_110 = str_detect(V1_model_name_WP, "110")
  dataset_str = paste0("data_full_", if (data_110) "110" else "", "_", if (WP) "WP" else "EP")
  print(dataset_str)
  DATASET = get(dataset_str)
  source("D1_model_fitting_functions.R")
}

### these variables are required for `model_fitting_functions`
model_type_WP = if (str_detect(V1_model_name_WP, "xgb")) "XGB"
WP = TRUE
if (!WP) {
  stop(paste0("only WP=TRUE is supported."))
}

### load models
V1_wp_model_fitList_boot <- list()
fg_model_fitList_boot <- list()
punt_model_fitList_boot <- list()
go_model_fitList_boot <- list()
go_Eoutcome_success_model_fitList_boot <- list()
go_Eoutcome_failure_model_fitList_boot <- list()
for (b in 1:B) {
  print(paste0("loading boostrap b = ", b, " of B = ", B))
  ### bootstrapped models
  V1_wp_model_fitList_boot[[b]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_WP, "_phi", phi, "_b", b), ".rds"))
  fg_model_fitList_boot[[b]] = load_lm(paste0("fitted_models/", "fg_model", "_b", b, ".rds"))
  punt_model_fitList_boot[[b]] = load_lm(paste0("fitted_models/", "punt_model", "_b", b, ".rds"))
  go_model_fitList_boot[[b]] = load_lm(paste0("fitted_models/", "go_model", "_b", b, ".rds"))
  go_Eoutcome_success_model_fitList_boot[[b]] = load_lm(paste0("fitted_models/", "go_Eoutcome_success_model", "_b", b, ".rds"))
  go_Eoutcome_failure_model_fitList_boot[[b]] = load_lm(paste0("fitted_models/", "go_Eoutcome_failure_model", "_b", b, ".rds"))
}
V1_wp_model_obs = V1_wp_model_fitList_boot[[1]]
fg_model_obs = fg_model_fitList_boot[[1]]
punt_model_obs = punt_model_fitList_boot[[1]]
go_model_obs = go_model_fitList_boot[[1]]
go_Eoutcome_success_model_obs = go_Eoutcome_success_model_fitList_boot[[1]]
go_Eoutcome_failure_model_obs = go_Eoutcome_failure_model_fitList_boot[[1]]
coach_model = xgb.load(paste0("fitted_models/", "coach_model.rds")) 



