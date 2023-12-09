
### num boot models to load 
B = 100 # justified by the stability analysis

### load data
filewd = getwd()
setwd("..")
source("00_main.R")
setwd(filewd)

### load Punt, FG, Go models
fg_model_obs = load_lm(paste0("fitted_models/fg_model.rds"))
punt_model_obs = load_lm(paste0("fitted_models/punt_model.rds"))
go_model_obs = load_lm(paste0("fitted_models/go_model.rds"))
fg_model = fg_model_obs
punt_model = punt_model_obs
go_model = go_model_obs

### load XGBoost files
filewd = getwd()
setwd("../3_model_selection/xgb_110")
source("models.R")
setwd(filewd)
filewd00 = getwd()
setwd("../3_model_selection/coach_decision_model/")
source("coach_decision_models.R")
setwd(filewd00)

### these variables are required for `model_fitting_functions`
V1_model_name_WP = xgb_wp_110_7_model_name
model_type_WP = if (str_detect(V1_model_name_WP, "xgb")) "XGB"
WP = TRUE
if (!WP) {
  stop(paste0("only WP=TRUE is supported."))
}
data_110 = str_detect(V1_model_name_WP, "110")
dataset_str = paste0("data_full_", if (data_110) "110" else "", "_", if (WP) "WP" else "EP")
print(dataset_str)
DATASET = get(dataset_str)
source("D1_model_fitting_functions.R")

### load models
# V1_model_fitList_boot <- list()
V1_wp_model_fitList_boot <- list()
for (b in 1:B) {
  print(paste0("loading boostrap b = ", b, " of B = ", B))
  
  # ### bootstrapped EP models
  # if (V1_model_type_WP_EP == "MLR") {
  #   V1_model_fitList_boot[[b]] = load_lm(paste0("fitted_models/", paste0(V1_model_name_EP, "_b", b), ".rds")) 
  # } else if (V1_model_type_WP_EP == "XGB") {
  #   V1_model_fitList_boot[[b]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_EP, "_b", b), ".rds"))
  # } 
  
  ### bootstrapped WP models
  V1_wp_model_fitList_boot[[b]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_WP, "_b", b), ".rds"))
  
  
}
# V1_model_obs = V1_model_fitList_boot[[1]]
V1_wp_model_obs = V1_wp_model_fitList_boot[[1]]
coach_model = xgb.load(paste0("fitted_models/", "coach_model.rds")) 



