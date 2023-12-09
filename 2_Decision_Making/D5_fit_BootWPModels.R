
### load bootstrap hyperparams
# args <- commandArgs(trailingOnly = TRUE)
# b = as.numeric(args[1])
# B = 500
brp = 0.5 # this is `phi`, justified by the simulation study
B = 100 # justified by the stability analysis

### load data
filewd = getwd()
setwd("..")
source("00_main.R")
setwd(filewd)

### load XGBoost files
filewd = getwd()
setwd("../3_model_selection/xgb_110")
source("models.R")
setwd(filewd)

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

### fit B bootstrapped WP models
for (b in 1:B) {
  print(paste0("**** bootstrap ", ", b=",b,"/B=",B," ****"))
  
  ### get the b^th bootstrap re-sampled datasets
  if (b==1) { ### actual data
    dataset_b = DATASET
  } else {
    set.seed(3493 + b*299)
    dataset_b = get_randomized_clustered_bootstrap_dataset(DATASET, wp=WP, brp=brp) 
  }
  
  ### fit the b^th WP model
  V1_model_b = fit_V1_model_best(model_name, model_type, dataset_b)
  ### write the b^th WP model
  model_wp_filename = paste0("fitted_models/", model_name, "_b", b, ".rds")
  xgb.save(V1_model_b, model_wp_filename) 
}

