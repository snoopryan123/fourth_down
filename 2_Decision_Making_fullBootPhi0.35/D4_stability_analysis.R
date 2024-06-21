
### load hyperparams
# m = 1
# B = 3
# WP = TRUE
phi = 1
args <- commandArgs(trailingOnly = TRUE)
m = as.numeric(args[1])
B = as.numeric(args[2])
param_str = paste0("m=",m,"_B=",B)
print(param_str)

### load data
filewd = getwd()
setwd("..")
source("00_main.R")
setwd(filewd)
fourth_downs_df = ALL_fourth_downs %>% filter(season %in% 2018:2022)

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

### fit B bootstrapped WP models
V1_model_name_WP = model_name
V1_wp_model_fitList_boot <- list()
fg_model_fitList_boot <- list()
punt_model_fitList_boot <- list()
go_model_fitList_boot <- list()
for (b in 1:B) {
  print(paste0("**** bootstrap ", ", b=",b,"/B=",B," ****"))
  
  ### get the b^th bootstrap re-sampled datasets
  if (b==1) { ### actual data
    dataset_wp_b = DATASET
    dataset_fg_b = fg_df
    dataset_punt_b = punt_df
    dataset_go_b = go_df
  } else { ### bootstrapped data
    set.seed(3493 + b*299 + m*7367)
    dataset_wp_b = get_randomized_clustered_bootstrap_dataset(DATASET, wp=WP, phi=phi) 
    dataset_fg_b = get_iid_bootstrap_dataset(fg_df)
    dataset_punt_b = get_iid_bootstrap_dataset(punt_df)
    dataset_go_b = get_iid_bootstrap_dataset(go_df)
  }
  
  ### fit the b^th bootstrapped models
  ### fit the b^th WP model
  V1_model_b = fit_V1_model_best(model_name, model_type, dataset_wp_b)
  V1_wp_model_fitList_boot[[b]] = V1_model_b
  ### fit and save the b^th FG Model ###
  fg_model_fit = fit_fgp_model_best(dataset_fg_b)
  fg_model_fitList_boot[[b]] = fg_model_fit
  ### fit and save the b^th Punt Model ###
  punt_model_fit = fit_punt_eny_model_best(dataset_punt_b)
  punt_model_fitList_boot[[b]] = punt_model_fit
  ### fit and save the b^th Convert Model ###
  go_model_fit = fit_convp_model_best(dataset_go_b)
  go_model_fitList_boot[[b]] = go_model_fit
}

### get the decision making functions
source("D3_decision_making_functions.R")

### make decisions on set of fourth downs 
if (model_type == "XGB") {
  ddf = get_all_decision_making(fourth_downs_df, wp=WP, SE=TRUE, coachBaseline=FALSE, bind_w_plays=FALSE) 
  ddf1 = ddf %>% select(prop_decision, decision)
  filename_ddf = paste0("job_output/stability_analysis_ddf_",param_str,".csv")
  write_csv(ddf1, filename_ddf)
} else {
  stop(paste0("model_type=",model_type," is not supported."))
}



