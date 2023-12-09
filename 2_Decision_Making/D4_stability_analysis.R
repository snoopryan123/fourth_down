
### load hyperparams
# m = 1
# B = 3
# WP = TRUE
brp = 0.5 # justified by the simulation study
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
fourth_downs_df = all_fourth_downs %>% filter(season %in% 2018:2022)
# fourth_downs_df = 
#   fourth_downs_df %>%
#   select(
#     posteam, season, posteam_coach, decision_actual, # kicker_player_name, punter_player_name,
#     yardline_100, ydstogo, down, score_differential, posteam_spread, game_seconds_remaining
#   )

### load Punt, FG, Go models
fg_model = load_lm(paste0("fitted_models/fg_model.rds"))
punt_model = load_lm(paste0("fitted_models/punt_model.rds"))
go_model = load_lm(paste0("fitted_models/go_model.rds"))

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
V1_MODELS = list()
for (b in 1:B) {
  print(paste0("**** bootstrap ", "m=",m,", b=",b,"/B=",B," ****"))
  
  ### get the b^th bootstrap re-sampled datasets
  if (b==1) { ### actual data
    dataset_b = DATASET
  } else {
    set.seed(8553 + m*88 + b*948)
    dataset_b = get_randomized_clustered_bootstrap_dataset(DATASET, wp=WP, brp=brp) 
  }
  
  ### fit b^th WP model
  V1_model_b = fit_V1_model_best(model_name, model_type, dataset_b)
  ### save b^th WP model
  V1_MODELS[[b]] = V1_model_b
}

### these variables are required for `decision_making_functions`
V1_model_name_WP = model_name
V1_wp_model_fitList_boot = V1_MODELS
V1_model_fitList_boot = NULL #FIXME # EP model not supported...
source("D3_decision_making_functions.R")

### make decisions on set of fourth downs 
if (model_type == "XGB") {
  
  ddf = get_all_decision_making(fourth_downs_df, wp=WP, SE=TRUE, coachBaseline=FALSE, bind_w_plays=FALSE) 
  ddf1 = 
    ddf %>% 
    select(
      prop_decision, decision
      # decision_intensity_L, decision_intensity, decision_intensity_U
    ) #%>% mutate(m = m, B = B) 
  filename_ddf = paste0("job_output/stability_analysis_ddf_",param_str,".csv")
  write_csv(ddf1, filename_ddf)
  
} else {
  stop(paste0("model_type=",model_type," is not supported."))
}



