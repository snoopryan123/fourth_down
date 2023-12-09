
################
filewd = getwd()
setwd("..")
source("00_main.R")
setwd(filewd)
################

source("D1_model_fitting_functions.R")

### fit and save FG Model ###
fg_model_fit = fit_fgp_model_best(fg_df)
save_lm(fg_model_fit, paste0("fitted_models/", "fg_model", ".rds"))

### fit and save Punt Model ###
punt_model_fit = fit_punt_eny_model_best(punt_df)
save_lm(punt_model_fit, paste0("fitted_models/", "punt_model",  ".rds"))

### fit and save Convert Model ###
go_model_fit = fit_convp_model_best(go_df)
save_lm(go_model_fit, paste0("fitted_models/", "go_model", ".rds"))

### load coach XGBoost model
filewd00 = getwd()
setwd("../3_model_selection/coach_decision_model/")
source("coach_decision_models.R")
setwd(filewd00)

### fit and save Coach Model ###
coach_model_fit = fit_coach_model_best(all_fourth_downs)
xgb.save(coach_model_fit, paste0("fitted_models/coach_model.rds"))

# ### checks
# plot_fg_prob_by_kq(fg_model_fit)
# plot_punt_eny_by_pq(punt_model_fit)
