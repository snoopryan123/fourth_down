
# This github repo contains the code for the following papers:
# `WP Paper`: "Analytics, have some humility: a statistical view of fourth-down decision making"

--------------------------------------------------------------------------------

# `WP Paper`

### 1. Get the data
* run `comparison_v110/d1_data_acquisition.R`
* then run `comparison_v110/d2_data_acquisition.R`
* then run `comparison_v110/d3_data_TeamQualityMetrics_epa0.R`
* output `data7b.csv`

### 2. Simulation study: random walk football in `comparison_v110/1_simulation`
* run `comparison_v110/1_simulation/sim_v2/sim2.R` parallelized on a cluster via `run_sim_2_1_AJ.sh` and `run_sim_2_2_AJ.sh`
* then run `comparison_v110/1_simulation/sim_v2/sim_2_aggregate_results.R` (optionally on a cluster via `run_sim_2_aggregate_results.sh`)

### 3. WP model bake-off in `comparison_v110/3_model_selection`
* test GAM models: run `comparison_v110/3_model_selection/gam_wp/test.R`
* test XGB models: run `comparison_v110/3_model_selection/xgb_110/test_wp.R`
* test RF models: run `comparison_v110/3_model_selection/rf/test_wp.R`
* XGBoost model tuning: run `comparison_v110/3_model_selection/xgb_110/param_tuning.R` via `run_param_tuning_xgb_WP_AJ.sh`

### 4. Fourth-down decision making in `comparison_v110/4_Decision_Making_v2`
* fit field goal success probability model, expected next yardline after punting model, conversion success probability model, and typical coach decision model in `comparison_v110/4_Decision_Making_v2/D2_fit_GoFgPunt_models.R`
* bootstrap stability analysis to select $B=100$ in `comparison_v110/4_Decision_Making_v2/D4_stability_analysis.R` and `comparison_v110/4_Decision_Making_v2/D4b_stability_analysis_results.R`
* fit $B=100$ bootstrapped first-down win probability models in `comparison_v110/4_Decision_Making_v2/D5_fit_BootWPModels.R`
* make the fourth-down decision plots (exs. 1 thru 5 in the paper) in `comparison_v110/4_Decision_Making_v2/D7_decision_making_makePlots.R`
* compare traditional decision making to ours and evaluate coaches in `comparison_v110/4_Decision_Making_v2/D8_confusion_matrix.R`


--------------------------------------------------------------------------------

<!---
# `EP Paper`: "Catalytic prior distributions in machine learning with application to expected points models in American football"

# `EP Paper`

## Model and data visualization
* Visualize the EP models in `comparison_v110/2_plotting/A_plot_EP.R`
  * Before doing so, need to train and save full XGBoost models via `comparison_v110/3_model_selection/xgb_110/train_full_models.R`, but some of these models should already be saved
* selection bias plots, team quality plots, and other plots of the data in `comparison_v110/2_plotting/A_plot_TQ.R`

## Model bake-off (out-of-sample predictive performance comparison)
* for `EP Paper`:
  * test OLS models: run `comparison_v110/3_model_selection/ols_110/test_ep_allDowns.R`
  * test MLR models: run `comparison_v110/3_model_selection/mlr_110/test_ep_allDowns.R`
  * test XGB models: run `comparison_v110/3_model_selection/xgb_110/test_ep_allDowns.R`
* __XGBoost model tuning:__
  * XGBoost:
    * tune XGB models: run `comparison_v110/3_model_selection/xgb_110/param_tuning.R` via `run_param_tuning_xgb_EP_AJ.sh` and `run_param_tuning_xgb_WP_AJ.sh`
    * final tuning parameters to be used in XGBoost model fitting found in `param_tuning_results_FINAL`
    * these tuned params are already included in the github repo
  * Catalytic XGBoost:
    * tune Catalytic XGB models: run `comparison_v110/3_model_selection/xgb_110/param_tuning_catalytic.R` via `run_param_tuning_catalytic_EP_AJ.sh` and `run_param_tuning_catalytic_WP_AJ.sh`
    * final tuning parameters to be used in Catalytic XGBoost model fitting found in `param_tuning_results_FINAL`
    * these tuned params are already included in the github repo

## Team quality knockoffs in `EP Paper`
* run `comparison_v110/6_knockoffs_TQ/knockoffs_EPA0tq.R`

--->

# fourth_down
