
# This github repo contains the code for "Analytics, have some humility: a statistical view of fourth-down decision making"

### 0. Get the data
* run `d1_data_acquisition.R`
* then run `d2_data_acquisition.R`
* then run `d3_data_KickerAndPunterQuality.R`
* output `data8a.csv`

### 1. Simulation study: random walk football in `1_simulation/sim_v2` (you can skip this tedious section and still run the more important subsequent code)
* run `sim2.R` parallelized on a cluster via `run_sim_2_1_AJ.sh` and `run_sim_2_2_AJ.sh`
* then run `sim_2_aggregate_results.R` (optionally on a cluster via `run_sim_2_aggregate_results.sh`)

### 2. Fourth-down decision making in `2_Decision_Making`
* models (you can skip this tedious section and still run the more important subsequent code)
  * tune the XGBoost first-down win probability models in `T2_param_tuning_xgb.R` on a cluster via `T2_run_param_tuning_xgb_WP_AJ.sh`
  * test the accuracy of the various first-down WP models in `T3_test_wp.R`
  * tune the baseline coach XGBoost model in `D2_coach_decision_model_tune.R`
  * bootstrap stability analysis to select $B=100$ in `D4_stability_analysis.R` and `D4b_stability_analysis_results.R`
  * fit $B=100$ bootstrapped first-down win probability models and the FG, Go, and Punt models in `D5_fit_BootWPModels.R`
* decision making
  * make the fourth-down decision plots (exs. 1 thru 5 in the paper) in `D7_decision_making_makePlots.R`
  * compare traditional decision making to ours and evaluate coaches in `D8_humility.R`

### 3. Shiny app in `3_shiny`
* run `app.R` for an interactive fourth-down decision making Shiny app!
