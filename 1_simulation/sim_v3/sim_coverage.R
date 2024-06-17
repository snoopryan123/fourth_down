
source("../../0_clean_lm.R")

###########################
### get hyperparameters ###
###########################

args <- commandArgs(trailingOnly = TRUE)
m = as.numeric(args[1]) ### sim index
source("sim_main.R")
RETUNE_XGB = FALSE
NUM_WP_ACTUAL_BINS = 50 
N = 56
K = 56
# G = 4101
G = 4096
g = G*(K/N)
sim_str = get_param_combo_str(g,G,N,K,m)
print(sim_str)

#######################################
### Get XGBoost WP model and params ###
#######################################

### generate training dataset
set.seed(23748 + m*143)
df_train = simulate_football_season(G,N,K)
print(df_train)

### generate testing dataset 
set.seed(875 + m*3298)
df_test = simulate_football_season(G,N,K)
print(df_test)
saveRDS(df_test, paste0("xgb_covg/", sim_str, "_", "test_df", ".rds")) ### save

### generate dataset for visualizing WP
df_visualize_wp = get_df_visualize_wp(N)

### hold-out half of the games in the training dataset for parameter tuning validation
all_game_idxs = 1:G
set.seed(9375689 + m*2947)
val_game_idxs = sort(sample(all_game_idxs, size=round(G*0.5), replace=FALSE))
train_game_idxs = setdiff(all_game_idxs, val_game_idxs)
train_game_idxs_OG = all_game_idxs
val_df = df_train %>% filter(g %in% val_game_idxs)
train_df = df_train %>% filter(g %in% train_game_idxs)
train_df_OG = df_train

### load XGBoost tuned parameters 
params_filename = paste0("xgb_params/", "xgb_params_", sim_str, ".yaml")
if (RETUNE_XGB | !file.exists(params_filename)) {
  tune_xgboost(train_df, val_df, params_filename)
} 
params = list.load(params_filename)

### load XGBoost WP model
xgb_filename = paste0("xgb_models/", "xgb_", sim_str, ".xgb")
if (!file.exists(xgb_filename)) {
  xgb_fit = fit_xgb(params, train_df_OG, nrounds=params$nrounds) 
} else {
  xgb_fit = xgb.load(xgb_filename)
}

# ### visualize the XGBoost WP model
# WP_true = get_WP_true_mat(N)
# plot_wp_both_vs_time_d = visualize_wp(WP_true, N=N, wp_true=TRUE, wp_xgb_model=xgb_fit, demo=TRUE, option=1)
# ggsave(paste0("xgb_covg/", sim_str, "_plot_wp_true_vs_time_d_both.png"), plot_wp_both_vs_time_d, width=8, height=6)

#################################################
### XGBoost Losses & Losses binned by true WP ###
#################################################

### losses dataframe
loss_results_df = tibble(
  y = df_test$y,
  wp_actual = df_test$wp_actual,
)
loss_results_df = bind_rows(
  bind_cols(
    loss_results_df,
    tibble(
      wp_pred = predict_xgb(xgb_fit, df_test),
      model_type = "xgb"
    )
  ),
)
loss_results_df = 
  loss_results_df %>%
  mutate(
    logloss = -y*log(wp_pred) - (1-y)*log(1-wp_pred),
    brier = (y - wp_pred)^2,
    bias_abs = abs(wp_actual - wp_pred),
  )
loss_results_df 

# ### loss
# loss_results_df_aggregated = 
#   loss_results_df %>%
#   group_by(model_type) %>%
#   summarise(
#     logloss = mean(logloss),
#     brier = mean(brier),
#     bias_abs = mean(bias_abs),
#   ) %>% mutate(m = m) 
# print(loss_results_df_aggregated)
# write_csv(loss_results_df_aggregated, paste0("xgb_covg/", sim_str, "_loss_df.csv"))

### bias as a function of actual WP
loss_results_df_binned = 
  loss_results_df %>%
  mutate(wp_actual_bin = cut(wp_actual, NUM_WP_ACTUAL_BINS)) %>%
  group_by(model_type, wp_actual_bin) %>%
  summarise(
    logloss = mean(logloss),
    brier = mean(brier),
    bias_abs = mean(bias_abs),
    .groups = "drop"
  ) %>% mutate(m = m) 
print(loss_results_df_binned)
write_csv(loss_results_df_binned, paste0("xgb_covg/", sim_str, "_loss_df_binned.csv"))

########################################
### XGBoost bootstrapped predictions ###
########################################

phi_vec = c(1, 0.75, 0.5, 0.25)
df_boot_method = tibble(
  boot_method = c("SB", "CB", rep("RCB", length(phi_vec))),
  phi = c(1, 1, phi_vec)
)
df_boot_method

wp_preds = array(dim=c(nrow(df_test), B, nrow(df_boot_method)))
wp_preds_viz = array(dim=c(nrow(df_visualize_wp), B, nrow(df_boot_method)))
dimnames(wp_preds) = list( paste0("i=",1:nrow(df_test)), paste0("b=",1:B), paste0("j=",1:nrow(df_boot_method)) )

for (j in 1:nrow(df_boot_method)) {
  boot_method = df_boot_method$boot_method[j]
  phi = df_boot_method$phi[j]
  
  for (b in 1:B) {
    print(""); print(paste0("*** idx=", j, "/", nrow(df_boot_method),", b=", b, "/B=", B, ", boot_method=", boot_method, ", phi=", phi,  " ***")); print(""); 
    
    ### standard iid bootstrap: sample plays with replacement
    num_resample_iidb = round(nrow(train_df_OG)*phi)
    set.seed(4458 + m*99 + b*221)
    train_game_idxs_iidb = sort(sample(1:nrow(train_df_OG), size=num_resample_iidb, replace=TRUE))
    train_df_iidb = train_df_OG[train_game_idxs_iidb,]
    ### clustered bootstrap: sample games with replacement
    num_resample_cb = round(length(train_game_idxs_OG)*phi)
    set.seed(8553 + m*88 + b*948)
    train_df_cb = tibble(
      g = sort(sample(train_game_idxs_OG, size=num_resample_cb, replace=TRUE))
    ) %>% mutate(ii = 1:n())
    train_df_cb = left_join(train_df_cb, train_df_OG)
    ### randomized clustered bootstrap: sample games with replacement, and within each game resample plays with replacement
    train_df_rcb = train_df_cb %>% select(g, ii, i)
    set.seed(3900 + m*203 + b*94)
    train_df_rcb = train_df_rcb %>% 
      group_by(ii) %>%
      sample_n(size = n(), replace = TRUE) %>%
      ungroup() %>%
      left_join(train_df_OG) %>%
      arrange(g,ii,n)
    
    ### get b^th bootstrapped dataset
    if (boot_method == "SB") {
      train_df_b = train_df_iidb
    } else if (boot_method == "CB") {
      train_df_b = train_df_cb
    } else if (boot_method == "RCB") {
      train_df_b = train_df_rcb
    } else {
      stop(paste0("boot_method=",boot_method," is not supported"))
    }
    
    ### fit b^th bootstrapped WP model
    xgb_b <- fit_xgb(params, train_df_b, df_test, params$nrounds) 
    
    ### get b^th predictions
    preds_b = predict_xgb(xgb_b, df_test)
    preds_viz_b = predict_xgb(xgb_b, df_visualize_wp)
    
    ### save the b^th bootstrapped WP predictions
    wp_preds[,b,j] = preds_b
    wp_preds_viz[,b,j] = preds_viz_b
  }
}

# ### save the bootstrapped predictions
# saveRDS(wp_preds, paste0("xgb_covg/", sim_str, "_", "wp_preds", ".rds"))
# saveRDS(wp_preds_viz, paste0("xgb_covg/", sim_str, "_", "wp_preds_viz", ".rds"))

#################################
### Visualize Bootstrap Dists ###
#################################

alpha = 0.05 ### use nominal 90% CI because for B=101 we can use the 6^th and 96^th models.
viz_df_boot = tibble()
for (j in 1:nrow(df_boot_method)) {
  viz_df_boot_j = tibble(
    wp_pred_L = apply(wp_preds_viz[,1:B,j], 1, function(x) quantile(x, alpha) ),
    wp_pred_U = apply(wp_preds_viz[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
    boot_method = df_boot_method$boot_method[j],
    phi = df_boot_method$phi[j],
  )
  viz_df_boot = bind_rows(viz_df_boot, viz_df_boot_j)
}
### write viz_df_boot
saveRDS(viz_df_boot, paste0("xgb_covg/", sim_str, "_viz_df_boot.rds"))

#########################################
### Bootstrapped Confidence Intervals ###
#########################################

### coverages 
get_covg_df <- function(B, alpha = 0.05) {
  boot_results_df = tibble()
  for (j in 1:nrow(df_boot_method)) {
    boot_results_df_j = tibble(
      wp_actual = df_test$wp_actual,
      wp_pred_L = apply(wp_preds[,1:B,j], 1, function(x) quantile(x, alpha) ),
      wp_pred_M = apply(wp_preds[,1:B,j], 1, function(x) quantile(x, 0.5) ),
      wp_pred_U = apply(wp_preds[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
      boot_method = df_boot_method$boot_method[j],
      phi = df_boot_method$phi[j],
    )
    ### widen CI for WP near 0 and 1
    boot_results_df_j = boot_results_df_j %>% mutate(
      wp_pred_2_L = ifelse(wp_pred_M <= 0.02, 0, wp_pred_L),
      wp_pred_2_U = ifelse(wp_pred_M >= 0.98, 1, wp_pred_U),
    )
    boot_results_df = bind_rows(boot_results_df, boot_results_df_j)
  }

  covg_results_df = 
    boot_results_df %>%
    group_by(boot_method,phi) %>%
    mutate(
      covered = as.numeric(wp_pred_L <= wp_actual & wp_actual <= wp_pred_U),
      covered_2 = as.numeric(wp_pred_2_L <= wp_actual & wp_actual <= wp_pred_2_U),
      width = wp_pred_U - wp_pred_L,
      width_2 = wp_pred_2_U - wp_pred_2_L,
    )
  return(covg_results_df)
}
# get_covg_df(B=2)

# B_list = c(10,25,50,100,150,200,250,500,1000)
# B_list = c(25)
B_list = c(B)
COVG_DF = tibble()
COVG_DF_BINNED = tibble()
for (BB in B_list) {
  if (BB <= B) {
    covg_results_df_BB = get_covg_df(B=BB)
    
    covg_df_BB = 
      covg_results_df_BB %>%
      group_by(boot_method,phi) %>%
      summarise(
        m = m,
        B = B,
        covered = mean(covered),
        covered_2 = mean(covered_2),
        width = mean(width),
        width_2 = mean(width_2),
        .groups = "drop"
      )
    
    covg_df_BB_binned = 
      covg_results_df_BB %>%
      mutate(wp_actual_bin = cut(wp_actual, NUM_WP_ACTUAL_BINS)) %>%
      group_by(boot_method,phi,wp_actual_bin) %>%
      summarise(
        m = m,
        B = B,
        covered = mean(covered),
        covered_2 = mean(covered_2),
        width = mean(width),
        width_2 = mean(width_2),
        .groups = "drop"
      )
    
    COVG_DF = bind_rows(COVG_DF, covg_df_BB)
    COVG_DF_BINNED = bind_rows(COVG_DF_BINNED, covg_df_BB_binned)
  }
}
print(data.frame(COVG_DF))
write_csv(COVG_DF, paste0("xgb_covg/", sim_str, "_covg_df.csv"))

print(data.frame(COVG_DF_BINNED))
write_csv(COVG_DF_BINNED, paste0("xgb_covg/", sim_str, "_covg_df_binned.csv"))

