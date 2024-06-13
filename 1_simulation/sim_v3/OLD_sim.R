
source("../../0_clean_lm.R")

# SIM_NUM = 1
# HYPERPARAM_COMBO_IDX = 1

args <- commandArgs(trailingOnly = TRUE)
SIM_NUM = as.numeric(args[1])
HYPERPARAM_COMBO_IDX = as.numeric(args[2])
grid_size = 40 ### num param combos to try for XGBoost tuning
RETUNE_XGB = FALSE
DO_BOOTSTRAP_ANALYSIS = TRUE #FIXME
NUM_WP_ACTUAL_BINS = 50 #10
# B=10; grid_size=2; #FIXME # for testing
#### B = if (exists(args[2])) as.numeric(args[2]) else 100 ### default 100 bootstrap samples

######################################
### G == num games
### N == num plays per game
### K == num plays to keep per game
######################################

source("sim_main.R")

#############################
### Simulation Parameters ###

sim_str = paste0("sim_h", HYPERPARAM_COMBO_IDX, "_G", G, "_N", N, "_K", K, "_L", L, "_m", SIM_NUM)
print(sim_str)
print(paste0("***** h #",HYPERPARAM_COMBO_IDX," sim #", SIM_NUM, " and B = ", B, " bootstrap samples *****"))

##########################
### Simulation XGBoost ###
##########################

### generate simulation df
set.seed(23748 + SIM_NUM*143)
df_train = simulate_football_season()
df_test = simulate_football_season()
print(df_train)
print(df_test)
### hold-out half of the games for testing
test_df = df_test
### hold-out half of the training games for validation
all_game_idxs = 1:G
set.seed(9375689 + SIM_NUM*2947)
val_game_idxs = sort(sample(all_game_idxs, size=round(G*0.5), replace=FALSE))
train_game_idxs_OG = setdiff(all_game_idxs, val_game_idxs)
val_df = df_train %>% filter(g %in% val_game_idxs)
train_df = df_train %>% filter(g %in% train_game_idxs_OG)
train_df_OG = df_train
### check
length(unique(test_df$g))
length(unique(train_df$g))
length(unique(val_df$g))
### write test df
saveRDS(test_df, paste0("job_output/", sim_str, "_", "test_df", ".rds"))

################################
### XGBoost Parameter Tuning ###
################################

fit_xgb <- function(params, train_df, val_df=NULL, nrounds=NULL) {
  ### fit the XGBoost model
  train_set_xgbDM = xgb.DMatrix(
    model.matrix(~ . + 0, data = train_df %>% select(all_of(xgb_features))),
    label = train_df$y
  )
  if (is.null(val_df)) {
    watchlist <- list(train=train_set_xgbDM)
  } else {
    val_set_xgbDM = xgb.DMatrix(
      model.matrix(~ . + 0, data = val_df %>% select(all_of(xgb_features))),
      label = val_df$y
    )
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  }
  ### train XGBoost
  xgb <- xgb.train( 
    data = train_set_xgbDM, 
    watchlist = watchlist,
    params = params, 
    nrounds = if (is.null(nrounds)) 15000 else nrounds,
    early_stopping_rounds = if (is.null(nrounds)) 50 else Inf,
    print_every_n = 50,
    verbose = 2
  )
  return(xgb)
}

################################################################################

params_filename = paste0("job_output/", sim_str, "_params.yaml")
if (!RETUNE_XGB & file.exists(params_filename)) {
  print(paste0("loading XGBoost params in Sim3 #", SIM_NUM))
  
  params = list.load(params_filename)
} else {
  print(paste0("tuning XGBoost in Sim3 #", SIM_NUM))
  ### Baldwin's XGBoost tuning https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
  
  ### parameter tuning grid
  get_param_grid <- function() {
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), train_df),
      dials::min_n(),
      dials::tree_depth(range=c(length(xgb_features), length(xgb_features))),
      # dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
      dials::learn_rate(range = c(-1, -0.5), trans = scales::log10_trans()),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = grid_size
    ) %>%
      dplyr::mutate(
        mtry = mtry / length(train_df),
        monotone_constraints = "(0,-1,1)"
      ) %>%
      # make these the right names for xgb
      dplyr::rename(
        eta = learn_rate,
        gamma = loss_reduction,
        subsample = sample_size,
        colsample_bytree = mtry,
        max_depth = tree_depth,
        min_child_weight = min_n
      )
  }
  # get_param_grid()
  
  # function to perform xgb.cv for a given row in a hyperparameter grid
  get_row <- function(row) {
    params <-
      list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = c("logloss"),
        eta = row$eta,
        gamma = row$gamma,
        subsample = row$subsample,
        colsample_bytree = row$colsample_bytree,
        max_depth = row$max_depth,
        min_child_weight = row$min_child_weight,
        monotone_constraints = row$monotone_constraints
      )
    
    ### fit the XGBoost model
    xgb = fit_xgb(params, train_df, val_df)
    
    # bundle up the results together for returning
    output <- params
    output$iter <- xgb$best_iteration
    output$logloss <- xgb$evaluation_log[output$iter]$validation_logloss
    
    row_result <- bind_rows(output)
    return(row_result)
  }
  
  # get results
  results <- purrr::map_df(1:grid_size, function(i) {
    print(paste0("tuning row ", i, " of ", grid_size))
    get_row(get_param_grid() %>% filter(row_number() == i)  )
  })
  
  # plot tuning results
  # {
  #   results %>%
  #     dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  #     tidyr::pivot_longer(
  #       eta:min_child_weight,
  #       values_to = "value",
  #       names_to = "parameter"
  #     ) %>%
  #     ggplot(aes(value, logloss, color = parameter)) +
  #     geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  #     facet_wrap(~parameter, scales = "free_x") +
  #     labs(x = NULL, y = "logloss") +
  #     theme_minimal()
  # }
  
  ### best XGBoost model
  best_model <- results %>% arrange(logloss) %>% slice_head(n=1)
  print(best_model)
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = best_model$eta,
      gamma = best_model$gamma,
      subsample = best_model$subsample,
      colsample_bytree = best_model$colsample_bytree,
      max_depth = best_model$max_depth,
      min_child_weight = best_model$min_child_weight,
      monotone_constraints = best_model$monotone_constraints,
      nrounds = best_model$iter
    )
  
  ### save xgboost params
  list.save(params, params_filename)
} 

###########
### GAM ###
###########

# fit_lr_1 <- function(dataset) {
#   fit = glm(
#     y ~ x + I(s/(N-n+1)),
#     data = train_df_OG,
#     family = "binomial"
#   )
#   clean_lm(fit)
# }

#################################
### XGBoost Point Predictions ###
#################################

########## fit the XGBoost model ########## 
xgb_pp <- fit_xgb(params, train_df_OG, test_df, params$nrounds) 
### write xgb_pp
xgb.save(xgb_pp, paste0("job_output/", sim_str, "_xgb_pp.xgb"))

########## visualize the XGBoost WP predictions ########## 
# plot_wp_both_vs_time_d = visualize_wp(wp_true=TRUE, wp_xgb_model=xgb_pp, demo=TRUE)
# ggsave(paste0("job_output/", sim_str, "_plot_wp_true_vs_time_d_both.png"), plot_wp_both_vs_time_d, width=8, height=6)
#  
# # plot_wp_both_vs_time = visualize_wp(wp_true=TRUE, wp_xgb_model=xgb_pp)
# # ggsave(paste0("job_output/", sim_str, "_plot_wp_both_vs_time.png"), plot_wp_both_vs_time, width=24, height=8)
# # 
# # plot_wp_both_vs_time_2 = visualize_wp(wp_true=TRUE, wp_xgb_model=xgb_pp, option=2)
# # ggsave(paste0("job_output/", sim_str, "_plot_wp_both_vs_time_2.png"), plot_wp_both_vs_time_2, width=16, height=12)
# #
# # plot_wp_xgb_pp_vs_time = visualize_wp(wp_true=FALSE, wp_xgb_model=xgb_pp)
# # ggsave(paste0("job_output/", sim_str, "_plot_wp_xgb_pp_vs_time.png"), plot_wp_xgb_pp_vs_time, width=24, height=8)
# # 
# # plot_wp_xgb_pp_vs_time_2 = visualize_wp(wp_true=FALSE, wp_xgb_model=xgb_pp, option=2)
# # ggsave(paste0("job_output/", sim_str, "_plot_wp_xgb_pp_vs_time.png"), plot_wp_xgb_pp_vs_time_2, width=16, height=12)
# #
########## Save Results: Losses ########## 
loss_results_df = tibble(
  y = test_df$y,
  wp_actual = test_df$wp_actual,
)
loss_results_df = bind_rows(
  bind_cols(
    loss_results_df,
    tibble(
      wp_pred = predict(xgb_pp, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) )),
      model_type = "xgb"
    )
  ),
  # bind_cols(
  #   loss_results_df,
  #   tibble(
  #     wp_pred = predict(lr1_pp, test_df, type = "response"),
  #     model_type = "lr"
  #   )
  # )
)
loss_results_df = loss_results_df %>%
  mutate(
    logloss_wp_y = -y*log(wp_pred) - (1-y)*log(1-wp_pred),
    brier_wp_y = (y - wp_pred)^2,
    brier_wp_pred = (wp_actual - wp_pred)^2,
    mae_wp_pred = abs(wp_actual - wp_pred)
  )
loss_results_df 

### loss
loss_df = loss_results_df %>%
  group_by(model_type) %>%
  summarise(
    logloss_wp_y = mean(logloss_wp_y),
    brier_wp_y = mean(brier_wp_y),
    brier_wp_pred = mean(brier_wp_pred),
    mae_wp_pred = mean(mae_wp_pred)
  ) %>% mutate(sim_num = SIM_NUM) %>% arrange(mae_wp_pred)
print(loss_df)
write_csv(loss_df, paste0("job_output/", sim_str, "_loss_df.csv"))
# gtsave(gt(loss_df), paste0("job_output/", sim_str, "_loss_df.png"))

### write_csv(loss_results_df, paste0("job_output/", "sim2_", SIM_NUM, "_loss_results_df.csv"))
### bias as a function of actual WP
loss_results_df_binned = loss_results_df %>%
  mutate(wp_actual_bin = cut(wp_actual, NUM_WP_ACTUAL_BINS)) %>%
  group_by(model_type, wp_actual_bin) %>%
  summarise(
    logloss_wp_y = mean(logloss_wp_y),
    brier_wp_y = mean(brier_wp_y),
    brier_wp_pred = mean(brier_wp_pred),
    mae_wp_pred = mean(mae_wp_pred)
  ) %>% mutate(sim_num = SIM_NUM)
print(loss_results_df_binned)
write_csv(loss_results_df_binned, paste0("job_output/", sim_str, "_loss_df_binned.csv"))

##############################################################
### XGBoost predictions and Bootstrap Confidence Intervals ###
##############################################################

if (DO_BOOTSTRAP_ANALYSIS) {
  
  # boot_resample_props = c(1, 0.5)
  # boot_resample_props = c(1, 0.4)
  # boot_resample_props = c(1, 0.6, 0.55, 0.5, 0.45, 0.4, 0.35)
  # boot_resample_props = c(1, 0.7, 0.65, 0.6)
  # boot_resample_props = c(1, 0.65)
  boot_resample_props = c(1, 0.5)
  
  
  BRP = length(boot_resample_props)
  wp_preds_iidb = array(dim=c(nrow(test_df), B, BRP))
  wp_preds_cb = array(dim=c(nrow(test_df), B, BRP))
  wp_preds_rcb = array(dim=c(nrow(test_df), B, BRP))
  wp_preds_iidb_viz = array(dim=c(nrow(df_visualize_wp), B, BRP))
  wp_preds_cb_viz = array(dim=c(nrow(df_visualize_wp), B, BRP))
  wp_preds_rcb_viz = array(dim=c(nrow(df_visualize_wp), B, BRP))
  dimnames(wp_preds_iidb) = list( paste0("i=",1:nrow(test_df)), paste0("b=",1:B), paste0("brp=",boot_resample_props) )
  dimnames(wp_preds_cb) = list( paste0("i=",1:nrow(test_df)), paste0("b=",1:B), paste0("brp=",boot_resample_props) )
  dimnames(wp_preds_rcb) = list( paste0("i=",1:nrow(test_df)), paste0("b=",1:B), paste0("brp=",boot_resample_props) )

  ### get bootstrapped WP predictions for each brp
  for (j in 1:BRP) {
    brp = boot_resample_props[j]

      for (b in 1:B) {
      print(""); print(paste0("*** bootstrap sample b = ", b, " of B = ", B, " with brp ", brp, " idx ", j, " of ", BRP, " ***")); print(""); 
      
      ### get the b^th bootstrap re-sampled datasets
      if (b==1) { ### actual data
        train_df_iidb = train_df_OG
        train_df_cb = train_df_OG
        train_df_rcb = train_df_OG
      } else {
        ### iid bootstrap: sample plays with replacement
        num_resample_iidb = round(nrow(train_df_OG)*brp)
        set.seed(4458 + SIM_NUM*99 + b*221)
        train_game_idxs_iidb = sort(sample(1:nrow(train_df_OG), size=num_resample_iidb, replace=TRUE))
        train_df_iidb = train_df_OG[train_game_idxs_iidb,]
        ### clustered bootstrap: sample games with replacement
        num_resample_cb = round(length(train_game_idxs_OG)*brp)
        set.seed(8553 + SIM_NUM*88 + b*948)
        train_df_cb = tibble(
          g = sort(sample(train_game_idxs_OG, size=num_resample_cb, replace=TRUE))
        ) %>% mutate(ii = 1:n())
        train_df_cb = left_join(train_df_cb, train_df_OG)
        ### randomized clustered bootstrap: sample games with replacement, and within each game resample plays with replacement
        train_df_rcb = train_df_cb %>% select(g, ii, i)
        set.seed(3900 + SIM_NUM*203 + b*94)
        train_df_rcb = train_df_rcb %>% 
          group_by(ii) %>%
          sample_n(size = n(), replace = TRUE) %>%
          ungroup() %>%
          left_join(train_df_OG) %>%
          arrange(g,ii,n)
      }
      
      ### fit the b^th bootstrapped WP models
      ### iid bootstrap
      xgb_iidb <- fit_xgb(params, train_df_iidb, test_df, params$nrounds) 
      preds_iidb = predict(xgb_iidb, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      preds_iidb_viz = predict(xgb_iidb, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) ))
      ### clustered bootstrap model
      xgb_cb <- fit_xgb(params, train_df_cb, test_df, params$nrounds) 
      preds_cb = predict(xgb_cb, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      preds_cb_viz = predict(xgb_cb, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) ))
      ### randomized clustered bootstrap model
      xgb_rcb <- fit_xgb(params, train_df_rcb, test_df, params$nrounds) 
      preds_rcb = predict(xgb_rcb, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      preds_rcb_viz = predict(xgb_rcb, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) ))
      
      ### save the b^th bootstrapped WP predictions
      wp_preds_iidb[,b,j] = preds_iidb
      wp_preds_cb[,b,j] = preds_cb
      wp_preds_rcb[,b,j] = preds_rcb
      wp_preds_iidb_viz[,b,j] = preds_iidb_viz
      wp_preds_cb_viz[,b,j] = preds_cb_viz
      wp_preds_rcb_viz[,b,j] = preds_rcb_viz
    }
  }
  
  #################################
  ### Visualize Bootstrap Dists ###
  #################################
  
  alpha = 0.05 ### use nominal 90% CI because for B=100 we can use the 5^th and 95^th models.
  viz_df_boot = tibble()
  for (j in 1:BRP) {
    viz_df_boot_j = tibble(
      wp_pred_iidb_L = apply(wp_preds_iidb_viz[,1:B,j], 1, function(x) quantile(x, alpha) ),
      wp_pred_iidb_U = apply(wp_preds_iidb_viz[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
      wp_pred_cb_L = apply(wp_preds_cb_viz[,1:B,j], 1, function(x) quantile(x, alpha) ),
      wp_pred_cb_U = apply(wp_preds_cb_viz[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
      wp_pred_rcb_L = apply(wp_preds_rcb_viz[,1:B,j], 1, function(x) quantile(x, alpha) ),
      wp_pred_rcb_U = apply(wp_preds_rcb_viz[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
      brp = boot_resample_props[j]
    )
    viz_df_boot = bind_rows(viz_df_boot, viz_df_boot_j)
  }
  ### write viz_df_boot
  write_csv(viz_df_boot, paste0("job_output/", sim_str, "_viz_df_boot.csv"))
  
  #########################################
  ### Bootstrapped Confidence Intervals ###
  #########################################
  
  ##### coverages ##### 
  get_covg_df <- function(B, alpha = 0.05) {
    boot_results_df = tibble()
    for (j in 1:BRP) {
      boot_results_df_j = tibble(
        wp_actual = test_df$wp_actual,
        wp_pred_iidb_L = apply(wp_preds_iidb[,1:B,j], 1, function(x) quantile(x, alpha) ),
        wp_pred_iidb_M = apply(wp_preds_iidb[,1:B,j], 1, function(x) quantile(x, 0.5) ),
        wp_pred_iidb_U = apply(wp_preds_iidb[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
        wp_pred_cb_L = apply(wp_preds_cb[,1:B,j], 1, function(x) quantile(x, alpha) ),
        wp_pred_cb_M = apply(wp_preds_cb[,1:B,j], 1, function(x) quantile(x, 0.5) ),
        wp_pred_cb_U = apply(wp_preds_cb[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
        wp_pred_rcb_L = apply(wp_preds_rcb[,1:B,j], 1, function(x) quantile(x, alpha) ),
        wp_pred_rcb_M = apply(wp_preds_rcb[,1:B,j], 1, function(x) quantile(x, 0.5) ),
        wp_pred_rcb_U = apply(wp_preds_rcb[,1:B,j], 1, function(x) quantile(x, 1-alpha) ),
        brp = boot_resample_props[j]
      )
      ### widen CI for WP near 0 and 1
      boot_results_df_j = boot_results_df_j %>% mutate(
        wp_pred_iidb_2_L = ifelse(wp_pred_iidb_M <= 0.02, 0, wp_pred_iidb_L),
        wp_pred_cb_2_L = ifelse(wp_pred_cb_M <= 0.02, 0, wp_pred_cb_L),
        wp_pred_rcb_2_L = ifelse(wp_pred_rcb_M <= 0.02, 0, wp_pred_rcb_L),
        wp_pred_iidb_2_U = ifelse(wp_pred_iidb_M >= 0.98, 1, wp_pred_iidb_U),
        wp_pred_cb_2_U = ifelse(wp_pred_cb_M >= 0.98, 1, wp_pred_cb_U),
        wp_pred_rcb_2_U = ifelse(wp_pred_rcb_M >= 0.98, 1, wp_pred_rcb_U),
      )
      boot_results_df = bind_rows(boot_results_df, boot_results_df_j)
    }
    ##### coverages ##### 
    covg_results_df = boot_results_df %>%
      mutate(
        covered_iidb = as.numeric(wp_pred_iidb_L <= wp_actual & wp_actual <= wp_pred_iidb_U),
        covered_cb = as.numeric(wp_pred_cb_L <= wp_actual & wp_actual <= wp_pred_cb_U),
        covered_rcb = as.numeric(wp_pred_rcb_L <= wp_actual & wp_actual <= wp_pred_rcb_U),
        covered_iidb_L = as.numeric(wp_pred_iidb_L <= wp_actual),
        covered_cb_L = as.numeric(wp_pred_cb_L <= wp_actual),
        covered_rcb_L = as.numeric(wp_pred_rcb_L <= wp_actual),
        covered_iidb_U = as.numeric(wp_actual <= wp_pred_iidb_U),
        covered_cb_U = as.numeric(wp_actual <= wp_pred_cb_U),
        covered_rcb_U = as.numeric(wp_actual <= wp_pred_rcb_U),
        
        covered_iidb_2 = as.numeric(wp_pred_iidb_2_L <= wp_actual & wp_actual <= wp_pred_iidb_2_U),
        covered_cb_2 = as.numeric(wp_pred_cb_2_L <= wp_actual & wp_actual <= wp_pred_cb_2_U),
        covered_rcb_2 = as.numeric(wp_pred_rcb_2_L <= wp_actual & wp_actual <= wp_pred_rcb_2_U),
        covered_iidb_2_L = as.numeric(wp_pred_iidb_2_L <= wp_actual),
        covered_cb_2_L = as.numeric(wp_pred_cb_2_L <= wp_actual),
        covered_rcb_2_L = as.numeric(wp_pred_rcb_2_L <= wp_actual),
        covered_iidb_2_U = as.numeric(wp_actual <= wp_pred_iidb_2_U),
        covered_cb_2_U = as.numeric(wp_actual <= wp_pred_cb_2_U),
        covered_rcb_2_U = as.numeric(wp_actual <= wp_pred_rcb_2_U),
        
        length_iidb = wp_pred_iidb_U - wp_pred_iidb_L,
        length_cb = wp_pred_cb_U - wp_pred_cb_L,
        length_rcb = wp_pred_rcb_U - wp_pred_rcb_L,
        
        length_iidb_2 = wp_pred_iidb_2_U - wp_pred_iidb_2_L,
        length_cb_2 = wp_pred_cb_2_U - wp_pred_cb_2_L,
        length_rcb_2 = wp_pred_rcb_2_U - wp_pred_rcb_2_L,
      )
      
    return(covg_results_df)
  }
  # get_covg_df(B=1)
  
  # B_list = c(10,25,50,100,150,200,250,500,1000)
  # B_list = c(25)
  B_list = c(B)
  COVG_DF = tibble()
  COVG_DF_BINNED = tibble()
  for (BB in B_list) {
    if (BB <= B) {
      covg_results_df_BB = get_covg_df(B=BB)
      
      covg_df_BB = covg_results_df_BB %>%
        group_by(brp) %>%
        summarise(
          sim_num = SIM_NUM,
          B = B,
          covered_iidb = mean(covered_iidb),
          covered_cb = mean(covered_cb),
          covered_rcb = mean(covered_rcb),
          covered_iidb_L = mean(covered_iidb_L),
          covered_cb_L = mean(covered_cb_L),
          covered_rcb_L = mean(covered_rcb_L),
          covered_iidb_U = mean(covered_iidb_U),
          covered_cb_U = mean(covered_cb_U),
          covered_rcb_U = mean(covered_rcb_U),
          covered_iidb_2 = mean(covered_iidb_2),
          covered_cb_2 = mean(covered_cb_2),
          covered_rcb_2 = mean(covered_rcb_2),
          covered_iidb_2_L = mean(covered_iidb_2_L),
          covered_cb_2_L = mean(covered_cb_2_L),
          covered_rcb_2_L = mean(covered_rcb_2_L),
          covered_iidb_2_U = mean(covered_iidb_2_U),
          covered_cb_2_U = mean(covered_cb_2_U),
          covered_rcb_2_U = mean(covered_rcb_2_U),
          length_iidb = mean(length_iidb),
          length_cb = mean(length_cb),
          length_rcb = mean(length_rcb),
          length_iidb_2 = mean(length_iidb_2),
          length_cb_2 = mean(length_cb_2),
          length_rcb_2 = mean(length_rcb_2),
        )
      
      covg_df_BB_binned = covg_results_df_BB %>%
        mutate(wp_actual_bin = cut(wp_actual, NUM_WP_ACTUAL_BINS)) %>%
        group_by(brp, wp_actual_bin) %>%
        summarise(
          sim_num = SIM_NUM,
          B = B,
          covered_iidb = mean(covered_iidb),
          covered_cb = mean(covered_cb),
          covered_rcb = mean(covered_rcb),
          covered_iidb_L = mean(covered_iidb_L),
          covered_cb_L = mean(covered_cb_L),
          covered_rcb_L = mean(covered_rcb_L),
          covered_iidb_U = mean(covered_iidb_U),
          covered_cb_U = mean(covered_cb_U),
          covered_rcb_U = mean(covered_rcb_U),
          length_iidb = mean(length_iidb),
          length_cb = mean(length_cb),
          length_rcb = mean(length_rcb),
          covered_iidb_2 = mean(covered_iidb_2),
          covered_cb_2 = mean(covered_cb_2),
          covered_rcb_2 = mean(covered_rcb_2),
          covered_iidb_2_L = mean(covered_iidb_2_L),
          covered_cb_2_L = mean(covered_cb_2_L),
          covered_rcb_2_L = mean(covered_rcb_2_L),
          covered_iidb_2_U = mean(covered_iidb_2_U),
          covered_cb_2_U = mean(covered_cb_2_U),
          covered_rcb_2_U = mean(covered_rcb_2_U),
          length_iidb_2 = mean(length_iidb_2),
          length_cb_2 = mean(length_cb_2),
          length_rcb_2 = mean(length_rcb_2),
        )
      
      COVG_DF = bind_rows(COVG_DF, covg_df_BB)
      COVG_DF_BINNED = bind_rows(COVG_DF_BINNED, covg_df_BB_binned)
      # print(data.frame(covg_df_BB))
      # write_csv(covg_df_BB, paste0("job_output/", "sim2_", SIM_NUM, "_covg_df_B", BB, ".csv"))
    }
  }
  print(data.frame(COVG_DF))
  write_csv(COVG_DF, paste0("job_output/", sim_str, "_covg_df.csv"))

  print(data.frame(COVG_DF_BINNED))
  write_csv(COVG_DF_BINNED, paste0("job_output/", sim_str, "_covg_df_binned.csv"))
}
