
source("../../0_clean_lm.R")

###########################
### get hyperparameters ###
###########################

source("sim_main.R")
RETUNE_XGB = FALSE
NUM_WP_ACTUAL_BINS = 10
N = 56
K = 56
# G = 4101
G = 4096
g = G*(K/N)
WP_true = get_WP_true_mat(N)

#######################################
### Get XGBoost WP model and params ###
#######################################

# M = 2
# M = 100
M = 60
df_true_wp = visualize_wp(WP_true, N, wp_true=TRUE, option=3) 
# df_xgb_preds = df_true_wp
# df_xgb_preds$m = 0
for (m in 1:M) {
  sim_str = get_param_combo_str(g,G,N,K,m)
  print(sim_str)
  
  ### generate training dataset
  set.seed(23748 + m*143)
  df_train = simulate_football_season(G,N,K)
  # print(df_train)
  
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
  xgb_fit
  
  xgb_preds = visualize_wp(WP_true, N, wp_true=FALSE, wp_xgb_model=xgb_fit, option=3) 
  xgb_preds$m = m
  df_xgb_preds = bind_rows(df_xgb_preds, xgb_preds)
}
df_xgb_preds
table(df_xgb_preds$m)

##################################
### plot sampling distribution ###
##################################

df_plot_samplingDist = 
  df_xgb_preds %>%
  left_join(
    df_true_wp %>% select(-wp_type) %>% rename(wp_true = wp)
  ) %>%
  select(-wp_type) %>%
  rename(wp_pred = wp) %>%
  filter(x==3, s %in% c(2,1,0,-1)) 
df_plot_samplingDist

# my_palette <- c(
#   brewer.pal(name="PuRd",n=9)[c(4,5,7,9)]
# )

my_palette <- c(
  brewer.pal(name="Blues",n=9)[4:9],
  rev(brewer.pal(name="Purples",n=9)[6:8]),
  "magenta", "black",
  rev(brewer.pal(name="Greens",n=9)[2:9])
)[c(7,10,4,1)] 
  
plot_samplingDist = 
  df_plot_samplingDist %>%
  ggplot(aes(x=n, group = interaction(m, s), color=factor(s), fill =  factor(s))) +
  geom_line(aes(y=wp_pred), linewidth = 0.5, alpha=0.5) +
  geom_line(aes(y=wp_true), linewidth = 1, color="black") +
  # geom_ribbon(aes(ymin=`lower boot._wp`,ymax=`upper boot._wp`), alpha=0.5) +
  scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
  scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
  guides(color=guide_legend(title=" score\n differential\n s"),
         fill=guide_legend(title=" score\n differential\n s")) +
  guides(linetype=guide_legend(title="WP")) +
  scale_color_manual(values = my_palette) +
  scale_fill_manual(values = my_palette)
plot_samplingDist
ggsave(paste0("job_output/","WP_plot_samplingDist_N",N,".png"), 
       plot_samplingDist, width=7, height=5)

df_plot_unbiased = 
  df_plot_samplingDist %>%
  group_by(s,x,n) %>%
  reframe(wp_pred = mean(wp_pred), wp_true=unique(wp_true))  %>%
  pivot_longer(c(wp_pred, wp_true), names_to = "wp_type", values_to = "wp") %>%
  mutate(wp_type = str_remove(wp_type, "wp_"),
         wp_type = ifelse(wp_type=="pred", "ML", wp_type))
df_plot_unbiased
  
plot_unbiased = 
  df_plot_unbiased %>%
  ggplot(aes(x=n, y = wp, linetype=wp_type, color=factor(s), fill =  factor(s))) +
  geom_line(linewidth = 1) +
  # geom_ribbon(aes(ymin=`lower boot._wp`,ymax=`upper boot._wp`), alpha=0.5) +
  scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
  scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
  guides(color=guide_legend(title=" score\n differential\n s"),
         fill=guide_legend(title=" score\n differential\n s")) +
  guides(linetype=guide_legend(title="WP")) +
  scale_color_manual(values = my_palette) +
  scale_fill_manual(values = my_palette)
plot_unbiased
ggsave(paste0("job_output/","WP_plot_unbiased_N",N,".png"), 
       plot_unbiased, width=7, height=5)

