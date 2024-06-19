
source("../../0_clean_lm.R")

###########################
### get hyperparameters ###
###########################

source("sim_main.R")
N = 56
K = 56
# G = 4101
G = 4096
g = G*(K/N)
sim_str_0 = get_param_combo_str_0(g,G,N,K)
print(sim_str_0)
phi_vec = c(1, 0.75, 0.5, 0.4, 0.3)

########################################################################
### Load the loss, CI coverage, and CI length data from all the sims ###
########################################################################

# loss_df = tibble()
loss_df_binned = tibble()
covg_df = tibble()
covg_df_binned = tibble()

M = 100
for (m in 1:M) {
  sim_str = get_param_combo_str(g,G,N,K,m)
  print(sim_str)
  
  # loss_df_s = read_csv(paste0("xgb_covg/", sim_str, "_loss_df.csv"), show_col_types = FALSE)
  # loss_df = bind_rows(loss_df, loss_df_s)
  
  loss_df_binned_s = read_csv(paste0("xgb_covg/", sim_str, "_loss_df_binned.csv"), show_col_types = FALSE)
  loss_df_binned = bind_rows(loss_df_binned, loss_df_binned_s)
  
  covg_df_s = read_csv(paste0("xgb_covg/", sim_str, "_covg_df.csv"), show_col_types = FALSE)
  covg_df = bind_rows(covg_df, covg_df_s)
  
  covg_df_binned_s = read_csv(paste0("xgb_covg/", sim_str, "_covg_df_binned.csv"), show_col_types = FALSE)
  covg_df_binned = bind_rows(covg_df_binned, covg_df_binned_s)
  
  # B_list = c(10,25)
  # for (BB in B_list) {
  #   if (B >= BB) {
  #     covg_df_BB = read_csv(paste0("job_output/", sim_str, "_covg_df_B", BB, ".csv"))
  #     covg_df = bind_rows(covg_df, covg_df_BB)
  #   }
  # }
}

#################################################################
### Aggregate the Loss, Coverage, and Length over the 25 sims ###
#################################################################

# ### loss DF 
# loss_df_A = 
#   loss_df %>%
#   summarise(
#     # logloss_wp_y = mean(logloss_wp_y),
#     # sd_logloss_wp_y = sd(logloss_wp_y),
#     # brier_wp_y = mean(brier_wp_y),
#     # sd_brier_wp_y = sd(brier_wp_y),
#     # brier_wp_pred = mean(brier_wp_pred),
#     # sd_brier_wp_pred = sd(brier_wp_pred),
#     avg_mae_wp_pp = mean(mae_wp_pred),
#     se_mae_wp_pp = sd(mae_wp_pred)/sqrt(n()),
#     mae_wp_pp_L = avg_mae_wp_pp - 2*se_mae_wp_pp,
#     mae_wp_pp_U = avg_mae_wp_pp + 2*se_mae_wp_pp,
#   ) %>% 
#   round_df(digits=6) 
# print(data.frame(loss_df_A))
# # write_csv(loss_df_A, paste0("results/", sim_str_0, "_loss.csv"))

### CI (covg,length) DF 
covg_df_A = 
  covg_df %>%
  group_by(B,boot_method,phi) %>%
  summarise(
    covg = mean(covered),
    covg_2 = mean(covered_2),
    width = mean(width),
    width_2 = mean(width_2),
    .groups = "drop"
  ) %>%
  arrange(-covg) %>%
  round_df(digits=4)
print(data.frame(covg_df_A))
write_csv(covg_df_A, paste0("plots/", sim_str_0, "_results_covg_len.csv"))
# htmlTable::htmlTable(covg_df_A)

### CI (covg,length) DF 
covg_df_B = 
  covg_df %>%
  group_by(B,boot_method,phi) %>%
  summarise(
    covg = mean(covered),
    twice_se_covg = 2*sd(covered)/sqrt(M),
    covg_2 = mean(covered_2),
    twice_se_covg_2 = 2*sd(covered_2)/sqrt(M),
    width_ = mean(width),
    twice_se_width = 2*sd(width)/sqrt(M),
    width_2_ = mean(width_2),
    twice_se_width_2 = 2*sd(width_2)/sqrt(M),
    .groups = "drop"
  ) %>%
  arrange(-covg) %>%
  round_df(digits=4)
print(data.frame(covg_df_B))
write_csv(covg_df_B, paste0("plots/", sim_str_0, "_results2_covg_len.csv"))
# htmlTable::htmlTable(covg_df_B)

# ### save loss, CI covg, and CI length results
# library(htmlTable)
# library(kableExtra)
# plot_combined_0 = concatHtmlTables(list(htmlTable(loss_df_A, spacing = 2), 
#                                         htmlTable(covg_df_A, spacing = 2)), 
#                                    headers = c("losses", "CI coverages and lengths")
# ) 
# # plot_combined_0
# save_kable(plot_combined_0, file = paste0("results/", sim_str_0, "_plot_LossCovgLen.png"))
# 
# ### save loss, CI covg, and CI length results
# plot_combined_1 = concatHtmlTables(list(htmlTable(loss_df_A, spacing = 2), 
#                                         htmlTable(covg_df_B, css.cell = "padding-left: .5em; padding-right: .2em;")), 
#                                    headers = c("losses", "CI coverages and lengths")
# )
# # plot_combined_1
# save_kable(plot_combined_1, file = paste0("results/", sim_str_0, "_plot_LossCovgLen_1.png"))

###########################################################################
### PLOT loss, coverage, and CI length across different bins of WP_true ###
###########################################################################

## PLOT bias as a function of actual WP
plot_loss_df_binned =
  loss_df_binned %>%
  group_by(wp_actual_bin) %>%
  summarise(
    avg_bias_abs = mean(bias_abs),
    se_bias_abs = sd(bias_abs)/sqrt(n())
  ) %>%
  mutate(
    bias_L = avg_bias_abs - 2*se_bias_abs,
    bias_U = avg_bias_abs + 2*se_bias_abs,
  ) %>%
  ggplot(aes(x = wp_actual_bin, y = avg_bias_abs)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=bias_L, ymax=bias_U)) +
  ylab("|bias|") +
  xlab("true win probability") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
# plot_loss_df_binned
ggsave(paste0("plots/", sim_str_0, "_plot_loss_df_binned.png"), plot_loss_df_binned, width=15, height=5)

for (phi_ in phi_vec) {
  print(paste0("phi=",phi_))
  
  ## PLOT CI covg as a function of actual WP
  plot_covg_df_binned = 
    covg_df_binned %>%
    filter(boot_method=="RCB", phi == phi_) %>%
    group_by(wp_actual_bin) %>%
    summarise(
      avg_covg_2 = mean(covered_2),
      se_covg_2 = sd(covered_2)/sqrt(n()),
    ) %>%
    mutate(
      covg_2_L = avg_covg_2 - 2*se_covg_2,
      covg_2_U = avg_covg_2 + 2*se_covg_2,
    ) %>%
    pivot_longer(cols = c(avg_covg_2)) %>%
    ggplot(aes(x = wp_actual_bin, y=value)) +
    geom_point() +
    geom_errorbar(aes(ymin=covg_2_L, ymax=covg_2_U)) +
    theme(
      legend.title=element_text(size=15),
      legend.text=element_text(size=15)
    ) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    ylab("CI coverage") +
    xlab("true win probability") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
  # plot_covg_df_binned
  ggsave(paste0("plots/", sim_str_0,"_plot_ci_covg_df_binned","_phi",phi_,".png"), plot_covg_df_binned, width=15, height=5)
  
  ## PLOT CI length as a function of actual WP
  plot_ci_len_df_binned = 
    covg_df_binned %>%
    filter(boot_method=="RCB", phi == phi_) %>%
    group_by(wp_actual_bin) %>%
    summarise(
      avg_length_2 = mean(width_2),
      se_length_2 = sd(width_2)/sqrt(n()),
    ) %>%
    mutate(
      length_2_L = avg_length_2 - 2*se_length_2,
      length_2_U = avg_length_2 + 2*se_length_2,
    ) %>%
    pivot_longer(cols = c(avg_length_2)) %>%
    ggplot(aes(x = wp_actual_bin, y=value)) +
    geom_point() +
    geom_errorbar(aes(ymin=length_2_L, ymax=length_2_U)) +
    theme(
      legend.title=element_text(size=15), 
      legend.text=element_text(size=15)
    ) +
    geom_point(size=2) +
    ylab("CI width") +
    xlab("true win probability") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
  # plot_ci_len_df_binned
  ggsave(paste0("plots/", sim_str_0,"_plot_ci_width_df_binned","_phi",phi_,".png"), plot_ci_len_df_binned, width=15, height=5)
  
  ### combined plot
  plot_combined = cowplot::plot_grid(plot_loss_df_binned, plot_covg_df_binned, plot_ci_len_df_binned, ncol=1)
  # plot_combined
  save_plot(paste0("plots/", sim_str_0,"_plot_wp_binned","_phi",phi_,".png"), plot_combined, base_width=18, base_height=21)
}

########################
### Visualize XGB WP ###
########################

# ms = 5
ms = c(20,30)
# phis = c(1,0.75,0.5,0.25)
phis = phi_vec
for (m in ms) {
  WP_true = get_WP_true_mat(N)
  sim_str = get_param_combo_str(g,G,N,K,m)
  xgb_pp = xgb.load(paste0("xgb_models/xgb_", sim_str, ".xgb"))
  viz_df_boot = readRDS(paste0("xgb_covg/", sim_str, "_viz_df_boot.rds"))
  
  ### visualize the XGBoost WP model
  WP_true = get_WP_true_mat(N)
  plot_wp_both_vs_time_d = visualize_wp(WP_true, N=N, wp_true=TRUE, wp_xgb_model=xgb_pp, demo=TRUE, option=1)
  ggsave(paste0("plots/", sim_str, "_plot_wp_true_vs_time_d_both.png"), plot_wp_both_vs_time_d, width=8, height=6)
  
  ### visualize the WP CIs
  for (phi in phis) {
    plot_wp_both_boot_rcb_vs_time_d = visualize_wp(WP_true, N=N, wp_true=TRUE, wp_xgb_model=xgb_pp, wp_boot_mat=viz_df_boot, boot_method_="RCB", phi_=phi, demo=TRUE, option=1)
    ggsave(paste0("plots/", sim_str, "_plot_wp_both_boot_","RCB","_phi",phi,"_vs_time_d.png"), plot_wp_both_boot_rcb_vs_time_d, width=8, height=6)
  }
}
