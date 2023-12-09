
source("../../0_clean_lm.R")
HYPERPARAM_COMBO_IDX = 1 #1 #2
source("sim_2_main.R")

########################################################################
### Load the loss, CI coverage, and CI length data from all the sims ###
########################################################################

loss_df = tibble()
loss_df_binned = tibble()
covg_df = tibble()
covg_df_binned = tibble()

M = 25
for (SIM_NUM in 1:M) {
  sim_str = paste0("sim2_h", HYPERPARAM_COMBO_IDX, "_G", G, "_N", N, "_K", K, "_L", L, "_m", SIM_NUM)
  print(sim_str)
  
  loss_df_s = read_csv(paste0("job_output/", sim_str, "_loss_df.csv"), show_col_types = FALSE)
  loss_df = bind_rows(loss_df, loss_df_s)
  
  loss_df_binned_s = read_csv(paste0("job_output/", sim_str, "_loss_df_binned.csv"), show_col_types = FALSE)
  loss_df_binned = bind_rows(loss_df_binned, loss_df_binned_s)
  
  covg_df_s = read_csv(paste0("job_output/", sim_str, "_covg_df.csv"), show_col_types = FALSE)
  covg_df = bind_rows(covg_df, covg_df_s)
  
  covg_df_binned_s = read_csv(paste0("job_output/", sim_str, "_covg_df_binned.csv"), show_col_types = FALSE)
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

### loss DF 
loss_df_A = 
  loss_df %>%
  summarise(
    # logloss_wp_y = mean(logloss_wp_y),
    # sd_logloss_wp_y = sd(logloss_wp_y),
    # brier_wp_y = mean(brier_wp_y),
    # sd_brier_wp_y = sd(brier_wp_y),
    # brier_wp_pred = mean(brier_wp_pred),
    # sd_brier_wp_pred = sd(brier_wp_pred),
    avg_mae_wp_pp = mean(mae_wp_pred),
    se_mae_wp_pp = sd(mae_wp_pred)/sqrt(n()),
    mae_wp_pp_L = avg_mae_wp_pp - 2*se_mae_wp_pp,
    mae_wp_pp_U = avg_mae_wp_pp + 2*se_mae_wp_pp,
  ) %>% 
  round_df(digits=6) 
print(data.frame(loss_df_A))
# write_csv(loss_df_A, paste0("results/", sim_str_0, "_loss.csv"))

### CI (covg,length) DF 
covg_df_A = 
  covg_df %>%
  group_by(B,brp) %>%
  summarise(
    covered_iidb = mean(covered_iidb),
    covered_cb = mean(covered_cb),
    covered_rcb = mean(covered_rcb),
    covered_iidb_2 = mean(covered_iidb_2),
    covered_cb_2 = mean(covered_cb_2),
    covered_rcb_2 = mean(covered_rcb_2),
    length_iidb = mean(length_iidb),
    length_cb = mean(length_cb),
    length_rcb = mean(length_rcb),
    length_iidb_2 = mean(length_iidb_2),
    length_cb_2 = mean(length_cb_2),
    length_rcb_2 = mean(length_rcb_2),
  ) %>%
  round_df(digits=4)
print(data.frame(covg_df_A))
# write_csv(covg_df_A, paste0("results/", sim_str_0, "_covg_len.csv"))

### CI (covg,length) DF 
covg_df_B = 
  covg_df %>%
  ungroup() %>%
  # filter(brp %in% c(1, 0.4)) %>% #FIXME
  group_by(B,brp) %>%
  summarise(
    avg_covg_iidb_2 = mean(covered_iidb_2),
    twice_se_covg_iidb_2 = 2*sd(covered_iidb_2)/sqrt(n()),
    avg_covg_cb_2 = mean(covered_cb_2),
    twice_se_covg_cb_2 = 2*sd(covered_cb_2)/sqrt(n()),
    avg_covg_rcb_2 = mean(covered_rcb_2),
    twice_se_covg_rcb_2 = 2*sd(covered_rcb_2)/sqrt(n()),
    
    avg_length_iidb_2 = mean(length_iidb_2),
    twice_se_length_iidb_2 = 2*sd(length_iidb_2)/sqrt(n()),
    avg_length_cb_2 = mean(length_cb_2),
    twice_se_length_cb_2 = 2*sd(length_cb_2)/sqrt(n()),
    avg_length_rcb_2 = mean(length_rcb_2),
    twice_se_length_rcb_2 = 2*sd(length_rcb_2)/sqrt(n()),
  ) %>%
  round_df(digits=4)
covg_df_B
# write_csv(covg_df_B, paste0("results/", sim_str_0, "_covg_len_1.csv"))

### save loss, CI covg, and CI length results
library(htmlTable)
library(kableExtra)
plot_combined_0 = concatHtmlTables(list(htmlTable(loss_df_A, spacing = 2), 
                                        htmlTable(covg_df_A, spacing = 2)), 
                                   headers = c("losses", "CI coverages and lengths")
) 
# plot_combined_0
save_kable(plot_combined_0, file = paste0("results/", sim_str_0, "_plot_LossCovgLen.png"))

### save loss, CI covg, and CI length results
plot_combined_1 = concatHtmlTables(list(htmlTable(loss_df_A, spacing = 2), 
                                        htmlTable(covg_df_B, css.cell = "padding-left: .5em; padding-right: .2em;")), 
                                   headers = c("losses", "CI coverages and lengths")
)
# plot_combined_1
save_kable(plot_combined_1, file = paste0("results/", sim_str_0, "_plot_LossCovgLen_1.png"))

########################
### set BRP variable ###
########################

### chosen so that nominal 90% coverage actually yields marginal 90% coverage
# {
# brp_ = 0.4 #FIXME
for (brp_ in c(1, 0.5)) {
  
  #########################################
  ### plot XGB point predictions and CI ###
  #########################################
  
  m = 4 #FIXME
  sim_str = paste0("sim2_h", HYPERPARAM_COMBO_IDX, "_G", G, "_N", N, "_K", K, "_L", L, "_m", m)
  xgb_pp = xgb.load(paste0("job_output/", sim_str, "_xgb_pp.xgb"))
  viz_df_boot = read_csv(paste0("job_output/", sim_str, "_viz_df_boot.csv"), show_col_types = F)
  
  plot_wp_both_vs_time_d = visualize_wp(wp_true=TRUE, wp_xgb_model=xgb_pp, demo=TRUE)
  # plot_wp_both_vs_time_d
  ggsave(paste0("results/", sim_str, "_plot_wp_both_vs_time_d.png"), plot_wp_both_vs_time_d, width=8, height=6)
  
  plot_wp_both_boot_rcb_vs_time_d = visualize_wp(wp_true=TRUE, wp_boot_mat=viz_df_boot, boot_method="rcb", brp_=brp_, demo=TRUE)
  # plot_wp_both_boot_rcb_vs_time_d
  ggsave(paste0("results/", sim_str, "_brp=",brp_, "_plot_wp_both_boot_rcb_vs_time_d.png"), plot_wp_both_boot_rcb_vs_time_d, width=8, height=6)
  
  ###########################################################################
  ### PLOT loss, coverage, and CI length across different bins of WP_true ###
  ###########################################################################
  
  ## PLOT bias as a function of actual WP
  plot_loss_df_binned =
    loss_df_binned %>%
    group_by(wp_actual_bin) %>%
    summarise(
      avg_mae = mean(mae_wp_pred),
      se_mae = sd(mae_wp_pred)/sqrt(n())
    ) %>%
    mutate(
      mae_L = avg_mae - 2*se_mae,
      mae_U = avg_mae + 2*se_mae,
    ) %>%
    ggplot(aes(x = wp_actual_bin, y = avg_mae)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=mae_L, ymax=mae_U)) +
    ylab("MAE") +
    xlab("true win probability") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
  # plot_loss_df_binned
  # ggsave(paste0("results/", sim_str_0, "_plot_loss_df_binned.png"), plot_loss_df_binned, width=15, height=5)
  
  ## PLOT CI covg as a function of actual WP
  plot_covg_df_binned = 
    covg_df_binned %>%
    filter(brp == brp_) %>%
    group_by(wp_actual_bin) %>%
    summarise(
      avg_covg_rcb_2 = mean(covered_rcb_2),
      se_covg_rcb_2 = sd(covered_rcb_2)/sqrt(n()),
    ) %>%
    mutate(
      covg_rcb_2_L = avg_covg_rcb_2 - 2*se_covg_rcb_2,
      covg_rcb_2_U = avg_covg_rcb_2 + 2*se_covg_rcb_2,
    ) %>%
    pivot_longer(cols = c(avg_covg_rcb_2)) %>%
    ggplot(aes(x = wp_actual_bin, y=value)) +
    geom_point() +
    geom_errorbar(aes(ymin=covg_rcb_2_L, ymax=covg_rcb_2_U)) +
    theme(
      legend.title=element_text(size=15),
      legend.text=element_text(size=15)
    ) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    ylab("CI coverage") +
    xlab("true win probability") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
  # plot_covg_df_binned
  # ggsave(paste0("results/", sim_str_0, "_brp=",brp_, "_plot_covg_df_binned.png"), plot_covg_df_binned, width=15, height=5)
  
  ## PLOT CI length as a function of actual WP
  plot_ci_len_df_binned = 
    covg_df_binned %>%
    filter(brp == brp_) %>%
    group_by(wp_actual_bin) %>%
    summarise(
      avg_length_rcb_2 = mean(length_rcb_2),
      se_length_rcb_2 = sd(length_rcb_2)/sqrt(n()),
    ) %>%
    mutate(
      length_rcb_2_L = avg_length_rcb_2 - 2*se_length_rcb_2,
      length_rcb_2_U = avg_length_rcb_2 + 2*se_length_rcb_2,
    ) %>%
    pivot_longer(cols = c(avg_length_rcb_2)) %>%
    ggplot(aes(x = wp_actual_bin, y=value)) +
    geom_point() +
    geom_errorbar(aes(ymin=length_rcb_2_L, ymax=length_rcb_2_U)) +
    theme(
      legend.title=element_text(size=15), 
      legend.text=element_text(size=15)
    ) +
    geom_point(size=2) +
    ylab("CI length") +
    xlab("true win probability") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
  # plot_ci_len_df_binned
  # ggsave(paste0("results/", sim_str_0, "_brp=",brp_, "_plot_ci_len_df_binned.png"), plot_ci_len_df_binned, width=15, height=5)
  
  ### combined plot
  plot_combined = cowplot::plot_grid(plot_loss_df_binned, plot_covg_df_binned, plot_ci_len_df_binned, ncol=1)
  # plot_combined
  save_plot(paste0("results/", sim_str_0,  "_brp=",brp_, "_plot_wp_binned.png"), plot_combined, base_width=18, base_height=21)
  
}
