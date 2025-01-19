
source("../0_clean_lm.R")
M = 100
B = 100
# M = 100
# B = 500

ddf_all = tibble()
for (m in 1:M) {
  param_str = paste0("m=",m,"_B=",B)
  print(param_str)
  filename_ddf = paste0("job_output/stability_analysis_ddf_",param_str,".csv")
  ddf1 = read_csv(filename_ddf, show_col_types = F)
  ddf1 = ddf1 %>% mutate(i = 1:n(), m = m, B = B)
  ddf_all = bind_rows(ddf_all, ddf1)
}

###############################################################
### how do the decision buckets {confident, lean, nebulous} ###
### change across different draws of the bootstrap?         ###
###############################################################

df_summary_2 =
  ddf_all %>%
  mutate(
    dec_cat = case_when(
      0.83 <= prop_decision ~ "confident",
      0.67 <= prop_decision & prop_decision < 0.83 ~ "lean",
      prop_decision < 0.67 ~ "nebulous",
      # 0.9 <= prop_decision ~ "confident",
      # 0.75 <= prop_decision & prop_decision < 0.9 ~ "lean",
      # prop_decision < 0.75 ~ "nebulous",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(i) %>%
  summarise(
    p_confident = mean(dec_cat == "confident"),
    p_lean = mean(dec_cat == "lean"),
    p_nebulous = mean(dec_cat == "nebulous"),
  ) %>%
  mutate(
    p = pmax(p_confident, p_lean, p_nebulous)
  )
df_summary_2

table(df_summary_2$p)

plot_boot_stability_2 = 
  df_summary_2 %>%
  ggplot(aes(x = p)) +
  geom_histogram(fill="black") +
  geom_vline(aes(xintercept = mean(p)), color="dodgerblue2", linewidth=1)
# plot_boot_stability_2
ggsave(paste0("results_stability_analysis/plot_boot_stability_2_",param_str,".png"), 
       plot_boot_stability_2, width=8, height=6)


# ######################################################################
# ### how does boot% change across different draws of the bootstrap? ###
# ######################################################################
# 
# ddf_summary =
#   ddf_all %>%
#   group_by(i) %>%
#   summarise(
#     mean_prop_decision = mean(prop_decision),
#     # se_prop_decision = sd(prop_decision)/sqrt(n()),
#     twice_sd_prop_dec = 2*sd(prop_decision)
#   )
# ddf_summary
# 
# xlab_ = paste0("for each fourth-down play, ", 
#                "twice the s.d. of boot%", 
#                "\nacross M = ", M, " draws of the bootstrap with B = ", B)
# 
# plot_boot_stability = 
#   ddf_summary %>%
#   ggplot(aes(x = twice_sd_prop_dec)) +
#   geom_histogram(fill="black", bins=100) +
#   geom_vline(aes(xintercept = mean(twice_sd_prop_dec)), 
#              color="dodgerblue2", linewidth=1) +
#   xlab(xlab_) +
#   # labs(title = paste0("B = ", B)) +
#   labs(title = paste0("bootstrap stability histogram for B = ", B)) +
#   scale_x_continuous(breaks=seq(0,1,by=0.05), labels = percent, limits = c(0,0.15))
# # plot_boot_stability
# ggsave(paste0("results_stability_analysis/plot_boot_stability_",param_str,".png"), 
#               plot_boot_stability, width=8, height=6)
# 
# # alpha = 0.05
# # ddf_BS =
# #   ddf_all %>%
# #   group_by(i) %>%
# #   mutate(
# #     BP_L = quantile(prop_decision, alpha),
# #     BP_U = quantile(prop_decision, 1-alpha),
# #     BS = BP_U - BP_L
# #   )
# # ddf_BS
# # 
# # ddf_BS %>%
# #   ggplot(aes(x = BS)) +
# #   geom_histogram(fill="black", bins=100) +
# #   geom_vline(aes(xintercept = mean(BS)), 
# #              color="dodgerblue2", linewidth=1) +
# #   # xlab(xlab_) +
# #   # labs(title = paste0("B = ", B)) +
# #   labs(title = paste0("bootstrap stability histogram for B = ", B)) +
# #   scale_x_continuous(breaks=seq(0,1,by=0.05), labels = percent, limits = c(0,0.2))

# ######################################################################################
# ### how does the majority decision change across different draws of the bootstrap? ###
# ######################################################################################
# 
# df_summary_1 =
#   ddf_all %>%
#   group_by(i) %>%
#   summarise(
#     p_go = sum(decision == "Go"),
#     p_fg = sum(decision == "FG"),
#     p_punt = sum(decision == "Punt"),
#     M = n()
#   ) %>%
#   mutate(
#     p_go = p_go/M,
#     p_fg = p_fg/M,
#     p_punt = p_punt/M,
#     p = pmax(p_go, p_fg, p_punt),
#     BS = 1-p
#   ) 
# df_summary_1
# 
# table(df_summary_1$BS)
# 
# sum(df_summary_1$BS == 0)/nrow(df_summary_1)



