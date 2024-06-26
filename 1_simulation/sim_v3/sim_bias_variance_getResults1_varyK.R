
### CALCULATE EFFECITVE SAMPLE SIZE ###
source("sim_bias_variance_main.R")

########################################
### load bias-variance sim 1 results ###
########################################

### generate testing dataset 
set.seed(8753298)
df_test = simulate_football_season(G=1000,N=56,K=1)
print(df_test)

### param combo dataframe for Bias-Variance Sim 2
df_bvsim_1 = 
  df_bvsim %>% 
  filter(bv_sim_idx == 1) %>%
  mutate(
    # desc = paste0("(",zeta_str,"=",zeta,", K=",K,", G=", G, ")")
  )
df_bvsim_1

# plot_bv1_title = paste0(zeta_str, "=", 4101, ", G=", )

### Results for Bias-Variance Sim 2
df_results = tibble()
for (i in 1:nrow(df_bvsim_1)) {
  N = df_bvsim_1$N[i]
  zeta = df_bvsim_1$zeta[i]
  K = df_bvsim_1$K[i]
  G = df_bvsim_1$G[i]
  
  df_test_gK = tibble()
  for (m in 1:M) {
    sim_str = get_param_combo_str(zeta,G,N,K,m)
    print(sim_str)
    
    xgb_filename = paste0("xgb_models/", "xgb_", sim_str, ".xgb")
    xgb_model = xgb.load(xgb_filename)
    
    df_test_gKm = df_test
    df_test_gKm$r = 1:nrow(df_test_gKm)
    df_test_gKm = 
      df_test_gKm %>% 
      mutate(
        wp_pred = predict_xgb(xgb_model, .),
        m = m
      )
    df_test_gK = bind_rows(df_test_gK, df_test_gKm)
  }
  
  df_results_gK = 
    df_test_gK %>%
    group_by(r) %>%
    mutate(mean_wp_pred = mean(wp_pred)) %>%
    group_by(m) %>%
    summarise(
      bias_sq_m = mean( (wp_actual - wp_pred)**2      ),
      var_m     = mean( (wp_pred   - mean_wp_pred)**2 ),
    ) %>%
    ungroup() %>%
    mutate(rmse_m = sqrt(bias_sq_m + var_m)) %>%
    summarise(
      bias_sq = mean(bias_sq_m), 
      var = mean(var_m), 
      rmse = mean(rmse_m), 
      se_bias_sq = sd(bias_sq_m)/sqrt(M),
      se_var = sd(var_m)/sqrt(M),
      se_rmse = sd(rmse_m)/sqrt(M)
    ) %>%
    mutate(K=K)
    # mutate(df_bvsim_1[i,])
    # mutate(zeta=zeta,G=G,N=N,K=K)
  df_results = bind_rows(df_results, df_results_gK)
}
df_results

#################################
### plot bias-variance curves ###
#################################

df_results_1 = 
  df_results %>%
  pivot_longer(-c(K)) %>%
  mutate(
    se = startsWith(name, "se_"),
    name = str_remove(name, "se_"),
  ) %>%
  pivot_wider(names_from = se, values_from = value, names_prefix = "SE_") %>%
  rename(value = SE_FALSE, SE = SE_TRUE) %>%
  mutate(
    value_L = value - 2*SE,
    value_U = value + 2*SE,
  )
df_results_1

### plot bias & variance as a function of zeta
label_names = as_labeller(c(
  'bias_sq'="bias^2",
  'var'="variance",
  'rmse'="RMSE"
), label_parsed)
# colors_vec = c("dodgerblue2", "firebrick", "orange")
# names(colors_vec) = c(unique(df_results_1$desc)[1], unique(df_results_1$desc)[3], unique(df_results_1$desc)[2]) 
# names(colors_vec) = unique(df_results_1$desc)

plot_bias_var_1A = 
  df_results_1 %>%
  filter(log(zeta,4)>4) %>%
  ggplot(aes(
    y = value, 
    x = K,
    # color=factor(K), fill = factor(K)
  )) +
  facet_wrap(~factor(name, levels=c("bias_sq", "var", "rmse")),
             scales = "free_y",
             labeller = label_names) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = value_L, ymax = value_U)) +
  geom_smooth(se=F, method = "lm", linewidth=1.5, color="gray60") +
  theme(
    strip.text.x = element_text(size=30),
    legend.text = element_text(size=25),
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
  )
# plot_bias_var_1A
ggsave("plots/plot_bias_var_1A.png", plot_bias_var_1A, width=16, height=4)

