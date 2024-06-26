
### CALCULATE EFFECITVE SAMPLE SIZE ###
source("sim_bias_variance_main.R")

########################################
### load bias-variance sim 2 results ###
########################################

### generate testing dataset 
set.seed(8753298)
df_test = simulate_football_season(G=1000,N=56,K=1)
print(df_test)

### param combo dataframe for Bias-Variance Sim 2
df_bvsim_2 = 
  df_bvsim %>% 
  filter(bv_sim_idx == 2) %>%
  mutate(
    desc = case_when(
      G == zeta & K == N ~ paste0("(G=",zeta_str,", K=","T",")"),
      G == zeta & K == 1 ~ paste0("(G=",zeta_str,", K=",1,")"),
      G == zeta*N & K == 1 ~ paste0("(G=",zeta_str,"•T, K=",1,")"),
    ),
    desc1 = case_when(
      G == zeta & K == N ~ "A",
      G == zeta & K == 1 ~ "B",
      G == zeta*N & K == 1 ~ "C",
    ),
  )
df_bvsim_2

### Results for Bias-Variance Sim 2
df_results = tibble()
for (i in 1:nrow(df_bvsim_2)) {
  N = df_bvsim_2$N[i]
  K = df_bvsim_2$K[i]
  G = df_bvsim_2$G[i]
  zeta = df_bvsim_2$zeta[i]
  desc = df_bvsim_2$desc[i]
  desc1 = df_bvsim_2$desc1[i]
  bvsimidx = df_bvsim_2$bv_sim_idx[i]
  
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
    mutate(zeta=zeta, desc=desc, desc1=desc1)
    # mutate(df_bvsim_2[i,])
    # mutate(zeta=zeta,G=G,N=N,K=K)
  df_results = bind_rows(df_results, df_results_gK)
}
df_results

#################################
### plot bias-variance curves ###
#################################

df_results_1 = 
  df_results %>%
  pivot_longer(-c(zeta,desc,desc1)) %>%
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
colors_vec = c("dodgerblue2", "firebrick", "orange")
# names(colors_vec) = c(unique(df_results_1$desc)[1], unique(df_results_1$desc)[3], unique(df_results_1$desc)[2]) 
names(colors_vec) = unique(df_results_1$desc)

plot_bias_var_2A = 
  df_results_1 %>%
  filter(log(zeta,4)>4) %>%
  ggplot(aes(
    y = value, 
    x=log(zeta,base=4),
    color=desc, fill = desc
  )) +
  facet_wrap(~factor(name, levels=c("bias_sq", "var", "rmse")),
             scales = "free_y",
             labeller = label_names) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name="", values=colors_vec) +
  scale_color_manual(name="", values=colors_vec) +
  xlab(TeX("$log_4(zeta)$")) +
  theme(
    strip.text.x = element_text(size=30),
    legend.text = element_text(size=25),
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
  )
# plot_bias_var_2A
ggsave("plots/plot_bias_var_2A.png", plot_bias_var_2A, width=16, height=4)

plot_bias_var_2B = 
  df_results_1 %>%
  filter(desc1 != "B") %>%
  filter(log(zeta,4)>4) %>%
  ggplot(aes(
    y = value, 
    x=log(zeta,base=4),
    color=desc, fill = desc
  )) +
  facet_wrap(~factor(name, levels=c("bias_sq", "var", "rmse")),
             scales = "free_y",
             labeller = label_names) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name="", values=colors_vec) +
  scale_color_manual(name="", values=colors_vec) +
  xlab(TeX("$log_4(zeta)$")) +
  theme(
    strip.text.x = element_text(size=30),
    legend.text = element_text(size=25),
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
  )
# plot_bias_var_2B
ggsave("plots/plot_bias_var_2B.png", plot_bias_var_2B, width=16, height=4)

# ### plot loss ratio
# df_loss_ratio = 
#   df_results_1 %>%
#   filter(name == "bv_rsum") %>%
#   select(g,K,value,value_L,value_U) %>%
#   pivot_wider(names_from="K", values_from=c(value,value_L,value_U), names_prefix="bv_rsum_K") %>%
#   mutate(
#     r = value_bv_rsum_K1/value_bv_rsum_K56,
#     r_L = value_L_bv_rsum_K1/value_L_bv_rsum_K56,
#     r_U = value_U_bv_rsum_K1/value_U_bv_rsum_K56,
#   )
# df_loss_ratio
# 
# plot_loss_ratio = 
#   df_loss_ratio %>%
#   drop_na() %>%
#   ggplot(aes(x = g, y = r)) +
#   # ggplot(aes(x = log(g,base=4), y = r)) +
#   geom_line(linewidth=1) +
#   # geom_ribbon(aes(ymin=r_L,ymax=r_U), alpha=0.375, linetype="dashed") +
#   geom_point()
# # plot_loss_ratio
# ggsave("plots/plot_loss_ratio.png", plot_loss_ratio, width=5, height=3)

################################
### fit bias-variance curves ###
################################

### fit the loss curves 
df_results_for_biexp_model = 
  df_results_1 %>% 
  filter(name == "rmse") %>%
  filter(desc1 != "B") %>%
  select(zeta,desc,desc1,value) 
df_results_for_biexp_model

model_loss_K1 = nls(
  value ~ a + b*exp(-c*zeta) + d*exp(-f*zeta),
  data = df_results_for_biexp_model%>% filter(desc1=="C"),
  start = list(a=0.02,b=0.06,c=0.001,d=0.01,f=0.0001)
)
model_loss_K1
model_loss_K56 = nls(
  value ~ a + b*exp(-c*zeta) + d*exp(-f*zeta),
  data = df_results_for_biexp_model %>% filter(desc1=="A"), 
  start = list(a=0.02,b=0.06,c=0.001,d=0.01,f=0.0001)
)
model_loss_K56

pred_model_loss_K1 = function(zeta) {
  predict(model_loss_K1, tibble(zeta))
}

pred_model_loss_K56 = function(zeta) {
  predict(model_loss_K56, tibble(zeta))
}

# ggplot() +
#   xlim(c(0, 4e4)) + ylim(c(0,0.2)) +
#   geom_function(fun = pred_model_loss_K56)

### plot fitted loss curves on top of empirical loss curves
plot_ass = 
  df_results_1 %>%
  filter(name == "rmse") %>%
  filter(desc1 != "B") %>%
  filter(log(zeta,4)>4) %>%
  ggplot(aes(
    y = value, 
    x=log(zeta,base=4),
    color=desc, fill = desc
  )) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
  geom_function(
    fun = function(x) { pred_model_loss_K1(4**x) },
    linewidth=1,
    colour = "red"
  ) +
  geom_function(
    fun = function(x) { pred_model_loss_K56(4**x) },
    linewidth=1,
    colour = "cyan"
  ) +
  xlab(TeX("$\\log_4(zeta)$")) +
  ylab("RMSE") +
  # ylab(expression(sqrt(bias^2 + var))) +
  scale_x_continuous(breaks = log(zeta_vec,4)) +
  scale_fill_manual(name="", values=colors_vec) +
  scale_color_manual(name="", values=colors_vec)
# plot_ass
ggsave("plots/plot_loss_fitted_curves_0.png", plot_ass, width=7, height=4)

### plot fitted loss curves
plot_ass_1 = 
  df_results_1 %>%
  filter(name == "rmse") %>%
  filter(desc1 != "B") %>%
  filter(log(zeta,4)>4) %>%
  ggplot(aes(
    y = value, 
    x = log(zeta,base=4),
  )) +
  geom_function(
    aes(color = names(colors_vec)[3]),
    fun = function(x) { pred_model_loss_K1(4**x) },
    linewidth=1,
  ) +
  geom_function(
    aes(color = names(colors_vec)[1]),
    fun = function(x) { pred_model_loss_K56(4**x) },
    linewidth=1,
  ) +
  xlab(TeX("$\\log_4(zeta)$")) +
  ylab("RMSE") +
  # ylab(expression(sqrt(bias^2 + var))) +
  scale_x_continuous(breaks = log(zeta_vec,4)) +
  scale_color_manual(name="", values=colors_vec)
# plot_ass_1
ggsave("plots/plot_loss_fitted_curves_1.png", plot_ass_1, width=7, height=4)

### solve for ESS
root_func = function(zeta) {
  function(zeta1) {
    pred_model_loss_K1(zeta1) - pred_model_loss_K56(zeta)
  }
}
# root_func(g=4101)

find_root <- function(zeta) {
  uniroot(root_func(zeta), interval=c(0,1e8))$root
  
}
sapply(4^seq(4.5,6.5,by=0.5), find_root)

### examples
print(find_root(zeta=4101))
print(find_root(zeta=4101)/4101)
print(find_root(zeta=8202))
print(find_root(zeta=8202)/8202)
print(find_root(zeta=2050))
print(find_root(zeta=2050)/2050)

### plot effective sample size
df_plot_ESS = 
  tibble(
    # zeta = 4^seq(4.5,6.5,by=0.5),
    zeta = seq(1000,10000,by=100),
  ) %>%
    rowwise() %>%
    mutate(
      zeta1 = find_root(zeta),
      ratio = zeta1/zeta,
    ) %>%
    ungroup() 
df_plot_ESS

xL = round(min(df_plot_ESS$zeta1)-50,-2)
xU = max(df_plot_ESS$zeta)
plot_ESS = 
  df_plot_ESS %>%
  ggplot(aes(
    y = zeta1,
    # y=ratio,
    x=zeta,
    # x=log(g,base=4),
  )) +
  # xlab(TeX("$\\log_4(g)$")) +
  # ylab("ESS") +
  # ylab(expression('ESS '*g*"'")) +
  ylab(expression('effective sample size '*zeta*"'")) +
  xlab(TeX("$\\zeta$")) +
  xlim(c(xL,xU)) + ylim(c(xL,xU)) +
  geom_line(linewidth=2) +
  theme(
    plot.margin = unit(c(0.2,1,0.2,0.2), "cm")
  ) +
  geom_point(data=tibble(zeta=4101,zeta1=find_root(zeta=4101)), color="firebrick", size=5, shape=16) 
# plot_ESS
ggsave("plots/plot_ESS_curve.png", plot_ESS, width=6, height=4)




