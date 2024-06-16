
source("../../0_clean_lm.R")
source("sim_main.R")

##################################
### load bias-variance results ###
##################################

### generate testing dataset 
set.seed(8753298)
df_test = simulate_football_season(G=1000,N=56,K=1)
print(df_test)

# gs_vec = 4**(4:8)
gs_vec = round(4**seq(4,7.5,by=0.5))
gs_vec
df_results = tibble()

for (g in gs_vec) {
  for (K in c(N,1)) {
    G = g*(N/K) ### number of games
    df_test_gK = tibble()
    for (m in 1:M) {
      sim_str = get_param_combo_str(g,G,N,K,m)
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
      mutate(bv_rsum_m = sqrt(bias_sq_m + var_m)) %>%
      summarise(
        bias_sq = mean(bias_sq_m), 
        var = mean(var_m), 
        bv_rsum = mean(bv_rsum_m), 
        se_bias_sq = sd(bias_sq_m)/sqrt(M),
        se_var = sd(var_m)/sqrt(M),
        se_bv_rsum = sd(bv_rsum_m)/sqrt(M)
      ) %>%
      mutate(g=g,G=G,N=N,K=K)
    df_results = bind_rows(df_results, df_results_gK)
  }
}

#################################
### plot bias-variance curves ###
#################################

df_results_1 = 
  df_results %>%
  pivot_longer(-c(g,G,N,K)) %>%
  mutate(
    se = startsWith(name, "se_"),
    name = str_remove(name, "se_"),
  ) %>%
  pivot_wider(names_from = se, values_from = value, names_prefix = "SE_") %>%
  rename(value = SE_FALSE, SE = SE_TRUE) %>%
  mutate(
    value_L = value - 2*SE,
    value_U = value + 2*SE
  )
df_results_1

### plot bias & variance as a function of g
label_names = as_labeller(c(
  'bias_sq'="bias^2",
  'var'="variance",
  'bv_rsum'="sqrt(bias^2 + var)"
), label_parsed)
plot_bias_var = 
  df_results_1 %>%
  # filter(log(g,4)<8) %>%
  filter(log(g,4)>4) %>%
  ggplot(aes(
    y = value, 
    x=log(g,base=4),
    color=factor(K), fill = factor(K)
  )) +
  facet_wrap(~factor(name, levels=c("bias_sq", "var", "bv_rsum")),
             scales = "free_y",
             labeller = label_names) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
  xlab(TeX("$\\log_4(g)$")) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name="K", values=c("firebrick", "dodgerblue2")) +
  scale_color_manual(name="K", values=c("firebrick", "dodgerblue2"))
# plot_bias_var
ggsave("plots/plot_bias_var.png", plot_bias_var, width=13, height=4)

### plot loss ratio
df_loss_ratio = 
  df_results_1 %>%
  filter(name == "bv_rsum") %>%
  select(g,K,value,value_L,value_U) %>%
  pivot_wider(names_from="K", values_from=c(value,value_L,value_U), names_prefix="bv_rsum_K") %>%
  mutate(
    r = value_bv_rsum_K1/value_bv_rsum_K56,
    r_L = value_L_bv_rsum_K1/value_L_bv_rsum_K56,
    r_U = value_U_bv_rsum_K1/value_U_bv_rsum_K56,
  )
df_loss_ratio

plot_loss_ratio = 
  df_loss_ratio %>%
  drop_na() %>%
  ggplot(aes(x = g, y = r)) +
  # ggplot(aes(x = log(g,base=4), y = r)) +
  geom_line(linewidth=1) +
  # geom_ribbon(aes(ymin=r_L,ymax=r_U), alpha=0.375, linetype="dashed") +
  geom_point()
# plot_loss_ratio
ggsave("plots/plot_loss_ratio.png", plot_loss_ratio, width=5, height=3)

################################
### fit bias-variance curves ###
################################

### fit the loss curves 
df_results_for_biexp_model = 
  df_results_1 %>% 
  filter(name == "bv_rsum") %>%
  select(g,K,value) %>%
  bind_rows(
    # tibble(g = 1e5, value=1e-5, K = c(1,56))
  )
df_results_for_biexp_model

model_loss_K1 = nls(
  value ~ a + b*exp(-c*g) + d*exp(-f*g),
  # value ~ a + b*exp(-c*g),
  data = df_results_for_biexp_model%>% filter(K==1),
  start = list(a=0.02,b=0.06,c=0.001,d=0.01,f=0.0001)
  # start = list(a=0,b=0.1,c=0,d=0.05,f=0.1)
  # start = list(a=0.02,b=0.06,c=0.001)
)
model_loss_K1
model_loss_K56 = nls(
  value ~ a + b*exp(-c*g) + d*exp(-f*g),
  data = df_results_for_biexp_model%>% filter(K==56), #%>% filter(g < 4**(7.5)),
  start = list(a=0.02,b=0.06,c=0.001,d=0.01,f=0.0001)
)
model_loss_K56

pred_model_loss_K1 = function(g) {
  predict(model_loss_K1, tibble(g))
}

pred_model_loss_K56 = function(g) {
  predict(model_loss_K56, tibble(g))
}

# ggplot() +
#   xlim(c(0, 4e4)) + ylim(c(0,0.2)) +
#   geom_function(fun = pred_model_loss_K56)

### plot fitted loss curves on top of empirical loss curves
plot_ass = 
  df_results_1 %>%
  filter(name == "bv_rsum") %>%
  filter(log(g,4)>4) %>%
  ggplot(aes(
    y = value, 
    x=log(g,base=4),
    color=factor(K), fill = factor(K)
  )) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
  geom_function(
    fun = function(x) { pred_model_loss_K1(4**x) },
    linewidth=1,
    colour = "magenta"
  ) +
  geom_function(
    fun = function(x) { pred_model_loss_K56(4**x) },
    linewidth=1,
    colour = "cyan"
  ) +
  xlab(TeX("$\\log_4(g)$")) +
  ylab(expression(sqrt(bias^2 + var))) +
  scale_x_continuous(breaks = log(gs_vec,4)) +
  scale_fill_manual(name="K", values=c("firebrick", "dodgerblue2")) +
  scale_color_manual(name="K", values=c("firebrick", "dodgerblue2"))
# plot_ass

### plot fitted loss curves
plot_ass_1 = 
  df_results_1 %>%
  filter(name == "bv_rsum") %>%
  filter(log(g,4)>4) %>%
  ggplot(aes(
    y = value, 
    x=log(g,base=4),
  )) +
  geom_function(
    aes(color = "1"),
    fun = function(x) { pred_model_loss_K1(4**x) },
    linewidth=1,
  ) +
  geom_function(
    aes(color = "56"),
    fun = function(x) { pred_model_loss_K56(4**x) },
    linewidth=1,
  ) +
  xlab(TeX("$\\log_4(g)$")) +
  ylab(expression(sqrt(bias^2 + var))) +
  scale_x_continuous(breaks = log(gs_vec,4)) +
  scale_color_manual(name="K", values=c("firebrick", "dodgerblue2"))
# plot_ass_1
ggsave("plots/plot_loss_fitted_curves.png", plot_ass_1, width=6, height=4)

### solve for ESS
root_func = function(g) {
  function(g1) {
    pred_model_loss_K1(g1) - pred_model_loss_K56(g)
  }
}
# root_func(g=4101)

find_root <- function(g) {
  uniroot(root_func(g), interval=c(0,1e8))$root
  
}
sapply(4^seq(4.5,6.5,by=0.5), find_root)

### examples
print(find_root(g=4101))
print(find_root(g=4101)/4101)
print(find_root(g=8202))
print(find_root(g=8202)/8202)
print(find_root(g=2050))
print(find_root(g=2050)/2050)

### plot effective sample size
df_plot_ESS = 
  tibble(
    # g = 4^seq(4.5,6.5,by=0.5),
    g = seq(1000,10000,by=100),
  ) %>%
    rowwise() %>%
    mutate(
      g1 = find_root(g),
      ratio = g1/g,
    ) %>%
    ungroup() 
df_plot_ESS

xL = round(min(df_plot_ESS$g1)-50,-2)
xU = max(df_plot_ESS$g)
plot_ESS = 
  df_plot_ESS %>%
  ggplot(aes(
    y = g1,
    # y=ratio,
    x=g,
    # x=log(g,base=4),
  )) +
  # xlab(TeX("$\\log_4(g)$")) +
  # ylab("ESS") +
  # ylab(expression('ESS '*g*"'")) +
  ylab(expression('effective sample size '*g*"'")) +
  xlim(c(xL,xU)) + ylim(c(xL,xU)) +
  geom_line(linewidth=2) +
  theme(
    plot.margin = unit(c(0.2,1,0.2,0.2), "cm")
  ) +
  geom_point(data=tibble(g=4101,g1=find_root(g=4101)), color="firebrick", size=5, shape=16) 
plot_ESS
ggsave("plots/plot_ESS_curve.png", plot_ESS, width=6, height=4)




