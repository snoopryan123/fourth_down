
source("../../0_clean_lm.R")
source("sim_main.R")

#################################################
###  ###
#################################################

df_test = simulate_football_season(G=1000,N=56,K=1)
print(df_test)

gs_vec = 4**(4:8)
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

### plot
label_names = as_labeller(c(
  'bias_sq'="bias^2",
  'var'="variance",
  'bv_rsum'="sqrt(bias^2 + var)"
), label_parsed)
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

### loss ratio
df_loss_ratio = 
  df_results_1 %>%
  filter(name == "bv_rsum") %>%
  select(g,K,value) %>%
  pivot_wider(names_from="K", values_from=value, names_prefix="bv_rsum_K") %>%
  mutate(r = bv_rsum_K1/bv_rsum_K56)
df_loss_ratio

df_loss_ratio %>%
  drop_na() %>%
  ggplot(aes(x = g, y = r)) +
  # ggplot(aes(x = log(g,base=4), y = r)) +
  geom_line(linewidth=1) +
  geom_point()

################################################################################





### ESS function
df_results_1 %>%
  filter(name == "bv_rsum") %>%
  ggplot(aes(y = value, 
             # x=g,
             x=log(g,base=4),
             color=factor(K)
  )) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  scale_fill_manual(name="K", values=c("firebrick", "dodgerblue2")) +
  scale_color_manual(name="K", values=c("firebrick", "dodgerblue2"))
  
df_results_1

model_power_law = lm(
  # log(value) ~ factor(K):I(-log(g) )
  log(value) ~ factor(K):(g + log(g))
  ,data = df_results_1 %>% filter(name == "bv_rsum")
)
model_power_law
### plot
df_results_2 =
  df_results_1 %>%
  mutate(
    linepred = predict(model_power_law, .),
    # pred = exp(linepred),
    pred = exp(linepred),
    pred = ifelse(name!="bv_rsum", NA, pred),
  )
df_results_2
label_names = as_labeller(c(
  'bias_sq'="bias^2",
  'var'="variance",
  'bv_rsum'="sqrt(bias^2 + var)"
), label_parsed)
df_results_2 %>%
  ggplot(aes(y = value, x=log(g,base=4),
             color=factor(K), fill = factor(K)
  )) +
  facet_wrap(~factor(name, levels=c("bias_sq", "var", "bv_rsum")),
             scales = "free_y",
             labeller = label_names) +
  geom_point(size=2) +
  geom_line(linewidth=1) +
  geom_line(aes(y = pred)) +
  # geom_function(
  #   fun = function(x) exp(predict(model_power_law,tibble(x))),
  #   # fun = function(x) exp(predict(model_power_law,tibble(4**x))),
  #   linewidth=1,
  #   colour = "cyan"
  # ) +
  # stat_function(fun = function(g) predict(model_power_law,g)) +
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
  xlab(TeX("$\\log_4(g)$")) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(name="K", values=c("firebrick", "dodgerblue2")) +
  scale_color_manual(name="K", values=c("firebrick", "dodgerblue2"))






### coefficients
beta0_K1 = model_power_law$coefficients[1]
beta0_K56 = model_power_law$coefficients[2]
beta1_K1 = model_power_law$coefficients[3]
beta1_K56 = model_power_law$coefficients[4]
###
g_K56 <- function(g_K1) {
  (exp(beta0_K56 - beta0_K1) * g_K1^beta1_K1)^(1/beta1_K56)
}
### ESS for N=56, K=56, G=4101
4101*56
g_K56(229656)
g_K56(229656) / 229656 * 100
### plot g_K56 / g_K1 as a function of g_K1
tibble(g_K1 = c(1e4, 5e4, 1e5, 2e5)) %>%
  mutate(ratio = g_K56(g_K1)/g_K1) %>%
  ggplot(aes(x = g_K1, y = ratio)) +
  geom_point() +
  geom_line()






# # ### power law regression WITH INTERCEPT
# # model_power_law = lm(
# #   log(value) ~ factor(K) + factor(K):I(-log(g)) + 0
# #   ,data = df_results_1 %>% filter(name == "bv_rsum")
# # )
# # model_power_law
# # ### coefficients
# # beta0_K1 = model_power_law$coefficients[1]
# # beta0_K56 = model_power_law$coefficients[2]
# # beta1_K1 = model_power_law$coefficients[3]
# # beta1_K56 = model_power_law$coefficients[4]
# # ###
# # g_K56 <- function(g_K1) {
# #   (exp(beta0_K56 - beta0_K1) * g_K1^beta1_K1)^(1/beta1_K56)
# # }
# # ### ESS for N=56, K=56, G=4101
# # 4101*56
# # g_K56(229656)
# # g_K56(229656) / 229656 * 100
# # ### plot g_K56 / g_K1 as a function of g_K1
# # tibble(g_K1 = c(1e4, 5e4, 1e5, 2e5)) %>%
# #   mutate(ratio = g_K56(g_K1)/g_K1) %>%
# #   ggplot(aes(x = g_K1, y = ratio)) +
# #   geom_point() +
# #   geom_line()
# 
# ### power law regression WITHOUT INTERCEPT
# model_power_law = lm(
#   # log(value) ~ factor(K):I(-log(g)) + 0
#   # log(value) ~ factor(K):I(-log(g))
#   log(value) ~ factor(K):I(-log(g))
#   ,data = df_results_1 %>% filter(name == "bv_rsum")
# )
# model_power_law
# # ### coefficients
# # # beta1_K1 = model_power_law$coefficients[1]
# # # beta1_K56 = model_power_law$coefficients[2]
# # beta1_K1 = model_power_law$coefficients[2]
# # beta1_K56 = model_power_law$coefficients[3]
# # beta1_K1/beta1_K56
# 
# ### plot
# df_results_2 = 
#   df_results_1 %>%
#   mutate(pred = ifelse(name=="bv_rsum", exp(predict(model_power_law, .)), NA))
# df_results_2
# label_names = as_labeller(c(
#   'bias_sq'="bias^2",
#   'var'="variance",
#   'bv_rsum'="sqrt(bias^2 + var)"
# ), label_parsed)
# df_results_2 %>%
#   ggplot(aes(y = value, x=log(g,base=4), 
#              color=factor(K), fill = factor(K)
#   )) +
#   facet_wrap(~factor(name, levels=c("bias_sq", "var", "bv_rsum")), 
#              scales = "free_y",
#              labeller = label_names) +
#   geom_point(size=2) +
#   geom_line(linewidth=1) +
#   geom_line(aes(y = pred)) +
#   # stat_function(fun = function(g) predict(model_power_law,g)) +
#   geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.375, linetype="dashed") +
#   xlab(TeX("$\\log_4(g)$")) +
#   # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   scale_fill_manual(name="K", values=c("firebrick", "dodgerblue2")) +
#   scale_color_manual(name="K", values=c("firebrick", "dodgerblue2")) 
# 
# ###
# g_K56 <- function(g_K1) {
#   g_K1^(beta1_K1/beta1_K56)
# }
# ### ESS for N=56, K=56, G=4101
# 4101*56
# g_K56(229656)
# 229656 / g_K56(229656)
# ### plot g_K56 / g_K1 as a function of g_K1
# tibble(g_K1 = c(4000, 5000, 6000)) %>%
#   mutate(ratio = g_K1/g_K56(g_K1)) %>%
#   ggplot(aes(x = g_K1, y = ratio)) +
#   geom_point() +
#   geom_line()





# ### accuracy ratio
# df_accuracy_ratio = 
#   df_results_1 %>%
#   filter(name == "bv_rsum") %>%
#   select(g,K,value) %>%
#   pivot_wider(names_from="K", values_from=value, names_prefix="bv_rsum_K") %>%
#   mutate(r = bv_rsum_K1/bv_rsum_K56)
# # mutate(r = bv_rsum_K56/bv_rsum_K1) 
# df_accuracy_ratio
# 
# # model_accuracy_ratio = lm(r ~ poly(g,3), data=df_accuracy_ratio)
# # model_accuracy_ratio = lm(
# #   log(1/(1-r)) ~ log(g),
# #   # log(1/(1-r)) ~ log(g) + I(log(g)^2) + I(log(g)^3) , 
# #   # r ~ g + I(g^2),
# #   data=df_accuracy_ratio 
# # )
# # model_accuracy_ratio
# 
# df_accuracy_ratio %>%
#   drop_na() %>%
#   # mutate(pred = predict(model_accuracy_ratio,.)) %>%
#   # mutate(pred = 1-1/exp(predict(model_accuracy_ratio,.)) ) %>%
#   ggplot(aes(x = g, y = r)) +
#   geom_line(linewidth=1) +
#   # geom_line(aes(y=pred), linetype="dashed", color="cyan", linewidth=1) +
#   # geom_function(
#   #   fun = function(g) 1 - 75 / g^(.8),
#   #   # fun = function(g) 1-1/exp(predict(model_accuracy_ratio,tibble(g))),
#   #   # fun = function(g) predict(model_accuracy_ratio,tibble(g)), 
#   #   linewidth=1,
#   #   colour = "cyan"
#   # ) +
#   # ylab("accuracy ratio") +
#   geom_point()



