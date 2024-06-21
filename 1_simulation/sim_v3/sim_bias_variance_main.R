
source("../../0_clean_lm.R")
source("sim_main.R")

### show that as dependence K decreases, accuracy increases
df_GK_1 = 
  tibble(data.frame(
    N = N,
    zeta = 4101,
    K = seq(1,N,by=5)
    # K = 1:N
  )) %>%
  # mutate(G = 4101*K)
  mutate(
    G0 = zeta*(N/K),
    G = round(G0),
    # G = zeta*(N/K),
    nrow_sample_size = K*G,
    bv_sim_idx = 1
  ) %>% arrange(N,zeta,-K)
df_GK_1

### show that it would be better if there were no dependence structure, i.e. if each play were iid
### but also that it is better to use all plays per game than one iid play per game despite the dependence structure
### and use this to calculate effective sample size
zeta_vec = round(4**seq(4,7.5,by=0.5))
zeta_vec
df_GK_2a = 
  tibble(expand.grid(
    N = N,
    zeta = zeta_vec,
    K = c(1,N)
  )) %>%
  mutate(
    G = zeta*(N/K),
    nrow_sample_size = K*G,
    bv_sim_idx = 2
  ) %>% arrange(N,zeta,K)
df_GK_2a
df_GK_2b = 
  tibble(expand.grid(
    N = N,
    zeta = zeta_vec,
    K = c(1,N)
  )) %>%
  mutate(
    G = zeta,
    nrow_sample_size = K*G,
    bv_sim_idx = 2
  ) %>% arrange(N,zeta,K)
df_GK_2b
df_GK_2 = bind_rows(df_GK_2a, df_GK_2b) %>% distinct() %>% arrange(N,zeta,G-K) 
df_GK_2

### all param combinations to try
df_bvsim = bind_rows(df_GK_1, df_GK_2)
df_bvsim
print(data.frame(df_bvsim))


