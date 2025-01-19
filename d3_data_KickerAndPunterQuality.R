
#####################
##### load data #####
#####################

source("0_clean_lm.R")
data1_a0 <- read_csv("data1a.csv")

data1_a0 = 
  data1_a0  %>%
  mutate(half = ifelse(qtr == 1 | qtr == 2, 1, 2)) %>%
  relocate(half, .after = game_id) %>%
  group_by(game_id, posteam) %>%
  mutate(
    qb_name = zoo::na.locf(passer_player_name, na.rm = F),
    qb_name = zoo::na.locf(passer_player_name, fromLast = T, na.rm = F)
  ) %>%
  ungroup() %>%
  group_by(posteam) %>%
  mutate(
    kicker_name = zoo::na.locf(kicker_player_name, fromLast = T, na.rm = F),
    kicker_name = zoo::na.locf(kicker_name, na.rm = F),
    punter_name = zoo::na.locf(punter_player_name, fromLast = T, na.rm = F),
    punter_name = zoo::na.locf(punter_name, na.rm = F),
  ) %>%
  ungroup() %>%
  relocate(qb_name, .after=passer_player_name) %>% 
  relocate(kicker_name, .after=kicker_player_name) %>% 
  relocate(punter_name, .after=punter_player_name) %>% 
  mutate(offensive_player_name = case_when(
    !sapply(passer_player_name, is.na) ~ passer_player_name,
    !sapply(rusher_player_name, is.na) ~ rusher_player_name,
    TRUE~NA_character_
  )) %>%
  mutate(qb_play = replace_na(offensive_player_name == qb_name, FALSE)) %>%
  mutate(pass_or_rush = case_when(
    replace_na(pass_attempt == 1, FALSE) ~ "pass",
    replace_na(rush_attempt == 1, FALSE) ~ "run",
    TRUE~NA_character_
  )) %>%
  mutate(row_idx = row_number()) %>% relocate(row_idx, .before = game_id)

# ### check
# View(
#   data1_a0 %>%
#     filter(row_idx %in% 709363:(709363+2000)) %>%
#     select(
#       game_id, row_idx, play_id, posteam, defteam,
#       passer_player_name, rusher_player_name, offensive_player_name, qb_name, qb_play,
#       kicker_player_name, kicker_name, punter_player_name, punter_name
#     )
# )
# View(data1_a0 %>% #filter(row_number() %in% 10000:10200) %>%
#     select(
#       game_id, play_id, season, posteam, defteam, yardline_100, down, ydstogo, epa0,
#       offensive_player_name, pass_or_rush,
#       # passer_player_name, pass_attempt, rusher_player_name, rush_attempt,
# ))

data1_a = data1_a0

##################
##### params #####
##################

ALPHA_PQ = 0.99
GAMMA_PQ = 50*3

ALPHA_KQ = 0.985
GAMMA_KQ = 32*3

##########################
##### KICKER QUALITY #####
##########################

### FG dataset
fg_df = 
  data1_a %>% 
  drop_na(field_goal_result) %>%
  select(row_idx, field_goal_result, yardline_100, posteam, season, week, kicker_player_name) %>%
  mutate(i = 1:n()) %>% 
  mutate(fg_made = as.numeric(field_goal_result == "made")) %>%
  filter(yardline_100 <= 50) %>%
  # filter(season >= 2006) %>%
  mutate(field_goal_attempt = 1)
tail(fg_df)  

### fit fgp0: kicker-agnostic probability of a field goal
fg_model0_RE <- glm(
  fg_made ~ bs(yardline_100,3),
  data = fg_df %>% filter(season >= 2006),
  family = "binomial" # Logistic regression
)
fg_model0_RE

# ### plot check 
# plot_check_fg_model = 
#   expand_grid(
#     tibble(yardline_100=1:65),
#     tibble(kicker_player_name = c(
#       "J.Tucker", "Y.Koo", "D.Rayner"
#     )),
#   ) %>%
#   mutate(p = predict(fg_model0_RE, ., type="response")) %>%
#   bind_rows(
#     tibble(yardline_100=1:65) %>%
#     mutate(
#       kicker_player_name = "baseline",
#       p = predict(fg_model0_RE, ., type="response", re.form = NA)
#     )
#   ) %>%
#   ggplot(aes(x = yardline_100)) + 
#   geom_line(aes(y = p, color=kicker_player_name)) 
# plot_check_fg_model

### fgpa
fg_df = 
  fg_df %>% mutate(
    fgp0 = predict(fg_model0_RE, ., type="response"), ### field goal probability
    fgpa0 = fg_made - fgp0 ### field goal probability added) 
  ) 

### median num field goals per season
fg_df %>%
  group_by(kicker_player_name,season) %>%
  reframe(n = n()) %>%
  arrange(-n) %>%
  mutate(med_n = median(n))

### kicker quality
get_kq_vec <- function(fg_df_kicker,alpha,gamma) {
  n = nrow(fg_df_kicker)
  kq_vec = numeric(n)
  if (n >= 2) {
    for (j in 2:n) {
      # if (j == 6) browser()
      alpha_vec = alpha**((j-2):0)
      fgpa_vec = unname(fg_df_kicker$fgpa0[1:(j-1)])
      top = sum(alpha_vec * fgpa_vec)
      bot = gamma + sum(alpha_vec)
      kq_vec[j] = top/bot
    }
  }
  kq_vec
}

all_kickers = unique(fg_df$kicker_player_name)
kq_df = tibble()
for (k in 1:length(all_kickers)) {
  kicker = all_kickers[k]
  print(paste0("kicker k = ", k, " of ", length(all_kickers), ", ", kicker))
  
  fg_df_kicker = fg_df %>% filter(kicker_player_name == kicker)
  kq_kicker = get_kq_vec(fg_df_kicker, alpha=ALPHA_KQ, gamma=GAMMA_KQ)
  fg_df_kicker$kq = kq_kicker
  kq_df = bind_rows(kq_df, fg_df_kicker)
}
kq_df = kq_df %>% mutate(kq = std(kq))

### visualize kicker quality trajectories
kq_df_plot = 
  kq_df %>%
  filter(season >= 2006) %>%
  select(season, week, kicker_player_name, kq) %>%
  group_by(kicker_player_name) %>%
  mutate(k = 1:n()) %>%
  ungroup() %>%
  group_by(kicker_player_name,season,week) %>%
  slice_head() %>%
  ungroup() %>%
  mutate(
    sw = (season - min(season))*17 + week,
    ss = ifelse(week==1, season, NA)
  )  %>%
  filter(kicker_player_name %in% c(
    "R.Gould",
    "J.Tucker",
    "Y.Koo",
    "D.Rayner"
  )) 
kq_df_plot
plot_kq_traj = 
  kq_df_plot %>%
  rename(kicker = kicker_player_name) %>%
  ggplot(aes(x = sw, y = kq, color = kicker)) +
  geom_vline(xintercept = seq(1,17*(2022-2006+2),by=17), 
             alpha=0.5, linetype="dashed") +
  geom_hline(yintercept = 0) +
  geom_point(size=2) +
  scale_x_continuous(
    labels = function(x) round(x  / 17 + 2006),
    breaks = seq((2006 - 2006)*17, (2022 - 2006)*17, by=17*4),
    name = "season",                 
  ) +
  ylab("kicker quality")
# plot_kq_traj
ggsave("plots_pqtq/plot_kq_trajs.png", plot_kq_traj, width=8, height=5)

### plot kicker quality rankings
kq_plot = 
  kq_df %>% 
  group_by(kicker_player_name) %>% 
  filter(first(season)>2006) %>%
  reframe(num_kicks = n(), mean_kq = mean(kq)) %>% 
  arrange(-mean_kq) %>%
  filter(num_kicks > 100) %>%
  ggplot(aes(y = fct_reorder(kicker_player_name, mean_kq), x = mean_kq)) +
  geom_vline(xintercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(size=8) +
  xlab("career mean kicker quality") + ylab("kicker") +
  theme(
    axis.title = element_text(size=35),
    axis.text.x = element_text(size=30),
    axis.text.y = element_text(size=20),
    # plot.margin = margin(20, 20, 20, 20),   
  ) 
# kq_plot
ggsave("plots_pqtq/plot_kq.png", kq_plot, width=8, height=12)

# ### check
# fit_fgp_model_best <- function(fg_data) {
#   ### impute fake missed field goals for yardlines beyond 50 (which has never been made before)
#   fg_data_1 = fg_data %>% bind_rows(tibble(
#     fg_made = 0, kq = 0,
#     yardline_100 = sample(51:99, size=500, replace=TRUE)
#   ))
# 
#   fit = glm(fg_made ~  bs(yardline_100, df=5) + kq, data = fg_data_1, family="binomial")
#   clean_lm(fit)
# }
# fg_model_fit = fit_fgp_model_best(kq_df)
# fg_model_fit
# plot_fg_prob_by_kq(fg_model_fit)

##########################
##### PUNTER QUALITY #####
##########################

### punt dataset
punt_df = 
  data1_a %>%
  group_by(game_id, half) %>%
  mutate(next_ydl = lead(yardline_100)) %>%
  ungroup() %>%
  drop_na(punter_player_name) %>%
  mutate(punt_attempt=1) %>%
  select(row_idx, yardline_100, next_ydl, posteam, season, week, punter_player_name, punt_attempt) %>% 
  mutate(i = 1:n()) %>%
  # filter(season >= 2006) %>%
  drop_na()
punt_df

### fit eny: punter-agnostic expected next yardline of a punt
punt_model0_RE <- lm(
  next_ydl ~ bs(yardline_100,3), 
  data = punt_df %>% filter(season >= 2006) 
)
punt_model0_RE

# ### plot check 
# plot_check_punt_model = 
#   expand_grid(
#   tibble(yardline_100=1:65),
#   tibble(punter_player_name = c(
#     "J.Hekker", "B.Kern", "R.Hodges"
#   )),
# ) %>%
#   mutate(p = predict(punt_model0_RE, ., type="response")) %>%
#   bind_rows(
#     tibble(yardline_100=1:65) %>%
#       mutate(
#         punter_player_name = "baseline",
#         p = predict(punt_model0_RE, ., type="response", re.form = NA)
#       )
#   ) %>%
#   ggplot(aes(x = yardline_100)) + 
#   geom_line(aes(y = p, color=punter_player_name)) 
# plot_check_punt_model

### punt nyae0
punt_df = 
  punt_df %>% mutate(
    eny0 = predict(punt_model0_RE, ., type="response"), ### punt expected next yardline
    nyae0 = next_ydl - eny0 ### punt next yardine above expected
  ) 

### median num punts per season
punt_df %>%
  group_by(punter_player_name,season) %>%
  reframe(n = n()) %>%
  arrange(-n) %>%
  mutate(med_n = median(n))

### punter quality
get_pq_vec <- function(punt_df_punter,alpha,gamma) {
  n = nrow(punt_df_punter)
  pq_vec = numeric(n)
  if (n >= 2) {
    for (j in 2:n) {
      # if (j == 6) browser()
      alpha_vec = alpha**((j-2):0)
      puntpa_vec = unname(punt_df_punter$nyae0[1:(j-1)])
      top = sum(alpha_vec * puntpa_vec)
      bot = gamma + sum(alpha_vec)
      pq_vec[j] = top/bot
    }
  }
  pq_vec
}

all_punters = unique(punt_df$punter_player_name)
pq_df = tibble()
for (k in 1:length(all_punters)) {
  punter = all_punters[k]
  print(paste0("punter k = ", k, " of ", length(all_punters), ", ", punter))
  
  punt_df_punter = punt_df %>% filter(punter_player_name == punter)
  pq_punter = get_pq_vec(punt_df_punter, alpha=ALPHA_PQ, gamma=GAMMA_PQ)
  punt_df_punter$pq = pq_punter
  pq_df = bind_rows(pq_df, punt_df_punter)
}
pq_df = pq_df %>% mutate(pq = std(pq))

### visualize punter quality trajectories
pq_df_plot = 
  pq_df %>%
  filter(season >= 2006) %>%
  select(season, week, punter_player_name, pq) %>%
  group_by(punter_player_name) %>%
  mutate(k = 1:n()) %>%
  ungroup() %>%
  group_by(punter_player_name,season,week) %>%
  slice_head() %>%
  ungroup() %>%
  mutate(
    sw = (season - min(season))*17 + week,
    ss = ifelse(week==1, season, NA)
  )  %>%
  filter(punter_player_name %in% c(
    "P.McAfee",
    "J.Hekker",
    # "M.Palardy",
    # "D.Colquitt"
    "J.Bailey"
    # "B.Kern", 
    # "R.Hodges"
  )) 
pq_df_plot
plot_pq_traj = 
  pq_df_plot %>%
  rename(punter = punter_player_name) %>%
  ggplot(aes(x = sw, y = pq, color = punter)) +
  geom_vline(xintercept = seq(1,17*(2022-2006+2),by=17), 
             alpha=0.5, linetype="dashed") +
  geom_hline(yintercept = 0) +
  geom_point(size=2) +
  scale_x_continuous(
    labels = function(x) round(x  / 17 + 2006),
    breaks = seq((2006 - 2006)*17, (2022 - 2006)*17, by=17*4),
    name = "season",                 
  ) +
  ylab("punter quality")
# plot_pq_traj
ggsave("plots_pqtq/plot_pq_trajs.png", plot_pq_traj, width=8, height=5)

### plot punter quality rankings
pq_plot = 
  pq_df %>% 
  group_by(punter_player_name) %>% 
  filter(first(season)>2006) %>%
  reframe(num_kicks = n(), mean_pq = mean(pq)) %>% 
  arrange(-mean_pq) %>%
  filter(num_kicks > 250) %>%
  ggplot(aes(y = fct_reorder(punter_player_name, mean_pq), x = mean_pq)) +
  geom_vline(xintercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(size=8) +
  xlab("career mean punter quality") + ylab("punter") +
  theme(
    axis.title = element_text(size=35),
    axis.text.x = element_text(size=30),
    axis.text.y = element_text(size=20),
    # plot.margin = margin(20, 20, 20, 20),   
  ) 
# pq_plot
ggsave("plots_pqtq/plot_pq.png", pq_plot, width=8, height=12)

# ### check
# fit_punt_eny_model_best <- function(punt_data) {
#   fit = lm(next_ydl ~ bs(yardline_100, df=4) + pq + pq:yardline_100, data = punt_data)
#   clean_lm(fit)
# }
# punt_model_fit = fit_punt_eny_model_best(pq_df)
# plot_punt_eny_by_pq(punt_model_fit)

#################################
#### Save our KQ, PQ metrics ####
#################################

### join kq to full datafame
data1aa = left_join(
  data1_a, 
  kq_df %>% select(row_idx, kicker_player_name, field_goal_attempt, fg_made, kq)
)
data1aa = data1aa %>%
  mutate(field_goal_attempt = replace_na(field_goal_attempt, 0)) %>%
  group_by(kicker_name) %>%
  mutate(
    kq = zoo::na.locf(kq, fromLast = T, na.rm = F),
    kq = zoo::na.locf(kq, na.rm = F),
  ) %>%
  ungroup()
sum(is.na(data1aa$kq))

# View(
#   data1aa %>% select(row_idx, game_id, posteam, kicker_player_name, kicker_name, kq, field_goal_attempt)
# )

### join kq to full datafame
data1aaa = left_join(data1aa, pq_df %>% select(-i))
data1aaa = data1aaa %>%
  mutate(punt_attempt = replace_na(punt_attempt, 0)) %>%
  group_by(punter_name) %>%
  mutate(
    pq = zoo::na.locf(pq, fromLast = T, na.rm = F),
    pq = zoo::na.locf(pq, na.rm = F),
  ) %>%
  ungroup()
sum(is.na(data1aaa$pq))

# View(
#   data1aaa %>% select(row_idx, game_id, posteam, punter_player_name, punter_name, pq, punt_attempt)
# )

### add back in earlier data
data8 = data1aaa
sum(is.na(data8$kq))
sum(is.na(data8$pq))
dim(data8)
table(data8$season)

### save
write_csv(data8, "data8a.csv")
print("done.")
