
###################
filewd = getwd()
setwd("..")
source("00_main.R") 
setwd(filewd)
###################

###########################################
### # plays  ###
###########################################

# num fourth downs vs. num first downs
sum(data_full_AA$down1)/sum(data_full_AA$down4)

# num first downs and num plays
# data_full_AA %>%
data_full_WP %>%
  group_by(down) %>%
  summarise(
    count = n()
  ) %>%
  mutate(p = count/sum(count), total=sum(count))

### num games
length(unique(data_full$game_id))

# on fourth down, decision breakdown
all_fourth_downs %>%
  group_by(decision_actual) %>%
  summarise(count = n()) %>%
  mutate(p = count/sum(count))

###########################################
### distribution in epoch size ###
###########################################

epoch_sizes_df = 
  data_full_0A %>%
  select(game_id, play_id, half_seconds_remaining, epoch) %>%
  group_by(game_id, epoch) %>%
  summarise(
    count = n(), 
    hsr_start = max(half_seconds_remaining),
    hsr_med = median(half_seconds_remaining),
    hsr_end = min(half_seconds_remaining),
    .groups = "drop"
  ) 
epoch_sizes_df

epoch_sizes_df %>%
  ggplot() +
  geom_histogram(aes(x = count), fill="black")

epoch_sizes_df %>%
  mutate(hsr_med_bin = cut(hsr_med, breaks=seq(0,1800,by=30))) %>%
  group_by(hsr_med_bin) %>%
  summarise(mean_num_plays = mean(count)) %>%
  ggplot() +
  geom_point(aes(y = hsr_med_bin, x = mean_num_plays))



####################################
### plot Punter Quality Rankings ###
####################################

### plot punter quality rankings
pq_plot = data_full_0A %>% 
  filter(punt_attempt==1) %>%
  filter(season >= 2006) %>%
  group_by(punter_name) %>% 
  filter(first(season)>2006) %>%
  summarise(num_kicks = n(), pq_mean = mean(pq_0_sum_std), career_fg_pa0 = sum(nyae0)) %>% 
  arrange(-pq_mean) %>%
  filter(num_kicks >= 250) %>%
  ggplot(aes(y = fct_reorder(punter_name, pq_mean), x = pq_mean)) +
  geom_point() +
  xlab("career mean punter quality\n (>250 attempts, rookie season >2006)") + ylab("punter")
pq_plot
ggsave("plot_pq.png", pq_plot, width=8, height=12)

####################################
### plot Kicker Quality Rankings ###
####################################

### plot kicker quality rankings
kq_plot = data_full_0A %>% 
  filter(field_goal_attempt==1) %>%
  filter(season >= 2006) %>%
  group_by(kicker_player_name) %>% 
  filter(first(season)>2006) %>%
  summarise(num_kicks = n(), kq_mean = mean(kq_0_sum_std), career_fg_pa0 = sum(fgpa0)) %>% 
  arrange(-kq_mean) %>%
  filter(num_kicks > 100) %>%
  ggplot(aes(y = fct_reorder(kicker_player_name, kq_mean), x = kq_mean)) +
  geom_point() +
  xlab("career mean kicker quality\n (>100 attempts, rookie season >2006)") + ylab("kicker")
kq_plot
ggsave("plot_kq.png", kq_plot, width=8, height=12)

##################################
### plot Team Quality Rankings ###
##################################

rookie_qb_sznz = data_full_0A %>%
  select(qb_name, season) %>%
  distinct(qb_name, season) %>%
  arrange(qb_name, season) %>%
  group_by(qb_name) %>%
  slice_head() %>%
  rename(qb_rookie_szn = season)
rookie_qb_sznz

plot_qbq = data_full_0A %>%
  left_join(rookie_qb_sznz) %>%
  group_by(qb_name) %>%
  filter(qb_play) %>%
  filter(qb_rookie_szn > 2006) %>%
  summarise(mean_qbq = mean(qbq_ot_0_sum), att = n()) %>%
  filter(att >= 1750) %>%
  ggplot() +
  geom_point(aes(x=mean_qbq, y=fct_reorder(qb_name, mean_qbq))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  # xlab("career mean quarterback quality") +
  xlab("career mean quarterback quality\n (>1750 attempts, rookie szn >2006)") +
  ylab("quarterback")
plot_qbq
ggsave("plot_TQ_qbq.png", plot_qbq, width=7, height=11)

plot_oq_rot = data_full_0A %>%
  filter(season == 2021) %>%
  group_by(posteam, season) %>%
  summarise(mean_oq_rot = mean(oq_rot_0_total_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_oq_rot, y=fct_reorder(posteam, mean_oq_rot))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team non-quarterback\n offensive quality in 2021") +
  ylab("team")
plot_oq_rot
ggsave("plot_TQ_oq_rot.png", plot_oq_rot, width=6, height=9)

# plot_dq_dt = data_full_0A %>%
#   filter(season == 2021) %>%
#   group_by(defteam, season) %>%
#   summarise(mean_dq_dt = mean(dq_dt_0_total_sum)) %>%
#   ggplot() +
#   geom_point(aes(x=mean_dq_dt, y=fct_reorder(defteam, -mean_dq_dt))) +
#   xlab("mean team defensive quality in 2021") +
#   ylab("team")
# plot_dq_dt
# # ggsave("plot_TQ_dq_dt.png", plot_dq_dt, width=6, height=9)

plot_dq_dt_againstPass = data_full_0A %>%
  filter(season == 2021) %>%
  group_by(defteam, season) %>%
  summarise(mean_dq_dt = mean(dq_dt_0_againstPass_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_dq_dt, y=fct_reorder(defteam, -mean_dq_dt))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team defensive quality\n against the pass in 2021") +
  ylab("team")
plot_dq_dt_againstPass
ggsave("plot_TQ_dq_dt_againstPass.png", plot_dq_dt_againstPass, width=6, height=9)

plot_dq_dt_againstRun = data_full_0A %>%
  filter(season == 2021) %>%
  group_by(defteam, season) %>%
  summarise(mean_dq_dt = mean(dq_dt_0_againstRun_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_dq_dt, y=fct_reorder(defteam, -mean_dq_dt))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team defensive quality\n against the run in 2021") +
  ylab("team")
plot_dq_dt_againstRun
ggsave("plot_TQ_dq_dt_againstRun.png", plot_dq_dt_againstRun, width=6, height=9)


plot_tq = plot_grid(plot_qbq, plot_oq_rot, plot_dq_dt_againstPass, plot_dq_dt_againstRun, nrow=1)
save_plot(paste0("plot_TQ.png"), plot_tq, base_width=28, base_height=10)



