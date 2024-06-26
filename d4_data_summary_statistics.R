
source("00_main.R") 

###########################################
### # plays  ###
###########################################

# num first downs and num plays
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
dim(all_fourth_downs)
dim(ALL_fourth_downs)

# num fourth downs vs. num first downs
sum(data_full_WP$down1)/sum(data_full_WP$down4)

### WP variables
names(data_full_WP)

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
  geom_vline(xintercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(size=8) +
  xlab("career mean punter quality") + ylab("punter") +
  theme(
      axis.title = element_text(size=35),
      axis.text.x = element_text(size=35),
      axis.text.y = element_text(size=20),
    ) 
# pq_plot
ggsave("data_plots/plot_pq.png", pq_plot, width=8, height=12)

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
  geom_vline(xintercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(size=8) +
  xlab("career mean kicker quality") + ylab("kicker") +
  theme(
    axis.title = element_text(size=35),
    axis.text.x = element_text(size=35),
    axis.text.y = element_text(size=20),
  ) 
# kq_plot
ggsave("data_plots/plot_kq.png", kq_plot, width=8, height=12)

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
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  geom_vline(xintercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(aes(x=mean_qbq, y=fct_reorder(qb_name, mean_qbq)), size=8) +
  xlab("career mean QB quality") + ylab("quarterback") +
  theme(
    axis.title = element_text(size=35),
    axis.text.x = element_text(size=35),
    axis.text.y = element_text(size=20),
  ) 
# plot_qbq
ggsave("data_plots/plot_TQ_qbq.png", plot_qbq, width=8, height=12)

plot_oq_rot = data_full_0A %>%
  filter(season == 2021) %>%
  group_by(posteam, season) %>%
  summarise(mean_oq_rot = mean(oq_rot_0_total_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_oq_rot, y=fct_reorder(posteam, mean_oq_rot))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team non-quarterback\n offensive quality in 2021") +
  ylab("team")
# plot_oq_rot
ggsave("data_plots/plot_TQ_oq_rot.png", plot_oq_rot, width=6, height=9)

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
# plot_dq_dt_againstPass
ggsave("data_plots/plot_TQ_dq_dt_againstPass.png", plot_dq_dt_againstPass, width=6, height=9)

plot_dq_dt_againstRun = data_full_0A %>%
  filter(season == 2021) %>%
  group_by(defteam, season) %>%
  summarise(mean_dq_dt = mean(dq_dt_0_againstRun_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_dq_dt, y=fct_reorder(defteam, -mean_dq_dt))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team defensive quality\n against the run in 2021") +
  ylab("team")
# plot_dq_dt_againstRun
ggsave("data_plots/plot_TQ_dq_dt_againstRun.png", plot_dq_dt_againstRun, width=6, height=9)


plot_tq = plot_grid(plot_qbq, plot_oq_rot, plot_dq_dt_againstPass, plot_dq_dt_againstRun, nrow=1)
save_plot(paste0("data_plots/plot_TQ.png"), plot_tq, base_width=28, base_height=10)




