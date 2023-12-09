
### LOAD 4TH DOWN PLOTTING DATA, MODELS, AND FUNCTIONS
source("D6_loadModelsAndData.R")
source("D3_decision_making_functions.R")
library(gt)

################################################################################
df4d = 
  all_fourth_downs %>% filter(qb_kneel == 0)  %>% 
  filter(era4==1) %>%
  filter(row_idx != 870355) ### play weird...
### check 
print(
  # data.frame(
  df4d %>% select(
    posteam, season, posteam_coach, decision_actual, 
    # kicker_player_name, punter_player_name,
    yardline_100, ydstogo, down, score_differential, posteam_spread, game_seconds_remaining
  )
  # )
)
################################################################################

### COMPUTE `OPTIMAL` FOURTH DOWN DECISIONS
fname = "results_humility/ddf_wp_se.csv"
if (file.exists(fname)) {
  ddf_wp_se = read_csv(fname)
} else {
  ddf_wp_se = get_all_decision_making(df4d, wp=TRUE, SE=TRUE, coachBaseline=TRUE) ### takes ~5-15 minutes
  write_csv(ddf_wp_se, fname)
}
# ddf_wp_se = ddf_wp_se %>% mutate(boot_perc = prop_decision*100)

### plot
# bds_boot = c(0,  0.8, 0.95, 1)
bds_boot = c(0,  0.75, 0.9, 1)
desc_boot = c("uncertain", "lean", "confident")
lower_bin_str = paste0("[", bds_boot[1:(length(bds_boot)-1)]*100, "%")
upper_bin_str = paste0(bds_boot[2:length(bds_boot)]*100, "%", ")")
upper_bin_str[length(upper_bin_str)] = str_replace(upper_bin_str[length(upper_bin_str)], "\\)", "]")
desc_boot_1 = paste0("\n", desc_boot, "\n(boot% in\n", lower_bin_str, ", ", upper_bin_str, ")\n")
desc_boot_1
colors_boot = c("firebrick", "goldenrod", "forestgreen")
names(colors_boot) = desc_boot_1

wp_gain_breaks = c(seq(0,0.15,by=0.01),1)
# wp_gain_breaks = quantile(ddf_wp_se$decision_intensity, seq(0,1,length.out=20)); wp_gain_breaks=c(wp_gain_breaks,1)
wp_gain_breaks
plot1_df = 
  ddf_wp_se %>%
  select(decision_intensity, prop_decision) %>%
  mutate(
    # wp_gain_perc = decision_intensity*100,
    # wp_gain_bin = cut(wp_gain_perc, breaks=wp_gain_breaks*100),
    wp_gain_bin = cut(decision_intensity, breaks=wp_gain_breaks),
    boot_perc_bin = cut(prop_decision, breaks=bds_boot, labels=desc_boot_1)
  ) %>%
  drop_na(wp_gain_bin) %>%
  group_by(wp_gain_bin) %>%
  mutate(count_wpgb = n()) %>%
  group_by(wp_gain_bin, boot_perc_bin) %>%
  mutate(count_wpgb_bpb = n(), p = count_wpgb_bpb/count_wpgb) %>%
  ungroup()
plot1_df

plot_overconfidence = 
  plot1_df %>%
  distinct(wp_gain_bin, boot_perc_bin, p) %>%
  ggplot(aes(x = wp_gain_bin)) +
  geom_point(size=5, aes(y=p, color=boot_perc_bin, shape=boot_perc_bin)) +
  scale_color_manual(name="", values = colors_boot) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  scale_shape_manual(name = "",
                     labels = names(colors_boot),
                     values = c(15,17,19)) +
  xlab("estimated gain in win probability\nby making the estimated optimal decision") +
  ylab("proportion of fourth-down plays from 2018-2022\n belonging to a given boot% decision category ") +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1, size=15))
# plot_overconfidence
ggsave("results_humility/plot_overconfidence.png", 
       plot_overconfidence, width=11, height=8)

plot1_df %>%
  group_by(wp_gain_bin) %>%
  summarise(
    count = n()
  ) %>%
  mutate(perc_wpgb = count/sum(count), pc = cumsum(perc_wpgb))

plot_freq = 
  plot1_df %>%
  group_by(wp_gain_bin) %>%
  summarise(
    count = n()
  ) %>%
  mutate(perc_wpgb = count/sum(count)) %>%
  ggplot(aes(x = wp_gain_bin, y = perc_wpgb)) +
  geom_col(fill="black") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  ylab("proportion of fourth-down plays \n from 2018-2022 in a given bin") +
  xlab("estimated gain in win probability\nby making the estimated optimal decision") +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1, size=15))
# plot_freq
ggsave("results_humility/plot_freq.png", 
       plot_freq, width=9, height=8)



plays_summary_df =
  plot1_df %>%
  group_by(boot_perc_bin) %>%
  summarise(
    count=n()
  ) %>%
  mutate(
    p = count/sum(count),
    p = round(p,2)
  ) %>%
  rename(`decision confidence` = boot_perc_bin)
plays_summary_df

plays_summary_gt = gt(plays_summary_df)
gtsave(plays_summary_gt, "results_humility/plays_summary_gt.png")


################################################################################

### COACH EVAL
# boot_threshold = 0.9
boot_threshold = bds_boot[length(bds_boot)-1]
min_num_attempts = 50
df_coach_decs = 
  ddf_wp_se %>%
  select(decision_intensity, prop_decision, decision, decision_actual, posteam_coach) %>%
  filter(prop_decision >= boot_threshold) %>%
  mutate(correct_dec = decision == decision_actual) %>%
  group_by(posteam_coach) %>%
  summarise(
    num_correct = sum(correct_dec),
    n = n(),
    prop_correct = num_correct/n
  ) %>%
  arrange(-prop_correct) %>%
  filter(n >= min_num_attempts)
df_coach_decs

xlab_ = paste0("proportion of confident 4th down calls the coach correctly made",
               "\namong all coaches with >=", min_num_attempts, 
               " attempts from ", min(df4d$season), " to ", max(df4d$season),
               ', \n where "confident" means boot% >= ', boot_threshold)
plot_coach_decs = 
  df_coach_decs %>%
  ggplot(aes(x=prop_correct, y=fct_reorder(posteam_coach, prop_correct))) +
  geom_point(size=3) +
  labs(x = xlab_, y="coach") 
# plot_coach_decs

ggsave(paste0("results_humility/plot_coach_decs_b=",boot_threshold,".png"), 
       plot_coach_decs, width=11, height=14)

################################################################################

### of the confident decisions, how many were right
NflTooConservativeDf = 
  ddf_wp_se %>%
  mutate(
    category_boot = cut(prop_decision, breaks=bds_boot, labels=desc_boot, include.lowest=T),
  ) %>%
  select(category_boot, decision, decision_actual) %>%
  filter(str_detect(category_boot, "confident")) %>%
  group_by(decision) %>%
  mutate(made_right_dec = decision == decision_actual) %>%
  mutate(decision = ifelse(decision == "Go", "Go", "Kick")) %>% ### combine Punt and FG into Kick
  group_by(decision) %>%
  summarise(p = sum(made_right_dec)/n())
# print(NflTooConservativeDf)
gtsave(gt(NflTooConservativeDf), "results_humility/NflTooConservativeDf.png")

# ### of the confident or lean decisions, how many were right
# NflTooConservativeDf = 
#   ddf_wp_se %>%
#   mutate(
#     category_boot = cut(prop_decision, breaks=bds_boot, labels=desc_boot, include.lowest=T),
#   ) %>%
#   select(category_boot, decision, decision_actual) %>%
#   filter(str_detect(category_boot, "confident") | str_detect(category_boot, "lean") ) %>%
#   group_by(decision) %>%
#   mutate(made_right_dec = decision == decision_actual) %>%
#   mutate(decision = ifelse(decision == "Go", "Go", "Kick")) %>% ### combine Punt and FG into Kick
#   group_by(decision) %>%
#   summarise(p = sum(made_right_dec)/n())
# print(NflTooConservativeDf)



