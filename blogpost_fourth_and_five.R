
###
source("00_main.R")

###
guesses = c(0.15, 0.20, 0.25, 0.30, 0.37, 0.40, 0.40, 0.45, 0.72)
# guesses = c(0.20, 0.25, 0.30, 0.37, 0.40, 0.40, 0.45)
mean(guesses)
median(guesses)
sd(guesses)

###
go_df_3 = go_df_ %>% filter(down==3)
go_df_4 = go_df_ %>% filter(down==4)
go_df_3_recent = go_df_ %>% filter(down==3 & season >= 2018)
go_df_4_recent = go_df_ %>% filter(down==4 & season >= 2018)

###
dim(go_df_3)
dim(go_df_4)
dim(go_df_3_recent)
dim(go_df_4_recent)

### base rate
data_full_WP %>%
  filter(!is.na(pass_or_rush)) %>%
  filter(ydstogo == 5) %>%
  select(ydstogo, yardline_100, yards_gained) %>%
  mutate(convert = as.numeric(yards_gained >= ydstogo)) %>%
  reframe(base_rate = mean(convert))

### base rate
data_full_WP %>%
  filter(down == 1 | down == 2) %>%
  filter(!is.na(pass_or_rush)) %>%
  filter(ydstogo == 5) %>%
  select(ydstogo, yardline_100, yards_gained) %>%
  mutate(convert = as.numeric(yards_gained >= ydstogo)) %>%
  reframe(base_rate = mean(convert))

### base rates
m_base_rate_3 = glm(convert ~ factor(ydstogo)
                    ,data=go_df_3, family="binomial") 
plot_conv_2(m_base_rate_3)
predict(m_base_rate_3, tibble(ydstogo=5), type="response")


m_base_rate_4 = glm(convert ~ factor(ydstogo)
                  ,data=go_df_4, family="binomial") 
plot_conv_2(m_base_rate_4)
predict(m_base_rate_4, tibble(ydstogo=5), type="response")

m_base_rate_3_recent = glm(convert ~ factor(ydstogo)
                    ,data=go_df_3_recent, family="binomial") 
plot_conv_2(m_base_rate_3_recent)
predict(m_base_rate_3_recent, tibble(ydstogo=5), type="response")


m_base_rate_4_recent = glm(convert ~ factor(ydstogo)
                    ,data=go_df_4_recent, family="binomial") 
plot_conv_2(m_base_rate_4_recent)
predict(m_base_rate_4_recent, tibble(ydstogo=5), type="response")

### 
go_df_4_5 = go_df_ %>% filter(down==4 & ydstogo == 5)
go_df_4_5_recent = go_df_ %>% filter(down==4 & ydstogo == 5 & season >= 2018)
  
go_df_4_5 %>%
  mutate(mean = mean(convert)) %>%
  ggplot(aes(x = posteam_spread)) +
  geom_density() +
  xlab("point spread")
mean(go_df_4_5$posteam_spread)

go_df_4_5_recent %>%
  mutate(mean = mean(convert)) %>%
  ggplot(aes(x = posteam_spread)) +
  geom_density() +
  xlab("point spread")
mean(go_df_4_5_recent$posteam_spread)

go_df_4_5 %>%
  mutate(mean = mean(convert)) %>%
  ggplot(aes(x = qbq_ot_0_sum)) +
  geom_density() 
mean(go_df_4_5$qbq_ot_0_sum)

### survival bias
go_df_4_drives = 
  go_df_4 %>% 
  group_by(drive_id) %>%
  summarise(count = n()) 
go_df_4_drives
mean(go_df_4_drives$count > 1)

### model
go_df_4_A = 
  go_df_4 %>% 
  group_by(drive_id) %>%
  slice_head() %>%
  ungroup()
go_df_4_A

###
m1 = glm(convert ~ 
           bs(log(ydstogo+1),4,intercept = FALSE) +
           # (down==4):bs(log(ydstogo+1),4,intercept = FALSE) +
           qbq_ot_0_sum + oq_rot_0_total_sum + dq_dt_0_againstPass_sum + dq_dt_0_againstRun_sum
         ,data=go_df_4_A, family="binomial") 
plot_conv_2(m1)
predict(m1, tibble(ydstogo=5, down=4, qbq_ot_0_sum=0, oq_rot_0_total_sum=0,
                   dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0), type="response")

m2 = glm(convert ~ 
           bs(log(ydstogo+1),4,intercept = FALSE) +
           # (down==4):bs(log(ydstogo+1),4,intercept = FALSE) +
           posteam_spread
         ,data=go_df_4_A, family="binomial") 
plot_conv_2(m2)
predict(m2, tibble(ydstogo=5, down=4, posteam_spread=0), type="response")

### plot team qualities
ytg_max = 14
tq_breaks = seq(-ytg_max,ytg_max,by=2)
max_ytg = 12
ydl = 40 #FIXME

go_plot_df = tibble()
for (tq in tq_breaks) {
  go_plot_df = bind_rows(go_plot_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, posteam_spread = tq, down=4))
}
go_plot_df = go_plot_df %>% mutate(p = predict(m2, ., type="response"))

go_plot_df %>%
  mutate(color_col = factor(round(posteam_spread,2))) %>%
  mutate(color_col = fct_reorder(color_col, -1*posteam_spread)) %>%
  ggplot(aes(x = ydstogo, y = p, color = color_col)) +
  geom_line(linewidth=1.5) +
  ylab("conversion probability") + xlab("yards to go") +
  # labs(color=" quarterback\n quality", title=title) +
  # scale_y_continuous(breaks=seq(0,1,by=0.1)) +
  scale_x_continuous(breaks= if (ydl==4) {seq(0,4,by=1)} else if (ydl==10) {seq(0,10,by=2)}else {seq(0,100,by=4)} ) +
  theme(
    axis.title = element_text(size=27.5),
    axis.text = element_text(size=20),
    legend.title = element_text(size=27.5),
    legend.text = element_text(size=20),
    strip.text.x = element_text(size = 20),
  ) +
  scale_colour_discrete(name = "point spread") +
  # scale_colour_manual(values = gradient1) +
  theme(
    axis.title = element_text(size=30)
  )

###
predict(m2, tibble(ydstogo=5, down=4, posteam_spread=-14), type="response")
predict(m2, tibble(ydstogo=5, down=4, posteam_spread=-7), type="response")
predict(m2, tibble(ydstogo=5, down=4, posteam_spread=0), type="response")
predict(m2, tibble(ydstogo=5, down=4, posteam_spread=7), type="response")
predict(m2, tibble(ydstogo=5, down=4, posteam_spread=14), type="response")







