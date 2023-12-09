
B = 100 ### num bootstrap samples
# HYPERPARAM_COMBO_IDX = 1
xgb_features = c("n","x","s")

######################################
### G == num games
### N == num plays per game
### K == num plays to keep per game
######################################

if (HYPERPARAM_COMBO_IDX == 1) {
  ### same hyperparams as observed data
  G = 4101
  N = 53
  K = N
} else if (HYPERPARAM_COMBO_IDX == 2) {
  ### same number of plays as observed data, but now each play is independent
  G = 4101*53
  N = 53
  K = 1
} else {
  stop(paste0("HYPERPARAM_COMBO_IDX=",HYPERPARAM_COMBO_IDX," is not supported."))
}

print(paste0("$$$$$ hyperparam combo #", HYPERPARAM_COMBO_IDX, ": G=",G,", N=",N, ", K=",K, " $$$$$"))

################################################################################
L = 4 # (EVEN NUMBER) number of yardline bins {0=oTD, 1,2,...,L-1, L=TD}, set so that the mean # epochs per game == actual mean # epochs per game (11.6)
SI = 1 # scoring increment (touchdown)
MIDFIELD = L/2 # midfield field position x = L/2;  L needs to be an even number
MAX_TD_SURPLUS = round(N/MIDFIELD-1) # (nearly) the max possible score
I = G*N # total number of plays in simulation
# G = 6; N = 10; L = 4; SI = 7; MAX_TD_SURPLUS = 3; I = G*N; # for testing

#############################
### Simulation Parameters ###
#############################

# length(unique(data_full_110_WP$game_id)) # actual num. games == 4101
# nrow(data_full_110_WP) / length(unique(data_full_110_WP$game_id)) # actual number of first down plays per game == 53
# nrow(data_full_WP) / length(unique(data_full_WP$game_id)) # actual number of plays per game == 125
# data_full_110_WP %>% group_by(game_id, home_team, drive) %>% summarise() %>% summarise(count = n(), .groups="drop") %>% summarise(avg_drives_per_game = mean(count)) ### 21.69 drives (possessions) per game
# data_full_110_WP %>% group_by(game_id, epoch) %>% summarise() %>% summarise(count = n(), .groups="drop") %>% summarise(avg_epochs_per_game = mean(count)) ### 11.6 epochs per game
# data_full_WP %>% group_by(game_id, home_team, drive) %>% summarise(num_plays_per_poss = n(), .groups="drop") %>% summarise(num_plays_per_poss = mean(num_plays_per_poss)) ### 5.6 number of plays per possession

sim_str_0 = paste0("sim2_h", HYPERPARAM_COMBO_IDX, "_G", G, "_N", N, "_K", K, "_L", L)
print(sim_str_0)

######################################
### G == num games
### N == num plays per game
### K == num plays to keep per game
######################################

############################
### True Win Probability ###
############################

WP_true <- array(dim=c(1+N+1,L-1,MAX_TD_SURPLUS*2+1))
dimnames(WP_true)[[1]] <- paste0("n=", 0:(N+1))
dimnames(WP_true)[[2]] <- paste0("x=", 1:(L-1))
dimnames(WP_true)[[3]] <- paste0("s=", SI*(-MAX_TD_SURPLUS:MAX_TD_SURPLUS))

get_WP_true <- function(n,x,s) {
  if (s < -MAX_TD_SURPLUS*SI) {
    s_ = -MAX_TD_SURPLUS*SI
  } else if (s > MAX_TD_SURPLUS*SI) {
    s_ = MAX_TD_SURPLUS*SI
  } else {
    s_ = s
  }
  
  WP_true[paste0("n=",n), paste0("x=",x), paste0("s=",s_)]
}

######################################
### fill the `WP_true` array and save it
set_WP_true <- function(value, n,x,s) {
  WP_true[paste0("n=",n), paste0("x=",x), paste0("s=",s)] <<- value
}
fill_WP_true_array <- function() {
  for (n in (N+1):0) {
    for (x in 1:(L-1)) {
      for (s in SI*(-MAX_TD_SURPLUS:MAX_TD_SURPLUS)) {
        # print(paste0("computing WP true for ", "n=", n, ", x=", x, ", s=", s))
        
        if (n == N+1) { ### base case
          if (s > 0) {
            wp = 1
          } else if (s == 0) {
            wp = 1/2
          } else { # s < 0
            wp = 0
          }
          set_WP_true(wp, n,x,s)
        } else { ### recursive case
          if (x == L-1) {
            wp_if_1 = get_WP_true(n+1, MIDFIELD, s-SI) # opposing team touchdown
            wp_if_n1 = get_WP_true(n+1, x-1, s)
          } else if (x == 1) {
            wp_if_1 = get_WP_true(n+1, x+1, s)
            wp_if_n1 = get_WP_true(n+1, MIDFIELD, s+SI) # touchdown
          } else { # 1 < x < L-1
            wp_if_1 = get_WP_true(n+1, x+1, s)
            wp_if_n1 = get_WP_true(n+1, x-1, s)
          }
          wp = 1/2*wp_if_1 + 1/2*wp_if_n1
          set_WP_true(wp, n,x,s) 
        }
      }
    }
  }
}

wp_true_filename = paste0("job_output/", sim_str_0, "_WP_true.rds")
if (!file.exists(wp_true_filename)) {
  fill_WP_true_array()
  saveRDS(WP_true, wp_true_filename)
} else {
  ### load the `WP_true` array 
  WP_true = readRDS(wp_true_filename)
}

####################################
### visualize WP_true and WP_hat

### WP vs. time for various score differential values, from midfield
df_visualize_wp = tibble()
for (x in c(1, MIDFIELD, L-1)) {
  for (s in -9:9) {
    df_visualize_wp = bind_rows(df_visualize_wp, tibble(n=1:N, x=x, s=s))
  }
}
df_visualize_wp = df_visualize_wp %>%
  rowwise() %>%
  mutate(x_ = case_when(
    x==MIDFIELD ~ paste0("x = ", MIDFIELD, " (midfield)"),
    x==1 ~ paste0("x = ", 1, " (about to score)"),
    x==L-1 ~ paste0("x = ", L-1, " (opponent about to score)"),
    TRUE ~ ""
  )) %>%
  mutate(
    s_ = paste0("score diff = ", s)
  ) %>%
  ungroup()

visualize_wp <- function(wp_true=TRUE, wp_xgb_model=NULL, wp_boot_mat=NULL, boot_method="rcb", brp_=1, option=1, demo=FALSE) {
  my_palette <- c(
    brewer.pal(name="Blues",n=9)[4:9],
    rev(brewer.pal(name="Purples",n=9)[6:8]),
    "magenta", "black",
    rev(brewer.pal(name="Greens",n=9)[2:9])
  )
  
  if (demo) {
    valid_xs = c(3)
    # valid_ss = c(0,1,2,3)
    # my_palette = my_palette[c(10,7,4,1)]
    # valid_ss = c(-1,0,1) 
    # my_palette = my_palette[c(7,10,4)] 
    valid_ss = c(-1,0,1,2) 
    my_palette = my_palette[c(7,10,4,1)] 
    
    df_visualize_wp1 = 
      df_visualize_wp %>% 
      filter(x == valid_xs) %>%
      filter(s %in% valid_ss) %>%
      ungroup()
  } else {
    df_visualize_wp1 = df_visualize_wp
  }
  
  plot_df1 = tibble()
  plot_df2 = tibble()
  plot_df3 = tibble()
  if (wp_true) {
    ### plot WP true 
    plot_df1 = df_visualize_wp1 %>% 
      rowwise() %>% 
      mutate(wp = get_WP_true(n,x,s), wp_type="true") %>% 
      ungroup()
  } 
  if (!is.null(wp_xgb_model)) {
    ### plot WP pred 
    plot_df2 = df_visualize_wp1 %>% 
      mutate(
        wp = predict(wp_xgb_model, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp1 %>% select(all_of(xgb_features))) )),
        wp_type = "ML"
        # wp_type = "XGBoost"
      )
  }
  if (!is.null(wp_boot_mat)) {
    wp_boot_mat_L = wp_boot_mat %>%
      filter(brp == brp_) %>%
      rename(wp = all_of(paste0("wp_pred_", boot_method, "_L"))) %>%
      mutate(wp_type = "lower boot.") %>%
      select(wp, wp_type)
    wp_boot_mat_U = wp_boot_mat %>%
      filter(brp == brp_) %>%
      rename(wp = all_of(paste0("wp_pred_", boot_method, "_U"))) %>%
      mutate(wp_type = "upper boot.") %>%
      select(wp, wp_type)
    plot_df3 = df_visualize_wp %>% bind_cols(wp_boot_mat_L)
    plot_df3 = bind_rows(plot_df3, df_visualize_wp%>% bind_cols(wp_boot_mat_U))
    
    if (demo) {
      plot_df3 = 
        plot_df3 %>% 
        filter(x == valid_xs) %>%
        filter(s %in% valid_ss) %>%
        ungroup()
    } 
  }
  
  plot_df_A = bind_rows(plot_df1, plot_df2, plot_df3) %>%
    mutate(wp_type = factor(wp_type, levels=c("true", sort(setdiff(wp_type,"true")) )))
  if (option == 1) {
    # here
    # browser()
    
    if (!is.null(wp_boot_mat)) {
      plot_df_A %>%
        pivot_wider(names_from = wp_type, values_from = wp, names_glue = "{wp_type}_{.value}") %>%
        ggplot(aes(x=n, color=factor(s), fill =  factor(s))) +
        facet_wrap(~x_) +
        geom_line(aes(y=true_wp), linewidth=1) +
        geom_ribbon(aes(ymin=`lower boot._wp`,ymax=`upper boot._wp`), alpha=0.5) +
        scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
        scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
        guides(color=guide_legend(title=" score\n differential\n s"),
               fill=guide_legend(title=" score\n differential\n s")) +
        guides(linetype=guide_legend(title="WP")) +
        scale_color_manual(values = my_palette) + 
        scale_fill_manual(values = my_palette)
    } else {
      plot_df_A %>%
        ggplot(aes(x=n, y=wp, color=factor(s))) +
        facet_wrap(~x_) +
        geom_line(aes(linetype=wp_type), linewidth=1) +
        scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
        scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
        guides(color=guide_legend(title=" score\n differential\n s")) +
        guides(linetype=guide_legend(title="WP")) +
        scale_color_manual(values = my_palette)
    }
  } else if (option == 2) {
    plot_df_A %>% 
      filter(s %in% -4:4) %>%
      ggplot(aes(x=n, y=wp, color=factor(x))) +
      facet_wrap(~fct_reorder(s_,s) ) +
      geom_line(aes(linetype=wp_type), linewidth=1) +
      scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
      scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
      guides(color=guide_legend(title=" field\n position\n x")) +
      guides(linetype=guide_legend(title="WP")) +
      scale_color_manual(values = c("firebrick", "black", "dodgerblue2"))
  }
}

######################################
### visualize WP_true

plot_wp_true_vs_time_d = visualize_wp(wp_true=TRUE, demo=TRUE)
plot_wp_true_vs_time = visualize_wp(wp_true=TRUE)
# ggsave(paste0("job_output/", sim_str_0, "_plot_wp_true_vs_time.png"), plot_wp_true_vs_time, width=24, height=8)
# ggsave(paste0("job_output/", sim_str_0, "_plot_wp_true_vs_time_d.png"), plot_wp_true_vs_time_d, width=8, height=6)
#####ggsave(paste0("job_output/", sim_str_0, "_plot_wp_true_vs_time_2.png"), plot_wp_true_vs_time_2, width=16, height=12)
#####plot_wp_true_vs_time_2 = visualize_wp(wp_true=TRUE, option=2)
### plot_wp_true_vs_time

###########################################
### Simulated Football Dataframes (X,y) ###
###########################################

sample_Rademacher <- function(size=1) {
  sample(c(1,-1), size=size, replace=TRUE)
}

simulate_football_season <- function() {
  XI = sample_Rademacher(size=I) ### simulated outcome of this play
  x = numeric(I) ### field position prior to this play
  s = numeric(I) ### score differential prior to this play
  TD = numeric(I) ### td on this play
  TD_OPP = numeric(I) ### opp td on this play
  for (g in 1:G) {
    for (n in 1:N) {
      if (n == 0 & G %% 100 == 0) print(paste0("simulating play n = ", n, " of N = ", N, " in game g = ", g, " of G = ", G))
      
      play_idx = n + (g-1)*N
      
      ########################################### next field position
      if (n == 1) { ### starting play of the game
        x[play_idx] = MIDFIELD 
        s[play_idx] = 0
        TD[play_idx] = FALSE
        TD_OPP[play_idx] = FALSE
      } else {
        if (TD[play_idx-1]) { ### TD on previous play
          x[play_idx] = MIDFIELD # back to midfield
          s[play_idx] = s[play_idx-1] + SI # score a TD
        } else if (TD_OPP[play_idx-1]) { ### opp TD on previous play
          x[play_idx] = MIDFIELD # back to midfield
          s[play_idx] = s[play_idx-1] - SI # opp scores a TD
        } else {
          x[play_idx] = x[play_idx-1] + XI[play_idx-1] # advance field position
          s[play_idx] = s[play_idx-1] # no score
        }
        TD[play_idx] = x[play_idx] + XI[play_idx] == 0
        TD_OPP[play_idx] = x[play_idx] + XI[play_idx] == L
        
      }
    }
  }
  
  df = tibble(
    i = 1:I,
    g = 1+floor((i-1)/N),
    n = i - (g-1)*N,
    x = x,
    s = s,
    XI = XI,
    TD = TD,
    TD_OPP = TD_OPP,
  ) %>%
    group_by(g) %>%
    mutate(
      s_final = case_when(
        TD[n()] == 1 ~ s[n()] + SI,
        TD_OPP[n()] == 1 ~ s[n()] - SI,
        TRUE ~ s[n()]
      ),
      y = case_when(
        s_final > 0 ~ 1,
        s_final < 0 ~ 0,
        TRUE ~ as.numeric(rbernoulli(1, p=1/2)) # if tied at end of regulation, win prob is 1/2, so use a coin flip
      )
    ) %>% 
    ungroup()
  
  if (K == N) {
    df1 = df
  } else if (K == 1) {
    df1 = df %>% filter( n-1 == ((g-1)%%N) )
  } else {
    stop("haven't implemented K where 1<K<N")
  }
  # df2 = df1 %>%
  #   group_by(g) %>%
  #   filter(row_number() == sort(sample(1:n(), size=K, replace=FALSE))) %>%
  #   ungroup() 
  
  df2 = df1 %>%
    rowwise() %>%
    mutate(
      wp_actual = get_WP_true(n, x, s)
    ) %>%
    ungroup()
  
  df2
}
# simulate_football_season()
# View(simulate_football_season())

#####################################################
### check number of epochs, num plays per epoch   ###
### are consistent with observed football dataset ###
#####################################################

{
  # sfs1 = simulate_football_season()
  # sfs1a = sfs1 %>%
  #   mutate(
  #     score = as.numeric(TD==1 | TD_OPP==1),
  #     just_scored = lag(score, default=0),
  #     epoch = 1+cumsum(just_scored),
  #   ) %>%
  #   group_by(epoch) %>%
  #   summarise(
  #     num_plays = n()
  #   ) %>%
  #   summarise(
  #     mean_num_plays_per_epoch = mean(num_plays),
  #     num_epochs = n()
  #   )
  # sfs1a
  # ### in simulated football data: mean_num_plays_per_epoch == 4.096,  num_epochs == 53069
  # 
  # ################
  # # filewd = getwd()
  # # setwd("..")
  # # source("00_main.R")
  # # setwd(filewd)
  # ################
  # # data_full_WP %>%
  # data_full_110_WP %>%
  #   distinct(game_id, epoch, play_id) %>%
  #   group_by(game_id, epoch) %>%
  #   summarise(num_plays = n(), .groups="drop") %>%
  #   summarise(
  #     mean_num_plays_per_epoch = mean(num_plays),
  #     num_epochs = n()
  #   )
  # ### in observed football data:  mean_num_plays_per_epoch == 4.549,  num_epochs == 47515
}
