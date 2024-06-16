
######################################
### Parameters common to all files ###
######################################

### parameters common to all files
SI = 1 # scoring increment (touchdown)
L = 4 # (EVEN NUMBER) number of yardline bins {0=oTD, 1,2,...,L-1, L=TD}, set so that the mean # epochs per game == actual mean # epochs per game (11.6)
MIDFIELD = L/2 # midfield field position x = L/2;  L needs to be an even number
B = 101 # num bootstrap samples
xgb_features = c("n","x","s") # XGBoost features
M = 100 ### number of simulations
N = 56 ### number of plays per game
RETUNE_XGB = FALSE

############################
### True Win Probability ###
############################

get_WP_true_filename <- function(N) {
  paste0("job_output/","WP_true_N",N,".rds")
}

get_WP_true <- function(n,x,s,WP_true,N) {
  MAX_TD_SURPLUS = round(N/MIDFIELD-1) # (nearly) the max possible score

    if (s < -MAX_TD_SURPLUS*SI) {
    s_ = -MAX_TD_SURPLUS*SI
  } else if (s > MAX_TD_SURPLUS*SI) {
    s_ = MAX_TD_SURPLUS*SI
  } else {
    s_ = s
  }
  WP_true[paste0("n=",n), paste0("x=",x), paste0("s=",s_)]
}

set_WP_true <- function(value,n,x,s,WP_true) {
  WP_true[paste0("n=",n), paste0("x=",x), paste0("s=",s)] = value
  WP_true
}

get_WP_true_mat <- function(N) {
  MAX_TD_SURPLUS = round(N/MIDFIELD-1) # (nearly) the max possible score

  WP_true <- array(dim=c(1+N+1,L-1,MAX_TD_SURPLUS*2+1))
  dimnames(WP_true)[[1]] <- paste0("n=", 0:(N+1))
  dimnames(WP_true)[[2]] <- paste0("x=", 1:(L-1))
  dimnames(WP_true)[[3]] <- paste0("s=", SI*(-MAX_TD_SURPLUS:MAX_TD_SURPLUS))
  
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
            WP_true = set_WP_true(wp,n,x,s,WP_true)
          } else { ### recursive case
            if (x == L-1) {
              wp_if_1 = get_WP_true(n+1, MIDFIELD, s-SI, WP_true, N) # opposing team touchdown
              wp_if_n1 = get_WP_true(n+1, x-1, s, WP_true, N)
            } else if (x == 1) {
              wp_if_1 = get_WP_true(n+1, x+1, s, WP_true, N)
              wp_if_n1 = get_WP_true(n+1, MIDFIELD, s+SI, WP_true, N) # touchdown
            } else { # 1 < x < L-1
              wp_if_1 = get_WP_true(n+1, x+1, s, WP_true, N)
              wp_if_n1 = get_WP_true(n+1, x-1, s, WP_true, N)
            }
            wp = 1/2*wp_if_1 + 1/2*wp_if_n1
            WP_true = set_WP_true(wp,n,x,s,WP_true) 
          }
        }
      }
    }
    WP_true
  }
  
  ### fill the `WP_true` array and save it
  wp_true_filename = get_WP_true_filename(N)
  if (!file.exists(wp_true_filename)) {
    WP_true = fill_WP_true_array()
    saveRDS(WP_true, wp_true_filename)
  } else {
    ### load the `WP_true` array 
    WP_true = readRDS(wp_true_filename)
  }
  
  return(WP_true)
}

get_df_visualize_wp <- function(N) {
  ### WP vs. time for various score differential values, from midfield
  df_visualize_wp = tibble()
  for (x in c(1, MIDFIELD, L-1)) {
    for (s in -9:9) {
      df_visualize_wp = bind_rows(df_visualize_wp, tibble(n=1:N, x=x, s=s))
    }
  }
  df_visualize_wp = 
    df_visualize_wp %>%
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
  df_visualize_wp
}

visualize_wp <- function(WP_true, N, wp_true=TRUE, wp_xgb_model=NULL, wp_boot_mat=NULL, boot_method="rcb", brp_=1, option=1, demo=FALSE) {
  my_palette <- c(
    brewer.pal(name="Blues",n=9)[4:9],
    rev(brewer.pal(name="Purples",n=9)[6:8]),
    "magenta", "black",
    rev(brewer.pal(name="Greens",n=9)[2:9])
  )
  
  ### WP vs. time for various score differential values, from midfield
  df_visualize_wp = get_df_visualize_wp(N)

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
    plot_df1 = 
      df_visualize_wp1 %>% 
      rowwise() %>% 
      mutate(wp = get_WP_true(n,x,s,WP_true,N), wp_type="true") %>% 
      ungroup()
  } 
  if (!is.null(wp_xgb_model)) {
    ### plot WP pred 
    plot_df2 = 
      df_visualize_wp1 %>% 
      mutate(
        wp = predict(wp_xgb_model, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp1 %>% select(all_of(xgb_features))) )),
        # wp_type = "ML"
        wp_type = "XGBoost"
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

visualize_WP_true <- function(N) {
  ### visualize WP_true
  WP_true = get_WP_true_mat(N)
  plot_wp_true_vs_time_d = visualize_wp(WP_true, N, wp_true=TRUE, demo=TRUE)
  plot_wp_true_vs_time = visualize_wp(WP_true, N, wp_true=TRUE)
  
  ggsave(paste0("job_output/","WP_true_plot_N",N,".png"), plot_wp_true_vs_time, width=24, height=8)
  ggsave(paste0("job_output/","WP_true_plot1_N",N,".png"), plot_wp_true_vs_time_d, width=8, height=6)
}

# ### check
# get_WP_true_mat(N=56)
# visualize_WP_true(N=56)

##########################################################
### Create a simulated football season dataframe (X,y) ###
##########################################################

sample_Rademacher <- function(size=1) {
  sample(c(1,-1), size=size, replace=TRUE)
}

simulate_football_season <- function(G,N,K) {
  I = G*N # total number of plays in simulation
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
  
  WP_true = get_WP_true_mat(N)
  df2 = df1 %>%
    rowwise() %>%
    mutate(
      wp_actual = get_WP_true(n, x, s, WP_true, N)
    ) %>%
    ungroup()
  
  df2
}

# ### check
# simulate_football_season(G=100,N=10,K=10)
# View(simulate_football_season(G=100,N=10,K=10))

#########################
### XGBoost Functions ###
#########################

fit_xgb <- function(params, train_df, val_df=NULL, nrounds=NULL) {
  ### fit the XGBoost model
  train_set_xgbDM = xgb.DMatrix(
    model.matrix(~ . + 0, data = train_df %>% select(all_of(xgb_features))),
    label = train_df$y
  )
  if (is.null(val_df)) {
    watchlist <- list(train=train_set_xgbDM)
  } else {
    val_set_xgbDM = xgb.DMatrix(
      model.matrix(~ . + 0, data = val_df %>% select(all_of(xgb_features))),
      label = val_df$y
    )
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  }
  ### train XGBoost
  xgb <- xgb.train( 
    data = train_set_xgbDM, 
    watchlist = watchlist,
    params = params, 
    nrounds = if (is.null(nrounds)) 15000 else nrounds,
    early_stopping_rounds = if (is.null(nrounds)) 50 else Inf,
    print_every_n = 50,
    verbose = 2
  )
  return(xgb)
}

predict_xgb <- function(xgb_model, df_test) {
  df_test_xgbDM = xgb.DMatrix(
    model.matrix(~ . + 0, data = df_test %>% select(all_of(xgb_features)))
  )
  predict(xgb_model, df_test_xgbDM)
}

tune_xgboost <- function(train_df, val_df, params_filename, grid_size=40) {
  print("tuning XGBoost")
  ### Baldwin's XGBoost tuning https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
  
  ### parameter tuning grid
  get_param_grid <- function(train_df) {
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), train_df),
      dials::min_n(),
      dials::tree_depth(range=c(length(xgb_features), length(xgb_features))),
      # dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
      dials::learn_rate(range = c(-1, -0.5), trans = scales::log10_trans()),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = grid_size
    ) %>%
      dplyr::mutate(
        mtry = mtry / length(train_df),
        monotone_constraints = "(0,-1,1)"
      ) %>%
      # make these the right names for xgb
      dplyr::rename(
        eta = learn_rate,
        gamma = loss_reduction,
        subsample = sample_size,
        colsample_bytree = mtry,
        max_depth = tree_depth,
        min_child_weight = min_n
      )
  }
  # get_param_grid()
  
  # function to perform xgb.cv for a given row in a hyperparameter grid
  get_row <- function(row) {
    params <-
      list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = c("logloss"),
        eta = row$eta,
        gamma = row$gamma,
        subsample = row$subsample,
        colsample_bytree = row$colsample_bytree,
        max_depth = row$max_depth,
        min_child_weight = row$min_child_weight,
        monotone_constraints = row$monotone_constraints
      )
    
    ### fit the XGBoost model
    xgb = fit_xgb(params, train_df, val_df)
    
    # bundle up the results together for returning
    output <- params
    output$iter <- xgb$best_iteration
    output$logloss <- xgb$evaluation_log[output$iter]$validation_logloss
    
    row_result <- bind_rows(output)
    return(row_result)
  }
  
  # get results
  results <- purrr::map_df(1:grid_size, function(i) {
    print(paste0("tuning row ", i, " of ", grid_size))
    get_row(get_param_grid(train_df) %>% filter(row_number() == i)  )
  })
  
  # plot tuning results
  # {
  #   results %>%
  #     dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  #     tidyr::pivot_longer(
  #       eta:min_child_weight,
  #       values_to = "value",
  #       names_to = "parameter"
  #     ) %>%
  #     ggplot(aes(value, logloss, color = parameter)) +
  #     geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  #     facet_wrap(~parameter, scales = "free_x") +
  #     labs(x = NULL, y = "logloss") +
  #     theme_minimal()
  # }
  
  ### best XGBoost model
  best_model <- results %>% arrange(logloss) %>% slice_head(n=1)
  print(best_model)
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = best_model$eta,
      gamma = best_model$gamma,
      subsample = best_model$subsample,
      colsample_bytree = best_model$colsample_bytree,
      max_depth = best_model$max_depth,
      min_child_weight = best_model$min_child_weight,
      monotone_constraints = best_model$monotone_constraints,
      nrounds = best_model$iter
    )
  
  ### save xgboost params
  list.save(params, params_filename)
}

##################################################
### Assume we have from some other file (G,N,K):
### G == num games
### N == num plays per game
### K == num plays to keep per game
##################################################

get_param_combo_str <- function(g,G,N,K,m) {
  paste0("sim", "_g", g, "_G", G, "_N", N, "_K", K, "_m", m)
}

### find reasonable values for (G,N)
# setwd("../..")
# source("00_main.R")
# length(unique(data_full_110_WP$game_id)) # actual num. games == 4101
# nrow(data_full_110_WP) / length(unique(data_full_110_WP$game_id)) # average number of first down plays per game == 56
# data_full_110_WP %>% group_by(game_id, epoch) %>% summarise() %>% summarise(count = n(), .groups="drop") %>% summarise(avg_epochs_per_game = mean(count)) ### avg 11.6 epochs per game
# # nrow(data_full_WP) / length(unique(data_full_WP$game_id)) # average number of plays per game == 147
# # data_full_110_WP %>% group_by(game_id, home_team, drive) %>% summarise() %>% summarise(count = n(), .groups="drop") %>% summarise(avg_drives_per_game = mean(count)) ### avg 22 drives per game
# # data_full_WP %>% group_by(game_id, home_team, drive) %>% summarise(num_plays_per_poss = n(), .groups="drop") %>% summarise(num_plays_per_poss = mean(num_plays_per_poss)) ### avg 6.6 number of plays per drive

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
