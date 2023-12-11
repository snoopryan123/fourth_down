
### num boot models to load 
B = 100 # justified by the stability analysis

### load XGBoost files
source("../0_clean_lm.R")
source("T2_models_xgb.R")
source("D2_coach_decision_models.R")

### load models
V1_model_name_WP = xgb_wp_110_7_model_name
V1_wp_model_fitList_boot <- list()
for (b in 1:B) {
  print(paste0("loading boostrap b = ", b, " of B = ", B))
  ### bootstrapped models
  V1_wp_model_fitList_boot[[b]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_WP, "_b", b), ".rds"))
}
V1_wp_model_obs = V1_wp_model_fitList_boot[[1]]
fg_model_obs = load_lm(paste0("fitted_models/", "fg_model", "_b", 1, ".rds"))
punt_model_obs = load_lm(paste0("fitted_models/", "punt_model", "_b", 1, ".rds"))
go_model_obs = load_lm(paste0("fitted_models/", "go_model", "_b", 1, ".rds"))
coach_model = xgb.load(paste0("fitted_models/", "coach_model.rds")) 

################################################################################

### plot the models

plot_models = FALSE
if (plot_models) {
  ### load data
  filewd = getwd()
  setwd("..")
  source("00_main.R")
  setwd(filewd)
  
  ### plot FG model
  plot_fg_model= plot_fg_prob_by_kq(fg_model_obs)
  ggsave(paste0("fitted_models/", "plot_fg_model.png"), plot_fg_model, width=8, height=6)
  
  ### plot Punt model
  plot_punt_model= plot_punt_eny_by_pq(punt_model_obs)
  ggsave(paste0("fitted_models/", "plot_punt_model.png"), plot_punt_model, width=8, height=6)
  
  ### plot Go model
  plot_go_model= plot_conv_(go_model_obs)
  ggsave(paste0("fitted_models/", "plot_conv.png"), plot_go_model, width=8, height=6)
  
  ### plot Coach model
  
  #########################################################
  ### Plot Models: P_conv as a function of team quality ###
  #########################################################
  
  plot_conv_model_varyQ <- function(go_plot_o_df_p, title="") {
    go_model_o_plot_ = go_plot_o_df_p %>%
      mutate(color_col = factor(round(qbq_ot_0_sum,2))) %>%
      mutate(color_col = fct_reorder(color_col, -1*qbq_ot_0_sum)) %>%
      ggplot(aes(x = ydstogo, y = p, color = color_col)) +
      # facet_wrap(~model) +
      geom_line(size=1) +
      ylab("conversion probability") + xlab("yards to go") +
      labs(color=" quarterback\n quality", title=title) +
      scale_y_continuous(breaks=seq(0,1,by=0.1)) +
      scale_x_continuous(breaks= if (ydl==4) {seq(0,4,by=1)} else if (ydl==10) {seq(0,10,by=2)}else {seq(0,100,by=4)} ) +
      theme(axis.title = element_text(size=20),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size=20),
            legend.text = element_text(size=20),
            legend.title = element_text(size=20)) +
      scale_colour_manual(values = gradient1) +
      theme(
        axis.title = element_text(size=30)
      )
    
    go_model_o_plot_
  }
  
  plot_conv_model_varyO <- function(go_plot_o_df_p, title="") {
    go_model_o_plot_ = go_plot_o_df_p %>%
      mutate(color_col = factor(round(oq_rot_0_total_sum,2))) %>%
      mutate(color_col = fct_reorder(color_col, -1*oq_rot_0_total_sum)) %>%
      ggplot(aes(x = ydstogo, y = p, color = color_col)) +
      # facet_wrap(~model) +
      geom_line(size=1) +
      ylab("conversion probability") + xlab("yards to go") +
      labs(color=" offensive\n quality\n of the\n rest of the\n offensive\n team", title=title) +
      scale_y_continuous(breaks=seq(0,1,by=0.1)) +
      scale_x_continuous(breaks= if (ydl==4) {seq(0,4,by=1)} else if (ydl==10) {seq(0,10,by=2)}else {seq(0,100,by=4)} ) +
      theme(axis.title = element_text(size=20),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size=20),
            legend.text = element_text(size=20),
            legend.title = element_text(size=20)) +
      scale_colour_manual(values = gradient1) +
      theme(
        axis.title = element_text(size=30)
      )
    
    go_model_o_plot_
  }
  
  plot_conv_model_varyDP <- function(go_plot_d_df_p, title="") {
    go_model_d_plot = go_plot_d_df_p %>%
      mutate(color_col = factor(round(dq_dt_0_againstPass_sum,2))) %>%
      mutate(color_col = fct_reorder(color_col, -1*dq_dt_0_againstPass_sum)) %>%
      ggplot(aes(x = ydstogo, y = p, color = color_col)) +
      # facet_wrap(~model) +
      geom_line(size=1) +
      ylab("conversion probability") + xlab("yards to go") +
      labs(color=" defensive\n quality\n against\n the pass", title=title) +
      scale_y_continuous(breaks=seq(0,1,by=0.1)) +
      scale_x_continuous(breaks= if (ydl==4) {seq(0,4,by=1)} else if (ydl==10) {seq(0,10,by=2)}else {seq(0,100,by=4)} ) +
      theme(axis.title = element_text(size=20),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size=20),
            legend.text = element_text(size=20),
            legend.title = element_text(size=20)) +
      scale_colour_manual(values = gradient1) +
      theme(
        axis.title = element_text(size=30)
      )
    go_model_d_plot
  }
  
  plot_conv_model_varyDR <- function(go_plot_d_df_p, title="") {
    go_model_d_plot = go_plot_d_df_p %>%
      mutate(color_col = factor(round(dq_dt_0_againstRun_sum,2))) %>%
      mutate(color_col = fct_reorder(color_col, -1*dq_dt_0_againstRun_sum)) %>%
      ggplot(aes(x = ydstogo, y = p, color = color_col)) +
      # facet_wrap(~model) +
      geom_line(size=1) +
      ylab("conversion probability") + xlab("yards to go") +
      labs(color=" defensive\n quality\n against\n the run", title=title) +
      scale_y_continuous(breaks=seq(0,1,by=0.1)) +
      scale_x_continuous(breaks= if (ydl==4) {seq(0,4,by=1)} else if (ydl==10) {seq(0,10,by=2)}else {seq(0,100,by=4)} ) +
      theme(axis.title = element_text(size=20),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size=20),
            legend.text = element_text(size=20),
            legend.title = element_text(size=20)) +
      scale_colour_manual(values = gradient1) +
      theme(
        axis.title = element_text(size=30)
      )
    go_model_d_plot
  }
  
  ydl = 40
  {
    tq_breaks = round(seq(-1,1,length=9),2)
    go_plot_q_df = tibble()
    go_plot_o_df = tibble()
    go_plot_dp_df = tibble()
    go_plot_dr_df = tibble()
    max_ytg = 40
    
    for (tq in tq_breaks) {
      go_plot_q_df = bind_rows(go_plot_q_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = tq, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = 0, down=4))
      go_plot_o_df = bind_rows(go_plot_o_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = 0, oq_rot_0_total_sum = tq, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = 0, down=4))
      go_plot_dp_df = bind_rows(go_plot_dp_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = tq, dq_dt_0_againstRun_sum = 0, down=4))
      go_plot_dr_df = bind_rows(go_plot_dr_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = tq, down=4))
    }
    
    ###################################################################################
    
    qbtib2 = bind_rows(
      bind_cols(
        go_plot_q_df %>% mutate(p = predict(go_model_obs, ., type="response")),
        model = "logistic regression (best)"
      )
    )
    oqtib2 = bind_rows(
      bind_cols(
        go_plot_o_df %>% mutate(p = predict(go_model_obs, ., type="response")),
        model = "logistic regression (best)"
      )
    )
    dpqtib2 = bind_rows(
      bind_cols(
        go_plot_dp_df %>% mutate(p = predict(go_model_obs, ., type="response")),
        model = "logistic regression (best)"
      )
    )
    drqtib2 = bind_rows(
      bind_cols(
        go_plot_dr_df %>% mutate(p = predict(go_model_obs, ., type="response")),
        model = "logistic regression (best)"
      )
    )
    
    pq2 = plot_conv_model_varyQ(qbtib2)
    # pq2
    po2 = plot_conv_model_varyO(oqtib2)
    # po2
    pdp2 = plot_conv_model_varyDP(dpqtib2)
    # pdp2
    pdr2 = plot_conv_model_varyDR(drqtib2)
    # pdr2
    pqod2 = cowplot::plot_grid(pq2, po2, pdp2, pdr2, nrow=1)
    # pqod2
    
    ggsave(paste0("fitted_models/", "plot_ydl_lr_best.png"), pqod2, width=30, height=6)
  }
}

