
library(tidyverse)
library(xgboost) 
library(randomForest)
library(splines)
library(matrixcalc)
library(lme4)
# library(nflfastR) 
library(nflreadr) 
library(grid)
library(nnet)
library(gridExtra)
library(loo)
library(latex2exp)
library(RColorBrewer)
library(cowplot)
library(dials)
library(rlist)
library(gam)
library(ggnewscale)
#####library(gt)
#####library(webshot2)
# this should be the default u y do this R
options(scipen = 999999)
# options(scipen = 50)
output_folder = "./plots/"
theme_set(theme_bw())
# theme_update(text = element_text(size=16))
# theme_update(plot.title = element_text(hjust = 0.5))
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20)
) 
options(pillar.sigfig=4)

### install.packages("gt",repos = "http://cran.us.r-project.org")

# ################################################################################
# ### installing packages (on HPC3)
# myPKGs <- c(
#   "tidyverse",
#   "xgboost",
#   "randomForest",
#   "splines",
#   "matrixcalc",
#   "lme4",
###   "nflfastR",
#   "nflreadr",
#   "grid",
#   "nnet",
#   "gridExtra",
#   "loo",
#   "latex2exp",
#   "RColorBrewer",
#   "cowplot",
#   "dials",
#   "rlist",
#   "gam",
#   "ggnewscale"
# )
# # check to see if each package is installed, and if not add it to a list to install
# InstalledPKGs <- names(installed.packages()[,'Package'])
# InstallThesePKGs <- myPKGs[!myPKGs %in% InstalledPKGs]
# # install any needed packages
# if (length(InstallThesePKGs) > 0) install.packages(InstallThesePKGs)
# ################################################################################

############################################
### Saving LM, GLM, and MULTINOM objects ###
############################################

###
compute_scoreTimeRatio <- function(score_differential, game_seconds_remaining) {
  score_differential/(game_seconds_remaining + 0.01)
}

clean_lm <- function(cm) {
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  cm$model = c()
  
  cm
}

save_lm <- function(cm, filename) {
  attr(cm$terms, ".Environment") <- NULL
  attr(cm$formula,".Environment") <- NULL
  saveRDS(cm, filename)
}

load_lm <- function(filename) {
  cm = readRDS(filename)
  attr(cm$terms, ".Environment") <- globalenv()
  if ( any(str_detect(class(cm), "glm")) ) {
    attr(cm$formula, ".Environment") <- globalenv()
  }
  return(cm)
}


###################################
######### Plot Functions ##########
###################################

gradient1 = c(brewer.pal(name="PuRd",n=9)[3:9], rev(brewer.pal(name="Blues",n=9)[3:8]))

smooth_me <- function(df) {
  v_y = df$v[1:99]
  #v_spline = smooth.spline(99:1, v_y)
  v_spline = smooth.spline(99:1, v_y,df=8)
  rev(v_spline$y)
  temp = tibble(yardline_100=1:99,v_smoothed=rev(v_spline$y))
  left_join(df,temp, by = "yardline_100") 
}

smooth_me2 <- function(yardline_100, v, df=8) {
  v_spline = smooth.spline(yardline_100, v, df=df)
  v_spline$y
}

### plot our field goal probability model
plot_fg_prob_by_kq <- function(fg_model, kq=NULL) {
  kq_breaks = seq(-1,1,length=7)
  fg_plot_df = tibble()
  if (is.null(kq)) {
    for (kq in kq_breaks) {
      fg_plot_df = bind_rows(fg_plot_df, tibble(yardline_100 = 1:99, kq_0_sum_std = kq))
    }
  } else {
    fg_plot_df = tibble(yardline_100 = 1:99, kq_0_sum_std = kq)
  }
  
  fg_model_plot = fg_plot_df %>%
    mutate(fgd = yardline_100 + 17) %>%
    mutate(p = predict(fg_model, ., type="response")) %>%
    mutate(color_col = factor(round(kq_0_sum_std,2))) %>%
    mutate(color_col = fct_reorder(color_col, -1*kq_0_sum_std)) %>%
    ggplot(aes(x = yardline_100, y = p, color = color_col)) +
    geom_line(linewidth=1) +
    ylab("field goal make probability") + xlab("yardline") +
    labs(color=" kicker\n quality") +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    scale_x_continuous(
      breaks=seq(0,100,by=10),
      sec.axis = sec_axis(~.x+17, breaks = seq(27,70,by=10), name="field goal distance")
      # sec.axis = sec_axis(~.x+17, breaks = seq(15,70,by=10), name="field goal distance")
      # sec.axis = sec_axis(~.x+17, breaks = seq(17,70,by=10))
    ) +
    theme(axis.title = element_text(size=20),
          axis.text = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) +
    scale_colour_manual(values = brewer.pal(name="PuRd",n=9)[3:11])
  fg_model_plot
}

### plot our punt expected next yardline model
plot_punt_eny_by_pq <- function(punt_model, pq=NULL) {
  pq_breaks = seq(-1,1,length=7)
  punt_plot_df = tibble()
  if (is.null(pq)) {
    for (pq in pq_breaks) {
      punt_plot_df = bind_rows(punt_plot_df, tibble(yardline_100 = 30:99, pq_0_sum_std = pq))
    }
  } else {
    punt_plot_df = tibble(yardline_100 = 30:99, pq_0_sum_std = pq)
  }
  punt_model_plot = punt_plot_df %>%
    mutate(p = predict(punt_model, ., type="response")) %>%
    mutate(color_col = factor(round(pq_0_sum_std,2))) %>%
    mutate(color_col = fct_reorder(color_col, -1*pq_0_sum_std)) %>%
    ggplot(aes(x = yardline_100, y = p, color = color_col)) +
    geom_line(size=1) +
    ylab("expected next yardline") + xlab("yardline") +
    labs(color=" punter\n quality") +
    scale_y_continuous(breaks=seq(0,100,by=10)) +
    scale_x_continuous(breaks=seq(0,100,by=10), limits=c(30,100)) +
    theme(axis.title = element_text(size=20),
          axis.text = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) +
    scale_colour_manual(values = brewer.pal(name="PuRd",n=9)[3:11])
  punt_model_plot
}

### plot our conversion probability model
plot_conv_prob_by_tq <- function(conv_prob_model) {
  
  plot_conv_prob_model_varyQ <- function(go_plot_o_df_p, title="") {
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
  
  plot_conv_prob_model_varyO <- function(go_plot_o_df_p, title="") {
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
  
  plot_conv_prob_model_varyDP <- function(go_plot_d_df_p, title="") {
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
  
  plot_conv_prob_model_varyDR <- function(go_plot_d_df_p, title="") {
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
  
  
  # for (ydl in c(4,7,10,15,40,50,70,95)) {
  # for (ydl in c(40)) {
  {
    tq_breaks = round(seq(-1,1,length=9),2)
    go_plot_q_df = tibble()
    go_plot_o_df = tibble()
    go_plot_dp_df = tibble()
    go_plot_dr_df = tibble()
    max_ytg = 40
    ydl = 40 #FIXME
    
    for (tq in tq_breaks) {
      go_plot_q_df = bind_rows(go_plot_q_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = tq, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = 0, down=4))
      go_plot_o_df = bind_rows(go_plot_o_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = 0, oq_rot_0_total_sum = tq, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = 0, down=4))
      go_plot_dp_df = bind_rows(go_plot_dp_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = tq, dq_dt_0_againstRun_sum = 0, down=4))
      go_plot_dr_df = bind_rows(go_plot_dr_df, tibble(ydstogo = as.numeric(1:min(ydl, max_ytg)), yardline_100 = ydl, qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = tq, down=4))
    }
    
    ###################################################################################
    
    qbtib2 = bind_rows(
      bind_cols(
        go_plot_q_df %>% mutate(p = predict(go_model_fit, ., type="response")),
        model = ""
      )
    )
    oqtib2 = bind_rows(
      bind_cols(
        go_plot_o_df %>% mutate(p = predict(go_model_fit, ., type="response")),
        # model = "logistic regression (best)"
        model = ""
      )
    )
    dpqtib2 = bind_rows(
      bind_cols(
        go_plot_dp_df %>% mutate(p = predict(go_model_fit, ., type="response")),
        # model = "logistic regression (best)"
        model = ""
      )
    )
    drqtib2 = bind_rows(
      bind_cols(
        go_plot_dr_df %>% mutate(p = predict(go_model_fit, ., type="response")),
        # model = "logistic regression (best)"
        model = ""
      )
    )
    
    pq2 = plot_conv_prob_model_varyQ(qbtib2)
    pq2
    po2 = plot_conv_prob_model_varyO(oqtib2)
    po2
    pdp2 = plot_conv_prob_model_varyDP(dpqtib2)
    pdp2
    pdr2 = plot_conv_prob_model_varyDR(drqtib2)
    pdr2
    pqod2 = cowplot::plot_grid(pq2, po2, pdp2, pdr2, nrow=1)
    pqod2
  }
}

plot_conv_1 <- function(conv_model, qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0) {
  plot_conv = 
    expand.grid(yardline_100 = 1:99, ydstogo=1:10) %>%
    # expand.grid(yardline_100 = 1:93, ydstogo=c(1,2,3,4,10)) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 + ydstogo < 100) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 - ydstogo < 100) %>%
    mutate(qbq_ot_0_sum=qbq_ot_0_sum, oq_rot_0_total_sum=oq_rot_0_total_sum, 
           dq_dt_0_againstPass_sum=dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum=dq_dt_0_againstRun_sum, 
           down=4) %>%
    mutate(p_conv = predict(conv_model, ., type="response")) %>%
    ggplot(aes(x = yardline_100, y=p_conv, color=factor(ydstogo))) +
    geom_line(linewidth=1) +
    ylab("conversion probability") +
    xlab("yardline") +
    scale_x_continuous(breaks=seq(0,100,by=10)) +
    scale_y_continuous(breaks=seq(0,1,by=0.05)) +
    labs(color=" yards\n to go") +
    scale_colour_manual(values = rev(c(
      rev(brewer.pal(name="Blues",n=9)[5:9]),
      # brewer.pal(name="Purples",n=9)[6:9],
      rev(brewer.pal(name="Reds",n=9)[3:7])
    ))) +
    theme(
      axis.title = element_text(size=30)
    )
  plot_conv
}

plot_conv_2 <- function(conv_model, qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0) {
  plot_conv = 
    expand.grid(yardline_100 = 1:99, ydstogo=1:15) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 + ydstogo < 100) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 - ydstogo < 100) %>%
    mutate(qbq_ot_0_sum=qbq_ot_0_sum, oq_rot_0_total_sum=oq_rot_0_total_sum, 
           dq_dt_0_againstPass_sum=dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum=dq_dt_0_againstRun_sum, 
           down=4) %>%
    mutate(p_conv = predict(conv_model, ., type="response")) %>%
    group_by(ydstogo) %>%
    summarise(
      p_conv = mean(p_conv),
    ) %>%
    ggplot() +
    # geom_col(aes(x=ydstogo,y=p_conv, fill=-p_conv), show.legend = FALSE) +
    geom_col(aes(x=ydstogo,y=p_conv), fill="black") +
    scale_fill_gradient2(low = muted("red"),
                         # mid ="white",
                         high = muted("black"),) +
    ylab("conversion probability") +
    xlab("yards to go") +
    scale_x_continuous(breaks=seq(0,100,by=2)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    scale_colour_manual(values = rev(c(
      rev(brewer.pal(name="Blues",n=9)[5:9]),
      # brewer.pal(name="Purples",n=9)[6:9],
      rev(brewer.pal(name="Reds",n=9)[3:7])
    ))) +
    theme(
      axis.title = element_text(size=30)
    )
  plot_conv
}

### plot our conversion expected outcome models

plot_go_exp_outcome_1 <- function(conv_model, success=TRUE, qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0) {
  plot_conv = 
    expand.grid(yardline_100 = 1:99, ydstogo=1:10) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 + ydstogo < 100) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 - ydstogo < 100) %>%
    mutate(qbq_ot_0_sum=qbq_ot_0_sum, oq_rot_0_total_sum=oq_rot_0_total_sum, 
           dq_dt_0_againstPass_sum=dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum=dq_dt_0_againstRun_sum, 
           down=4) %>%
    mutate(E_outcome = predict(conv_model, .)) %>%
    ggplot(aes(x = yardline_100, y=E_outcome, color=factor(ydstogo))) +
    geom_line(linewidth=1) +
    ylab("expected yards gained") +
    xlab("yardline") +
    scale_x_continuous(breaks=seq(0,100,by=10)) +
    labs(color=" yards\n to go", 
         subtitle= paste0("given a ", if (success) "successful" else "failed",  " conversion")) +
    scale_colour_manual(values = rev(c(
      rev(brewer.pal(name="Blues",n=9)[5:9]),
      # brewer.pal(name="Purples",n=9)[6:9],
      rev(brewer.pal(name="Reds",n=9)[3:7])
    ))) +
    theme(
      axis.title = element_text(size=30)
    )
  plot_conv
}

######################
### Loss Functions ###
######################

MAE <- function(x,y) {
  mean(abs(x-y))
}

RMSE <- function(x,y) {
  sqrt( mean((x-y)**2)  )
}

LOGLOSS <- function(y,p) {
  ### if p == 0, replace p with 10^-15
  ### if p == 1, replace p with 1-10^-15
  p = (tibble(p) %>% rowwise() %>% mutate(p_ = max(min(p, 1-10^-15), 10^-15)) )$p_
  -1 * mean( y*log(p) + (1-y)*log(1-p) )
}

compute_loss_by_time_bin <- function(model, dataset, model_name, m=2.5, xgb_features=NULL, xgb_wp=TRUE, endgame=FALSE) {
  model_type = case_when(
    str_detect(model_name, "mlr") ~ "mlr",
    str_detect(model_name, "lr") ~ "lr",
    str_detect(model_name, "gam") ~ "gam",
    str_detect(model_name, "xgb") & str_detect(model_name, "TT") ~ "xgbTT",
    str_detect(model_name, "xgb") ~ "xgb",
    TRUE~ "other"
  )
  if (model_type == "mlr") {
    predss = predict_mlr_ep(model, dataset, model_name)$pred 
  } else if (model_type == "lr") {
    predss = predict_lr(model, dataset)
  } else if (model_type == "gam") {
    predss = predict_gam(model, dataset)
  } else if (model_type == "xgb") {
    if (xgb_wp) {
      predss = predict_probs_xgb(model, dataset, xgb_features, wp=TRUE)
    } else {
      predss = predict_ep_xgb(model, dataset, xgb_features, model_name, 
                              Regression=str_detect(model_name, "_R_"), BoundedRegression = str_detect(model_name, "_BR_"))$pred
    }
  } else if (model_type == "xgbTT") {
    predss = predict_probs_xgb_wp_TT(model, dataset, xgb_features)
  } 
  df_ =  dataset %>% 
    mutate(pred = predss) %>%
    mutate(game_sec_rem_bin = cut(
      game_seconds_remaining, 
      breaks = if (!endgame) seq(0,3600,by= 60*m) else seq(0,180,by= 15), 
      include.lowest=TRUE, dig.lab = 5)
    ) %>%
    group_by(game_sec_rem_bin) %>%
    filter(!is.na(game_sec_rem_bin))
  
  if (xgb_wp) {
    df_ = df_ %>% summarise(loss = LOGLOSS(label_win, pred)) %>% mutate(model = model_name)
  } else {
    df_ = df_ %>% summarise(loss = MAE(pts_next_score, pred)) %>% mutate(model = model_name)
  }
  return(df_)
}

plot_grouped_results <- function(grouped_preds) {
  grouped_results = grouped_preds %>%
    mutate(loss = round(loss, 2)) %>%
    arrange(game_sec_rem_bin, loss) %>%
    group_by(game_sec_rem_bin) %>%
    mutate(rank = rank(loss), i =1:n(), dd = rank-i) %>%
    mutate(rank1 = ifelse(rank - round(rank+0.01) == -0.5, rank - 0.5, rank)) %>%
    mutate(rank2 = rank1 + dd/15) %>%
    group_by(game_sec_rem_bin, loss) %>%
    mutate(loss = c(loss[1], rep(NA, n()-1)))
  print(grouped_results)
  # write_csv(grouped_results, paste0("test_results_grouped_wp.csv"))
  
  plot_grouped_results = grouped_results %>%
    mutate(lll = round(loss,2)) %>%
    ggplot(aes(y = game_sec_rem_bin, x = rank2)) +
    # geom_point(aes(y = game_sec_rem_bin, x = i, shape = model, color=model)) +
    geom_point(aes(color=model), size=3) +
    geom_text(aes(label = lll), color="black", size=3, nudge_x = 0.3) +
    scale_x_continuous(breaks=seq(0,100,by=1)) +
    ylab("game seconds remaining bin") +
    scale_shape_manual(values=seq(0,25)) +
    scale_colour_manual(values = c("firebrick", "#E69F00", "#56B4E9", "#009E73",
                                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    xlab("loss rank")
  plot_grouped_results
  # ggsave("plot_grouped_results.png", plot_grouped_results, width=20, height=10)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


# cross_entropy <- function(preds_p, y) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   ent_df = preds_p %>% filter(label == y) %>% mutate(log_q = -log(p)) %>% arrange(i)
#   (ent_df %>% summarise(e = mean(log_q)))$e 
# }
# 
# brier <- function(preds_p, y) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   brier_df = preds_p %>% filter(label == y) %>% mutate(b = (1-p)**2) %>% arrange(i)
#   (brier_df %>% summarise(e = mean(b)))$e 
# }
# 
# weighted_MAE <- function(x,y,w) {
#   sum(abs(x-y)*w) / sum(w)
# }
# 
# weighted_RMSE <- function(x,y,w) {
#   sqrt( sum(w*(x-y)**2) / sum(w) )
# }
# 
# weighted_cross_entropy <- function(preds_p, y, w) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   ent_df = preds_p %>% filter(label == y) %>% mutate(log_q = -log(p)) %>% arrange(i)
#   ent_df = ent_df %>% group_by(model) %>% mutate(w=w)
#   (ent_df %>% summarise(e = sum(w*log_q)))$e / sum(w)
# }
# 
# weighted_brier <- function(preds_p, y, w) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   brier_df = preds_p %>% filter(label == y) %>% mutate(b = (1-p)**2) %>% arrange(i)
#   brier_df = brier_df %>% group_by(model) %>% mutate(w=w)
#   (brier_df %>% summarise(e = sum(w*b)))$e / sum(w)
# }

##########################
### Sampling Functions ###
##########################

sample_one_play_per_epoch <- function(dataset, seed=NA) {
  if (!is.na(seed)) set.seed(seed)
  dataset %>% group_by(epoch) %>% slice_sample(n=1)
}


