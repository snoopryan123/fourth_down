
source("header.R")

##########
### UI ###
########## 

{
  ui <- fluidPage(
    # titlePanel("Fourth Down Visualizer"),
    # titlePanel("Fourth Down Visualizer", windowTitle="Fourth Down Visualizer"),
    titlePanel("", windowTitle="Fourth Down Visualizer"),

    sidebarLayout(
  
      sidebarPanel(
        actionButton("generate_plots", "Generate Plots", class = "btn-warning"),

        # h4("Post-TD Settings:"),
        # selectInput("post_TD", "post-TD decision (extra point vs. conversion)", choices = c("TRUE", "FALSE"), selected="FALSE"),
        # selectInput("post_TD_custom_conv_prob", "use post-TD custom conv prob (e.g. 0.48)", choices = c("TRUE", "FALSE"), selected="TRUE"),
        # selectInput("post_TD_custom_xp_prob", "use post-TD custom extra point prob (e.g. 0.94)", choices = c("TRUE", "FALSE"), selected="TRUE"),
        # sliderInput("custom_post_TD_conv_prob", "custom conversion probability (only used if above is set to TRUE)", min = 0, max = 1, value = 0.48, step=0.01, pre = ""),
        # sliderInput("custom_xp_prob", "custom extra point probability (only used if above is set to TRUE)", min = 0, max = 1, value = 0.94, step=0.01, pre = ""),

        h4("Game State:"),
        sliderInput("score_differential", "score differential", min = -30, max = 30, value = 0, step=1, pre = ""),
        sliderInput("total_score", "total score", min = 0, max = 120, value = 29, step=1, pre = ""),
        numericInput("game_seconds_remaining", "game seconds remaining", min = 0, max = 3600, value = 720, step=1),
        sliderInput("posteam_spread", "pre-game point spread (of the offensive team)", min = -17, max = 17, value = 0, step=0.5, pre = ""),
        sliderInput("total_line", "pre-game total points over/under line", min = 0, max = 125, value = 44, step=0.5, pre = ""),
        sliderInput("yardline", "yardline", min = 1, max = 99, value = 50, step=1, pre = ""),
        sliderInput("ydstogo", "yards to go", min = 1, max = 15, value = 3, step=1, pre = ""),
        selectInput("receive_2h_ko", "receive second half kickoff", choices = c("yes", "no"), selected="yes"),
        selectInput("posteam_timeouts_remaining", "offensive team's number of timeouts remaining", choices = c("3", "2", "1", "0"), selected="3"),
        selectInput("defteam_timeouts_remaining", "defensive team's number of timeouts remaining", choices = c("3", "2", "1", "0"), selected="3"),
        selectInput("era_A", "era", choices = c("2007-2013", "2014-2017", "2018-present"), selected="2018-present"),
        selectInput("home", "home", choices = c("home", "away"), selected="home"),
        
        h4("For the field goal model:"),
        sliderInput("kq_input", "kicker quality", min=std_var_min, max=std_var_max, value = 0, step=std_var_step, pre = ""),

        h4("For the punt model:"),
        sliderInput("pq_input", "punter quality", min=std_var_min, max=std_var_max, value = 0, step=std_var_step, pre = ""),
  
        # h4("For the conversion model:"),
        # sliderInput("oqot_minus_dq_input", "offensive team's quarterback quality", min=std_var_min, max=std_var_max, value = 0, step=std_var_step, pre = "")
        
        h4("Uncertainty Settings:"),
        selectInput("num_B", "number of bootstrap samples", choices = c("0", "25", "100"), selected="100"),
        # h5("• 0 bootstrap samples means no uncertainty plots."),
        # h5("• fewer bootstrap samples means faster runtime."),
        
        h4("Settings:"),
        # selectInput("ignore_EP", "ignore expected points models", choices = c("no", "yes"), selected="yes"),
        selectInput("ignore_EP", "ignore expected points models", choices = c("yes"), selected="yes"),
        selectInput("dec_conf_str", "decision confidence label", choices = c("TRUE", "FALSE"), selected="FALSE"),
        # h5("• ignoring EP means faster runtime."),
        
        h4("4th Down Conversion Probability Settings:"),
        selectInput("use_custom_conv_prob", "use custom conversion probability", choices = c("TRUE", "FALSE"), selected="FALSE"),
        sliderInput("custom_conv_prob", "custom conversion probability (only used if above is set to TRUE)", min = 0, max = 1, value = 0.50, step=0.01, pre = ""),
        
        width=3 
      ),
      mainPanel(
        h1("Fourth Down Visualizer",align="center"),
        br(), 
        
        withSpinner(uiOutput("loading_spinner"), type=5, image="football.gif"),
        
        # h3(uiOutput("decision"), align="left"),
        # h3(uiOutput("decision_wpa"), align="left"),
        # h3(uiOutput("decision_wp_CI"), align="left"),
        # h3(uiOutput("decision_confidence"), align="left"),
        # br(), br(),
        
        h2("Win Probability:",align="center"),
        fluidRow(
          gt_output("plot_gt_4th_down_summary"), 
        ), br(), br(),
        fluidRow(
          splitLayout(
            cellWidths = c("40%", "10%", "40%", "10%"),
            plotOutput("WP_plot"),
            plotOutput("WP_plot_legend"), 
            plotOutput("WP_plot_SE"),
            plotOutput("WP_plot_SE_legend"), 
          )
        ), br(), br(),
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            plotOutput("WP_plot_VLines"), 
            plotOutput("WP_plot_VLines_SE"),
          )
        ), br(), br(),
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            plotOutput("WP_plot_lookahead"), 
            plotOutput("WP_plot_lookahead_SE"),
          )
        ), br(), br(),
        
        h3("Field Goal, Punt, and Conversion Models:",align="center"),
        fluidRow(
          splitLayout(
            cellWidths = c("48%", "2%", "48%"),
            plotOutput("fg_prob_plot"),
            plotOutput(""),
            plotOutput("punt_eny_plot"),
            # plotOutput("conv_prov_plot"), 
          )
        ), br(), br(),
        fluidRow(
          splitLayout(
            cellWidths = c("48%", "52%"),
            plotOutput("conv_prob_plot"),
            plotOutput(""),
          )
        ), br(), br(),
        
        h2("Expected Points:",align="center"),
        fluidRow(
          gt_output("plot_gt_4th_down_summary_ep"), 
        ), br(), br(),
        fluidRow(
          splitLayout(
            cellWidths = c("40%", "10%", "40%", "10%"),
            plotOutput("EP_plot"),
            plotOutput("EP_plot_legend"),
            plotOutput("EP_plot_SE"), 
            plotOutput("EP_plot_SE_legend"),
          )
        ), br(), br(),
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            plotOutput("EP_plot_VLines"), 
            plotOutput("EP_plot_VLines_SE"),
          )
        ), br(), br(),
        
      )
    ),
    
    h4("Developed by Ryan Brill and Adi Wyner.", align="center")
  )
}
 
##############
### SERVER ###
##############

server <- function(input, output, session) {
  
  wipe_all <- function() {
    ### silence spinner
    output$loading_spinner = renderText({""})
    
    # ### no decision at first
    # output$decision = renderText({""})
    # output$decision_wpa = renderText({""})
    # output$decision_wp_CI = renderText({""})
    # output$decision_confidence = renderText({""})
    
    ### wipe all plots
    output$plot_gt_4th_down_summary <- render_gt({})
    output$plot_gt_4th_down_summary_ep <- render_gt({})
    output$WP_plot <- renderPlot({})
    output$EP_plot <- renderPlot({})
    output$WP_plot_legend <- renderPlot({})
    output$EP_plot_legend <- renderPlot({})
    output$WP_plot_SE <- renderPlot({})
    output$EP_plot_SE <- renderPlot({})
    output$WP_plot_SE_legend <- renderPlot({})
    output$EP_plot_SE_legend <- renderPlot({})
    output$EP_plot_VLines <- renderPlot({})
    output$WP_plot_VLines <- renderPlot({})
    output$WP_plot_lookahead <- renderPlot({})
    output$EP_plot_VLines_SE <- renderPlot({})
    output$WP_plot_VLines_SE <- renderPlot({})
    output$WP_plot_lookahead_SE <- renderPlot({})
    output$fg_prob_plot <- renderPlot({})
    output$punt_eny_plot <- renderPlot({})
    output$conv_prob_plot <- renderPlot({})
  }
  
  wipe_all()
  
  #####################
  ### INPUT SLIDERS ###
  #####################
  
  # canSwitchKqQ = TRUE
  # # if choose a qualitative kicker quality input, update the quantitative kicker quality slider
  # observeEvent(input$kq_input_Q, {
  #   if (input$kq_input_Q != "quantitative") {
  #     updateSliderInput(session, "kq_input", value = map_qualitativeButton_to_stdSlider(input$kq_input_Q))
  #   }
  #   canSwitchKqQ <<- FALSE
  # })
  # # if choose a quantitative kicker quality input, update the qualitative kicker quality button
  # observeEvent(input$kq_input, {
  #   if (canSwitchKqQ) {
  #     updateSelectInput(session, "kq_input_Q", selected = "quantitative")
  #   }
  #   canSwitchKqQ <<- TRUE;
  # })
 
  ################################
  ### dataframe for our models ###
  ################################

  generate_gamestate_df <- function() {
    df <- tibble(
      yardline_100 = isolate(input$yardline),
      ydstogo = isolate(input$ydstogo),
      score_differential = isolate(input$score_differential),
      total_score = isolate(input$total_score),
      posteam_spread = isolate(input$posteam_spread),
      total_line = isolate(input$total_line),
      game_seconds_remaining = isolate(input$game_seconds_remaining),
      home = isolate(case_when(input$home == "home" ~ 1, input$home == "away" ~ 0)),
      receive_2h_ko = isolate(case_when(input$receive_2h_ko == "yes" ~ 1, input$receive_2h_ko == "no" ~ 0)),
      posteam_timeouts_remaining = isolate(as.numeric(input$posteam_timeouts_remaining)),
      defteam_timeouts_remaining = isolate(as.numeric(input$defteam_timeouts_remaining)),
      # era_A = isolate(input$era_A),
      era_A = case_when(
        input$era_A == "2007-2013" ~ 2,
        input$era_A == "2014-2017" ~ 3,
        input$era_A == "2018-present" ~ 4,
        # TRUE ~ 4
      ),
      kq = isolate(input$kq_input),
      pq = isolate(input$pq_input),
      market_OQOT_minus_DQDT = (total_line + -posteam_spread) / 2,
      market_OQDT_minus_DQOT = (total_line - -posteam_spread) / 2,
      mu_momd = 21.7405, #FIXME
      sig_momd = 4.030279, #FIXME
      mu_mdmo = 21.67276, #FIXME
      sig_mdmo = 4.046451, #FIXME
      market_OQOT_minus_DQDT_std = (market_OQOT_minus_DQDT - mu_momd) / sig_momd,
      market_OQDT_minus_DQOT_std = (market_OQDT_minus_DQOT - mu_mdmo) / sig_mdmo,
      
      half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining),
      half_sec_rem_std = 1 - half_seconds_remaining/1800,
      half = ifelse(game_seconds_remaining > 1800, 1, 2),
      qtr = case_when(
        game_seconds_remaining > 2700 ~ 1,
        game_seconds_remaining > 1800 ~ 2,
        game_seconds_remaining > 900 ~ 3,
        game_seconds_remaining > 0 ~ 4,
        TRUE ~ NA_real_
      ),
      utm = as.numeric(half_seconds_remaining <= 120),
      elapsed_share = (3600 - game_seconds_remaining) / 3600,
      spread_time = posteam_spread * exp(-4 * elapsed_share),
      Diff_Time_Ratio = score_differential / (exp(-4 * elapsed_share)),
      scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
    )
    # browser()
    # print(df)
    # print(data.frame(df))
    return(df)
  }
  
  observeEvent(input$generate_plots, {
    
    wipe_all()
    
    ### begin spinner
    output$loading_spinner = renderUI({
      withSpinner(uiOutput("loading_spinner"), type=5, color=loading_color)
    })

    ignore_EP = isolate(input$ignore_EP == "yes") ### ignore EP models
    uu = isolate(as.numeric(input$num_B)) != 0 ### ignore uncertainty
    # post_TD = as.logical(isolate(input$post_TD)) ### analyze a post-TD decision (extra point vs. 2 pt conversion)
    post_TD = FALSE
    custom_conv_prob_ = if (as.logical(isolate(input$use_custom_conv_prob))) input$custom_conv_prob else NULL
    
    if (post_TD) {
      ### post-TD decision: extra point or 2 pt conversion?
      
      ### custom probabilities
      custom_conv_prob_ = if (as.logical(isolate(input$post_TD_custom_conv_prob))) input$custom_post_TD_conv_prob else NULL
      custom_xp_prob_ = if (as.logical(isolate(input$post_TD_custom_xp_prob))) input$custom_xp_prob else NULL
      
      play_df=generate_gamestate_df()[1,]
      ddf_post_TD = suppressWarnings(
        get_full_decision_making(play_df=play_df, wp=TRUE, SE=uu, 
                                 custom_conv_prob=custom_conv_prob_, custom_xp_prob = custom_xp_prob_,
                                 coachBaseline=TRUE, post_TD=TRUE)
      )
      decision_post_TD_df = get_post_TD_decision(ddf_post_TD, include_uncertainty=uu) 
      dec_conf_str = isolate(as.logical(input$dec_conf_str)) ### decision confidence string
      plot_gt_post_TD_summary_ = plot_gt_post_TD_summary(play_df, ddf_post_TD, 
                                                         decision_df=decision_post_TD_df, 
                                                         SE=uu, wp=TRUE, dec_conf_str=dec_conf_str)
      output$plot_gt_4th_down_summary <- render_gt(plot_gt_post_TD_summary_)
      # browser()
    } else {
      ### FOURTH DOWN
      
      ### generate WP plots
      # browser(); ddf_wp = get_full_decision_making(play_df=generate_gamestate_df(), wp=TRUE, SE=FALSE);
      # browser()
      # get_full_decision_making(play_df=generate_gamestate_df(), wp=TRUE, SE=FALSE, custom_conv_prob=custom_conv_prob_)
      ddf_wp = suppressWarnings(
        get_full_decision_making(play_df=generate_gamestate_df(), wp=TRUE, SE=FALSE, custom_conv_prob=custom_conv_prob_, coachBaseline=TRUE, post_TD=FALSE)
      )
      
      # browser()
      list_heatmap_wp = plot_4thDownHeatmap(ddf_wp, wp=TRUE, og_method=FALSE, title=TRUE, SE=FALSE, 
                                            legend_pos="separate", ydl=isolate(input$yardline), ytg=isolate(input$ydstogo))
      heatmap_wp = list_heatmap_wp$plot
      legend_heatmap_wp = list_heatmap_wp$legend
      Vplot_wp = plot_Vs(ddf_wp, ytg = isolate(input$ydstogo), SE=FALSE, wp=TRUE)
      plotLookahead_wp = plot_4th_down_lookahead(ddf_wp, isolate(input$yardline), SE=FALSE, wp=TRUE)
      
      ### generate EP plots
      if (!ignore_EP) {
        ddf_ep = suppressWarnings(
          get_full_decision_making(play_df=generate_gamestate_df(), wp=FALSE, SE=FALSE, custom_conv_prob=custom_conv_prob_)
        )
        list_heatmap_ep = plot_4thDownHeatmap(ddf_ep, wp=FALSE, og_method=FALSE, title=TRUE, SE=FALSE, 
                                              legend_pos="separate", ydl=isolate(input$yardline), ytg=isolate(input$ydstogo))
        heatmap_ep = list_heatmap_ep$plot
        legend_heatmap_ep = list_heatmap_ep$legend
        Vplot_ep = plot_Vs(ddf_ep, ytg = isolate(input$ydstogo), SE=FALSE, wp=FALSE)
      }
      
      ### set WP plots
      output$WP_plot <- renderPlot({ heatmap_wp + theme(aspect.ratio = 0.8) })
      output$WP_plot_legend <- renderPlot({ legend_heatmap_wp })
      output$WP_plot_VLines <- renderPlot({ Vplot_wp + theme(aspect.ratio = 0.8) })
      output$WP_plot_lookahead <- renderPlot({ plotLookahead_wp + theme(aspect.ratio = 0.65) })
      if (!ignore_EP) {
        ### set EP plots
        output$EP_plot <- renderPlot({ heatmap_ep + theme(aspect.ratio = 0.8) })
        output$EP_plot_legend <- renderPlot({ legend_heatmap_ep })
        output$EP_plot_VLines <- renderPlot({ Vplot_ep + theme(aspect.ratio = 0.8)  })
      } 
      # else {  
      #   ### if we ignore EP plots, put the WP line plot where the EP plot normally goes
      #   output$EP_plot <- renderPlot({ Vplot_wp + theme(aspect.ratio = 0.8) })
      # }
      
      if (uu) {
        ddf_wp_se = suppressWarnings(
          get_full_decision_making(play_df=generate_gamestate_df(), wp=TRUE, SE=TRUE, b_max=isolate(as.numeric(input$num_B)), custom_conv_prob=custom_conv_prob_, coachBaseline = TRUE)
        )
        ddf_wp_se = ddf_wp_se$Vs ###
        
        if (!ignore_EP) {
          ddf_ep_se = suppressWarnings(
            get_full_decision_making(play_df=generate_gamestate_df(), wp=FALSE, SE=TRUE, b_max=isolate(as.numeric(input$num_B)), custom_conv_prob=custom_conv_prob_, coachBaseline = TRUE)
          )
        }
      }
      
      ### update decision
      decision_wp = get_decision(isolate(input$yardline), isolate(input$ydstogo), 
                                 if (uu) ddf_wp_se else ddf_wp, include_uncertainty=uu) 
      
      #########
      #########
      if (uu) {
        # dec_conf_str = TRUE #TRUE #FALSE
        dec_conf_str = isolate(as.logical(input$dec_conf_str)) ### decision confidence string
        plot_gt_4th_down_summary_wp_se = plot_gt_4th_down_summary(generate_gamestate_df(), ddf_wp_se, decision_wp, SE=TRUE, wp=TRUE, dec_conf_str=dec_conf_str) 
        # browser()
        list_heatmap_wp_se = plot_4thDownHeatmap(ddf_wp_se, wp=TRUE, title=TRUE, SE=TRUE, 
                                                 legend_pos="separate", ydl=isolate(input$yardline), ytg=isolate(input$ydstogo), dec_conf_str=dec_conf_str)
        heatmap_wp_se = list_heatmap_wp_se$plot
        legend_heatmap_wp_se = ggdraw(list_heatmap_wp_se$legend)
        Vplot_wp_se = plot_Vs(ddf_wp_se, ytg = isolate(input$ydstogo), SE=TRUE, wp=TRUE)
        plotLookahead_wp = plot_4th_down_lookahead(ddf_wp_se, isolate(input$yardline), SE=TRUE, wp=TRUE)
        plotLookahead_wp_se = plot_4th_down_lookahead(ddf_wp_se, isolate(input$yardline), boot_plot = TRUE, SE=TRUE, wp=TRUE, dec_conf_str=dec_conf_str)
        
        ### generate EP uncertainty plots
        if (!ignore_EP) {
          decision_ep = get_decision(isolate(input$yardline), isolate(input$ydstogo), 
                                     if (uu) ddf_ep_se else ddf_ep, include_uncertainty=uu) 
          plot_gt_4th_down_summary_ep_se = plot_gt_4th_down_summary(generate_gamestate_df(), ddf_ep_se, decision_ep, SE=TRUE, wp=FALSE, dec_conf_str=dec_conf_str) 
          list_heatmap_ep_se = plot_4thDownHeatmap(ddf_ep_se, wp=FALSE, title=TRUE, SE=TRUE, 
                                                   legend_pos="separate", ydl=isolate(input$yardline), ytg=isolate(input$ydstogo), dec_conf_str=dec_conf_str)
          heatmap_ep_se = list_heatmap_ep_se$plot
          legend_heatmap_ep_se = ggdraw(list_heatmap_ep_se$legend)
          Vplot_ep_se = plot_Vs(ddf_ep_se, ytg = isolate(input$ydstogo), SE=TRUE, wp=FALSE)
        }
        
        ### set WP uncertainty plots
        output$WP_plot_SE <- renderPlot({ heatmap_wp_se + theme(aspect.ratio = 0.8) })
        output$WP_plot_SE_legend <- renderPlot({ legend_heatmap_wp_se })
        output$WP_plot_VLines_SE <- renderPlot({ Vplot_wp_se + theme(aspect.ratio = 0.8) })
        output$WP_plot_lookahead_SE <- renderPlot({ plotLookahead_wp_se + theme(aspect.ratio = 0.65) })
        output$plot_gt_4th_down_summary <- render_gt(plot_gt_4th_down_summary_wp_se)
        if (!ignore_EP) {
          ### set EP uncertainty plots
          output$EP_plot_SE <- renderPlot({ heatmap_ep_se + theme(aspect.ratio = 0.8) })
          output$EP_plot_SE_legend <- renderPlot({ legend_heatmap_ep_se })
          output$plot_gt_4th_down_summary_ep <- render_gt(plot_gt_4th_down_summary_ep_se)
          output$EP_plot_VLines_SE <- renderPlot({ Vplot_ep_se + theme(aspect.ratio = 0.8) })
        } 
        # else {  
        #   ### if we ignore EP uncertainty plots, put the WP uncertainty line plot where the EP uncertainty plot normally goes
        #   output$EP_plot_SE <- renderPlot({ Vplot_wp_se + theme(aspect.ratio = 0.8) })
        # }
      } else {
        plot_gt_4th_down_summary_wp = plot_gt_4th_down_summary(generate_gamestate_df(), ddf_wp, SE=FALSE, wp=TRUE) 
        output$plot_gt_4th_down_summary <- render_gt(plot_gt_4th_down_summary_wp)
        if (!ignore_EP) {
          plot_gt_4th_down_summary_ep = plot_gt_4th_down_summary(generate_gamestate_df(), ddf_ep, SE=FALSE, wp=FALSE) 
          output$plot_gt_4th_down_summary_ep <- render_gt(plot_gt_4th_down_summary_ep)
        }
      }
      
      ### field goal, punt, and conversion plots
      # browser()
      fg_plot = plot_fg_prob_by_kq(fg_model_obs, kq=isolate(input$kq_input))
      output$fg_prob_plot <- renderPlot({ fg_plot })
      punt_plot = plot_punt_eny_by_pq(punt_model_obs, pq=isolate(input$pq_input))
      output$punt_eny_plot <- renderPlot({ punt_plot })
      if (!as.logical(isolate(input$use_custom_conv_prob))) {
        momds = generate_gamestate_df()$market_OQOT_minus_DQDT_std
        conv_plot = plot_conv_prob_1(go_model_obs, momds)
        # conv_plot = plot_conv_prob_momd(go_model_obs)
        # conv_plot = plot_conv_2(go_model_obs, qbq_ot_0_sum=isolate(input$qbq_ot_input), 
        #                        oq_rot_0_total_sum=isolate(input$oq_rot_input), 
        #                        dq_dt_0_againstPass_sum=isolate(input$dq_dt_0_againstPass_input), 
        #                        dq_dt_0_againstRun_sum=isolate(input$dq_dt_0_againstRun_input)) 
        output$conv_prob_plot <- renderPlot({ conv_plot })
      }
    }
      
    
    
    ### end spinner
    output$loading_spinner = renderText({""})
  })#,ignoreNULL = F)
}

###############
### RUN APP ###
###############

shinyApp(ui = ui, server = server)
############ LAST LINE ###########
# 
# 
# > options(shiny.autoreload = TRUE)
# > runApp(host="192.168.86.31",port=2000)


