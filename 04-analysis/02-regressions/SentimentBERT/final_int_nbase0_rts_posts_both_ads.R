# 2.0 Define constants
country <- 'joint'
data_type <- 'SentimentBERT'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('log_')
file_code <- 'next_nbase0_rts_posts_ads_both'
ini <- '../../../../data/04-analysis/joint/'

final1 <- tibble()
for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_sent_bert_final2(stage = stage, batches = 'b1b2',
                                            initial_path = '../../../../')
  df <- df |> filter(n_posts_base>0)
  
  df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                     strat_block2 = paste0(strat_block2, batch_id, pais))
  df_int <- tibble()
  for (type in list_types){
    
    aux <- paste0(type, aux_s_b_tot2)
    
    # Get the Robust S.E.s
    se <- c()
    coef <- c()
    count2 <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ ads_treatment + ", x, "_base  | strat_block1"))
      nam1 <- paste("lm_", count2, "_se", sep = "")
      assign(nam1, feols(fmla1, vcov = 'HC1', data = df))
      se <- c(se, get(nam1, envir = globalenv())$se['ads_treatment'])
      coef <- c(coef, get(nam1, envir = globalenv())$coefficients['ads_treatment'])
      count2 <- count2 + 1
    }
    
    df_coef <- tibble(coef = coef, sd = se, var = aux_s_b_tot2) |> 
      mutate(stage = stage, type = type)
    
    df_int <- rbind(df_int, df_coef)
  }
  final1 <- rbind(final1, df_int)
  print(stage)
}


for (type in list_types){
  
  if (type == 'log_'){
    addon <- 'log '
    final <- final1 |> filter(type == 'log_')
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
    final <- final1 |> filter(type == 'arc_')
  }else {
    addon <- ''
    final <- final1 |> filter(type == '')
  }
  
  final <- final |> 
    mutate(Variable = case_when(var == 'pos_b_covid' ~ paste0(addon, 'Positive COVID Posts + Shares'),
                                var == "neutral_b_covid" ~ paste0(addon, 
                                                           'Neutral COVID Posts + Shares'),
                                var == "neg_b_covid" ~ paste0(addon, 
                                                           'Negative COVID Posts + Shares'),
                                var == "n_posts_covid" ~ paste0(addon, 
                                                               'Number of COVID Posts + Shares')),
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'))
  
  final$Variable <- factor(final$Variable, 
                              levels = c(paste0(addon, 'Number of COVID Posts + Shares'),
                                         paste0(addon, 'Positive COVID Posts + Shares'), 
                                         paste0(addon, 'Neutral COVID Posts + Shares'), 
                                         paste0(addon, 'Negative COVID Posts + Shares')))
  
  writexl::write_xlsx(final, paste0(ini, 'EstimatesFinal/',type, data_type, '_ads.xlsx'))
  
  results_plot <- ggplot(data = final, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4), name = 'Outcome') +
    scale_color_manual(values = rep('black', 4), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (addon == 'log '){
    results_plot <- results_plot + ylim(-.025, .025)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code, '.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
