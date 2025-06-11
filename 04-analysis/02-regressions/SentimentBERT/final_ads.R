# 2.0 Define constants
country <- 'joint'
data_type <- 'SentimentBERT'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('log_')
ini <- '../../../../data/04-analysis/joint/'
file_code <- 'ads'

final1 <- tibble()
for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_sent_bert_final_winsor(stage = stage, batches = 'b1b2',
                                            initial_path = '../../../../')
  df <- df |> filter(n_posts_base>0)
  
  df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                     strat_block2 = paste0(strat_block2, batch_id, pais))
  df_int <- tibble()
  for (type in list_types){
    
    aux <- paste0(type, aux_s_b)
    
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
    
    df_coef <- tibble(coef = coef, sd = se, var = aux_s_b) |> 
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
    mutate(Variable = case_when(var == 'pos_b_rt_covid' ~ paste0(addon, 'Positive COVID Shares'),
                                var == "neutral_b_rt_covid" ~ paste0(addon, 
                                                                     'Neutral COVID Shares'),
                                var == "neg_b_rt_covid" ~ paste0(addon, 
                                                                 'Negative COVID Shares'),
                                var == "n_posts_rt_covid" ~ paste0(addon, 
                                                                   'Number of COVID Shares'),
                                var == "pos_b_rt_vax" ~ paste0(addon, 'Positive COVID Vaccine Shares'),
                                var == "neutral_b_rt_vax" ~ paste0(addon, 
                                                                   'Neutral COVID Vaccine Shares'),
                                var == "neg_b_rt_vax" ~ paste0(addon, 
                                                               'Negative COVID Vaccine Shares'),
                                var == "n_posts_rt_vax" ~ paste0(addon, 
                                                                 'Number of COVID Vaccine Shares'), 
                                var == 'pos_b_no_rt_covid' ~ paste0(addon, 'Positive COVID Posts'),
                                var == "neutral_b_no_rt_covid" ~ paste0(addon, 
                                                                     'Neutral COVID Posts'),
                                var == "neg_b_no_rt_covid" ~ paste0(addon, 
                                                                 'Negative COVID Posts'),
                                var == "n_posts_no_rt_covid" ~ paste0(addon, 
                                                                   'Number of COVID Posts'),
                                var == "pos_b_no_rt_vax" ~ paste0(addon, 'Positive COVID Vaccine Posts'),
                                var == "neutral_b_no_rt_vax" ~ paste0(addon, 
                                                                   'Neutral COVID Vaccine Posts'),
                                var == "neg_b_no_rt_vax" ~ paste0(addon, 
                                                               'Negative COVID Vaccine Posts'),
                                var == "n_posts_no_rt_vax" ~ paste0(addon, 
                                                                 'Number of COVID Vaccine Posts')),
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'),
           RT = ifelse(grepl('Shares', Variable, fixed = T) == T, 1, 0))
  
  final_rt <- final |> filter(RT == 1)
  
  final_rt$Variable <- factor(final_rt$Variable, 
                              levels = c(paste0(addon, 'Number of COVID Shares'),
                                         paste0(addon, 'Number of COVID Vaccine Shares'), 
                                         paste0(addon, 'Positive COVID Shares'), 
                                         paste0(addon, 'Positive COVID Vaccine Shares'), 
                                         paste0(addon, 'Neutral COVID Shares'), 
                                         paste0(addon, 'Neutral COVID Vaccine Shares'), 
                                         paste0(addon, 'Negative COVID Shares'), 
                                         paste0(addon, 'Negative COVID Vaccine Shares')))
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7, 1, 3, 11), name = 'Outcome') +
    scale_color_manual(values = rep('black', 8), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code, '_rt.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
  
  final_rt <- final |> filter(RT == 0)
  
  final_rt$Variable <- factor(final_rt$Variable, 
                              levels = c(paste0(addon, 'Number of COVID Posts'),
                                         paste0(addon, 'Number of COVID Vaccine Posts'), 
                                         paste0(addon, 'Positive COVID Posts'), 
                                         paste0(addon, 'Positive COVID Vaccine Posts'), 
                                         paste0(addon, 'Neutral COVID Posts'), 
                                         paste0(addon, 'Neutral COVID Vaccine Posts'), 
                                         paste0(addon, 'Negative COVID Posts'), 
                                         paste0(addon, 'Negative COVID Vaccine Posts')))
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 2.5, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7, 1, 3, 11), name = 'Outcome') +
    scale_color_manual(values = rep('black', 8), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/', 
                           data_type, '/', type, file_code, '_post.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
