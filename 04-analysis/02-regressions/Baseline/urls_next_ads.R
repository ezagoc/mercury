# 2.0 Define constants
country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('log_')
file_code <- 'next_urls_ads'
ini <- '../../../../data/04-analysis/joint/'

df <- get_analysis_urls(stage = stage, batches = 'b1b2',
                        initial_path = '../../../../')
df <- df |> filter(n_posts_base>0)

df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                   strat_block2 = paste0(strat_block2, batch_id, pais))
df_int <- tibble()

for (type in list_types){
  
  aux <- paste0(type, aux_urls_base)
  
  # Get the Robust S.E.s
  se <- c()
  coef <- c()
  count2 <- 1
  for (x in aux) {
    fmla1 <- as.formula(paste0(x, "~ ads_treatment | strat_block1"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = df))
    se <- c(se, get(nam1, envir = globalenv())$se['ads_treatment'])
    coef <- c(coef, get(nam1, envir = globalenv())$coefficients['ads_treatment'])
    count2 <- count2 + 1
  }
  
  df_coef <- tibble(coef = coef, sd = se, var = aux_urls_base) |> 
    mutate(stage = stage, type = type)
  
  df_int <- rbind(df_int, df_coef)
  
  if (type == 'log_'){
    addon <- 'log '
    final <- df_int |> filter(type == 'log_')
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
  }else {
    addon <- ''
    final <- df_int |> filter(type == '')
  }
  
  final <- final |> 
    mutate(Variable = case_when(var == 'total_urls_base' ~ paste0(addon, 'Total URLs'),
                                var == 'total_info_base' ~ paste0(addon, 'Total Info.'),
                                var == 'total_news_base' ~ paste0(addon, 'Total News'),
                                var == 'fact_check_base' ~ paste0(addon, 'Fact-Checks'),
                                var == 'rel_news_base' ~ paste0(addon, 'Reliable'),
                                var == 'non_rel_news_base' ~ paste0(addon, 'Non-Reliable'),
                                var == 'other_base' ~ paste0(addon, 'Other Not Info.')
                                
    ))
  
  
  final$Variable <- factor(final$Variable, levels = c(paste0(addon, 'Other Not Info.'),
                                                      paste0(addon, 'Non-Reliable'), 
                                                      paste0(addon, 'Reliable'),
                                                      paste0(addon, 'Total News'),
                                                      paste0(addon, 'Fact-Checks'),
                                                      paste0(addon, 'Total Info.'),
                                                      paste0(addon, 'Total URLs')))
  
  results_plot <- ggplot(data = final, aes(y = Variable, x = coef)) + 
    geom_point() +
    geom_linerange(aes(xmin = coef - 1.96 * sd, xmax = coef + 1.96 * sd), size = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    xlab("Total Treated Estimate with 95% Confidence Interval") + 
    ylab("Variable") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank())
  
  if (addon == 'log '){
     results_plot <- results_plot + xlim(-.025, .025)
   }else{
     results_plot <- results_plot
   }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
