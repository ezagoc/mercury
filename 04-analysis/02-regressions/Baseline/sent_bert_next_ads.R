# 2.0 Define constants
country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('log_')
file_code <- 'next_sent_bert_ads'
ini <- '../../../../data/04-analysis/joint/'

df <- get_analysis_sent_bert_final2(stage = stage, batches = 'b1b2',
                                          initial_path = '../../../../')
df <- df |> filter(n_posts_base>0)

df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                   strat_block2 = paste0(strat_block2, batch_id, pais))
df_int <- tibble()

for (type in list_types){
  aux_aux <- aux_s_b_base
  aux <- paste0(type, aux_aux)
  
  # Get the Robust S.E.s
  se <- c()
  coef <- c()
  count2 <- 1
  for (x in aux) {
    fmla1 <- as.formula(paste0(x, "~ ads_treatment  | strat_block1"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = df))
    se <- c(se, get(nam1, envir = globalenv())$se['ads_treatment'])
    coef <- c(coef, get(nam1, envir = globalenv())$coefficients['ads_treatment'])
    count2 <- count2 + 1
  }
  
  df_coef <- tibble(coef = coef, sd = se, var = aux_aux) |> 
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
    mutate(Variable = case_when(var == aux_aux[1] ~ paste0(addon, 'Positive COVID Posts + Shares'),
                                var == aux_aux[2] ~ paste0(addon, 'Neutral COVID Posts + Shares'),
                                var == aux_aux[3] ~ paste0(addon, 'Negative COVID Posts + Shares'),
                                var == aux_aux[4] ~ paste0(addon, 'Number of COVID Posts + Shares'),
                                var == aux_aux[5] ~ paste0(addon, 'Positive COVID Vaccine Posts + Shares'), 
                                var == aux_aux[6] ~ paste0(addon, 'Neutral COVID Vaccine Posts + Shares'),
                                var == aux_aux[7] ~ paste0(addon, 'Negative COVID Vaccine Posts + Shares'),
                                var == aux_aux[8] ~ paste0(addon, 'Number of COVID Vaccine Posts + Shares'),
                                var == aux_aux[9] ~ paste0(addon, 'Positive COVID Shares'),
                                var == aux_aux[10] ~ paste0(addon, 'Neutral COVID Shares'), 
                                var == aux_aux[11] ~ paste0(addon, 'Negative COVID Shares'),
                                var == aux_aux[12] ~ paste0(addon, 'Number of COVID Shares'),
                                var == aux_aux[13] ~ paste0(addon, 'Positive COVID Vaccine Shares'),
                                var == aux_aux[14] ~ paste0(addon, 'Neutral COVID Vaccine Shares'),
                                var == aux_aux[15] ~ paste0(addon, 'Negative COVID Vaccine Shares'), 
                                var == aux_aux[16] ~ paste0(addon, 'Number of COVID Vaccine Shares'),
                                var == aux_aux[17] ~ paste0(addon, 'Positive COVID Posts'),
                                var == aux_aux[18] ~ paste0(addon, 'Neutral COVID Posts'),
                                var == aux_aux[19] ~ paste0(addon, 'Negative COVID Posts'),
                                var == aux_aux[20] ~ paste0(addon, 'Number of COVID Posts'), 
                                var == aux_aux[21] ~ paste0(addon, 'Positive COVID Vaccine Posts'),
                                var == aux_aux[22] ~ paste0(addon, 'Neutral COVID Vaccine Posts'),
                                var == aux_aux[23] ~ paste0(addon, 'Negative COVID Vaccine Posts'), 
                                var == aux_aux[24] ~ paste0(addon, 'Number of COVID Vaccine Posts')))
  
  
  final$Variable <- factor(final$Variable, 
                           levels = c(paste0(addon, 'Negative COVID Vaccine Posts'),
                                      paste0(addon, 'Negative COVID Posts'), 
                                      paste0(addon, 'Neutral COVID Vaccine Posts'), 
                                      paste0(addon, 'Neutral COVID Posts'), 
                                      paste0(addon, 'Positive COVID Vaccine Posts'), 
                                      paste0(addon, 'Positive COVID Posts'), 
                                      paste0(addon, 'Number of COVID Vaccine Posts'), 
                                      paste0(addon, 'Number of COVID Posts'),
                                      paste0(addon, 'Negative COVID Vaccine Shares'),
                                      paste0(addon, 'Negative COVID Shares'), 
                                      paste0(addon, 'Neutral COVID Vaccine Shares'), 
                                      paste0(addon, 'Neutral COVID Shares'), 
                                      paste0(addon, 'Positive COVID Vaccine Shares'), 
                                      paste0(addon, 'Positive COVID Shares'), 
                                      paste0(addon, 'Number of COVID Vaccine Shares'), 
                                      paste0(addon, 'Number of COVID Shares'),
                                      paste0(addon, 'Negative COVID Vaccine Posts + Shares'),
                                      paste0(addon, 'Negative COVID Posts + Shares'), 
                                      paste0(addon, 'Neutral COVID Vaccine Posts + Shares'), 
                                      paste0(addon, 'Neutral COVID Posts + Shares'), 
                                      paste0(addon, 'Positive COVID Vaccine Posts + Shares'), 
                                      paste0(addon, 'Positive COVID Posts + Shares'), 
                                      paste0(addon, 'Number of COVID Vaccine Posts + Shares'), 
                                      paste0(addon, 'Number of COVID Posts + Shares')))
  
  
  results_plot <- ggplot(data = final, aes(y = Variable, x = coef)) + 
    geom_point() +
    geom_linerange(aes(xmin = coef - 1.96 * sd, xmax = coef + 1.96 * sd), size = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    xlab("Treated Estimate with 95% Confidence Interval") + 
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
