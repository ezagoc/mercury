# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# 2.0 Define constants
country <- 'joint'
data_type <- 'Verifiability'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('', 'log_')
file_code <- 'next_nbase0_ads'
ini <- '../../../../data/04-analysis/joint/'


final1 <- tibble()
for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../')
  
  f <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |> 
    select(follower_id, pais, batch_id, eng_rt, eng_rt_base, 
           log_eng_rt, log_eng_rt_base, eng_no_rt, eng_no_rt_base, 
           log_eng_no_rt, log_eng_no_rt_base)
  
  df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
  
  df <- df |> filter(n_posts_base>0)
  
  df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                     strat_block2 = paste0(strat_block2, batch_id, pais))
  df_int <- tibble()
  for (type in list_types){
    
    aux <- paste0(type, aux_v)
    
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
    
    df_coef <- tibble(coef = coef, sd = se, var = aux_v) |> 
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
    mutate(Variable = case_when(var == 'verifiability_rt' ~ paste0(addon, 'Verifiable RTs'),
                                var == 'verifiability_no_rt' ~ paste0(addon, 
                                                            'Verifiable Posts'),
                                var == 'true_rt' ~ paste0(addon, 'True RTs'),
                                var == 'true_no_rt' ~ paste0(addon, 'True Posts'),
                                var == 'fake_rt' ~ paste0(addon, 'Fake RTs'),
                                var == 'fake_no_rt' ~ paste0(addon, 'Fake Posts'),
                                var == 'eng_rt' ~ paste0(addon, 
                                                             'Number of RTs (English)'),
                                var == 'eng_no_rt' ~ paste0(addon, 
                                                                'Number of Posts (English)'), 
                                var == 'non_ver_no_rt' ~ paste0(addon, 
                                                              'Non-Verifiable Posts'), 
                                var == 'non_ver_rt' ~ paste0(addon, 
                                                           'Non-Verifiable RTs')),
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'),
           RT = ifelse(grepl('RTs', Variable, fixed = T) == T, 1, 0))
  
  final_rt <- final |> filter(RT == 1)
  
  final_rt$Variable <- factor(final_rt$Variable, levels = c(paste0(addon, 
                                                                   'Number of RTs (English)'),
                                                            paste0(addon, 
                                                                   'Non-Verifiable RTs'), 
                                                            paste0(addon, 'Verifiable RTs'), 
                                                            paste0(addon, 'True RTs'), 
                                                            paste0(addon, 'Fake RTs')))
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7), name = 'Outcome') +
    scale_color_manual(values = rep('black', 5), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (addon == 'log '){
    results_plot <- results_plot + ylim(-.1, .1)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code, '_RT.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
  
  results_plot
  
  final_rt <- final |> filter(RT == 0)
  
  final_rt$Variable <- factor(final_rt$Variable, levels = c(paste0(addon, 'Number of Posts (English)'),
                                                            paste0(addon, 
                                                                   'Non-Verifiable Posts'), 
                                                            paste0(addon, 'Verifiable Posts'), 
                                                            paste0(addon, 'True Posts'), 
                                                            paste0(addon, 'Fake Posts')))
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 2.5, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7), name = 'Outcome') +
    scale_color_manual(values = rep('black', 5), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (addon == 'log '){
    results_plot <- results_plot + ylim(-.1, .1)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/', 
                           data_type, '/', type, file_code, '_post.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
