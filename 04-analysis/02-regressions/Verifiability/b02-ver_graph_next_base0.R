rm(list = ls())
library("purrr")
library(tidyverse)

## Initial paths:

ini <- '../../../data/04-analysis/joint/'
file_code <- 'ver_next_base0'

### functions:

proc_coefs <- function(stage, file){
  coef <- readxl::read_excel(paste0(ini, stage,'/original/', file, '.xlsx')) |>
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef') |> 
    mutate(stage = stage)
  return(coef)
}

proc_ses <- function(stage, file){
  
  perm <- readxl::read_excel(paste0(ini, stage, '/permutations/', file, 
                                    '.xlsx')) |> 
    summarise(across(everything(), ~sd(.x))) |> 
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd') |> 
    mutate(stage = stage)
  
  return(perm)
}

### Actual Coefficients:

for (type in list('', 'log_', 'arc_')){
  file_coefs <- paste0(type, file_code)
  coefs <- c('stage5_6', 'stage3_4', 'stage1_2') |> 
    map_dfr(~proc_coefs(.x, file_coefs))
  ses <- c('stage5_6', 'stage3_4', 'stage1_2') |> 
    map_dfr(~proc_ses(.x, file_coefs))
  
  final <- coefs |> left_join(ses, by = c('stage', 'var')) 
  
  if (type == 'log_'){
    addon <- 'log '
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
  }else {
    addon <- ''
  }
  
  final <- final |> 
    mutate(Variable = case_when(var == 'ver_rt' ~ paste0(addon, 'Verifiable RTs'),
                                var == 'ver_no_rt' ~ paste0(addon, 
                                                            'Verifiable Posts'),
                                var == 'true_rt' ~ paste0(addon, 'True RTs'),
                                var == 'true_no_rt' ~ paste0(addon, 'True Posts'),
                                var == 'fake_rt' ~ paste0(addon, 'Fake RTs'),
                                var == 'fake_no_rt' ~ paste0(addon, 'Fake Posts'),
                                var == 'n_posts_rt' ~ paste0(addon, 
                                                             'Number of RTs'),
                                var == 'n_posts_no_rt' ~ paste0(addon, 
                                                                'Number of Posts')),
                           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                                             stage == 'stage3_4' ~ 'Weeks 5-8',
                                             stage == 'stage5_6' ~ 'Weeks 9-12'),
                           RT = ifelse(grepl('RTs', Variable, fixed = T) == T, 1, 0))
  
  final_rt <- final |> filter(RT == 1)
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4), name = 'Outcome') +
    scale_color_manual(values = rep('black', 4), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(results_plot, filename = paste0('../../../results/01-regression_graphs/', 
                                         type, file_code, '_RT.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
  
  final_rt <- final |> filter(RT == 0)
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 2.5, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4), name = 'Outcome') +
    scale_color_manual(values = rep('black', 4), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(results_plot, filename = paste0('../../../results/01-regression_graphs/', 
                                         type, file_code, '_post.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
  
  
}





