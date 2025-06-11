# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(tidyverse)
library(purrr)
library(fastDummies)
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



type <- 'log_'

for (stage in list_stages){
  file_code <- 'intensive_interactions'
  df2 <- readxl::read_excel(paste0("../../../../data/04-analysis/",country,
                                   "/",stage,"/original/", data_type, '/', type, 
                                   file_code, ".xlsx")) |> 
    mutate(influencers = 45)
  
  list_numbers <- c(8:15)
  
  import <- function(num){
    df <- readxl::read_excel(paste0("../../../../data/04-analysis/",country,
                                    "/",stage,"/original/", data_type, '/', type, 
                                    file_code, num, ".xlsx")) |> 
      mutate(influencers = num)
    
    return(df)
  }
  
  
  df <- list_numbers |> map_dfr(~import(.x))
  
  file_code <- 'ext_nbase0_rts_posts'
  df1 <- readxl::read_excel(paste0("../../../../data/04-analysis/",country,
                                   "/",stage,"/original/", data_type, '/', type, 
                                   file_code, ".xlsx")) |> 
    mutate(influencers = 1)
  
  
  df_final <- rbind(df, df2, df1)
  
  df_final_long <- df_final |> pivot_longer(!influencers, names_to = 'var', values_to = 'estimates')
  
  addon <- 'log '
  df_final_long <- df_final_long |> 
    mutate(Variable = case_when(var == 'ver' ~ paste0(addon, 'Verifiable Posts + Shares'),
                                var == 'non_ver' ~ paste0(addon, 'Non Verifiable Posts + Shares'),
                                var == 'true' ~ paste0(addon, 'True Posts + Shares'),
                                var == 'fake' ~ paste0(addon, 'Fake Posts + Shares'),
                                var == 'n_posts' ~ paste0(addon, 'Number of Posts + Shares (English)')))
  
  df_final_long$Variable <- factor(df_final_long$Variable, 
                                   levels = c(paste0(addon, 'Number of Posts + Shares (English)'),
                                                      paste0(addon, 
                                                             'Non Verifiable Posts + Shares'), 
                                                      paste0(addon, 'Verifiable Posts + Shares'), 
                                                      paste0(addon, 'True Posts + Shares'), 
                                                      paste0(addon, 'Fake Posts + Shares')))
  
  df_final_long <- df_final_long |> filter(influencers %in% c(1, 8:14))
  results_plot <- ggplot(data = df_final_long, aes(x = factor(influencers), y = estimates)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.9)) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7), name = 'Outcome') +
    scale_color_manual(values = rep('black', 5), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate") + 
    xlab("Number of Influencers in Sample") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylim(-.07, .01)
  results_plot
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/coefficients_', stage, 
                           '.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}



