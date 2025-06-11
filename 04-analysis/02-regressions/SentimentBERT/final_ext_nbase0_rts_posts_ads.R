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
data_type <- 'SentimentBERT'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('', 'log_', 'arc_')
file_code <- 'ext_nbase0_rts_posts_ads'
ini <- '../../../../data/04-analysis/joint/'

blocks_ke <- read_parquet(paste0('../../../../data/04-analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../../../data/04-analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks <- rbind(blocks_ke, blocks_sa)

final1 <- tibble()
for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_sent_bert_final_winsor(stage = stage, batches = 'b1b2',
                                            initial_path = '../../../../')
  df <- df |> filter(n_posts_base>0) |> filter(total_influencers == 1)
  df <- df |> left_join(blocks, by = c('follower_id', 'batch_id', 'pais'))
  
  df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                     strat_block2 = paste0(strat_block2, batch_id, pais))
  df_int <- tibble()
  for (type in list_types){
    
    aux <- paste0(type, aux_s_b_tot)
    
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
    
    df_coef <- tibble(coef = coef, sd = se, var = aux_s_b_tot) |> 
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
    mutate(Variable = case_when(var == 'pos_b_rt_covid' ~ paste0(addon, 'Positive COVID RTs + Posts'),
                                var == "neutral_b_rt_covid" ~ paste0(addon, 
                                                                     'Neutral COVID RTs + Posts'),
                                var == "neg_b_rt_covid" ~ paste0(addon, 
                                                                 'Negative COVID RTs + Posts'),
                                var == "n_posts_rt_covid" ~ paste0(addon, 
                                                                   'Number of COVID RTs + Posts'),
                                var == "pos_b_rt_vax" ~ paste0(addon, 'Positive Vaccine RTs + Posts'),
                                var == "neutral_b_rt_vax" ~ paste0(addon, 
                                                                   'Neutral Vaccine RTs + Posts'),
                                var == "neg_b_rt_vax" ~ paste0(addon, 
                                                               'Negative Vaccine RTs + Posts'),
                                var == "n_posts_rt_vax" ~ paste0(addon, 
                                                                 'Number of Vaccine RTs + Posts')),
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'))
  
  results_plot <- ggplot(data = final, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7, 1, 3, 10), name = 'Outcome') +
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
                           data_type, '/', type, file_code, '.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
