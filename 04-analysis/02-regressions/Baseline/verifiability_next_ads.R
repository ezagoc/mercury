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
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('', 'log_')
file_code <- 'next_ads'
ini <- '../../../../data/04-analysis/joint/'

df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                    initial_path = '../../../../')

f <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                 initial_path = '../../../../') |> 
  select(follower_id, pais, batch_id, eng, eng_base, eng_rt, eng_rt_base, 
         eng_no_rt, eng_no_rt_base, log_eng, log_eng_base, log_eng_rt, 
         log_eng_rt_base, log_eng_no_rt, log_eng_no_rt_base)

df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))

df <- df |> filter(n_posts_base>0)

df <- df |> mutate(strat_block1 = paste0(strat_block1, batch_id, pais), 
                   strat_block2 = paste0(strat_block2, batch_id, pais))
df_int <- tibble()

for (type in list_types){
  
  aux <- paste0(type, aux_t_base)
  
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
  
  df_coef <- tibble(coef = coef, sd = se, var = aux_t_base) |> 
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
    mutate(Variable = case_when(var == 'verifiability_base' ~ paste0(addon, 'Verifiable RTs + Posts'),
                                var == 'verifiability_rt_base' ~ paste0(addon, 
                                                       'Verifiable RTs'),
                                var == 'verifiability_no_rt_base' ~ paste0(addon, 
                                                       'Verifiable Posts'),
                                var == 'non_ver_base' ~ paste0(addon, 'Non-Verifiable RTs + Posts'),
                                var == 'non_ver_rt_base' ~ paste0(addon, 
                                                           'Non-Verifiable RTs'),
                                var == 'non_ver_no_rt_base' ~ paste0(addon, 
                                                           'Non-Verifiable Posts'),
                                var == 'true_base' ~ paste0(addon, 'True RTs + Posts'),
                                var == 'true_rt_base' ~ paste0(addon, 'True RTs'),
                                var == 'true_no_rt_base' ~ paste0(addon, 'True Posts'),
                                var == 'fake_base' ~ paste0(addon, 'Fake RTs + Posts'),
                                var == 'fake_rt_base' ~ paste0(addon, 'Fake RTs'),
                                var == 'fake_no_rt_base' ~ paste0(addon, 'Fake Posts'),
                                var == 'n_posts_base' ~ paste0(addon, 
                                                          'Number of RTs + Posts'),
                                var == 'n_posts_rt_base' ~ paste0(addon, 
                                                           'Number of RTs'),
                                var == 'n_posts_no_rt_base' ~ paste0(addon, 
                                                           'Number of Posts'),
                                var == 'eng_base' ~ paste0(addon, 
                                                               'Number of RTs + Posts (English)'),
                                var == 'eng_rt_base' ~ paste0(addon, 
                                                                  'Number of RTs (English)'),
                                var == 'eng_no_rt_base' ~ paste0(addon, 
                                                                     'Number of Posts (English)')
    ))
  
  
  final$Variable <- factor(final$Variable, levels = c(paste0(addon, 'Fake Posts'), 
                                                      paste0(addon, 'True Posts'), 
                                                      paste0(addon, 'Verifiable Posts'), 
                                                      paste0(addon, 'Non-Verifiable Posts'),
                                                      paste0(addon, 'Number of Posts (English)'),
                                                      paste0(addon, 'Number of Posts'),
                                                      paste0(addon, 'Fake RTs'), 
                                                      paste0(addon, 'True RTs'), 
                                                      paste0(addon, 'Verifiable RTs'), 
                                                      paste0(addon, 'Non-Verifiable RTs'),
                                                      paste0(addon, 'Number of RTs (English)'),
                                                      paste0(addon, 'Number of RTs'),
                                                      paste0(addon, 'Fake RTs + Posts'), 
                                                      paste0(addon, 'True RTs + Posts'), 
                                                      paste0(addon, 'Verifiable RTs + Posts'), 
                                                      paste0(addon, 'Non-Verifiable RTs + Posts'),
                                                      paste0(addon, 'Number of RTs + Posts (English)'),
                                                      paste0(addon, 'Number of RTs + Posts')))
  
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
    results_plot <- results_plot + xlim(-.1, .1)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
