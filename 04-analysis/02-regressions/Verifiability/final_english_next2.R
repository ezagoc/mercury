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

fes <- read_parquet('../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet')

# 2.0 Define constants
country <- 'joint'
data_type <- 'Verifiability'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('')
file_code <- 'next_english'
ini <- '../../../../data/04-analysis/joint/'

for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../')
  df <- df |> filter(n_posts_base>0)
  
  df <- df |> mutate(eng_prop = ifelse(n_posts > 0, eng/n_posts, 1), 
                     dummy_eng_prop = ifelse(n_posts == 0, 1, 0), 
                     eng_prop_base = ifelse(n_posts_base > 0, 
                                            eng_base/n_posts_base, 1), 
                     dummy_eng_prop_base = ifelse(n_posts_base == 0, 1, 0), 
                     n_eng_prop = ifelse(n_posts_rt > 0, n_eng/n_posts, 1), 
                     n_eng_prop_base = ifelse(n_posts_base > 0, 
                                               n_eng_base/n_posts_base, 1))
  
  df <- df |> left_join(fes, by = c('follower_id', 'pais', 'batch_id'))
  for (type in list_types){
    
    aux <- paste0(type, aux_eng)
    
    # 4.0 Run original estimates
    aux_data <- df[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated + dummy_eng_prop +",
                                 x, "_base + ", int_fes, " | total_influencers"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = df))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0(x)
      coefs <- cbind('treatment' = rownames(coefs), coefs) |>
        filter(treatment != paste0(x, '_base'))
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_all <- lm_list_ols %>% 
      reduce(left_join, by = "treatment")
    
    # Build matrix
    eng <- coefs_all %>% 
      select(aux[1])
    eng <- eng[1,]
    
    n_eng <- coefs_all %>% 
      select(aux[2])
    n_eng <- n_eng[1,]
    
    coefs_perm <- data.frame(eng, n_eng)
    
    write_xlsx(
      coefs_perm, paste0("../../../../data/04-analysis/",country,
                         "/",stage,"/original/", data_type, '/', type, file_code,
                         ".xlsx"))
    ### 5.0 Run 1000 Permutations: 
    
    i <- 1
    coefs_fin <- tibble()
    for (m in 1:1000){
      print(i)
      followers <- read_parquet(paste0("../../../../data/04-analysis/joint/",
                                       'small_ties_b1b2p', "/small_tie", 
                                       i,".parquet"))
      
      data <- df
      
      c1 = paste0("n_influencers_followed_control_no_weak_tie_p", i)
      c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", i)
      c3 = paste0("n_influencers_followed_treatment_weak_tie_p", i)
      c4 = paste0("n_influencers_followed_control_weak_tie_p", i)
      c5 = paste0("n_influencers_followed_control_strong_tie_p", i)
      c6 = paste0("n_influencers_followed_treatment_strong_tie_p", i)
      c7 = paste0("n_influencers_followed_control_no_strong_tie_p", i)
      c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", i)
      c9 = paste0("n_influencers_followed_control_p", i)
      c10 = paste0("n_influencers_followed_treatment_p", i)
      c11 = paste0("n_influencers_followed_p_", i)
      
      followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                             c5, c6, c7, c8, c9, c10, c11, pais, 
                                             batch_id) 
      
      data <- left_join(
        data, 
        followers_iter,
        by = c('follower_id', 'batch_id', 'pais')
      )
      # Pool treatment variables
      data <- poolTreatmentBalance2(data, c10, c11)
      
      # Balance tables 
      aux_data <- data[aux]
      coefs_list <- list()
      lm_list_ols <- list()
      count <- 1
      for (au in aux) {
        fmla1 <- as.formula(paste0(au, "~ total_treated + dummy_eng_prop +",
                                   au, "_base + ", int_fes, " | total_influencers"))
        nam1 <- paste("lm_", count, "_ols", sep = "")
        assign(nam1, feols(fmla1, data = data))
        coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
          select(Estimate)
        names(coefs) <- paste0(au)
        coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
          filter(treatment != paste0(au, '_base'))
        rownames(coefs) <- 1:nrow(coefs)
        lm_list_ols[[count]] <- coefs
        count <- count + 1
      }
      coefs_list <- append(coefs_list, lm_list_ols)
      coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
      
      # Build matrix
      eng <- coefs_all %>% 
        select(aux[1])
      eng <- eng[1,]
      
      n_eng <- coefs_all %>% 
        select(aux[2])
      n_eng <- n_eng[1,]
      
      coefs_perm <- data.frame(eng, n_eng)
      
      coefs_fin <- rbind(coefs_fin, coefs_perm)
      
      
      i <- i + 1}
    print(type)
    write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", stage, 
                                 "/permutations/", data_type, '/', type, file_code,
                                 ".xlsx"))
  }
  print(stage)
}

# 6.0 Make the graphs

for (type in list_types){
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
    mutate(Variable = case_when(var == 'eng' ~ paste0(addon, 'Proportion of English Posts + Shares'),
                                var == 'n_eng' ~ paste0(addon, 
                                                          'Proportion of Non-English Posts + Shares')), 
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'))
  
  final$Variable <- factor(final$Variable, levels = c(paste0(addon, 'Proportion of English Posts + Shares'),
                                                      paste0(addon, 
                                                             'Proportion of Non-English Posts + Shares')))
  
  results_plot <- ggplot(data = final, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7, 10), name = 'Outcome') +
    scale_color_manual(values = rep('black', 6), name = 'Outcome') +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    ylab("Total Treated Estimate with 95% Confidence Interval") + 
    xlab("Stage") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(-.02, .02)
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', type, file_code, '.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
