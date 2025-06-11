# 2.0 Define constants
country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('', 'log_')
file_code <- 'next_urls'
ini <- '../../../../data/04-analysis/joint/'

fes <- read_parquet('../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet')

# 3.0 Import data and manipulate
df <- get_analysis_urls(stage = stage, batches = 'b1b2',
                        initial_path = '../../../../')
df <- df |> filter(n_posts_base>0)

df <- df |> left_join(fes, by = c('follower_id', 'pais', 'batch_id'))

for (type in list_types){
  
  aux <- paste0(type, aux_urls_base)
  
  # 4.0 Run original estimates
  aux_data <- df[aux]
  lm_list_ols <- list()
  count <- 1
  for (x in aux) {
    fmla1 <- as.formula(paste0(x, "~ total_treated | total_influencers"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, feols(fmla1, data = df))
    coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
      select(Estimate)
    names(coefs) <- paste0(x)
    coefs <- cbind('treatment' = rownames(coefs), coefs)
    rownames(coefs) <- 1:nrow(coefs)
    lm_list_ols[[count]] <- coefs
    count <- count + 1
  }
  coefs_all <- lm_list_ols %>% 
    reduce(left_join, by = "treatment")
  
  # Build matrix
  tot_urls <- coefs_all %>% 
    select(ends_with(aux[1]))
  tot_urls <- tot_urls[1,]
  
  tot_info <- coefs_all %>% 
    select(ends_with(aux[2]))
  tot_info <- tot_info[1,]
  
  tot_news <- coefs_all %>% 
    select(ends_with(aux[3]))
  tot_news <- tot_news[1,]
  
  fc <- coefs_all %>% 
    select(ends_with(aux[4]))
  fc <- fc[1,]
  
  rel <- coefs_all %>% 
    select(starts_with(aux[5]))
  rel <- rel[1,]
  
  nrel <- coefs_all %>% 
    select(ends_with(aux[6]))
  nrel <- nrel[1,]
  
  other <- coefs_all %>% 
    select(ends_with(aux[7]))
  other <- other[1,]
  
  coefs_perm <- data.frame(tot_urls, tot_info, tot_news, fc, rel, nrel, other)
  
  write_xlsx(
    coefs_perm, paste0("../../../../data/04-analysis/",country,
                       "/Baseline/original/", type, file_code,
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
      fmla1 <- as.formula(paste0(au, "~ total_treated | total_influencers"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = data))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0(au)
      coefs <- cbind('treatment' = rownames(coefs), coefs)
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_list <- append(coefs_list, lm_list_ols)
    coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
    
    # Build matrix
    tot_urls <- coefs_all %>% 
      select(ends_with(aux[1]))
    tot_urls <- tot_urls[1,]
    
    tot_info <- coefs_all %>% 
      select(ends_with(aux[2]))
    tot_info <- tot_info[1,]
    
    tot_news <- coefs_all %>% 
      select(ends_with(aux[3]))
    tot_news <- tot_news[1,]
    
    fc <- coefs_all %>% 
      select(ends_with(aux[4]))
    fc <- fc[1,]
    
    rel <- coefs_all %>% 
      select(starts_with(aux[5]))
    rel <- rel[1,]
    
    nrel <- coefs_all %>% 
      select(ends_with(aux[6]))
    nrel <- nrel[1,]
    
    other <- coefs_all %>% 
      select(ends_with(aux[7]))
    other <- other[1,]
    
    coefs_perm <- data.frame(tot_urls, tot_info, tot_news, fc, rel, nrel, other)
    
    coefs_fin <- rbind(coefs_fin, coefs_perm)
    
    
    i <- i + 1}
  print(type)
  write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/Baseline/permutations/", 
                               type, file_code,
                               ".xlsx"))
}

for (type in list_types){
  
  
  file_coefs <- paste0(type, file_code)
  coefs <- proc_coefs_base(file_coefs)
  ses <- proc_ses_base(file_coefs)
  
  if (type == 'log_'){
    addon <- 'log '
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
  }else {
    addon <- ''
  }
  
  final <- coefs |> left_join(ses, by = c('stage', 'var'))
  
  final <- final |> 
    mutate(Variable = case_when(var == 'tot_urls' ~ paste0(addon, 'Total URLs'),
                                var == 'tot_info' ~ paste0(addon, 'Total Info.'),
                                var == 'tot_news' ~ paste0(addon, 'Total News'),
                                var == 'fc' ~ paste0(addon, 'Fact-Checks'),
                                var == 'rel' ~ paste0(addon, 'Reliable'),
                                var == 'nrel' ~ paste0(addon, 'Non-Reliable'),
                                var == 'other' ~ paste0(addon, 'Other Not Info.')))
  
  
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
    results_plot <- results_plot + xlim(-.065, .065)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', file_coefs,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
