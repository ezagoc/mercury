# 2.0 Define constants
country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('', 'log_')
file_code <- 'next_interactions'
ini <- '../../../../data/04-analysis/joint/'

# 3.0 Import data and manipulate
df <- get_analysis_int_ver_final_winsor(stage = stage, batches = 'b1b2',
                                              initial_path = '../../../../')
df <- df |> filter(n_posts_base>0)
for (type in list_types){
  
  aux <- paste0(type, aux_int_reac_base)
  
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
  
  # Shares
  
  ver_rt <- coefs_all %>% 
    select(ends_with(aux[1]))
  ver_rt <- ver_rt[1,]
  
  true_rt <- coefs_all %>% 
    select(ends_with(aux[2]))
  true_rt <- true_rt[1,]
  
  fake_rt <- coefs_all %>% 
    select(ends_with(aux[3]))
  fake_rt <- fake_rt[1,]
  
  n_ver_rt <- coefs_all %>% 
    select(ends_with(aux[4]))
  n_ver_rt <- n_ver_rt[1,]
  
  tot_rt <- coefs_all %>% 
    select(ends_with(aux[5]))
  tot_rt <- tot_rt[1,]
  
  # Comments
  
  ver_rt1 <- coefs_all %>% 
    select(ends_with(aux[6]))
  ver_rt1 <- ver_rt1[1,]
  
  true_rt1 <- coefs_all %>% 
    select(ends_with(aux[7]))
  true_rt1 <- true_rt1[1,]
  
  fake_rt1 <- coefs_all %>% 
    select(ends_with(aux[8]))
  fake_rt1 <- fake_rt1[1,]
  
  n_ver_rt1 <- coefs_all %>% 
    select(ends_with(aux[9]))
  n_ver_rt1 <- n_ver_rt1[1,]
  
  tot_rt1 <- coefs_all %>% 
    select(ends_with(aux[10]))
  tot_rt1 <- tot_rt1[1,]
  
  # Likes
  
  ver_rt2 <- coefs_all %>% 
    select(ends_with(aux[11]))
  ver_rt2 <- ver_rt2[1,]
  
  true_rt2 <- coefs_all %>% 
    select(ends_with(aux[12]))
  true_rt2 <- true_rt2[1,]
  
  fake_rt2 <- coefs_all %>% 
    select(ends_with(aux[13]))
  fake_rt2 <- fake_rt2[1,]
  
  n_ver_rt2 <- coefs_all %>% 
    select(ends_with(aux[14]))
  n_ver_rt2 <- n_ver_rt2[1,]
  
  tot_rt2 <- coefs_all %>% 
    select(ends_with(aux[15]))
  tot_rt2 <- tot_rt2[1,]
  
  # Reactions
  
  ver_rt3 <- coefs_all %>% 
    select(ends_with(aux[16]))
  ver_rt3 <- ver_rt3[1,]
  
  true_rt3 <- coefs_all %>% 
    select(ends_with(aux[17]))
  true_rt3 <- true_rt3[1,]
  
  fake_rt3 <- coefs_all %>% 
    select(ends_with(aux[18]))
  fake_rt3 <- fake_rt3[1,]
  
  n_ver_rt3 <- coefs_all %>% 
    select(ends_with(aux[19]))
  n_ver_rt3 <- n_ver_rt3[1,]
  
  tot_rt3 <- coefs_all %>% 
    select(ends_with(aux[20]))
  tot_rt3 <- tot_rt3[1,]
  
  coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_ver_rt, tot_rt, 
                           ver_rt1, true_rt1, fake_rt1, n_ver_rt1, tot_rt1, 
                           ver_rt2, true_rt2, fake_rt2, n_ver_rt2, tot_rt2,
                           ver_rt3, true_rt3, fake_rt3, n_ver_rt3, tot_rt3)
  
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
    
    #  Shares
    
    ver_rt <- coefs_all %>% 
      select(ends_with(aux[1]))
    ver_rt <- ver_rt[1,]
    
    true_rt <- coefs_all %>% 
      select(ends_with(aux[2]))
    true_rt <- true_rt[1,]
    
    fake_rt <- coefs_all %>% 
      select(ends_with(aux[3]))
    fake_rt <- fake_rt[1,]
    
    n_ver_rt <- coefs_all %>% 
      select(ends_with(aux[4]))
    n_ver_rt <- n_ver_rt[1,]
    
    tot_rt <- coefs_all %>% 
      select(ends_with(aux[5]))
    tot_rt <- tot_rt[1,]
    
    # Comments
    
    ver_rt1 <- coefs_all %>% 
      select(ends_with(aux[6]))
    ver_rt1 <- ver_rt1[1,]
    
    true_rt1 <- coefs_all %>% 
      select(ends_with(aux[7]))
    true_rt1 <- true_rt1[1,]
    
    fake_rt1 <- coefs_all %>% 
      select(ends_with(aux[8]))
    fake_rt1 <- fake_rt1[1,]
    
    n_ver_rt1 <- coefs_all %>% 
      select(ends_with(aux[9]))
    n_ver_rt1 <- n_ver_rt1[1,]
    
    tot_rt1 <- coefs_all %>% 
      select(ends_with(aux[10]))
    tot_rt1 <- tot_rt1[1,]
    
    # Likes
    
    ver_rt2 <- coefs_all %>% 
      select(ends_with(aux[11]))
    ver_rt2 <- ver_rt2[1,]
    
    true_rt2 <- coefs_all %>% 
      select(ends_with(aux[12]))
    true_rt2 <- true_rt2[1,]
    
    fake_rt2 <- coefs_all %>% 
      select(ends_with(aux[13]))
    fake_rt2 <- fake_rt2[1,]
    
    n_ver_rt2 <- coefs_all %>% 
      select(ends_with(aux[14]))
    n_ver_rt2 <- n_ver_rt2[1,]
    
    tot_rt2 <- coefs_all %>% 
      select(ends_with(aux[15]))
    tot_rt2 <- tot_rt2[1,]
    
    # Reactions
    
    ver_rt3 <- coefs_all %>% 
      select(ends_with(aux[16]))
    ver_rt3 <- ver_rt3[1,]
    
    true_rt3 <- coefs_all %>% 
      select(ends_with(aux[17]))
    true_rt3 <- true_rt3[1,]
    
    fake_rt3 <- coefs_all %>% 
      select(ends_with(aux[18]))
    fake_rt3 <- fake_rt3[1,]
    
    n_ver_rt3 <- coefs_all %>% 
      select(ends_with(aux[19]))
    n_ver_rt3 <- n_ver_rt3[1,]
    
    tot_rt3 <- coefs_all %>% 
      select(ends_with(aux[20]))
    tot_rt3 <- tot_rt3[1,]
    
    coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_ver_rt, tot_rt, 
                             ver_rt1, true_rt1, fake_rt1, n_ver_rt1, tot_rt1, 
                             ver_rt2, true_rt2, fake_rt2, n_ver_rt2, tot_rt2,
                             ver_rt3, true_rt3, fake_rt3, n_ver_rt3, tot_rt3)
    
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
    mutate(Variable = case_when(var == 'ver_rt' ~ paste0(addon, 'Total Shares (Verifiable)'),
                                var == 'true_rt' ~ paste0(addon, 'Total Shares (True)'),
                                var == 'fake_rt' ~ paste0(addon, 'Total Shares (Fake)'),
                                var == 'tot_rt' ~ paste0(addon, 'Total Shares'),
                                var == 'n_ver_rt' ~ paste0(addon, 
                                                           'Total Shares (Non-Verifiable)'), 
                                var == 'ver_rt1' ~ paste0(addon, 'Total Comments (Verifiable)'),
                                var == 'true_rt1' ~ paste0(addon, 'Total Comments (True)'),
                                var == 'fake_rt1' ~ paste0(addon, 'Total Comments (Fake)'),
                                var == 'tot_rt1' ~ paste0(addon, 'Total Comments'),
                                var == 'n_ver_rt1' ~ paste0(addon, 
                                                           'Total Comments (Non-Verifiable)'), 
                                var == 'ver_rt2' ~ paste0(addon, 'Total Likes (Verifiable)'),
                                var == 'true_rt2' ~ paste0(addon, 'Total Likes (True)'),
                                var == 'fake_rt2' ~ paste0(addon, 'Total Likes (Fake)'),
                                var == 'tot_rt2' ~ paste0(addon, 'Total Likes'),
                                var == 'n_ver_rt2' ~ paste0(addon, 
                                                           'Total Likes (Non-Verifiable)'), 
                                var == 'ver_rt3' ~ paste0(addon, 'Total Reactions (Verifiable)'),
                                var == 'true_rt3' ~ paste0(addon, 'Total Reactions (True)'),
                                var == 'fake_rt3' ~ paste0(addon, 'Total Reactions (Fake)'),
                                var == 'tot_rt3' ~ paste0(addon, 'Total Reactions'),
                                var == 'n_ver_rt3' ~ paste0(addon, 
                                                           'Total Reactions (Non-Verifiable)')))
  
  
  final$Variable <- factor(final$Variable, levels = c(paste0(addon, 'Total Shares (Fake)'),
                                                      paste0(addon, 
                                                             'Total Shares (True)'), 
                                                      paste0(addon, 'Total Shares (Verifiable)'), 
                                                      paste0(addon, 'Total Shares (Non-Verifiable)'), 
                                                      paste0(addon, 'Total Shares'), 
                                                      paste0(addon, 'Total Comments (Fake)'),
                                                      paste0(addon, 
                                                             'Total Comments (True)'), 
                                                      paste0(addon, 'Total Comments (Verifiable)'), 
                                                      paste0(addon, 'Total Comments (Non-Verifiable)'), 
                                                      paste0(addon, 'Total Comments'), 
                                                      paste0(addon, 'Total Likes (Fake)'),
                                                      paste0(addon, 
                                                             'Total Likes (True)'), 
                                                      paste0(addon, 'Total Likes (Verifiable)'), 
                                                      paste0(addon, 'Total Likes (Non-Verifiable)'), 
                                                      paste0(addon, 'Total Likes'), 
                                                      paste0(addon, 'Total Reactions (Fake)'),
                                                      paste0(addon, 
                                                             'Total Reactions (True)'), 
                                                      paste0(addon, 'Total Reactions (Verifiable)'), 
                                                      paste0(addon, 'Total Reactions (Non-Verifiable)'), 
                                                      paste0(addon, 'Total Reactions')))
  
  
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
    results_plot <- results_plot + xlim(-.3, .3)
  }else{
    results_plot <- results_plot
   }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', file_coefs,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
