# 2.0 Define constants
country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('log_')
file_code <- 'next_sent_bert'
ini <- '../../../../data/04-analysis/joint/'

# 3.0 Import data and manipulate
df <- get_analysis_sent_bert_final_winsor(stage = stage, batches = 'b1b2',
                                          initial_path = '../../../../')
df <- df |> filter(n_posts_base>0)

for (type in list_types){
  
  aux <- paste0(type, aux_s_b_base)
  
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
  
  # Total
  pos_rt_c <- coefs_all %>% 
    select(ends_with(aux[1]))
  pos_rt_c <- pos_rt_c[1,]
  
  neu_rt_c <- coefs_all %>% 
    select(ends_with(aux[2]))
  neu_rt_c <- neu_rt_c[1,]
  
  neg_rt_c <- coefs_all %>% 
    select(ends_with(aux[3]))
  neg_rt_c <- neg_rt_c[1,]
  
  n_posts_rt_c <- coefs_all %>% 
    select(ends_with(aux[4]))
  n_posts_rt_c <- n_posts_rt_c[1,]
  
  pos_rt_v <- coefs_all %>% 
    select(ends_with(aux[5]))
  pos_rt_v <- pos_rt_v[1,]
  
  neu_rt_v <- coefs_all %>% 
    select(ends_with(aux[6]))
  neu_rt_v <- neu_rt_v[1,]
  
  neg_rt_v <- coefs_all %>% 
    select(ends_with(aux[7]))
  neg_rt_v <- neg_rt_v[1,]
  
  n_posts_rt_v <- coefs_all %>% 
    select(ends_with(aux[8]))
  n_posts_rt_v <- n_posts_rt_v[1,]
  
  # RTs
  
  pos_rt_c1 <- coefs_all %>% 
    select(ends_with(aux[9]))
  pos_rt_c1 <- pos_rt_c1[1,]
  
  neu_rt_c1 <- coefs_all %>% 
    select(ends_with(aux[10]))
  neu_rt_c1 <- neu_rt_c1[1,]
  
  neg_rt_c1 <- coefs_all %>% 
    select(ends_with(aux[11]))
  neg_rt_c1 <- neg_rt_c1[1,]
  
  n_posts_rt_c1 <- coefs_all %>% 
    select(ends_with(aux[12]))
  n_posts_rt_c1 <- n_posts_rt_c1[1,]
  
  pos_rt_v1 <- coefs_all %>% 
    select(ends_with(aux[13]))
  pos_rt_v1 <- pos_rt_v1[1,]
  
  neu_rt_v1 <- coefs_all %>% 
    select(ends_with(aux[14]))
  neu_rt_v1 <- neu_rt_v1[1,]
  
  neg_rt_v1 <- coefs_all %>% 
    select(ends_with(aux[15]))
  neg_rt_v1 <- neg_rt_v1[1,]
  
  n_posts_rt_v1 <- coefs_all %>% 
    select(ends_with(aux[16]))
  n_posts_rt_v1 <- n_posts_rt_v1[1,]
  
  # Posts
  
  pos_rt_c2 <- coefs_all %>% 
    select(ends_with(aux[17]))
  pos_rt_c2 <- pos_rt_c2[1,]
  
  neu_rt_c2 <- coefs_all %>% 
    select(ends_with(aux[18]))
  neu_rt_c2 <- neu_rt_c2[1,]
  
  neg_rt_c2 <- coefs_all %>% 
    select(ends_with(aux[19]))
  neg_rt_c2 <- neg_rt_c2[1,]
  
  n_posts_rt_c2 <- coefs_all %>% 
    select(ends_with(aux[20]))
  n_posts_rt_c2 <- n_posts_rt_c2[1,]
  
  pos_rt_v2 <- coefs_all %>% 
    select(ends_with(aux[21]))
  pos_rt_v2 <- pos_rt_v2[1,]
  
  neu_rt_v2 <- coefs_all %>% 
    select(ends_with(aux[22]))
  neu_rt_v2 <- neu_rt_v2[1,]
  
  neg_rt_v2 <- coefs_all %>% 
    select(ends_with(aux[23]))
  neg_rt_v2 <- neg_rt_v2[1,]
  
  n_posts_rt_v2 <- coefs_all %>% 
    select(ends_with(aux[24]))
  n_posts_rt_v2 <- n_posts_rt_v2[1,]
  
  coefs_perm <- data.frame(pos_rt_c, neu_rt_c, neg_rt_c, n_posts_rt_c, pos_rt_v, 
                           neu_rt_v, neg_rt_v, n_posts_rt_v, 
                           pos_rt_c1, neu_rt_c1, neg_rt_c1, n_posts_rt_c1, pos_rt_v1, 
                           neu_rt_v1, neg_rt_v1, n_posts_rt_v1, 
                           pos_rt_c2, neu_rt_c2, neg_rt_c2, n_posts_rt_c2, pos_rt_v2, 
                           neu_rt_v2, neg_rt_v2, n_posts_rt_v2)
  
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
    
    # Total
    pos_rt_c <- coefs_all %>% 
      select(ends_with(aux[1]))
    pos_rt_c <- pos_rt_c[1,]
    
    neu_rt_c <- coefs_all %>% 
      select(ends_with(aux[2]))
    neu_rt_c <- neu_rt_c[1,]
    
    neg_rt_c <- coefs_all %>% 
      select(ends_with(aux[3]))
    neg_rt_c <- neg_rt_c[1,]
    
    n_posts_rt_c <- coefs_all %>% 
      select(ends_with(aux[4]))
    n_posts_rt_c <- n_posts_rt_c[1,]
    
    pos_rt_v <- coefs_all %>% 
      select(ends_with(aux[5]))
    pos_rt_v <- pos_rt_v[1,]
    
    neu_rt_v <- coefs_all %>% 
      select(ends_with(aux[6]))
    neu_rt_v <- neu_rt_v[1,]
    
    neg_rt_v <- coefs_all %>% 
      select(ends_with(aux[7]))
    neg_rt_v <- neg_rt_v[1,]
    
    n_posts_rt_v <- coefs_all %>% 
      select(ends_with(aux[8]))
    n_posts_rt_v <- n_posts_rt_v[1,]
    
    # RTs
    
    pos_rt_c1 <- coefs_all %>% 
      select(ends_with(aux[9]))
    pos_rt_c1 <- pos_rt_c1[1,]
    
    neu_rt_c1 <- coefs_all %>% 
      select(ends_with(aux[10]))
    neu_rt_c1 <- neu_rt_c1[1,]
    
    neg_rt_c1 <- coefs_all %>% 
      select(ends_with(aux[11]))
    neg_rt_c1 <- neg_rt_c1[1,]
    
    n_posts_rt_c1 <- coefs_all %>% 
      select(ends_with(aux[12]))
    n_posts_rt_c1 <- n_posts_rt_c1[1,]
    
    pos_rt_v1 <- coefs_all %>% 
      select(ends_with(aux[13]))
    pos_rt_v1 <- pos_rt_v1[1,]
    
    neu_rt_v1 <- coefs_all %>% 
      select(ends_with(aux[14]))
    neu_rt_v1 <- neu_rt_v1[1,]
    
    neg_rt_v1 <- coefs_all %>% 
      select(ends_with(aux[15]))
    neg_rt_v1 <- neg_rt_v1[1,]
    
    n_posts_rt_v1 <- coefs_all %>% 
      select(ends_with(aux[16]))
    n_posts_rt_v1 <- n_posts_rt_v1[1,]
    
    # Posts
    
    pos_rt_c2 <- coefs_all %>% 
      select(ends_with(aux[17]))
    pos_rt_c2 <- pos_rt_c2[1,]
    
    neu_rt_c2 <- coefs_all %>% 
      select(ends_with(aux[18]))
    neu_rt_c2 <- neu_rt_c2[1,]
    
    neg_rt_c2 <- coefs_all %>% 
      select(ends_with(aux[19]))
    neg_rt_c2 <- neg_rt_c2[1,]
    
    n_posts_rt_c2 <- coefs_all %>% 
      select(ends_with(aux[20]))
    n_posts_rt_c2 <- n_posts_rt_c2[1,]
    
    pos_rt_v2 <- coefs_all %>% 
      select(ends_with(aux[21]))
    pos_rt_v2 <- pos_rt_v2[1,]
    
    neu_rt_v2 <- coefs_all %>% 
      select(ends_with(aux[22]))
    neu_rt_v2 <- neu_rt_v2[1,]
    
    neg_rt_v2 <- coefs_all %>% 
      select(ends_with(aux[23]))
    neg_rt_v2 <- neg_rt_v2[1,]
    
    n_posts_rt_v2 <- coefs_all %>% 
      select(ends_with(aux[24]))
    n_posts_rt_v2 <- n_posts_rt_v2[1,]
    
    coefs_perm <- data.frame(pos_rt_c, neu_rt_c, neg_rt_c, n_posts_rt_c, pos_rt_v, 
                             neu_rt_v, neg_rt_v, n_posts_rt_v, 
                             pos_rt_c1, neu_rt_c1, neg_rt_c1, n_posts_rt_c1, pos_rt_v1, 
                             neu_rt_v1, neg_rt_v1, n_posts_rt_v1, 
                             pos_rt_c2, neu_rt_c2, neg_rt_c2, n_posts_rt_c2, pos_rt_v2, 
                             neu_rt_v2, neg_rt_v2, n_posts_rt_v2)
    
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
    mutate(Variable = case_when(var == 'pos_rt_c' ~ paste0(addon, 'Positive COVID RTs + Posts'),
                                var == "neu_rt_c" ~ paste0(addon, 
                                                           'Neutral COVID RTs + Posts'),
                                var == "neg_rt_c" ~ paste0(addon, 
                                                           'Negative COVID RTs + Posts'),
                                var == "n_posts_rt_c" ~ paste0(addon, 
                                                               'Number of COVID RTs + Posts'),
                                var == "pos_rt_v" ~ paste0(addon, 'Positive Vaccine RTs + Posts'),
                                var == "neu_rt_v" ~ paste0(addon, 
                                                           'Neutral Vaccine RTs + Posts'),
                                var == "neg_rt_v" ~ paste0(addon, 
                                                           'Negative Vaccine RTs + Posts'),
                                var == "n_posts_rt_v" ~ paste0(addon, 
                                                               'Number of Vaccine RTs + Posts'), 
                                var == 'pos_rt_c1' ~ paste0(addon, 'Positive COVID RTs'),
                                var == "neu_rt_c1" ~ paste0(addon, 
                                                           'Neutral COVID RTs'),
                                var == "neg_rt_c1" ~ paste0(addon, 
                                                           'Negative COVID RTs'),
                                var == "n_posts_rt_c1" ~ paste0(addon, 
                                                               'Number of COVID RTs'),
                                var == "pos_rt_v1" ~ paste0(addon, 'Positive Vaccine RTs'),
                                var == "neu_rt_v1" ~ paste0(addon, 
                                                           'Neutral Vaccine RTs'),
                                var == "neg_rt_v1" ~ paste0(addon, 
                                                           'Negative Vaccine RTs'),
                                var == "n_posts_rt_v1" ~ paste0(addon, 
                                                               'Number of Vaccine RTs'), 
                                var == 'pos_rt_c2' ~ paste0(addon, 'Positive COVID Posts'),
                                var == "neu_rt_c2" ~ paste0(addon, 
                                                           'Neutral COVID Posts'),
                                var == "neg_rt_c2" ~ paste0(addon, 
                                                           'Negative COVID Posts'),
                                var == "n_posts_rt_c2" ~ paste0(addon, 
                                                               'Number of COVID Posts'),
                                var == "pos_rt_v2" ~ paste0(addon, 'Positive Vaccine Posts'),
                                var == "neu_rt_v2" ~ paste0(addon, 
                                                           'Neutral Vaccine Posts'),
                                var == "neg_rt_v2" ~ paste0(addon, 
                                                           'Negative Vaccine Posts'),
                                var == "n_posts_rt_v2" ~ paste0(addon, 
                                                               'Number of Vaccine Posts')))
  
  
  final$Variable <- factor(final$Variable, 
                           levels = c(paste0(addon, 'Negative Vaccine Posts'),
                                      paste0(addon, 'Negative COVID Posts'), 
                                      paste0(addon, 'Neutral Vaccine Posts'), 
                                      paste0(addon, 'Neutral COVID Posts'), 
                                      paste0(addon, 'Positive Vaccine Posts'), 
                                      paste0(addon, 'Positive COVID Posts'), 
                                      paste0(addon, 'Number of Vaccine Posts'), 
                                      paste0(addon, 'Number of COVID Posts'),
                                      paste0(addon, 'Negative Vaccine RTs'),
                                      paste0(addon, 'Negative COVID RTs'), 
                                      paste0(addon, 'Neutral Vaccine RTs'), 
                                      paste0(addon, 'Neutral COVID RTs'), 
                                      paste0(addon, 'Positive Vaccine RTs'), 
                                      paste0(addon, 'Positive COVID RTs'), 
                                      paste0(addon, 'Number of Vaccine RTs'), 
                                      paste0(addon, 'Number of COVID RTs'),
                                      paste0(addon, 'Negative Vaccine RTs + Posts'),
                                      paste0(addon, 'Negative COVID RTs + Posts'), 
                                      paste0(addon, 'Neutral Vaccine RTs + Posts'), 
                                      paste0(addon, 'Neutral COVID RTs + Posts'), 
                                      paste0(addon, 'Positive Vaccine RTs + Posts'), 
                                      paste0(addon, 'Positive COVID RTs + Posts'), 
                                      paste0(addon, 'Number of Vaccine RTs + Posts'), 
                                      paste0(addon, 'Number of COVID RTs + Posts')))
  
  
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
     results_plot <- results_plot + xlim(-.03, .03)
  }else{
    results_plot <- results_plot
   }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', file_coefs,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
