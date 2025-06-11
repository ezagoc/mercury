# 2.0 Define constants
country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('log_')
file_code <- 'next_verifiability'
ini <- '../../../../data/04-analysis/joint/'

# 3.0 Import data and manipulate
df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../')

f <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                 initial_path = '../../../../') |> 
  select(follower_id, pais, batch_id, eng, eng_base, eng_rt, eng_rt_base, 
         eng_no_rt, eng_no_rt_base, log_eng, log_eng_base, log_eng_rt, 
         log_eng_rt_base, log_eng_no_rt, log_eng_no_rt_base)

df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))

df <- df |> filter(n_posts_base>0)

for (type in list_types){
    
    aux <- paste0(type, aux_t_base)
    
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
    ver <- coefs_all %>% 
      select(ends_with(aux[1]))
    ver <- ver[1,]
    
    non_ver <- coefs_all %>% 
      select(ends_with(aux[2]))
    non_ver <- non_ver[1,]
    
    true <- coefs_all %>% 
      select(ends_with(aux[3]))
    true <- true[1,]
    
    fake <- coefs_all %>% 
      select(ends_with(aux[4]))
    fake <- fake[1,]
    
    n_posts <- coefs_all %>% 
      select(ends_with(aux[5]))
    n_posts <- n_posts[1,]
    
    # RT
    
    ver1 <- coefs_all %>% 
      select(ends_with(aux[6]))
    ver1 <- ver1[1,]
    
    non_ver1 <- coefs_all %>% 
      select(ends_with(aux[7]))
    non_ver1 <- non_ver1[1,]
    
    true1 <- coefs_all %>% 
      select(ends_with(aux[8]))
    true1 <- true1[1,]
    
    fake1 <- coefs_all %>% 
      select(ends_with(aux[9]))
    fake1 <- fake1[1,]
    
    n_posts1 <- coefs_all %>% 
      select(ends_with(aux[10]))
    n_posts1 <- n_posts1[1,]
    
    # posts
    
    ver2 <- coefs_all %>% 
      select(ends_with(aux[11]))
    ver2 <- ver2[1,]
    
    non_ver2 <- coefs_all %>% 
      select(ends_with(aux[12]))
    non_ver2 <- non_ver2[1,]
    
    true2 <- coefs_all %>% 
      select(ends_with(aux[13]))
    true2 <- true2[1,]
    
    fake2 <- coefs_all %>% 
      select(ends_with(aux[14]))
    fake2 <- fake2[1,]
    
    n_posts2 <- coefs_all %>% 
      select(ends_with(aux[15]))
    n_posts2 <- n_posts2[1,]
    
    # Eng: 
    
    eng <- coefs_all %>% 
      select(ends_with(aux[16]))
    eng <- eng[1,]
    
    eng1 <- coefs_all %>% 
      select(ends_with(aux[17]))
    eng1 <- eng1[1,]
    
    eng2 <- coefs_all %>% 
      select(ends_with(aux[18]))
    eng2 <- eng2[1,]
    
    coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts, 
                             ver1, non_ver1, true1, fake1, n_posts1,
                             ver2, non_ver2, true2, fake2, n_posts2, 
                             eng, eng1, eng2)
    
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
      ver <- coefs_all %>% 
        select(ends_with(aux[1]))
      ver <- ver[1,]
      
      non_ver <- coefs_all %>% 
        select(ends_with(aux[2]))
      non_ver <- non_ver[1,]
      
      true <- coefs_all %>% 
        select(ends_with(aux[3]))
      true <- true[1,]
      
      fake <- coefs_all %>% 
        select(ends_with(aux[4]))
      fake <- fake[1,]
      
      n_posts <- coefs_all %>% 
        select(ends_with(aux[5]))
      n_posts <- n_posts[1,]
      
      # RT
      
      ver1 <- coefs_all %>% 
        select(ends_with(aux[6]))
      ver1 <- ver1[1,]
      
      non_ver1 <- coefs_all %>% 
        select(ends_with(aux[7]))
      non_ver1 <- non_ver1[1,]
      
      true1 <- coefs_all %>% 
        select(ends_with(aux[8]))
      true1 <- true1[1,]
      
      fake1 <- coefs_all %>% 
        select(ends_with(aux[9]))
      fake1 <- fake1[1,]
      
      n_posts1 <- coefs_all %>% 
        select(ends_with(aux[10]))
      n_posts1 <- n_posts1[1,]
      
      # posts
      
      ver2 <- coefs_all %>% 
        select(ends_with(aux[11]))
      ver2 <- ver2[1,]
      
      non_ver2 <- coefs_all %>% 
        select(ends_with(aux[12]))
      non_ver2 <- non_ver2[1,]
      
      true2 <- coefs_all %>% 
        select(ends_with(aux[13]))
      true2 <- true2[1,]
      
      fake2 <- coefs_all %>% 
        select(ends_with(aux[14]))
      fake2 <- fake2[1,]
      
      n_posts2 <- coefs_all %>% 
        select(ends_with(aux[15]))
      n_posts2 <- n_posts2[1,]
      
      # Eng: 
      
      eng <- coefs_all %>% 
        select(ends_with(aux[16]))
      eng <- eng[1,]
      
      eng1 <- coefs_all %>% 
        select(ends_with(aux[17]))
      eng1 <- eng1[1,]
      
      eng2 <- coefs_all %>% 
        select(ends_with(aux[18]))
      eng2 <- eng2[1,]
      
      coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts, 
                               ver1, non_ver1, true1, fake1, n_posts1,
                               ver2, non_ver2, true2, fake2, n_posts2, 
                               eng, eng1, eng2)
      
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
    mutate(Variable = case_when(var == 'ver' ~ paste0(addon, 'Verifiable RTs + Posts'),
                                var == 'ver1' ~ paste0(addon, 
                                                       'Verifiable RTs'),
                                var == 'ver2' ~ paste0(addon, 
                                                       'Verifiable Posts'),
                                var == 'non_ver' ~ paste0(addon, 'Non-Verifiable RTs + Posts'),
                                var == 'non_ver1' ~ paste0(addon, 
                                                           'Non-Verifiable RTs'),
                                var == 'non_ver2' ~ paste0(addon, 
                                                           'Non-Verifiable Posts'),
                                var == 'true' ~ paste0(addon, 'True RTs + Posts'),
                                var == 'true1' ~ paste0(addon, 'True RTs'),
                                var == 'true2' ~ paste0(addon, 'True Posts'),
                                var == 'fake' ~ paste0(addon, 'Fake RTs + Posts'),
                                var == 'fake1' ~ paste0(addon, 'Fake RTs'),
                                var == 'fake2' ~ paste0(addon, 'Fake Posts'),
                                var == 'n_posts' ~ paste0(addon, 
                                                          'Number of RTs + Posts'),
                                var == 'n_posts1' ~ paste0(addon, 
                                                           'Number of RTs'),
                                var == 'n_posts2' ~ paste0(addon, 
                                                           'Number of Posts'), 
                                var == 'eng' ~ paste0(addon, 
                                                          'Number of RTs + Posts (English)'),
                                var == 'eng1' ~ paste0(addon, 
                                                           'Number of RTs (English)'),
                                var == 'eng2' ~ paste0(addon, 
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
    results_plot <- results_plot + xlim(-.2, .2)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../../../results/01-regression_graphs/',
                           data_type, '/', file_coefs,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
