# 2.0 Define constants
country <- 'joint'
data_type <- 'SentimentBERT'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('log_')
ini <- '../../../../data/04-analysis/joint/'
file_code <- 'ext_nbase0'


for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_sent_bert_final_winsor(stage = stage, batches = 'b1b2',
                                            initial_path = '../../../../')
  df <- df |> filter(n_posts_base>0) |> filter(total_influencers == 1)
  df <- df |> left_join(blocks, by = c('follower_id', 'batch_id', 'pais'))
  for (type in list_types){
    
    aux <- paste0(type, aux_s_b)
    
    # 4.0 Run original estimates
    aux_data <- df[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                 x, "_base | block1_fe"))
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
    pos_rt_c <- coefs_all %>% 
      select(ends_with(aux[1]))
    pos_rt_c <- pos_rt_c[1,]
    
    pos_no_rt_c <- coefs_all %>% 
      select(ends_with(aux[2]))
    pos_no_rt_c <- pos_no_rt_c[1,]
    
    neu_rt_c <- coefs_all %>% 
      select(ends_with(aux[3]))
    neu_rt_c <- neu_rt_c[1,]
    
    neu_no_rt_c <- coefs_all %>% 
      select(ends_with(aux[4]))
    neu_no_rt_c <- neu_no_rt_c[1,]
    
    neg_rt_c <- coefs_all %>% 
      select(ends_with(aux[5]))
    neg_rt_c <- neg_rt_c[1,]
    
    neg_no_rt_c <- coefs_all %>% 
      select(ends_with(aux[6]))
    neg_no_rt_c <- neg_no_rt_c[1,]
    
    n_posts_rt_c <- coefs_all %>% 
      select(ends_with(aux[7]))
    n_posts_rt_c <- n_posts_rt_c[1,]
    
    n_posts_no_rt_c <- coefs_all %>% 
      select(ends_with(aux[8]))
    n_posts_no_rt_c <- n_posts_no_rt_c[1,]
    
    pos_rt_v <- coefs_all %>% 
      select(ends_with(aux[9]))
    pos_rt_v <- pos_rt_v[1,]
    
    pos_no_rt_v <- coefs_all %>% 
      select(ends_with(aux[10]))
    pos_no_rt_v <- pos_no_rt_v[1,]
    
    neu_rt_v <- coefs_all %>% 
      select(ends_with(aux[11]))
    neu_rt_v <- neu_rt_v[1,]
    
    neu_no_rt_v <- coefs_all %>% 
      select(ends_with(aux[12]))
    neu_no_rt_v <- neu_no_rt_v[1,]
    
    neg_rt_v <- coefs_all %>% 
      select(ends_with(aux[13]))
    neg_rt_v <- neg_rt_v[1,]
    
    neg_no_rt_v <- coefs_all %>% 
      select(ends_with(aux[14]))
    neg_no_rt_v <- neg_no_rt_v[1,]
    
    n_posts_rt_v <- coefs_all %>% 
      select(ends_with(aux[15]))
    n_posts_rt_v <- n_posts_rt_v[1,]
    
    n_posts_no_rt_v <- coefs_all %>% 
      select(ends_with(aux[16]))
    n_posts_no_rt_v <- n_posts_no_rt_v[1,]
    
    coefs_perm <- data.frame(pos_rt_c, pos_no_rt_c, neu_rt_c, neu_no_rt_c, neg_rt_c, 
                             neg_no_rt_c, n_posts_rt_c, n_posts_no_rt_c, pos_rt_v, 
                             pos_no_rt_v, neu_rt_v, neu_no_rt_v, neg_rt_v, 
                             neg_no_rt_v, n_posts_rt_v, n_posts_no_rt_v)
    
    write_xlsx(
      coefs_perm, paste0("../../../../data/04-analysis/",country,
                         "/",stage,"/original/", data_type, '/', type, 
                         "ext_nbase0.xlsx"))
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
        fmla1 <- as.formula(paste0(au, "~ total_treated + ",
                                   au, "_base | block1_fe"))
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
      pos_rt_c <- coefs_all %>% 
        select(ends_with(aux[1]))
      pos_rt_c <- pos_rt_c[1,]
      
      pos_no_rt_c <- coefs_all %>% 
        select(ends_with(aux[2]))
      pos_no_rt_c <- pos_no_rt_c[1,]
      
      neu_rt_c <- coefs_all %>% 
        select(ends_with(aux[3]))
      neu_rt_c <- neu_rt_c[1,]
      
      neu_no_rt_c <- coefs_all %>% 
        select(ends_with(aux[4]))
      neu_no_rt_c <- neu_no_rt_c[1,]
      
      neg_rt_c <- coefs_all %>% 
        select(ends_with(aux[5]))
      neg_rt_c <- neg_rt_c[1,]
      
      neg_no_rt_c <- coefs_all %>% 
        select(ends_with(aux[6]))
      neg_no_rt_c <- neg_no_rt_c[1,]
      
      n_posts_rt_c <- coefs_all %>% 
        select(ends_with(aux[7]))
      n_posts_rt_c <- n_posts_rt_c[1,]
      
      n_posts_no_rt_c <- coefs_all %>% 
        select(ends_with(aux[8]))
      n_posts_no_rt_c <- n_posts_no_rt_c[1,]
      
      pos_rt_v <- coefs_all %>% 
        select(ends_with(aux[9]))
      pos_rt_v <- pos_rt_v[1,]
      
      pos_no_rt_v <- coefs_all %>% 
        select(ends_with(aux[10]))
      pos_no_rt_v <- pos_no_rt_v[1,]
      
      neu_rt_v <- coefs_all %>% 
        select(ends_with(aux[11]))
      neu_rt_v <- neu_rt_v[1,]
      
      neu_no_rt_v <- coefs_all %>% 
        select(ends_with(aux[12]))
      neu_no_rt_v <- neu_no_rt_v[1,]
      
      neg_rt_v <- coefs_all %>% 
        select(ends_with(aux[13]))
      neg_rt_v <- neg_rt_v[1,]
      
      neg_no_rt_v <- coefs_all %>% 
        select(ends_with(aux[14]))
      neg_no_rt_v <- neg_no_rt_v[1,]
      
      n_posts_rt_v <- coefs_all %>% 
        select(ends_with(aux[15]))
      n_posts_rt_v <- n_posts_rt_v[1,]
      
      n_posts_no_rt_v <- coefs_all %>% 
        select(ends_with(aux[16]))
      n_posts_no_rt_v <- n_posts_no_rt_v[1,]
      
      coefs_perm <- data.frame(pos_rt_c, pos_no_rt_c, neu_rt_c, neu_no_rt_c, neg_rt_c, 
                               neg_no_rt_c, n_posts_rt_c, n_posts_no_rt_c, pos_rt_v, 
                               pos_no_rt_v, neu_rt_v, neu_no_rt_v, neg_rt_v, 
                               neg_no_rt_v, n_posts_rt_v, n_posts_no_rt_v)
      
      coefs_fin <- rbind(coefs_fin, coefs_perm)
      
      
      i <- i + 1}
    print(type)
    write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", stage, 
                                 "/permutations/", data_type, '/', type, 
                                 "ext_nbase0.xlsx"))
  }
  print(stage)
}

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
    mutate(Variable = case_when(var == 'pos_rt_c' ~ paste0(addon, 'Positive COVID Shares'),
                                var == "pos_no_rt_c" ~ paste0(addon, 
                                                              'Positive COVID Posts'),
                                var == "neu_rt_c" ~ paste0(addon, 
                                                           'Neutral COVID Shares'),
                                var == "neu_no_rt_c" ~ paste0(addon, 
                                                              'Neutral COVID Posts'),
                                var == "neg_rt_c" ~ paste0(addon, 
                                                           'Negative COVID Shares'),
                                var == "neg_no_rt_c" ~ paste0(addon, 
                                                              'Negative COVID Posts'),
                                var == "n_posts_rt_c" ~ paste0(addon, 
                                                               'Number of COVID Shares'),
                                var == "n_posts_no_rt_c" ~ paste0(addon, 
                                                                  'Number of COVID Posts'), 
                                var == "pos_rt_v" ~ paste0(addon, 'Positive COVID Vaccine Shares'),
                                var == "pos_no_rt_v" ~ paste0(addon, 
                                                              'Positive COVID Vaccine Posts'),
                                var == "neu_rt_v" ~ paste0(addon, 
                                                           'Neutral COVID Vaccine Shares'),
                                var == "neu_no_rt_v" ~ paste0(addon, 
                                                              'Neutral COVID Vaccine Posts'),
                                var == "neg_rt_v" ~ paste0(addon, 
                                                           'Negative COVID Vaccine Shares'),
                                var == "neg_no_rt_v" ~ paste0(addon, 
                                                              'Negative COVID Vaccine Posts'),
                                var == "n_posts_rt_v" ~ paste0(addon, 
                                                               'Number of COVID Vaccine Shares'),
                                var == "n_posts_no_rt_v" ~ paste0(addon, 
                                                                  'Number of COVID Vaccine Posts')),
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'),
           RT = ifelse(grepl('Shares', Variable, fixed = T) == T, 1, 0))
  
  final_rt <- final |> filter(RT == 1)
  
  final_rt$Variable <- factor(final_rt$Variable, 
                              levels = c(paste0(addon, 'Number of COVID Shares'),
                                         paste0(addon, 'Number of COVID Vaccine Shares'), 
                                         paste0(addon, 'Positive COVID Shares'), 
                                         paste0(addon, 'Positive COVID Vaccine Shares'), 
                                         paste0(addon, 'Neutral COVID Shares'), 
                                         paste0(addon, 'Neutral COVID Vaccine Shares'), 
                                         paste0(addon, 'Negative COVID Shares'), 
                                         paste0(addon, 'Negative COVID Vaccine Shares')))
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
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
                           data_type, '/', type, file_code, '_RT.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
  
  final_rt <- final |> filter(RT == 0)
  
  final_rt$Variable <- factor(final_rt$Variable, 
                              levels = c(paste0(addon, 'Number of COVID Posts'),
                                         paste0(addon, 'Number of COVID Vaccine Posts'), 
                                         paste0(addon, 'Positive COVID Posts'), 
                                         paste0(addon, 'Positive COVID Vaccine Posts'), 
                                         paste0(addon, 'Neutral COVID Posts'), 
                                         paste0(addon, 'Neutral COVID Vaccine Posts'), 
                                         paste0(addon, 'Negative COVID Posts'), 
                                         paste0(addon, 'Negative COVID Vaccine Posts')))
  
  results_plot <- ggplot(data = final_rt, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 2.5, 
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
                           data_type, '/', type, file_code, '_post.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot
