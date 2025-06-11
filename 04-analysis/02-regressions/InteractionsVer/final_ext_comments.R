# 2.0 Define constants (DO NOT CHANGE)
country <- 'joint'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list( 'log_')
ini <- '../../../../data/04-analysis/joint/'

# 2.1 Change from code to code
data_type <- 'InteractionsVer'
file_code <- 'ext_comments'

##########################################

for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_int_ver_final_winsor(stage = stage, batches = 'b1b2',
                                          initial_path = '../../../../') |> 
    filter(total_influencers == 1)
  df <- df |> left_join(blocks, by = c('follower_id', 'batch_id', 'pais'))
  df <- df |> filter(n_posts_base>0)
  for (type in list_types){
    
    aux <- paste0(type, aux_int_comm)
    
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
    
    tot <- coefs_all %>% 
      select(ends_with(aux[5]))
    tot <- tot[1,]
    
    coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_ver_rt, tot)
    
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
        fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                   x, "_base | block1_fe"))
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
      
      tot <- coefs_all %>% 
        select(ends_with(aux[5]))
      tot <- tot[1,]
      
      coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_ver_rt, tot)
      
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

proc_coefs <- function(stage, file){
  coef <- readxl::read_excel(paste0(ini, stage, '/original/', data_type, '/',
                                    file, '.xlsx')) |>
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef') |> 
    mutate(stage = stage)
  return(coef)
}

proc_ses <- function(stage, file){
  
  perm <- readxl::read_excel(paste0(ini, stage, '/permutations/', data_type, '/',
                                    file, '.xlsx')) |> 
    summarise(across(everything(), ~sd(.x))) |> 
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd') |> 
    mutate(stage = stage)
  
  return(perm)
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
    mutate(Variable = case_when(var == 'ver_rt' ~ paste0(addon, 'Total Comments (Verifiable)'),
                                var == 'true_rt' ~ paste0(addon, 'Total Comments (True)'),
                                var == 'fake_rt' ~ paste0(addon, 'Total Comments (Fake)'),
                                var == 'tot' ~ paste0(addon, 'Total Comments (English)'),
                                var == 'n_ver_rt' ~ paste0(addon, 
                                                           'Total Comments (Non-Verifiable)')),
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'))
  
  final$Variable <- factor(final$Variable, levels = c(paste0(addon, 'Total Comments (English)'),
                                                      paste0(addon, 
                                                             'Total Comments (Non-Verifiable)'), 
                                                      paste0(addon, 'Total Comments (Verifiable)'), 
                                                      paste0(addon, 'Total Comments (True)'), 
                                                      paste0(addon, 'Total Comments (Fake)')))
  
  results_plot <- ggplot(data = final, aes(x = factor(Stage), y = coef)) + 
    geom_point(aes(shape = factor(Variable), color = factor(Variable)), size = 3, 
               position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                       color = factor(Variable)),
                   position = position_dodge(width = 0.5), size = 1) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7), name = 'Outcome') +
    scale_color_manual(values = rep('black', 5), name = 'Outcome') +
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
