# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
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
list_types <- list('log_')
file_code <- 'intensive_interactions_linear9'
ini <- '../../../../data/04-analysis/joint/'

fes <- read_parquet('../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet')

stage <- 'stage3_4'
f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |>
  filter(n_posts_base>0) |> select(follower_id, pais, batch_id, total_treated, 
                                   total_influencers) |> filter(total_influencers < 10)
f9 <- f |> filter(total_influencers == 9)

generate_interactions <- function(ints){
  
  ints <- ints |> select(follower_id, pais, batch_id, total_treated, 
                         total_influencers)
  
  ints <- dummy_cols(ints, select_columns = "total_treated", 
                     remove_first_dummy = FALSE)
  
  ints <- dummy_cols(ints, select_columns = "total_influencers", 
                     remove_first_dummy = FALSE)
  
  ints <- ints |>  
    mutate(across(starts_with('total_influencers_'), 
                  ~.x - mean(.x)))
  
  interactions_terms <- ints |> select(follower_id, pais, batch_id)
  
  treated_columns <- grep("^total_treated_", colnames(ints), value = TRUE)
  
  total_columns <- grep("^total_influencers_", colnames(ints), value = TRUE)
  
  treated_columns <- treated_columns[treated_columns != "total_treated_0"]
  
  total_columns <- total_columns[total_columns != "total_influencers_1"]
  
  count_i <- 1
  for (i in treated_columns) {
    count_j <- 2
    for (j in total_columns) {
      interaction_name <- paste0('tao_', count_i, '_', count_j)
      interactions_terms[[interaction_name]] <- ints[[i]] * ints[[j]]
      
      count_j <- count_j + 1
    }
    count_i <- count_i + 1
  }
  
  return(interactions_terms)
  
}

interactions <- generate_interactions(f)

interactions <- interactions |> left_join(fes, 
                                          by = c('follower_id', 'pais', 'batch_id'))

rm(fes)

int_cols <- paste(grep("^tao_", colnames(interactions), value = TRUE),
                  collapse = ' + ')


for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../') |>
    filter(n_posts_base>0) |> filter(total_influencers < 10)
  
  df <- dummy_cols(df, select_columns = "total_treated", 
                   remove_first_dummy = T)
  
  treat_cols <- paste(grep("^total_treated_", colnames(df), value = TRUE),
                      collapse = ' + ')
  
  f <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |> 
    select(follower_id, pais, batch_id, eng, eng_base, 
           log_eng, log_eng_base)
  
  df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))  |> 
    left_join(interactions, by = c('follower_id', 'pais', 'batch_id'))
  
  for (type in list_types){
    
    aux <- paste0(type, aux_t)
    
    # 4.0 Run original estimates
    aux_data <- df[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ ", treat_cols, " + ",
                                 x, "_base + ", int_fes, ' + ', int_cols, 
                                 "| total_influencers"))
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
      reduce(left_join, by = "treatment") |> 
      filter(str_detect(treatment, 'total_treated_') == T)
    
    # # Build matrix
    # ver <- coefs_all %>% 
    #   select(ends_with(aux[1]))
    # ver <- ver[1,]
    # 
    # non_ver <- coefs_all %>% 
    #   select(ends_with(aux[2]))
    # non_ver <- non_ver[1,]
    # 
    # true <- coefs_all %>% 
    #   select(ends_with(aux[3]))
    # true <- true[1,]
    # 
    # fake <- coefs_all %>% 
    #   select(ends_with(aux[4]))
    # fake <- fake[1,]
    # 
    # n_posts <- coefs_all %>% 
    #   select(ends_with(aux[5]))
    # n_posts <- n_posts[1,]
    # 
    # coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts)
    
    write_xlsx(
      coefs_all, paste0("../../../../data/04-analysis/",country,
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
      
      data <- data |> select(-c(starts_with('tao_')))
      
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
      
      interactions_perms <- generate_interactions(data)
      
      int_cols <- paste(grep("^tao_", colnames(interactions_perms), value = TRUE),
                        collapse = ' + ')
      
      data <- data |> left_join(interactions_perms, 
                                by = c('follower_id', 'pais', 'batch_id'))
      
      data <- dummy_cols(data, select_columns = "total_treated", 
                         remove_first_dummy = T)
      
      treat_cols <- paste(grep("^total_treated_", colnames(data), value = TRUE),
                          collapse = ' + ')
      # Balance tables 
      aux_data <- data[aux]
      coefs_list <- list()
      lm_list_ols <- list()
      count <- 1
      for (au in aux) {
        fmla1 <- as.formula(paste0(au, "~ ", treat_cols, " + ",
                                   au, "_base +", int_fes, ' + ', int_cols,
                                   "| total_influencers"))
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
      coefs_all <- coefs_list %>% reduce(left_join, by = "treatment") |> 
        filter(str_detect(treatment, 'total_treated_') == T)
      
      # # Build matrix
      # ver <- coefs_all %>% 
      #   select(ends_with(aux[1]))
      # ver <- ver[1,]
      # 
      # non_ver <- coefs_all %>% 
      #   select(ends_with(aux[2]))
      # non_ver <- non_ver[1,]
      # 
      # true <- coefs_all %>% 
      #   select(ends_with(aux[3]))
      # true <- true[1,]
      # 
      # fake <- coefs_all %>% 
      #   select(ends_with(aux[4]))
      # fake <- fake[1,]
      # 
      # n_posts <- coefs_all %>% 
      #   select(ends_with(aux[5]))
      # n_posts <- n_posts[1,]
      # 
      # coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts)
      
      coefs_fin <- rbind(coefs_fin, coefs_all)
      
      write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", stage, 
                                   "/permutations/", data_type, '/', type, file_code,
                                   ".xlsx"))
      
      i <- i + 1}
    print(type)
    # write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", stage, 
    #                              "/permutations/", data_type, '/', type, file_code,
    #                              ".xlsx"))
  }
  print(stage)
}

# 6.0 Make the graphs


for (type in list_types){
  file_coefs <- paste0(type, file_code)
  coefs <- c('stage5_6', 'stage3_4', 'stage1_2') |> 
    map_dfr(~proc_coefs_linear(.x, file_coefs, type))
  ses <- c('stage5_6', 'stage3_4', 'stage1_2') |> 
    map_dfr(~proc_ses_linear(.x, file_coefs, type))
  
  if (type == 'log_'){
    addon <- 'log '
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
  }else {
    addon <- ''
  }
  
  final <- coefs |> left_join(ses, by = c('stage', 'var', 'treatment')) 
  
  vector <- unique(final$var)
  
  final <- final |> 
    mutate(Variable = case_when(var == paste0(type, 'verifiability') ~ paste0(addon, 'Verifiable Posts + Shares'),
                                var == paste0(type, 'non_ver') ~ paste0(addon, 
                                                                        'Non Verifiable Posts + Shares'),
                                var == paste0(type,'true') ~ paste0(addon, 'True Posts + Shares'),
                                var == paste0(type,'fake') ~ paste0(addon, 'Fake Posts + Shares'),
                                var == paste0(type,'eng') ~ paste0(addon, 'Number of Posts + Shares (English)')),
           Dummy = case_when(treatment == 'total_treated_1' ~ '1 Treated Inf.',
                             treatment == 'total_treated_2' ~ '2 Treated Inf.',
                             treatment == 'total_treated_3' ~ '3 Treated Inf.',
                             treatment == 'total_treated_4' ~ '4 Treated Inf.',
                             treatment == 'total_treated_5' ~ '5 Treated Inf.',
                             treatment == 'total_treated_6' ~ '6 Treated Inf.',
                             treatment == 'total_treated_7' ~ '7 Treated Inf.',
                             treatment == 'total_treated_8' ~ '8 Treated Inf.',
                             treatment == 'total_treated_9' ~ '9 Treated Inf.'), 
           Stage = case_when(stage == 'stage1_2' ~ 'Weeks 1-4',
                             stage == 'stage3_4' ~ 'Weeks 5-8',
                             stage == 'stage5_6' ~ 'Weeks 9-12'))
  
  for (i in vector){
    final_graph <- final |> filter(var == i)
    
    final_graph$Dummy <- factor(final_graph$Dummy, levels = c('1 Treated Inf.',
                                                              '2 Treated Inf.',
                                                              '3 Treated Inf.',
                                                              '4 Treated Inf.',
                                                              '5 Treated Inf.',
                                                              '6 Treated Inf.',
                                                              '7 Treated Inf.',
                                                              '8 Treated Inf.',
                                                              '9 Treated Inf.'))
    writexl::write_xlsx(final_graph, paste0(ini, 'EstimatesFinal/',type, i,
                                            '_intensive_interactions_linear9.xlsx'))
    results_plot <- ggplot(data = final_graph, aes(x = factor(Stage), y = coef)) + 
      geom_point(aes(shape = factor(Dummy), color = factor(Dummy)), size = 3, 
                 position = position_dodge(width = 0.5)) +
      geom_linerange(aes(ymin = coef - 1.96 * sd, ymax = coef + 1.96 * sd, 
                         color = factor(Dummy)),
                     position = position_dodge(width = 0.5), size = 1) +
      scale_shape_manual(values = c(15, 16, 17, 4, 7, 1, 3, 10, 11, 13), name = 'Dummy') +
      scale_color_manual(values = rep('black', 10), name = 'Dummy') +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
      theme_bw() +  
      ylab("Total Treated Estimate with 95% Confidence Interval") + 
      xlab("Stage") +  # Change title color
      #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
      theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(results_plot, 
           filename = paste0('../../../../results/01-regression_graphs/',
                             data_type, '/', type, file_code, '_', i, '.pdf'), 
           device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
  }
  
}

results_plot
