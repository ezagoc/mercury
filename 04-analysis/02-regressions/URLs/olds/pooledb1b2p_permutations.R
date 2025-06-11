rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# Define round and set of dependent variables:
country <- 'joint'
# Define round and set of dependent variables:
# Define the dependent variables


for (stage in list('stage5_6')){
  
  for (type in list('')){
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage, 
                                 '/final_data_b1b2p_urls.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage, 
                                 '/final_data_b1b2p_urls.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> filter(n_posts_base > 0)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           fact_check_base:total_info) |> 
      mutate(across(c(fact_check_base:total_info), ~log(.x + 1)))
    
    colnames(df_log)[4:length(df_log)] <- paste0('log_', colnames(df_log)[4:length(df_log)])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              fact_check_base:total_info) |> 
      mutate(across(c(fact_check_base:total_info), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[4:length(df_arcsin)] <- paste0('arc_', 
                                                       colnames(df_arcsin)[4:length(df_arcsin)])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    aux <- c('total_urls', 'total_info', 'total_news', 'fact_check', 
             'rel_news', 'non_rel_news', 'other')
    
    aux <- paste0(type, aux)
    
    i <- 1
    coefs_fin <- tibble()
    for (m in 1:1000){
      print(i)
      followers <- read_parquet(paste0("../../../data/04-analysis/joint/",
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
                                             c5, c6, c7, c8, c9, c10, c11, pais, batch_id) 
      
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
                                   au, "_base | total_influencers"))
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
        select(ends_with(aux[5]))
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
    write_xlsx(coefs_fin, paste0("../../../data/04-analysis/joint/", stage, 
                                 "/pestimates_final/", type, "ver_b1b2p_urls.xlsx"))
  }
}