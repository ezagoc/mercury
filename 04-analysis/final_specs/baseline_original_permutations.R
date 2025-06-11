rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# Define round and set of dependent variables:
country <- 'joint'
# Define round and set of dependent variables:
# Define the dependent variables

blocks_ke <- read_parquet(paste0('../../../data/04-analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../../data/04-analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)


for (stage in list('baseline')){
  for (type in list('', 'log_', 'arc_')){
    
    aux <- c('verifiability_rt', 'true_rt', 'fake_rt', 'n_posts_rt', 
             'verifiability_no_rt', 'true_no_rt',
             'fake_no_rt',  'n_posts_no_rt')
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/stage1_2/final_data_b1b2p.parquet')) |> 
      mutate(pais = 'KE')
    
    df_ke <- df_ke |> mutate(total_treated = t_strong + t_weak + t_neither,
                             total_influencers = c_t_strong_total + c_t_weak_total + 
                               c_t_neither_total)
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage1_2', 
                                 '/final_data_b1b2p.parquet')) |> mutate(pais = 'SA')
    
    df_sa <- df_sa |> mutate(total_treated = t_strong + t_weak + t_neither,
                             total_influencers = c_t_strong_total + c_t_weak_total + 
                               c_t_neither_total)
    df <- rbind(df_sa, df_ke)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_shares_base:fake_no_rt) |> 
      mutate(across(c(total_shares_base:fake_no_rt), ~log(.x + 1)))
    
    colnames(df_log)[4:43] <- paste0('log_', colnames(df_log)[4:43])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_shares_base:fake_no_rt) |> 
      mutate(across(c(total_shares_base:fake_no_rt), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[4:43] <- paste0('arc_', colnames(df_arcsin)[4:43])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    aux <- paste0(type, aux, '_base')
    
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
    ver_rt <- coefs_all %>% 
      select(ends_with(aux[1]))
    ver_rt <- ver_rt[1,]
    
    true_rt <- coefs_all %>% 
      select(ends_with(aux[2]))
    true_rt <- true_rt[1,]
    
    fake_rt <- coefs_all %>% 
      select(ends_with(aux[3]))
    fake_rt <- fake_rt[1,]
    
    n_posts_rt <- coefs_all %>% 
      select(ends_with(aux[4]))
    n_posts_rt <- n_posts_rt[1,]
    
    ver_no_rt <- coefs_all %>% 
      select(ends_with(aux[5]))
    ver_no_rt <- ver_no_rt[1,]
    
    true_no_rt <- coefs_all %>% 
      select(ends_with(aux[6]))
    true_no_rt <- true_no_rt[1,]
    
    fake_no_rt <- coefs_all %>% 
      select(ends_with(aux[7]))
    fake_no_rt <- fake_no_rt[1,]
    
    n_posts_no_rt <- coefs_all %>% 
      select(ends_with(aux[8]))
    n_posts_no_rt <- n_posts_no_rt[1,]
    
    coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_posts_rt, ver_no_rt, 
                             true_no_rt, fake_no_rt,
                             n_posts_no_rt)
    
    write_xlsx(
      coefs_perm, paste0("../../../data/04-analysis/",country,
                         "/",stage,"/", type, "pestimates_b1b2p_all.xlsx"))
    
    coefs_final <- tibble()
    i <- 1
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
        fmla1 <- as.formula(paste0(au," ~ total_treated | total_influencers"))
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
      ver_rt <- coefs_all %>% 
        select(ends_with(aux[1]))
      ver_rt <- ver_rt[1,]
      
      true_rt <- coefs_all %>% 
        select(ends_with(aux[2]))
      true_rt <- true_rt[1,] 
      
      fake_rt <- coefs_all %>% 
        select(ends_with(aux[3]))
      fake_rt <- fake_rt[1,]
      
      n_posts_rt <- coefs_all %>% 
        select(ends_with(aux[4]))
      n_posts_rt <- n_posts_rt[1,]
      
      ver_no_rt <- coefs_all %>% 
        select(ends_with(aux[5]))
      ver_no_rt <- ver_no_rt[1,]
      
      true_no_rt <- coefs_all %>% 
        select(ends_with(aux[6]))
      true_no_rt <- true_no_rt[1,]
      
      fake_no_rt <- coefs_all %>% 
        select(ends_with(aux[7]))
      fake_no_rt <- fake_no_rt[1,]
      
      n_posts_no_rt <- coefs_all %>% 
        select(ends_with(aux[8]))
      n_posts_no_rt <- n_posts_no_rt[1,]
      
      coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_posts_rt, ver_no_rt,
                               true_no_rt, fake_no_rt, 
                               n_posts_no_rt)
      
      coefs_final <- rbind(coefs_final, coefs_perm)
      
      
      i <- i + 1}
    write_xlsx(coefs_final, paste0("../../../data/04-analysis/joint/", stage, 
                                  "/pestimates/", type, "b1b2p_all.xlsx"))
  }
}