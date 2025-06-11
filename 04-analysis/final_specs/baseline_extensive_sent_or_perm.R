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

blocks_ke <- read_parquet(paste0('../../../data/04-analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../../data/04-analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

aux_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', 'stage3_4', 
                              '/final_data_b1b2p.parquet')) |> select(follower_id, 
                                                                      batch_id, n_posts_base)

aux_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage3_4', 
                              '/final_data_b1b2p.parquet')) |> select(follower_id, 
                                                                      batch_id, n_posts_base)

for (stage in list('baseline')){
  for (type in list('', 'log_', 'arc_')){ 
    
    aux <- c('pos_v_rt_covid', 'pos_v_no_rt_covid', 'neutral_v_rt_covid', 
             'neutral_v_no_rt_covid', 'neg_v_rt_covid', 'neg_v_no_rt_covid',
             'n_posts_rt_covid', 'n_posts_no_rt_covid',
             'pos_v_rt_vax', 'pos_v_no_rt_vax', 'neutral_v_rt_vax', 
             'neutral_v_no_rt_vax', 'neg_v_rt_vax', 'neg_v_no_rt_vax', 
             'n_posts_rt_vax', 'n_posts_no_rt_vax')
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', 'stage3_4', 
                                 '/final_data_b1b2p_sent.parquet'))
    
    df_ke <- df_ke |> mutate(total_treated = t_strong + t_weak + t_neither,
                             total_influencers = c_t_strong_total + c_t_weak_total + 
                               c_t_neither_total) |> 
      filter(total_influencers == 1)
    
    df_ke <- df_ke |> left_join(blocks_ke, by = c('batch_id', 'follower_id', 'pais'))
    
    df_ke <- df_ke |> left_join(aux_ke, by = c('batch_id', 'follower_id')) |> 
      filter(n_posts_base>0)
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage3_4', 
                                 '/final_data_b1b2p_sent.parquet'))
    
    df_sa <- df_sa |> mutate(total_treated = t_strong + t_weak + t_neither,
                             total_influencers = c_t_strong_total + c_t_weak_total + 
                               c_t_neither_total) |> 
      filter(total_influencers == 1)
    
    df_sa <- df_sa |> left_join(blocks_sa, by = c('batch_id', 'follower_id', 'pais'))
    
    df_sa <- df_sa |> left_join(aux_sa, by = c('batch_id', 'follower_id')) |> 
      filter(n_posts_base>0)
    
    df <- rbind(df_ke, df_sa)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           pos_v_rt_base:n_posts_no_rt_vax) |> 
      mutate(across(c(pos_v_rt_base:n_posts_no_rt_vax), ~log(.x + 1)))
    
    colnames(df_log)[4:length(df_log)] <- paste0('log_', 
                                                 colnames(df_log)[4:length(df_log)])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              pos_v_rt_base:n_posts_no_rt_vax) |> 
      mutate(across(c(pos_v_rt_base:n_posts_no_rt_vax), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[4:length(df_arcsin)] <- paste0('arc_', 
                                                       colnames(df_arcsin)[4:length(df_arcsin)])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    aux <- paste0(type, aux, '_base')
    
    aux_data <- df[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated  | block1_fe"))
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
      coefs_perm, paste0("../../../data/04-analysis/", country,
                         "/", stage,"/", type,
                         "pestimates_sent_extensive_b1b2p.xlsx"))
    
    coefs_final <- tibble()
    i <- 1
    for (m in 1:1000){
      a <- i
      
      followers <- read_parquet(paste0("../../../data/04-analysis/",country, "/",
                                       'small_ties_b1b2p', "/small_tie", 
                                       a,".parquet"))
      
      c1 = paste0("n_influencers_followed_control_no_weak_tie_p", a)
      c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", a)
      c3 = paste0("n_influencers_followed_treatment_weak_tie_p", a)
      c4 = paste0("n_influencers_followed_control_weak_tie_p", a)
      c5 = paste0("n_influencers_followed_control_strong_tie_p", a)
      c6 = paste0("n_influencers_followed_treatment_strong_tie_p", a)
      c7 = paste0("n_influencers_followed_control_no_strong_tie_p", a)
      c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", a)
      c9 = paste0("n_influencers_followed_control_p", a)
      c10 = paste0("n_influencers_followed_treatment_p", a)
      c11 = paste0("n_influencers_followed_p_", a)
      
      followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                             c5, c6, c7, c8, c9, c10, c11, batch_id, 
                                             pais)
      
      data <- df
      
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
        fmla1 <- as.formula(paste0(au, "~ total_treated  | block1_fe"))
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
      
      coefs_final <- rbind(coefs_final, coefs_perm)
      
      
      print(i)
      i <- i + 1
      
    } 
    write_xlsx(coefs_final, paste0("../../../data/04-analysis/",country, "/",
                                   stage, "/pestimates/", type, 
                                   "sent_extensive_b1b2p.xlsx"))
    
  }}