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

for (stage in list('stage1_2', 'stage3_4', 'stage5_6')){
  for (type in list('', 'log_', 'arc_')){ 
    
    aux <- c('pos_b_rt_covid', 'pos_b_no_rt_covid', 'neutral_b_rt_covid', 
             'neutral_b_no_rt_covid', 'neg_b_rt_covid', 'neg_b_no_rt_covid',
             'n_posts_rt_covid', 'n_posts_no_rt_covid',
             'pos_b_rt_vax', 'pos_b_no_rt_vax', 'neutral_b_rt_vax', 
             'neutral_b_no_rt_vax', 'neg_b_rt_vax', 'neg_b_no_rt_vax', 
             'n_posts_rt_vax', 'n_posts_no_rt_vax')
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage, 
                                 '/endline_sent_bert_b1.parquet')) |> 
      mutate(pais='KE')
    
    bots_ke <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
      rename(follower_id = author_id)
    
    df_ke <- df_ke |> 
      left_join(bots_ke, by = 'follower_id') |> 
      filter(dummy_95 == 0) |>
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage, 
                                 '/endline_sent_bert_b1.parquet')) |>
      mutate(pais='SA')
    
    bots_sa <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
      rename(follower_id = author_id) |> select(-c("__index_level_0__"))
    
    df_sa <- df_sa |> 
      left_join(bots_sa, by = 'follower_id') |> 
      filter(dummy_95 == 0) |>
      mutate(pais = 'SA') 
    
    df <- rbind(df_ke, df_sa)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df_log <- df |> select(follower_id, pais, 
                           pos_b_rt_base:n_posts_no_rt_vax) |> 
      mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), ~log(.x + 1)))
    
    colnames(df_log)[3:length(df_log)] <- paste0('log_', 
                                                 colnames(df_log)[3:length(df_log)])
    
    df_arcsin <- df |> select(follower_id, pais, 
                              pos_b_rt_base:n_posts_no_rt_vax) |> 
      mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[3:length(df_arcsin)] <- paste0('arc_', 
                                                       colnames(df_arcsin)[3:length(df_arcsin)])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais'))
    
    aux <- paste0(type, aux)
    
    data <- df
      # Balance tables 
      aux_data <- data[aux]
      coefs_list <- list()
      lm_list_ols <- list()
      count <- 1
      for (au in aux) {
        fmla1 <- as.formula(paste0(au, "~ total_treated + ",
                                   au, "_base  | total_influencers"))
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
      
      
      write_xlsx(coefs_perm, paste0("../../../data/04-analysis/",country, "/",
                                    stage, "/", type, 
                                    "all_country_b1_sent_bert.xlsx"))
  }
  
}