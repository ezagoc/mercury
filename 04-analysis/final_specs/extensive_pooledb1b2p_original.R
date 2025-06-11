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

country <- 'joint'
# Define round and set of dependent variables:
# Define the dependent variables

blocks_ke <- read_parquet(paste0('../../../data/04-analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../../data/04-analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

for (stage in list('stage1_2')){
  for (type in list('', 'log_', 'arc_')){
    
    aux <- c('verifiability_rt', 'true_rt', 'fake_rt', 'n_posts_rt', 
             'verifiability_no_rt', 'true_no_rt',
             'fake_no_rt',  'n_posts_no_rt')
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage, 
                                 '/final_data_b1b2p.parquet')) |> mutate(pais = 'KE')
    
    df_ke <- df_ke |> mutate(total_treated = t_strong + t_weak + t_neither,
                                    total_influencers = c_t_strong_total + c_t_weak_total + 
                                      c_t_neither_total) |> 
      filter(total_influencers == 1)
    
    df_ke <- df_ke |> left_join(blocks_ke, by = c('batch_id', 'follower_id', 'pais'))
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage, 
                                 '/final_data_b1b2p.parquet')) |> mutate(pais = 'SA')
    
    df_sa <- df_sa |> mutate(total_treated = t_strong + t_weak + t_neither,
                             total_influencers = c_t_strong_total + c_t_weak_total + 
                               c_t_neither_total) |> 
      filter(total_influencers == 1)
    
    df_sa <- df_sa |> left_join(blocks_sa, by = c('batch_id', 'follower_id', 'pais'))
    
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
    
    aux <- paste0(type, aux)
    
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
                         "/",stage,"/", type, "pestimates_b1b2p_extensive.xlsx"))
  }
}
