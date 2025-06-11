rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# Define round and set of dependent variables:
country <- 'joint'
stage <- 'stage5_6'
i <- 1

# Define the dependent variables
aux <- c('pos_v_rt_covid', 'pos_v_no_rt_covid', 'neutral_v_rt_covid', 
         'neutral_v_no_rt_covid', 'neg_v_rt_covid', 'neg_v_no_rt_covid',
         'n_posts_rt_covid', 'n_posts_no_rt_covid',
         'pos_v_rt_vax', 'pos_v_no_rt_vax', 'neutral_v_rt_vax', 
         'neutral_v_no_rt_vax', 'neg_v_rt_vax', 'neg_v_no_rt_vax', 
         'n_posts_rt_vax', 'n_posts_no_rt_vax')

for (m in 1:1000){
a <- i

followers <- read_parquet(paste0("../../data/04-analysis/",country, "/",
                                 'small_ties', "/small_tie", 
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
                                           c5, c6, c7, c8, c9, c10, c11)
    
    data <- rbind(read_parquet(paste0('../../data/04-analysis/KE/', stage,
                                      '/endline_sent_b1.parquet')), 
                  read_parquet(paste0('../../data/04-analysis/SA/', stage,
                                      '/endline_sent_b1.parquet')))
    
    data <- left_join(
      data, 
      followers_iter,
      by = 'follower_id'
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
  
  
  write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                                stage, "/pestimates/all_country_", 
                                a,".xlsx"))
  
  i <- i + 1
  
}
