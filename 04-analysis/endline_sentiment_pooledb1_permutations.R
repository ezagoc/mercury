#############################################################################
# RCT: Endline on followers KE & SA: 
# SMIs Treatments Permutations
# Date: Thursday June 22nd, 2023
#############################################################################
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
country <- 'KE'
stage <- 'stage5_6'
i <- 3

data <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/endline_sent_b1.parquet'))

# Define the dependent variables
aux <- c('pos_v_rt_covid', 'pos_v_no_rt_covid', 'neutral_v_rt_covid', 
         'neutral_v_no_rt_covid', 'neg_v_rt_covid', 'neg_v_no_rt_covid',
         'n_posts_rt_covid', 'n_posts_no_rt_covid',
         'pos_v_rt_vax', 'pos_v_no_rt_vax', 'neutral_v_rt_vax', 
         'neutral_v_no_rt_vax', 'neg_v_rt_vax', 'neg_v_no_rt_vax', 
         'n_posts_rt_vax', 'n_posts_no_rt_vax')

for (m in 1:2){
  followers <- read_parquet(paste0("../../data/04-analysis/",country,
                                   "/ties",i,".parquet"), 
                            as_tibble = TRUE)
  
  a <- 250*i + 1
  b <- 250*(i+1)
  coefs_list <- list()
  for (x in a:b) {
    print(x)
    
    c1 = paste0("n_influencers_followed_control_no_weak_tie_p", x)
    c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", x)
    c3 = paste0("n_influencers_followed_treatment_weak_tie_p", x)
    c4 = paste0("n_influencers_followed_control_weak_tie_p", x)
    c5 = paste0("n_influencers_followed_control_strong_tie_p", x)
    c6 = paste0("n_influencers_followed_treatment_strong_tie_p", x)
    c7 = paste0("n_influencers_followed_control_no_strong_tie_p", x)
    c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", x)
    c9 = paste0("n_influencers_followed_control_p", x)
    c10 = paste0("n_influencers_followed_treatment_p", x)
    c11 = paste0("n_influencers_followed_p_", x)
    
    followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                           c5, c6, c7, c8, c9, c10, c11) 
    
    data <- left_join(
      data, 
      followers_iter,
      by = 'follower_id'
    )
    
    # Pool treatment variables
    data <- poolTreatmentBalance1(data, c5, c6, c4, c3, c10, c11)
    
    # Balance tables 
    aux_data <- data[aux]
    lm_list_ols <- list()
    count <- 1
    for (au in aux) {
      fmla1 <- as.formula(paste0(au, "~ t_strong + t_weak + t_neither + ",
                                 au, "_base  | c_t_strong_total + c_t_weak_total + 
                                 c_t_neither_total"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = data))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0('p', x, '_' , au)
      coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
        filter(treatment != paste0(x, '_base'))
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_list <- append(coefs_list, lm_list_ols)
  }
  coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
  
  # Build matrix
  pos_rt_c <- coefs_all %>% 
    select(ends_with(aux[1]))
  strong_pos_rt_c <- pos_rt_c[1,] %>% flatten_chr()
  weak_pos_rt_c <- pos_rt_c[2,] %>% flatten_chr()
  neither_pos_rt_c <- pos_rt_c[3,] %>% flatten_chr()
  
  pos_no_rt_c <- coefs_all %>% 
    select(ends_with(aux[2]))
  strong_pos_no_rt_c <- pos_no_rt_c[1,] %>% flatten_chr()
  weak_pos_no_rt_c <- pos_no_rt_c[2,] %>% flatten_chr()
  neither_pos_no_rt_c <- pos_no_rt_c[3,] %>% flatten_chr()
  
  neu_rt_c <- coefs_all %>% 
    select(ends_with(aux[3]))
  strong_neu_rt_c <- neu_rt_c[1,] %>% flatten_chr()
  weak_neu_rt_c <- neu_rt_c[2,] %>% flatten_chr()
  neither_neu_rt_c <- neu_rt_c[3,] %>% flatten_chr()
  
  neu_no_rt_c <- coefs_all %>% 
    select(ends_with(aux[4]))
  strong_neu_no_rt_c <- neu_no_rt_c[1,] %>% flatten_chr()
  weak_neu_no_rt_c <- neu_no_rt_c[2,] %>% flatten_chr()
  neither_neu_no_rt_c <- neu_no_rt_c[3,] %>% flatten_chr()
  
  neg_rt_c <- coefs_all %>% 
    select(ends_with(aux[5]))
  strong_neg_rt_c <- neg_rt_c[1,] %>% flatten_chr()
  weak_neg_rt_c <- neg_rt_c[2,] %>% flatten_chr()
  neither_neg_rt_c <- neg_rt_c[3,] %>% flatten_chr()
  
  neg_no_rt_c <- coefs_all %>% 
    select(ends_with(aux[6]))
  strong_neg_no_rt_c <- neg_no_rt_c[1,] %>% flatten_chr()
  weak_neg_no_rt_c <- neg_no_rt_c[2,] %>% flatten_chr()
  neither_neg_no_rt_c <- neg_no_rt_c[3,] %>% flatten_chr()
  
  n_posts_rt_c <- coefs_all %>% 
    select(ends_with(aux[7]))
  strong_n_posts_rt_c <- n_posts_rt_c[1,] %>% flatten_chr()
  weak_n_posts_rt_c <- n_posts_rt_c[2,] %>% flatten_chr()
  neither_n_posts_rt_c <- n_posts_rt_c[3,] %>% flatten_chr()
  
  n_posts_no_rt_c <- coefs_all %>% 
    select(ends_with(aux[8]))
  strong_n_posts_no_rt_c <- n_posts_no_rt_c[1,] %>% flatten_chr()
  weak_n_posts_no_rt_c <- n_posts_no_rt_c[2,] %>% flatten_chr()
  neither_n_posts_no_rt_c <- n_posts_no_rt_c[3,] %>% flatten_chr()
  
  pos_rt_v <- coefs_all %>% 
    select(ends_with(aux[9]))
  strong_pos_rt_v <- pos_rt_v[1,] %>% flatten_chr()
  weak_pos_rt_v <- pos_rt_v[2,] %>% flatten_chr()
  neither_pos_rt_v <- pos_rt_v[3,] %>% flatten_chr()
  
  pos_no_rt_v <- coefs_all %>% 
    select(ends_with(aux[10]))
  strong_pos_no_rt_v <- pos_no_rt_v[1,] %>% flatten_chr()
  weak_pos_no_rt_v <- pos_no_rt_v[2,] %>% flatten_chr()
  neither_pos_no_rt_v <- pos_no_rt_v[3,] %>% flatten_chr()
  
  neu_rt_v <- coefs_all %>% 
    select(ends_with(aux[11]))
  strong_neu_rt_v <- neu_rt_v[1,] %>% flatten_chr()
  weak_neu_rt_v <- neu_rt_v[2,] %>% flatten_chr()
  neither_neu_rt_v <- neu_rt_v[3,] %>% flatten_chr()
  
  neu_no_rt_v <- coefs_all %>% 
    select(ends_with(aux[12]))
  strong_neu_no_rt_v <- neu_no_rt_v[1,] %>% flatten_chr()
  weak_neu_no_rt_v <- neu_no_rt_v[2,] %>% flatten_chr()
  neither_neu_no_rt_v <- neu_no_rt_v[3,] %>% flatten_chr()
  
  neg_rt_v <- coefs_all %>% 
    select(ends_with(aux[13]))
  strong_neg_rt_v <- neg_rt_v[1,] %>% flatten_chr()
  weak_neg_rt_v <- neg_rt_v[2,] %>% flatten_chr()
  neither_neg_rt_v <- neg_rt_v[3,] %>% flatten_chr()
  
  neg_no_rt_v <- coefs_all %>% 
    select(ends_with(aux[14]))
  strong_neg_no_rt_v <- neg_no_rt_v[1,] %>% flatten_chr()
  weak_neg_no_rt_v <- neg_no_rt_v[2,] %>% flatten_chr()
  neither_neg_no_rt_v <- neg_no_rt_v[3,] %>% flatten_chr()
  
  n_posts_rt_v <- coefs_all %>% 
    select(ends_with(aux[15]))
  strong_n_posts_rt_v <- n_posts_rt_v[1,] %>% flatten_chr()
  weak_n_posts_rt_v <- n_posts_rt_v[2,] %>% flatten_chr()
  neither_n_posts_rt_v <- n_posts_rt_v[3,] %>% flatten_chr()
  
  n_posts_no_rt_v <- coefs_all %>% 
    select(ends_with(aux[16]))
  strong_n_posts_no_rt_v <- n_posts_no_rt_v[1,] %>% flatten_chr()
  weak_n_posts_no_rt_v <- n_posts_no_rt_v[2,] %>% flatten_chr()
  neither_n_posts_no_rt_v <- n_posts_no_rt_v[3,] %>% flatten_chr()
  
  
  coefs_perm <- data.frame(strong_pos_rt_c, weak_pos_rt_c, neither_pos_rt_c,
                           strong_pos_no_rt_c, weak_pos_no_rt_c, neither_pos_no_rt_c,
                           strong_neu_rt_c, weak_neu_rt_c, neither_neu_rt_c,
                           strong_neu_no_rt_c, weak_neu_no_rt_c, neither_neu_no_rt_c,
                           strong_neg_rt_c, weak_neg_rt_c, neither_neg_rt_c,
                           strong_neg_no_rt_c, weak_neg_no_rt_c, neither_neg_no_rt_c,
                           strong_n_posts_rt_c, weak_n_posts_rt_c, neither_n_posts_rt_c,
                           strong_n_posts_no_rt_c, weak_n_posts_no_rt_c, neither_n_posts_no_rt_c,
                           strong_pos_rt_v, weak_pos_rt_v, neither_pos_rt_v,
                           strong_pos_no_rt_v, weak_pos_no_rt_v, neither_pos_no_rt_v,
                           strong_neu_rt_v, weak_neu_rt_v, neither_neu_rt_v,
                           strong_neu_no_rt_v, weak_neu_no_rt_v, neither_neu_no_rt_v,
                           strong_neg_rt_v, weak_neg_rt_v, neither_neg_rt_v,
                           strong_neg_no_rt_v, weak_neg_no_rt_v, neither_neg_no_rt_v,
                           strong_n_posts_rt_v, weak_n_posts_rt_v, neither_n_posts_rt_v,
                           strong_n_posts_no_rt_v, weak_n_posts_no_rt_v, neither_n_posts_no_rt_v)
  
  
  write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                                stage, "/pestimates_sentiment_b1_", i,".xlsx"))
  followers <- tibble()
  i <- i + 1
}
