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
stage <- 'stage1_2'
i <- 1

df_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                             '/endline_b1_b2_p.parquet'))

fake_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                               '/endline_fake_b1_b2_p.parquet'))

df_ke <- df_ke |> left_join(fake_ke, by = c('follower_id', 'batch_id')) |> 
  mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0)) |>
  mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, verifiability_rt),
         true_rt_f = ifelse(ver_dummy == 0, NA, true_rt),
         n_posts_rt_f = ifelse(ver_dummy == 0, NA, n_posts_rt),
         verifiability_no_rt_f = ifelse(ver_dummy == 0, NA, verifiability_no_rt),
         true_no_rt_f = ifelse(ver_dummy == 0, NA, true_no_rt),
         n_posts_no_rt_f = ifelse(ver_dummy == 0, NA, n_posts_no_rt),
         fake_rt_f = ifelse(ver_dummy == 0, NA, fake_rt),
         fake_no_rt_f = ifelse(ver_dummy == 0, NA, fake_no_rt),
         fake_rt_f_base = fake_rt_base,
         fake_no_rt_f_base = fake_no_rt_base,
         verifiability_no_rt_f_base = verifiability_no_rt_base,
         verifiability_rt_f_base = verifiability_rt_base, 
         true_rt_f_base = true_rt_base, 
         true_no_rt_f_base = true_no_rt_base,
         n_posts_rt_f_base = n_posts_rt_base,
         n_posts_no_rt_f_base = n_posts_no_rt_base,
         pais = 'KE') 

df_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                             '/endline_b1_b2_p.parquet'))

fake_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                               '/endline_fake_b1_b2_p.parquet'))

df_sa <- df_sa |> left_join(fake_sa, by = c('follower_id', 'batch_id')) |> 
  mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0)) |>
  mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, verifiability_rt),
         true_rt_f = ifelse(ver_dummy == 0, NA, true_rt),
         n_posts_rt_f = ifelse(ver_dummy == 0, NA, n_posts_rt),
         verifiability_no_rt_f = ifelse(ver_dummy == 0, NA, verifiability_no_rt),
         true_no_rt_f = ifelse(ver_dummy == 0, NA, true_no_rt),
         n_posts_no_rt_f = ifelse(ver_dummy == 0, NA, n_posts_no_rt),
         fake_rt_f = ifelse(ver_dummy == 0, NA, fake_rt),
         fake_no_rt_f = ifelse(ver_dummy == 0, NA, fake_no_rt),
         fake_rt_f_base = fake_rt_base,
         fake_no_rt_f_base = fake_no_rt_base,
         verifiability_no_rt_f_base = verifiability_no_rt_base,
         verifiability_rt_f_base = verifiability_rt_base, 
         true_rt_f_base = true_rt_base, 
         true_no_rt_f_base = true_no_rt_base,
         n_posts_rt_f_base = n_posts_rt_base,
         n_posts_no_rt_f_base = n_posts_no_rt_base,
         pais = 'SA') 

df <- rbind(df_ke, df_sa)

# Define the dependent variables
aux <- c('verifiability_rt', 'verifiability_rt_f', 'true_rt', 'true_rt_f', 
         'fake_rt', 'fake_rt_f',
         'n_posts_rt', 'n_posts_rt_f', 'verifiability_no_rt', 
         'verifiability_no_rt_f', 'true_no_rt', 'true_no_rt_f', 
         'fake_no_rt', 'fake_no_rt_f', 'n_posts_no_rt',
         'n_posts_no_rt_f')


# Dependent Variables:


i <- 1
for (m in 1:1000){
  print(i)
  followers <- read_parquet(paste0("../../data/04-analysis/",country, "/",
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
  
  followers_iter <- followers %>% select(follower_id, batch_id, c1, c2, c3, c4, 
                                         c5, c6, c7, c8, c9, c10, c11, pais) 
  
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
  ver_rt <- coefs_all %>% 
    select(ends_with(aux[1]))
  ver_rt <- ver_rt[1,] 
  
  ver_rt_f <- coefs_all %>% 
    select(ends_with(aux[2]))
  ver_rt_f <- ver_rt_f[1,] 
  
  true_rt <- coefs_all %>% 
    select(ends_with(aux[3]))
  true_rt <- true_rt[1,] 
  
  true_rt_f <- coefs_all %>% 
    select(ends_with(aux[4]))
  true_rt_f <- true_rt_f[1,] 
  
  fake_rt <- coefs_all %>% 
    select(ends_with(aux[5]))
  fake_rt <- fake_rt[1,] 
  
  fake_rt_f <- coefs_all %>% 
    select(ends_with(aux[6]))
  fake_rt_f <- fake_rt_f[1,] 
  
  n_posts_rt <- coefs_all %>% 
    select(ends_with(aux[7]))
  n_posts_rt <- n_posts_rt[1,] 
  
  n_posts_rt_f <- coefs_all %>% 
    select(ends_with(aux[8]))
  n_posts_rt_f <- n_posts_rt_f[1,] 
  
  ver_no_rt <- coefs_all %>% 
    select(ends_with(aux[9]))
  ver_no_rt <- ver_no_rt[1,] 
  
  ver_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[10]))
  ver_no_rt_f <- ver_no_rt_f[1,] 
  
  true_no_rt <- coefs_all %>% 
    select(ends_with(aux[11]))
  true_no_rt <- true_no_rt[1,] 
  
  true_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[12]))
  true_no_rt_f <- true_no_rt_f[1,] 
  
  fake_no_rt <- coefs_all %>% 
    select(ends_with(aux[13]))
  fake_no_rt <- fake_no_rt[1,] 
  
  fake_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[14]))
  fake_no_rt_f <- fake_no_rt_f[1,] 
  
  n_posts_no_rt <- coefs_all %>% 
    select(ends_with(aux[15]))
  n_posts_no_rt <- n_posts_no_rt[1,] 
  
  n_posts_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[16]))
  n_posts_no_rt_f <- n_posts_no_rt_f[1,] 
  
  coefs_perm <- data.frame(ver_rt, ver_rt_f, true_rt, 
                           true_rt_f, fake_rt, 
                           fake_rt_f, n_posts_rt, n_posts_rt_f, 
                           ver_no_rt, ver_no_rt_f, true_no_rt, 
                           true_no_rt_f, fake_no_rt, 
                           fake_no_rt_f, n_posts_no_rt, n_posts_no_rt_f)
  
  write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                                stage, "/pestimates_ver/b1_b2_p_fake_all", i,".xlsx"))
  i <- i + 1
}

