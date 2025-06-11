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

i <- 0

df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_b1.parquet'))

fake <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/endline_fake_b1.parquet'))

df <- df |> left_join(fake, by = c('follower_id', 'batch_id'))

df <- df |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

df <- df |> mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, 
                                               verifiability_rt),
                   true_rt_f = ifelse(ver_dummy == 0, NA, 
                                      true_rt),
                   n_posts_rt_f = ifelse(ver_dummy == 0, NA, 
                                         n_posts_rt),
                   verifiability_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                                  verifiability_no_rt),
                   true_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                         true_no_rt),
                   n_posts_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                            n_posts_no_rt),
                   fake_rt_f = ifelse(ver_dummy == 0, NA, 
                                      fake_rt),
                   fake_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                         fake_no_rt),
                   fake_rt_f_base = fake_rt_base,
                   fake_no_rt_f_base = fake_no_rt_base,
                   verifiability_no_rt_f_base = verifiability_no_rt_base,
                   verifiability_rt_f_base = verifiability_rt_base, 
                   true_rt_f_base = true_rt_base, 
                   true_no_rt_f_base = true_no_rt_base,
                   n_posts_rt_f_base = n_posts_rt_base,
                   n_posts_no_rt_f_base = n_posts_no_rt_base) 

# Define the dependent variables
aux <- c('verifiability_rt', 'verifiability_rt_f', 'true_rt', 'true_rt_f', 
         'fake_rt', 'fake_rt_f',
         'n_posts_rt', 'n_posts_rt_f', 'verifiability_no_rt', 
         'verifiability_no_rt_f', 'true_no_rt', 'true_no_rt_f', 
         'fake_no_rt', 'fake_no_rt_f', 'n_posts_no_rt',
         'n_posts_no_rt_f')


for (m in 1:4){
  
  followers <- read_parquet(paste0("../../data/04-analysis/",country,
                                   "/ties",i,".parquet"), 
                            as_tibble = TRUE)
  
  data <- df
  
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
      by = c('follower_id')
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
        filter(treatment != paste0(au, '_base'))
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_list <- append(coefs_list, lm_list_ols)
  }
  coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
  
  # Build matrix
  ver_rt <- coefs_all %>% 
    select(ends_with(aux[1]))
  strong_ver_rt <- ver_rt[1,] %>% flatten_chr()
  weak_ver_rt <- ver_rt[2,] %>% flatten_chr()
  neither_ver_rt <- ver_rt[3,] %>% flatten_chr()
  
  ver_rt_f <- coefs_all %>% 
    select(ends_with(aux[2]))
  strong_ver_rt_f <- ver_rt_f[1,] %>% flatten_chr()
  weak_ver_rt_f <- ver_rt_f[2,] %>% flatten_chr()
  neither_ver_rt_f <- ver_rt_f[3,] %>% flatten_chr()
  
  true_rt <- coefs_all %>% 
    select(ends_with(aux[3]))
  strong_true_rt <- true_rt[1,] %>% flatten_chr()
  weak_true_rt <- true_rt[2,] %>% flatten_chr()
  neither_true_rt <- true_rt[3,] %>% flatten_chr()
  
  true_rt_f <- coefs_all %>% 
    select(ends_with(aux[4]))
  strong_true_rt_f <- true_rt_f[1,] %>% flatten_chr()
  weak_true_rt_f <- true_rt_f[2,] %>% flatten_chr()
  neither_true_rt_f <- true_rt_f[3,] %>% flatten_chr()
  
  fake_rt <- coefs_all %>% 
    select(ends_with(aux[5]))
  strong_fake_rt <- fake_rt[1,] %>% flatten_chr()
  weak_fake_rt <- fake_rt[2,] %>% flatten_chr()
  neither_fake_rt <- fake_rt[3,] %>% flatten_chr()
  
  fake_rt_f <- coefs_all %>% 
    select(ends_with(aux[6]))
  strong_fake_rt_f <- fake_rt_f[1,] %>% flatten_chr()
  weak_fake_rt_f <- fake_rt_f[2,] %>% flatten_chr()
  neither_fake_rt_f <- fake_rt_f[3,] %>% flatten_chr()
  
  n_posts_rt <- coefs_all %>% 
    select(ends_with(aux[7]))
  strong_n_posts_rt <- n_posts_rt[1,] %>% flatten_chr()
  weak_n_posts_rt <- n_posts_rt[2,] %>% flatten_chr()
  neither_n_posts_rt <- n_posts_rt[3,] %>% flatten_chr()
  
  n_posts_rt_f <- coefs_all %>% 
    select(ends_with(aux[8]))
  strong_n_posts_rt_f <- n_posts_rt_f[1,] %>% flatten_chr()
  weak_n_posts_rt_f <- n_posts_rt_f[2,] %>% flatten_chr()
  neither_n_posts_rt_f <- n_posts_rt_f[3,] %>% flatten_chr()
  
  ver_no_rt <- coefs_all %>% 
    select(ends_with(aux[9]))
  strong_ver_no_rt <- ver_no_rt[1,] %>% flatten_chr()
  weak_ver_no_rt <- ver_no_rt[2,] %>% flatten_chr()
  neither_ver_no_rt <- ver_no_rt[3,] %>% flatten_chr()
  
  ver_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[10]))
  strong_ver_no_rt_f <- ver_no_rt_f[1,] %>% flatten_chr()
  weak_ver_no_rt_f <- ver_no_rt_f[2,] %>% flatten_chr()
  neither_ver_no_rt_f <- ver_no_rt_f[3,] %>% flatten_chr()
  
  true_no_rt <- coefs_all %>% 
    select(ends_with(aux[11]))
  strong_true_no_rt <- true_no_rt[1,] %>% flatten_chr()
  weak_true_no_rt <- true_no_rt[2,] %>% flatten_chr()
  neither_true_no_rt <- true_no_rt[3,] %>% flatten_chr()
  
  true_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[12]))
  strong_true_no_rt_f <- true_no_rt_f[1,] %>% flatten_chr()
  weak_true_no_rt_f <- true_no_rt_f[2,] %>% flatten_chr()
  neither_true_no_rt_f <- true_no_rt_f[3,] %>% flatten_chr()
  
  fake_no_rt <- coefs_all %>% 
    select(ends_with(aux[13]))
  strong_fake_no_rt <- fake_no_rt[1,] %>% flatten_chr()
  weak_fake_no_rt <- fake_no_rt[2,] %>% flatten_chr()
  neither_fake_no_rt <- fake_no_rt[3,] %>% flatten_chr()
  
  fake_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[14]))
  strong_fake_no_rt_f <- fake_no_rt_f[1,] %>% flatten_chr()
  weak_fake_no_rt_f <- fake_no_rt_f[2,] %>% flatten_chr()
  neither_fake_no_rt_f <- fake_no_rt_f[3,] %>% flatten_chr()
  
  n_posts_no_rt <- coefs_all %>% 
    select(ends_with(aux[15]))
  strong_n_posts_no_rt <- n_posts_no_rt[1,] %>% flatten_chr()
  weak_n_posts_no_rt <- n_posts_no_rt[2,] %>% flatten_chr()
  neither_n_posts_no_rt <- n_posts_no_rt[3,] %>% flatten_chr()
  
  n_posts_no_rt_f <- coefs_all %>% 
    select(ends_with(aux[16]))
  strong_n_posts_no_rt_f <- n_posts_no_rt_f[1,] %>% flatten_chr()
  weak_n_posts_no_rt_f <- n_posts_no_rt_f[2,] %>% flatten_chr()
  neither_n_posts_no_rt_f <- n_posts_no_rt_f[3,] %>% flatten_chr()
  
  coefs_perm <- data.frame(strong_ver_rt, weak_ver_rt, neither_ver_rt,
                           strong_ver_rt_f, weak_ver_rt_f, neither_ver_rt_f,
                           strong_true_rt, weak_true_rt, neither_true_rt,
                           strong_true_rt_f, weak_true_rt_f, neither_true_rt_f,
                           strong_fake_rt, weak_fake_rt, neither_fake_rt,
                           strong_fake_rt_f, weak_fake_rt_f, neither_fake_rt_f,
                           strong_n_posts_rt, weak_n_posts_rt, neither_n_posts_rt,
                           strong_n_posts_rt_f, weak_n_posts_rt_f, 
                           neither_n_posts_rt_f,
                           strong_ver_no_rt, weak_ver_no_rt, neither_ver_no_rt,
                           strong_ver_no_rt_f, weak_ver_no_rt_f, 
                           neither_ver_no_rt_f,
                           strong_true_no_rt, weak_true_no_rt, neither_true_no_rt,
                           strong_true_no_rt_f, weak_true_no_rt_f, 
                           neither_true_no_rt_f,
                           strong_fake_no_rt, weak_fake_no_rt, neither_fake_no_rt,
                           strong_fake_no_rt_f, weak_fake_no_rt_f, 
                           neither_fake_no_rt_f,
                           strong_n_posts_no_rt, weak_n_posts_no_rt, 
                           neither_n_posts_no_rt,
                           strong_n_posts_no_rt_f, weak_n_posts_no_rt_f, 
                           neither_n_posts_no_rt_f)
  
  write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                                stage, "/pestimates_b1_fake_", i,".xlsx"))
  i <- i + 1
}

