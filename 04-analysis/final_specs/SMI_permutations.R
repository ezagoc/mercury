rm(list = ls())
library("purrr")
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

library(lfe)
library(fixest)

# ORIGINAL ---------------------------------------------
stage <- 'SMIs'
country <- 'joint'

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
                             '/SMIs_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_sa <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> 
  select(-c("__index_level_0__"))

df_sa <- df_sa |> 
  left_join(bots_sa, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'SA')

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
                             '/SMIs_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_ke <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id)

df_ke <- df_ke |> 
  left_join(bots_ke, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'KE')

df <- rbind(df_sa, df_ke)

df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                   total_influencers = c_t_strong_total + c_t_weak_total + 
                     c_t_neither_total)

# Define the dependent variables
aux <- c('SMIs')

i <- 1
for (m in 1:1000){
  print(i)
  followers <- read_parquet(paste0("../../../data/04-analysis/joint/",
                                   'small_ties', "/small_tie", 
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
                                         c5, c6, c7, c8, c9, c10, c11, pais) 
  
  data <- left_join(
    data, 
    followers_iter,
    by = c('follower_id', 'pais')
  )
  # Pool treatment variables
  data <- poolTreatmentBalance2(data, c10, c11)
  
  # Balance tables 
  aux_data <- data[aux]
  coefs_list <- list()
  lm_list_ols <- list()
  count <- 1
  for (au in aux) {
    fmla1 <- as.formula(paste0(au, "~ total_treated | total_influencers"))
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
  AC <- coefs_all %>% 
    select(ends_with(aux[1]))
  strong_smi <- AC[1,]
  
  coefs_perm <- data.frame(strong_smi)
  
  write_xlsx(coefs_perm, paste0("../../../data/04-analysis/joint/",
                                stage, "/pestimates/b1_all",i,".xlsx"))
  i <- i + 1
}