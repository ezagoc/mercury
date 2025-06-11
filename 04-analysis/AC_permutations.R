# Baseline Permutations:
#############################################################################
# RCT: 
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
country <- 'SA'
stage <- 'AC'

data <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/AC_final.parquet'))

# Define the dependent variables

aux <- c('AC')

i <- 3

for (m in 1:4){
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
    data <- poolTreatmentBalance(data, c5, c6, c4, c3, c1, c2)
    
    # Balance tables 
    aux_data <- data[aux]
    lm_list_ols <- list()
    count <- 1
    for (au in aux) {
      fmla1 <- as.formula(paste0(au, "~ t_strong + t_weak + t_neither + AC_base | 
                                 c_t_strong_total + c_t_weak_total + c_t_neither_total"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = data))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0('p', x, '_' , au)
      coefs <- cbind('treatment' = rownames(coefs), coefs)
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_list <- append(coefs_list, lm_list_ols)
  }
  coefs_all <- coefs_list %>% reduce(left_join, by = "treatment") |> 
    filter(treatment != 'AC_base')
  
  # Build matrix
  AC <- coefs_all %>% 
    select(ends_with(aux[1]))
  strong_AC <- AC[1,] %>% flatten_chr()
  weak_AC <- AC[2,] %>% flatten_chr()
  neither_AC <- AC[3,] %>% flatten_chr()
  
  coefs_perm <- data.frame(strong_AC, weak_AC, neither_AC)
  
  
  write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                                stage, "/pestimates_", i,".xlsx"))
  followers <- tibble()
  i <- i + 1
}
