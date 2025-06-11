#############################################################################
# RCT: Balance on followers KE: 
# Joint Specification, Twitter Ads and SMIs Treatments Permutations
# Date: Tuesday April 18th, 2023
#############################################################################

rm(list = ls())
library("purrr")
src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

library(lfe)
library(fixest)

# Define round and set of dependent variables:
i <- 0
country <- 'KE'

# Choose set of dependent variables
aux <- aux
#aux <- aux_log

# PERMUTATIONS:

# data <- read_parquet(paste0('../../data/04-analysis/',country,
#                             '/baseline_features.parquet')) |> 
#   rename(w_reactions_sum = w_reactions,
#          w_shares_sum = w_shares,
#          w_comments_sum = w_comments,
#          log_w_reactions_sum = log_w_reactions,
#          log_w_shares_sum = log_w_shares,
#          log_w_comments_sum = log_w_comments)

data <- read_parquet(paste0('../../data/04-analysis/',country,
                          '/baseline_features_filter.parquet'), as_tibble = T)


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
    fmla1 <- as.formula(paste0(au, fixed_effects))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, feols(fmla1, data = data))
    coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
      select(Estimate)
    names(coefs) <- paste0('p', x, '_' , au)
    coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
      filter(treatment != 'ads_treatment')
    rownames(coefs) <- 1:nrow(coefs)
    lm_list_ols[[count]] <- coefs
    count <- count + 1
  }
  coefs_list <- append(coefs_list, lm_list_ols)
}
coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")

# Build matrix
share_base <- coefs_all %>% 
  select(ends_with("shares_sum"))
strong_share_base <- share_base[1,] %>% flatten_chr()
weak_share_base <- share_base[2,] %>% flatten_chr()
neither_share_base <- share_base[3,] %>% flatten_chr()

reac_base <- coefs_all %>% 
  select(ends_with("reactions_sum"))
strong_reac_base <- reac_base[1,] %>% flatten_chr()
weak_reac_base <- reac_base[2,] %>% flatten_chr()
neither_reac_base <- reac_base[3,] %>% flatten_chr()

com_base <- coefs_all %>% 
  select(ends_with("comments_sum"))
strong_com_base <- com_base[1,] %>% flatten_chr()
weak_com_base <- com_base[2,] %>% flatten_chr()
neither_com_base <- com_base[3,] %>% flatten_chr()

int_base <- coefs_all %>% select(ends_with("index_int_base"))
strong_int_base <- int_base[1,] %>% flatten_chr()
weak_int_base <- int_base[2,] %>% flatten_chr()
neither_int_base <- int_base[3,] %>% flatten_chr()

ver_base <- coefs_all %>% select(ends_with("verifiability_base"))
strong_ver_base <- ver_base[1,] %>% flatten_chr()
weak_ver_base <- ver_base[2,] %>% flatten_chr()
neither_ver_base <- ver_base[3,] %>% flatten_chr()

true_base <- coefs_all %>% select(ends_with("true_base"))
strong_true_base <- true_base[1,] %>% flatten_chr()
weak_true_base <- true_base[2,] %>% flatten_chr()
neither_true_base <- true_base[3,] %>% flatten_chr()

n_base <- coefs_all %>%
  select(ends_with("n_posts_base"))
strong_n_base <- n_base[1,] %>% flatten_chr()
weak_n_base <- n_base[2,] %>% flatten_chr()
neither_n_base <- n_base[3,] %>% flatten_chr()

coefs_perm <- data.frame(strong_share_base, weak_share_base, neither_share_base,
                         strong_reac_base, weak_reac_base, neither_reac_base,
                         strong_com_base, weak_com_base, neither_com_base,
                         strong_int_base, weak_int_base, neither_int_base
                         ,
                         strong_ver_base, weak_ver_base, neither_ver_base,
                         strong_true_base, weak_true_base, neither_true_base,
                         strong_n_base, weak_n_base, neither_n_base
)

write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country,
                              "/pestimates_joint_balance_filter", i,".xlsx"))
