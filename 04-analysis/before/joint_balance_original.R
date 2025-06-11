#############################################################################
# RCT: Balance on followers KE: 
# Joint Specification, Twitter Ads and SMIs Treatments
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

# ORIGINAL ---------------------------------------------
country <- 'KE'
# Read df (Aggregated Data Set)
# df <- read_parquet(paste0('../../data/04-analysis/',country,
#                           '/baseline_features.parquet'),
#                    as_tibble = T) |> rename(w_reactions_sum = w_reactions,
#                                             w_shares_sum = w_shares,
#                                             w_comments_sum = w_comments,
#                                             log_w_reactions_sum = log_w_reactions,
#                                             log_w_shares_sum = log_w_shares,
#                                             log_w_comments_sum = log_w_comments)

df <- read_parquet(paste0('../../data/04-analysis/',country,
                          '/baseline_features_filter.parquet'), as_tibble = T)

aux <- aux
#aux <- aux_log

aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
    filter(treatment != 'ads_treatment')
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

# Build matrix
share_base <- coefs_all %>% 
  select(ends_with("shares_sum"))
strong_share_base <- share_base[1,]
weak_share_base <- share_base[2,]
neither_share_base <- share_base[3,]

reac_base <- coefs_all %>% 
  select(ends_with("reactions_sum"))
strong_reac_base <- reac_base[1,]
weak_reac_base <- reac_base[2,]
neither_reac_base <- reac_base[3,]

com_base <- coefs_all %>% 
  select(ends_with("comments_sum"))
strong_com_base <- com_base[1,]
weak_com_base <- com_base[2,]
neither_com_base <- com_base[3,]

int_base <- coefs_all %>% 
  select(ends_with("index_int_base"))
strong_int_base <- int_base[1,]
weak_int_base <- int_base[2,]
neither_int_base <- int_base[3,]

ver_base <- coefs_all %>%
  select(ends_with("verifiability_base"))
strong_ver_base <- ver_base[1,]
weak_ver_base <- ver_base[2,]
neither_ver_base <- ver_base[3,]

true_base <- coefs_all %>%
  select(ends_with("true_base"))
strong_true_base <- true_base[1,]
weak_true_base <- true_base[2,]
neither_true_base <- true_base[3,]

n_base <- coefs_all %>%
  select(ends_with("n_posts_base"))
strong_n_base <- n_base[1,]
weak_n_base <- n_base[2,]
neither_n_base <- n_base[3,]

coefs_perm <- data.frame(strong_share_base, weak_share_base, neither_share_base,
                         strong_reac_base, weak_reac_base, neither_reac_base,
                         strong_com_base, weak_com_base, neither_com_base,
                         strong_int_base, weak_int_base, neither_int_base
                         ,
                         strong_ver_base, weak_ver_base, neither_ver_base,
                         strong_true_base, weak_true_base, neither_true_base,
                         strong_n_base, weak_n_base, neither_n_base
                         )


write_xlsx(coefs_perm, 
           paste0("../../data/04-analysis/",country,
                  "/pestimates_joint_balance_filter_original.xlsx"))
