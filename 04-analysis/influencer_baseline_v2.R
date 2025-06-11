# Influencer Baseline:

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

country <- 'SA'
stage <- 'baseline'
# Read df (Aggregated Data Set)
df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

# Define the dependent variables
aux <- c('verifiability_rt_base', 'true_rt_base', 'n_posts_rt_base',
              'verifiability_no_rt_base', 'true_no_rt_base', 
              'n_posts_no_rt_base')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither | 
                             c_t_strong_total + c_t_weak_total + c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs)
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

# Build matrix
ver_rt <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_ver_rt <- ver_rt[1,]
weak_ver_rt <- ver_rt[2,]
neither_ver_rt <- ver_rt[3,]

true_rt <- coefs_all %>% 
  select(ends_with(aux[2]))
strong_true_rt <- true_rt[1,]
weak_true_rt <- true_rt[2,]
neither_true_rt <- true_rt[3,]

n_posts_rt <- coefs_all %>% 
  select(ends_with(aux[3]))
strong_n_posts_rt <- n_posts_rt[1,]
weak_n_posts_rt <- n_posts_rt[2,]
neither_n_posts_rt <- n_posts_rt[3,]

ver_no_rt <- coefs_all %>% 
  select(ends_with(aux[4]))
strong_ver_no_rt <- ver_no_rt[1,]
weak_ver_no_rt <- ver_no_rt[2,]
neither_ver_no_rt <- ver_no_rt[3,]

true_no_rt <- coefs_all %>% 
  select(ends_with(aux[5]))
strong_true_no_rt <- true_no_rt[1,]
weak_true_no_rt <- true_no_rt[2,]
neither_true_no_rt <- true_no_rt[3,]

n_posts_no_rt <- coefs_all %>% 
  select(ends_with(aux[6]))
strong_n_posts_no_rt <- n_posts_no_rt[1,]
weak_n_posts_no_rt <- n_posts_no_rt[2,]
neither_n_posts_no_rt <- n_posts_no_rt[3,]

coefs_perm <- data.frame(strong_ver_rt, weak_ver_rt, neither_ver_rt,
                         strong_true_rt, weak_true_rt, neither_true_rt,
                         strong_n_posts_rt, weak_n_posts_rt, neither_n_posts_rt,
                         strong_ver_no_rt, weak_ver_no_rt, neither_ver_no_rt,
                         strong_true_no_rt, weak_true_no_rt, neither_true_no_rt,
                         strong_n_posts_no_rt, weak_n_posts_no_rt, 
                         neither_n_posts_no_rt)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates.xlsx"))
