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
stage <- 'stage2'
# Read df (Aggregated Data Set)
df <- read_parquet(paste0('../../data/03-experiment/',country,
                          '/treatment/followers/01-preprocess/aggregated/',stage
                          ,'.parquet'))

df <- df |> rename(total_shares_base = shares_base, 
                   total_reactions_base = reactions_base,
                   total_comments_base = comments_base)

#df <- df |> filter(bot_account == 0)

# Generate the indexes
w_9_int_base <- df[
  c(
    'total_shares_base', 'total_reactions_base', 'total_comments_base'
  )
]
w_9_index_int <- icwIndex(w_9_int_base |> as.matrix())
df$index_int_base <- scale(w_9_index_int$index)

w_9_int_base <- df[
  c(
    'total_shares', 'total_comments' , 'total_reactions'
  )
]
w_9_index_int <- icwIndex(w_9_int_base |> as.matrix())
df$index_int <- scale(w_9_index_int$index)

# Define the dependent variables
aux <- c('total_shares', 'total_reactions', 'total_comments', 'index_int',
         'verifiability', 'true', 'n_posts')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ads_treatment + ",
                             x, "_base  | strat_block1 + c_t_strong_total + c_t_weak_total + c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
    filter(treatment != 'ads_treatment') |> 
    filter(treatment != paste0(x, '_base'))
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

# Build matrix
share_base <- coefs_all %>% 
  select(ends_with("shares"))
strong_share_base <- share_base[1,]
weak_share_base <- share_base[2,]
neither_share_base <- share_base[3,]

reac_base <- coefs_all %>% 
  select(ends_with("reactions"))
strong_reac_base <- reac_base[1,]
weak_reac_base <- reac_base[2,]
neither_reac_base <- reac_base[3,]

com_base <- coefs_all %>% 
  select(ends_with("comments"))
strong_com_base <- com_base[1,]
weak_com_base <- com_base[2,]
neither_com_base <- com_base[3,]

int_base <- coefs_all %>% 
  select(ends_with("index_int"))
strong_int_base <- int_base[1,]
weak_int_base <- int_base[2,]
neither_int_base <- int_base[3,]

ver_base <- coefs_all %>% 
  select(ends_with("verifiability"))
strong_ver_base <- ver_base[1,]
weak_ver_base <- ver_base[2,]
neither_ver_base <- ver_base[3,]

true_base <- coefs_all %>% 
  select(ends_with("true"))
strong_true_base <- true_base[1,]
weak_true_base <- true_base[2,]
neither_true_base <- true_base[3,]

n_base <- coefs_all %>% 
  select(ends_with("n_posts"))
strong_n_base <- n_base[1,]
weak_n_base <- n_base[2,]
neither_n_base <- n_base[3,]

coefs_perm <- data.frame(strong_share_base, weak_share_base, neither_share_base,
                         strong_reac_base, weak_reac_base, neither_reac_base,
                         strong_com_base, weak_com_base, neither_com_base,
                         strong_int_base, weak_int_base, neither_int_base,
                         strong_ver_base, weak_ver_base, neither_ver_base,
                         strong_true_base, weak_true_base, neither_true_base,
                         strong_n_base, weak_n_base, neither_n_base)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates_joint_endline_filter.xlsx"))
