rm(list = ls())
library("purrr")

src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
library(lfe)
library(fixest)
library(DescTools)

country <- 'KE'

data_reg <- read_parquet(paste0('../../data/04-analysis/',country,
                            '/baseline_features.parquet'))

data_reg <- data_reg |> mutate(w_9_reactions_sum = Winsorize(total_reactions_sum, 
                                                         probs = c(0.1, 0.90), 
                                                         na.rm = T),
                               w_9_shares_sum = Winsorize(total_shares_sum, 
                                                      probs = c(0.1, 0.90), 
                                                      na.rm = T),
                               w_9_comments_sum = Winsorize(total_comments_sum, 
                                                        probs = c(0.1, 0.90), 
                                                        na.rm = T))

w_9_int_base <- data_reg[
  c(
    "w_9_reactions_sum",
    "w_9_comments_sum",
    "w_9_shares_sum"
  )
]
w_9_index_int <- icwIndex(w_9_int_base |> as.matrix())
data_reg$w_9_index_int_base <- scale(w_9_index_int$index)

write_parquet(data_reg, paste0('../../data/04-analysis/',country,
                               '/baseline_features.parquet'))

max_reac_9 <- max(data_reg$w_9_reactions_sum, na.rm=T)
max_shares_9 <- max(data_reg$w_9_shares_sum, na.rm=T)
max_comments_9 <- max(data_reg$w_9_comments_sum, na.rm=T)

# Filtered Data Set
data_regf <- data_reg |> 
  mutate(across(c(total_shares_sum:n_posts_base), 
                ~ifelse(total_shares_sum < max_shares_9 & 
                          total_reactions_sum < max_reac_9 | 
                          n_posts_base == 0, .x, NA)))

w_9_int_base <- data_regf[
  c(
    "total_reactions_sum",
    "total_comments_sum",
    "total_shares_sum"
  )
]
w_9_index_int <- icwIndex(w_9_int_base |> as.matrix())
data_regf$index_int_base <- scale(w_9_index_int$index)

write_parquet(data_regf, paste0('../../data/04-analysis/',country,
                                '/baseline_features_filter.parquet'))
