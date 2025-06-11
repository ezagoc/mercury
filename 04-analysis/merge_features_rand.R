## Merging the Features with Randomization Variables

# Load packages: 
library(tidyverse)
library(arrow)
library(DescTools)

rm(list = ls()) # Clean out elements in R environment
setwd("../../data/")
src_path <- c("../src/utils/")             
source_files <- list(
  "funcs.R"
)
map(paste0(src_path, source_files), source)
#source("../../code/02-randomize/funcs.R")

##############
################# 

country <- 'SA'
# covariates:
twitter <- read_parquet(paste0("02-randomize/", country,
                               "/03-assignment/input/twitter_followers_filtered.parquet"))

# rand:

df <- read_parquet(paste0('02-randomize/',country
                          ,'/04-stratification/integrate/followers_randomized.parquet'))

df <- df |> left_join(twitter |> select(-c(username)), by = "follower_id")

# Kenya Features:

features <- read_parquet(paste0('03-experiment/',country
                                ,'/baseline/01-preprocess/followers/aggregated/baseline.parquet.gzip'))

# Merge

df <- df |> left_join(features |> select(-handle) |> 
                        rename(follower_id = author_id), by = 'follower_id')

df <- df |> mutate(n_posts = ifelse(is.na(n_posts)==T, 0, n_posts))

df <- df |> rename(c_t_strong_total = n_strong, c_t_weak_total = n_weak,
                   c_t_neither_total = n_absent, t_strong = n_strong_treated,
                   t_weak = n_weak_treated, t_neither = n_absent_treated,
                   verifiability_base = verifiability, n_posts_base = n_posts,
                   verifiability_mean_base = verifiability_mean, true_base = true, 
                   total_reactions_sum = total_reactions, 
                   total_shares_sum = total_shares, 
                   total_comments_sum = total_comments)

# Generate Relevant Index:
int_base <- df[
  c(
    "total_reactions_sum",
    "total_comments_sum",
    "total_shares_sum"
  )
]
index_int <- icwIndex(int_base |> as.matrix())
df$index_int_base <- scale(index_int$index)

df <- df |> 
  mutate(strat_block1 = ifelse(is.na(blockid1) == F, paste0(blockid1, id), id),
         strat_block2 = ifelse(is.na(blockid2) == F, paste0(blockid2, id), id)) |>
  rename(ads_treatment = treatment)

df <- df |> select(-c(index_influence,days_old_account))

df <- df |> mutate(w_reactions = Winsorize(total_reactions_sum, na.rm = T),
                   w_shares = Winsorize(total_shares_sum, na.rm = T),
                   w_comments = Winsorize(total_comments_sum, na.rm = T),
                   log_w_reactions = log(w_reactions + 1),
                   log_w_shares = log(w_shares + 1),
                   log_w_comments = log(w_comments + 1))

w_int_base <- df[
  c(
    "w_reactions",
    "w_comments",
    "w_shares"
  )
]
w_index_int <- icwIndex(w_int_base |> as.matrix())
df$w_index_int_base <- scale(w_index_int$index)

log_int_base <- df[
  c(
    "log_w_reactions",
    "log_w_comments",
    "log_w_shares"
  )
]
log_w_index_int <- icwIndex(log_int_base |> as.matrix())
df$log_w_index_int_base <- scale(log_w_index_int$index)

write_parquet(df, paste0('04-analysis/', country, '/baseline_features.parquet'))

################### SA

################# 
# SA covariates:
load("02-randomize/SA/03-assignment/input/twitter_followers.Rda")

# Kenya rand:

df <- read_parquet('02-randomize/SA/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')

df <- df |> left_join(twitter |> select(-c(username, name)), by = "follower_id")

# Kenya Features:

features <- read_parquet('03-experiment/SA/baseline/01-preprocess/followers/aggregated/baseline.parquet.gzip')

# Merge

df <- df |> left_join(features |> select(-handle) |> 
                        rename(follower_id = author_id), by = 'follower_id')

df <- df |> mutate(n_posts = ifelse(is.na(n_posts)==T, 0, n_posts))

write_parquet(df, '04-analysis/SA/baseline_features.parquet')

# Adding Treu/False Pred:

# Kenya Features:

features <- read_parquet('03-experiment/KE/baseline/01-preprocess/followers/aggregated/baseline_true.parquet.gzip')

df <- read_parquet('04-analysis/KE/baseline_features_winsor.parquet')

df <- df |> left_join(features |> select(-handle) |> 
                        rename(follower_id = author_id), by = 'follower_id')

write_parquet(df, '04-analysis/KE/baseline_features_winsor.parquet')

# SA Features:

features <- read_parquet('03-experiment/SA/baseline/01-preprocess/followers/aggregated/baseline_true.parquet.gzip')

df <- read_parquet('04-analysis/SA/baseline_features_winsor.parquet')

df <- df |> left_join(features |> select(-handle) |> 
                        rename(follower_id = author_id), by = 'follower_id')

write_parquet(df, '04-analysis/SA/baseline_features_winsor.parquet')


# Kenya Endline

# Kenya covariates:
load("02-randomize/KE/03-assignment/input/twitter_followers.Rda")

# Kenya rand:

df <- read_parquet('02-randomize/KE/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')

df <- df |> left_join(twitter |> select(-c(username, name)), by = "follower_id")

# Kenya Features:

features <- read_parquet('03-experiment/KE/treatment/followers/01-preprocess/aggregated/endline.parquet.gzip')

# Merge

df <- df |> left_join(features |> select(-handle) |> 
                        rename(follower_id = author_id), by = 'follower_id')

df <- df |> mutate(n_posts = ifelse(is.na(n_posts)==T, 0, n_posts))

# Cleaning and adding all variables:

df <- df |> rename(c_t_strong_total = n_strong, c_t_weak_total = n_weak,
                   c_t_neither_total = n_absent, t_strong = n_strong_treated,
                   t_weak = n_weak_treated, t_neither = n_absent_treated,
                   verifiability_end = verifiability, n_posts_end = n_posts,
                   total_reactions_end = total_reactions, 
                   total_shares_end = total_shares, 
                   total_comments_end = total_comments, true_end = true)

int_base <- df[
  c(
    "total_reactions_end",
    "total_comments_end",
    "total_shares_end"
  )
]
index_int <- icwIndex(int_base |> as.matrix())
df$index_int_end <- scale(index_int$index)

df <- df |> 
  mutate(strat_block1 = ifelse(is.na(blockid1) == F, paste0(blockid1, id), id),
         strat_block2 = ifelse(is.na(blockid2) == F, paste0(blockid2, id), id)) |>
  rename(ads_treatment = treatment)

df <- df |> select(-c(index_influence, `__index_level_0__`, days_old_account))

df <- df |> mutate(w_reactions = Winsorize(total_reactions_end, na.rm = T),
                   w_shares = Winsorize(total_shares_end, na.rm = T),
                   w_comments = Winsorize(total_comments_end, na.rm = T),
                   log_w_reactions = log(w_reactions + 1),
                   log_w_shares = log(w_shares + 1),
                   log_w_comments = log(w_comments + 1))

w_int_base <- df[
  c(
    "w_reactions",
    "w_comments",
    "w_shares"
  )
]
w_index_int <- icwIndex(w_int_base |> as.matrix())
df$w_index_int_end <- scale(w_index_int$index)

log_int_base <- df[
  c(
    "log_w_reactions",
    "log_w_comments",
    "log_w_shares"
  )
]
log_w_index_int <- icwIndex(log_int_base |> as.matrix())
df$log_w_index_int_end <- scale(log_w_index_int$index)

write_parquet(df, '04-analysis/KE/endline_features.parquet')


# SA Endline

# SA covariates:
load("02-randomize/SA/03-assignment/input/twitter_followers.Rda")

# Kenya rand:

df <- read_parquet('02-randomize/SA/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')

df <- df |> left_join(twitter |> select(-c(username, name)), by = "follower_id")

# Kenya Features:

features <- read_parquet('03-experiment/SA/treatment/followers/01-preprocess/aggregated/endline.parquet.gzip')

# Merge

df <- df |> left_join(features |> select(-handle) |> 
                        rename(follower_id = author_id), by = 'follower_id')

df <- df |> mutate(n_posts = ifelse(is.na(n_posts)==T, 0, n_posts))

# Cleaning and adding all variables:

df <- df |> rename(c_t_strong_total = n_strong, c_t_weak_total = n_weak,
                   c_t_neither_total = n_absent, t_strong = n_strong_treated,
                   t_weak = n_weak_treated, t_neither = n_absent_treated,
                   verifiability_end = verifiability, n_posts_end = n_posts,
                   total_reactions_end = total_reactions, 
                   total_shares_end = total_shares, 
                   total_comments_end = total_comments, true_end = true)

int_base <- df[
  c(
    "total_reactions_end",
    "total_comments_end",
    "total_shares_end"
  )
]
index_int <- icwIndex(int_base |> as.matrix())
df$index_int_end <- scale(index_int$index)

df <- df |> 
  mutate(strat_block1 = ifelse(is.na(blockid1) == F, paste0(blockid1, id), id),
         strat_block2 = ifelse(is.na(blockid2) == F, paste0(blockid2, id), id)) |>
  rename(ads_treatment = treatment)

df <- df |> select(-c(index_influence, `__index_level_0__`, days_old_account))

df <- df |> mutate(w_reactions = Winsorize(total_reactions_end, na.rm = T),
                   w_shares = Winsorize(total_shares_end, na.rm = T),
                   w_comments = Winsorize(total_comments_end, na.rm = T),
                   log_w_reactions = log(w_reactions + 1),
                   log_w_shares = log(w_shares + 1),
                   log_w_comments = log(w_comments + 1))

w_int_base <- df[
  c(
    "w_reactions",
    "w_comments",
    "w_shares"
  )
]
w_index_int <- icwIndex(w_int_base |> as.matrix())
df$w_index_int_end <- scale(w_index_int$index)

log_int_base <- df[
  c(
    "log_w_reactions",
    "log_w_comments",
    "log_w_shares"
  )
]
log_w_index_int <- icwIndex(log_int_base |> as.matrix())
df$log_w_index_int_end <- scale(log_w_index_int$index)

write_parquet(df, '04-analysis/SA/endline_features.parquet')


### BATCH2

country <- 'KE'
# covariates:
twitter <- read_parquet(paste0("02-randomize/", country,
                               "/03-assignment/input/twitter_followers_filtered_batch2.parquet"))

# rand:

df <- read_parquet(paste0('02-randomize/',country
                          ,'/04-stratification/integrate/followers_randomized_batch2.parquet'))

df <- df |> left_join(twitter |> select(-c(username)), by = "follower_id")

df <- df |> 
  mutate(strat_block1 = ifelse(is.na(blockid1) == F, paste0(blockid1, id), id),
         strat_block2 = ifelse(is.na(blockid2) == F, paste0(blockid2, id), id)) |>
  rename(ads_treatment = treatment)

df <- df |> rename(c_t_strong_total = n_strong, c_t_weak_total = n_weak,
                   c_t_neither_total = n_absent, t_strong = n_strong_treated,
                   t_weak = n_weak_treated, t_neither = n_absent_treated)

df <- df |> select(-c(index_influence, days_old_account,
                      treatment1))

write_parquet(df, paste0('04-analysis/',country,
                         '/baseline_batch2.parquet'))
