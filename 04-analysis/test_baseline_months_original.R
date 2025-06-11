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

country <- 'KE'

path_pilot <- '../../../social-media-influencers-africa/data/04-analysis/'
path_normal <- '../../data/04-analysis/'

get_panel_stages <- function(stage, path, batch, country){
  df <- read_parquet(paste0(path, country, '/',stage,
                            '/endline_', batch,'.parquet'))
  df <- df |> mutate(stage_id = stage)
  df
}

df_stages_b2 <- paste0(rep('stage', 2), c(1:2)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'batch2', country))

df_stages_agg <- df_stages_b2 |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b2')

base1 <- read_parquet(paste0(path_normal, country, 
                             '/stage1/endline_batch2.parquet')) |>
  select(username:n_posts_no_rt_base) |> select(-c(id, blockid1, blockid2))

base2 <- read_parquet(paste0(path_normal, country, 
                             '/baseline/baseline_batch2_april.parquet')) |>
  select(username:n_posts_no_rt_base) |> select(-c(id, blockid1, blockid2))

colnames(base2)[12:length(base2)] <- paste0(colnames(base2)[12:length(base2)],'_april')

base2 <- base2 |> select(follower_id, total_shares_rt_base_april:n_posts_no_rt_base_april)

final_batch2 <- base1 |> left_join(df_stages_agg, by = 'follower_id')

final_batch2 <- final_batch2 |> left_join(base2, by = 'follower_id')

df <- final_batch2

# Define the dependent variables
aux <- c('verifiability_rt', 'true_rt', 'n_posts_rt', 'verifiability_no_rt',
         'true_no_rt', 'n_posts_no_rt')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1

for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ", x, "_base_april  | 
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
                     "/baseline/pestimates_prueba_april.xlsx"))

#####

aux_data <- df[aux]
lm_list_ols <- list()
count <- 1

for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ", x, "_base  | 
                                c_t_strong_total + c_t_weak_total + c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- coefs |> mutate(names_coefs = row.names(coefs))
  coefs <- coefs |> mutate(names_coefs = ifelse(str_detect(names_coefs, '_base') == TRUE, 
                                                'base_control', names_coefs))
  rownames(coefs) <- coefs$names_coefs
  coefs <- coefs |> select(-names_coefs)
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
                     "/baseline/pestimates_prueba.xlsx"))
