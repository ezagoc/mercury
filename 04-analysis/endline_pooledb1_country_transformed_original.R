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

country <- 'joint'
stage <- 'stage5_6'
base <- '_2_month'
type <- 'arc_'

df_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                             '/endline_b1.parquet'))

fake_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                               '/endline_fake_b1.parquet'))

bots_ke <- read_parquet(paste0('../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id)

baseline_ke <- read_parquet('../../data/04-analysis/KE/baseline_months.parquet')

df_ke <- df_ke |> left_join(fake_ke, by = c('follower_id', 'batch_id')) |> 
  left_join(bots_ke, by = 'follower_id') |> 
  left_join(baseline_ke, by = c('follower_id', 'username')) |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'KE')

df_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                             '/endline_b1.parquet'))

fake_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                               '/endline_fake_b1.parquet'))

bots_sa <- read_parquet(paste0('../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> select(-c("__index_level_0__"))

baseline_sa <- read_parquet('../../data/04-analysis/SA/baseline_months.parquet')

df_sa <- df_sa |> left_join(fake_sa, by = c('follower_id', 'batch_id')) |> 
  left_join(bots_sa, by = 'follower_id') |> 
  left_join(baseline_sa, by = c('follower_id', 'username')) |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'SA') 

df <- rbind(df_ke, df_sa)

df_log <- df |> select(follower_id, pais, batch_id, 
                       total_shares_base:n_posts_no_rt_base_2_month) |> 
  mutate(across(c(total_shares_base:n_posts_no_rt_base_2_month), ~log(.x + 1)))

colnames(df_log)[4:60] <- paste0('log_', colnames(df_log)[4:60])

df_arcsin <- df |> select(follower_id, pais, batch_id, 
                          total_shares_base:n_posts_no_rt_base_2_month) |> 
  mutate(across(c(total_shares_base:n_posts_no_rt_base_2_month), ~asinh(.x + 1)))

colnames(df_arcsin)[4:60] <- paste0('arc_', colnames(df_arcsin)[4:60])

df_share <- df |> select(follower_id, pais, batch_id, 
                         total_shares_base:n_posts_no_rt_base_2_month)

df_share <- df_share |> 
  mutate(across(c(total_shares_base:n_posts_no_rt_base_2_month), 
                ~.x/sum(df_share$.x)))

colnames(df_share)[4:60] <- paste0('share_', colnames(df_share)[4:60])

df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
  left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id')) |> 
  left_join(df_share, by = c('follower_id', 'pais', 'batch_id'))

# Define the dependent variables
aux <- c('verifiability_rt', 'true_rt', 'fake_rt', 'n_posts_rt', 
         'verifiability_no_rt', 'true_no_rt',  'fake_no_rt', 'n_posts_no_rt')

aux <- paste0(type, aux)


# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ",
                             x, "_base", base, " | c_t_strong_total + c_t_weak_total + 
                             c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs) |>
    filter(treatment != paste0(x, '_base'))
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

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

fake_rt <- coefs_all %>% 
  select(ends_with(aux[3]))
strong_fake_rt <- fake_rt[1,] 
weak_fake_rt <- fake_rt[2,] 
neither_fake_rt <- fake_rt[3,] 

n_posts_rt <- coefs_all %>% 
  select(ends_with(aux[4]))
strong_n_posts_rt <- n_posts_rt[1,]
weak_n_posts_rt <- n_posts_rt[2,]
neither_n_posts_rt <- n_posts_rt[3,]

ver_no_rt <- coefs_all %>% 
  select(ends_with(aux[5]))
strong_ver_no_rt <- ver_no_rt[1,]
weak_ver_no_rt <- ver_no_rt[2,]
neither_ver_no_rt <- ver_no_rt[3,]

true_no_rt <- coefs_all %>% 
  select(ends_with(aux[6]))
strong_true_no_rt <- true_no_rt[1,] 
weak_true_no_rt <- true_no_rt[2,] 
neither_true_no_rt <- true_no_rt[3,] 

fake_no_rt <- coefs_all %>% 
  select(ends_with(aux[7]))
strong_fake_no_rt <- fake_no_rt[1,] 
weak_fake_no_rt <- fake_no_rt[2,] 
neither_fake_no_rt <- fake_no_rt[3,] 

n_posts_no_rt <- coefs_all %>% 
  select(ends_with(aux[8]))
strong_n_posts_no_rt <- n_posts_no_rt[1,] 
weak_n_posts_no_rt <- n_posts_no_rt[2,] 
neither_n_posts_no_rt <- n_posts_no_rt[3,] 

coefs_perm <- data.frame(strong_ver_rt, weak_ver_rt, neither_ver_rt,
                         strong_true_rt, weak_true_rt, neither_true_rt,
                         strong_fake_rt, weak_fake_rt, neither_fake_rt,
                         strong_n_posts_rt, weak_n_posts_rt, neither_n_posts_rt,
                         strong_ver_no_rt, weak_ver_no_rt, neither_ver_no_rt,
                         strong_true_no_rt, weak_true_no_rt, neither_true_no_rt,
                         strong_fake_no_rt, weak_fake_no_rt, neither_fake_no_rt,
                         strong_n_posts_no_rt, weak_n_posts_no_rt, 
                         neither_n_posts_no_rt)

write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                              stage, "/pestimates_b1_good_", base, "_" ,type,
                              ".xlsx"))
