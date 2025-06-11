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
country <- 'joint'
stage <- 'stage5_6'

df_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                             '/endline_b1.parquet'))

fake_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                               '/endline_fake_b1.parquet'))

df_ke <- df_ke |> left_join(fake_ke, by = c('follower_id', 'batch_id')) |> 
  mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0)) |>
  mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, verifiability_rt),
         true_rt_f = ifelse(ver_dummy == 0, NA, true_rt),
         n_posts_rt_f = ifelse(ver_dummy == 0, NA, n_posts_rt),
         verifiability_no_rt_f = ifelse(ver_dummy == 0, NA, verifiability_no_rt),
         true_no_rt_f = ifelse(ver_dummy == 0, NA, true_no_rt),
         n_posts_no_rt_f = ifelse(ver_dummy == 0, NA, n_posts_no_rt),
         fake_rt_f = ifelse(ver_dummy == 0, NA, fake_rt),
         fake_no_rt_f = ifelse(ver_dummy == 0, NA, fake_no_rt),
         fake_rt_f_base = fake_rt_base,
         fake_no_rt_f_base = fake_no_rt_base,
         verifiability_no_rt_f_base = verifiability_no_rt_base,
         verifiability_rt_f_base = verifiability_rt_base, 
         true_rt_f_base = true_rt_base, 
         true_no_rt_f_base = true_no_rt_base,
         n_posts_rt_f_base = n_posts_rt_base,
         n_posts_no_rt_f_base = n_posts_no_rt_base,
         pais = 'KE') 

df_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                             '/endline_b1.parquet'))

fake_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                               '/endline_fake_b1.parquet'))

df_sa <- df_sa |> left_join(fake_sa, by = c('follower_id', 'batch_id')) |> 
  mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0)) |>
  mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, verifiability_rt),
         true_rt_f = ifelse(ver_dummy == 0, NA, true_rt),
         n_posts_rt_f = ifelse(ver_dummy == 0, NA, n_posts_rt),
         verifiability_no_rt_f = ifelse(ver_dummy == 0, NA, verifiability_no_rt),
         true_no_rt_f = ifelse(ver_dummy == 0, NA, true_no_rt),
         n_posts_no_rt_f = ifelse(ver_dummy == 0, NA, n_posts_no_rt),
         fake_rt_f = ifelse(ver_dummy == 0, NA, fake_rt),
         fake_no_rt_f = ifelse(ver_dummy == 0, NA, fake_no_rt),
         fake_rt_f_base = fake_rt_base,
         fake_no_rt_f_base = fake_no_rt_base,
         verifiability_no_rt_f_base = verifiability_no_rt_base,
         verifiability_rt_f_base = verifiability_rt_base, 
         true_rt_f_base = true_rt_base, 
         true_no_rt_f_base = true_no_rt_base,
         n_posts_rt_f_base = n_posts_rt_base,
         n_posts_no_rt_f_base = n_posts_no_rt_base,
         pais = 'SA') 

df <- rbind(df_ke, df_sa)


# Define the dependent variables
aux <- c('verifiability_rt', 'verifiability_rt_f', 'true_rt', 'true_rt_f', 
         'fake_rt', 'fake_rt_f',
         'n_posts_rt', 'n_posts_rt_f', 'verifiability_no_rt', 
         'verifiability_no_rt_f', 'true_no_rt', 'true_no_rt_f', 
         'fake_no_rt', 'fake_no_rt_f', 'n_posts_no_rt',
         'n_posts_no_rt_f')

df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                   total_influencers = c_t_strong_total + c_t_weak_total + 
                     c_t_neither_total)

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                             x, "_base  | total_influencers"))
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
ver_rt <- ver_rt[1,]

ver_rt_f <- coefs_all %>% 
  select(ends_with(aux[2]))
ver_rt_f <- ver_rt_f[1,]

true_rt <- coefs_all %>% 
  select(ends_with(aux[3]))
true_rt <- true_rt[1,]

true_rt_f <- coefs_all %>% 
  select(ends_with(aux[4]))
true_rt_f <- true_rt_f[1,]

fake_rt <- coefs_all %>% 
  select(ends_with(aux[5]))
fake_rt <- fake_rt[1,]

fake_rt_f <- coefs_all %>% 
  select(ends_with(aux[6]))
fake_rt_f <- fake_rt_f[1,]

n_posts_rt <- coefs_all %>% 
  select(ends_with(aux[7]))
n_posts_rt <- n_posts_rt[1,]

n_posts_rt_f <- coefs_all %>% 
  select(ends_with(aux[8]))
n_posts_rt_f <- n_posts_rt_f[1,]

ver_no_rt <- coefs_all %>% 
  select(ends_with(aux[9]))
ver_no_rt <- ver_no_rt[1,]

ver_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[10]))
ver_no_rt_f <- ver_no_rt_f[1,]

true_no_rt <- coefs_all %>% 
  select(ends_with(aux[11]))
true_no_rt <- true_no_rt[1,]

true_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[12]))
true_no_rt_f <- true_no_rt_f[1,]

fake_no_rt <- coefs_all %>% 
  select(ends_with(aux[13]))
fake_no_rt <- fake_no_rt[1,] 

fake_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[14]))
fake_no_rt_f <- fake_no_rt_f[1,] 

n_posts_no_rt <- coefs_all %>% 
  select(ends_with(aux[15]))
n_posts_no_rt <- n_posts_no_rt[1,] 

n_posts_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[16]))
n_posts_no_rt_f <- n_posts_no_rt_f[1,] 

coefs_perm <- data.frame(ver_rt, ver_rt_f, true_rt, 
                         true_rt_f, fake_rt, 
                         fake_rt_f, n_posts_rt, n_posts_rt_f, 
                         ver_no_rt, ver_no_rt_f, true_no_rt, 
                         true_no_rt_f, fake_no_rt, 
                         fake_no_rt_f, n_posts_no_rt, n_posts_no_rt_f)

write_xlsx(coefs_perm, paste0("../../data/04-analysis/",country, "/",
                              stage, "/pestimates_b1_fake_all.xlsx"))