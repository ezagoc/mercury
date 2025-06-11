### Pooled Endline, Batch 1 and 2:

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

country <- 'SA'
stage <- 'stage1_2'

df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_b1_b2.parquet'))

fake <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/endline_fake_b1_b2.parquet'))

df <- df |> left_join(fake, by = c('follower_id', 'batch_id'))

df <- df |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

df <- df |> mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, 
                                               verifiability_rt),
                   true_rt_f = ifelse(ver_dummy == 0, NA, 
                                      true_rt),
                   n_posts_rt_f = ifelse(ver_dummy == 0, NA, 
                                         n_posts_rt),
                   verifiability_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                                  verifiability_no_rt),
                   true_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                         true_no_rt),
                   n_posts_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                            n_posts_no_rt),
                   fake_rt_f = ifelse(ver_dummy == 0, NA, 
                                      fake_rt),
                   fake_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                         fake_no_rt),
                   fake_rt_f_base = fake_rt_base,
                   fake_no_rt_f_base = fake_no_rt_base,
                   verifiability_no_rt_f_base = verifiability_no_rt_base,
                   verifiability_rt_f_base = verifiability_rt_base, 
                   true_rt_f_base = true_rt_base, 
                   true_no_rt_f_base = true_no_rt_base,
                   n_posts_rt_f_base = n_posts_rt_base,
                   n_posts_no_rt_f_base = n_posts_no_rt_base) 

# Define the dependent variables
aux <- c('verifiability_rt', 'verifiability_rt_f', 'true_rt', 'true_rt_f', 
         'fake_rt', 'fake_rt_f',
         'n_posts_rt', 'n_posts_rt_f', 'verifiability_no_rt', 
         'verifiability_no_rt_f', 'true_no_rt', 'true_no_rt_f', 
         'fake_no_rt', 'fake_no_rt_f', 'n_posts_no_rt',
         'n_posts_no_rt_f')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ",
                             x, "_base  | c_t_strong_total + c_t_weak_total + 
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

# Build matrix
ver_rt <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_ver_rt <- ver_rt[1,] 
weak_ver_rt <- ver_rt[2,] 
neither_ver_rt <- ver_rt[3,] 

ver_rt_f <- coefs_all %>% 
  select(ends_with(aux[2]))
strong_ver_rt_f <- ver_rt_f[1,] 
weak_ver_rt_f <- ver_rt_f[2,] 
neither_ver_rt_f <- ver_rt_f[3,] 

true_rt <- coefs_all %>% 
  select(ends_with(aux[3]))
strong_true_rt <- true_rt[1,] 
weak_true_rt <- true_rt[2,] 
neither_true_rt <- true_rt[3,] 

true_rt_f <- coefs_all %>% 
  select(ends_with(aux[4]))
strong_true_rt_f <- true_rt_f[1,] 
weak_true_rt_f <- true_rt_f[2,] 
neither_true_rt_f <- true_rt_f[3,] 

fake_rt <- coefs_all %>% 
  select(ends_with(aux[5]))
strong_fake_rt <- fake_rt[1,] 
weak_fake_rt <- fake_rt[2,] 
neither_fake_rt <- fake_rt[3,] 

fake_rt_f <- coefs_all %>% 
  select(ends_with(aux[6]))
strong_fake_rt_f <- fake_rt_f[1,] 
weak_fake_rt_f <- fake_rt_f[2,] 
neither_fake_rt_f <- fake_rt_f[3,] 

n_posts_rt <- coefs_all %>% 
  select(ends_with(aux[7]))
strong_n_posts_rt <- n_posts_rt[1,] 
weak_n_posts_rt <- n_posts_rt[2,] 
neither_n_posts_rt <- n_posts_rt[3,] 

n_posts_rt_f <- coefs_all %>% 
  select(ends_with(aux[8]))
strong_n_posts_rt_f <- n_posts_rt_f[1,] 
weak_n_posts_rt_f <- n_posts_rt_f[2,] 
neither_n_posts_rt_f <- n_posts_rt_f[3,] 

ver_no_rt <- coefs_all %>% 
  select(ends_with(aux[9]))
strong_ver_no_rt <- ver_no_rt[1,] 
weak_ver_no_rt <- ver_no_rt[2,] 
neither_ver_no_rt <- ver_no_rt[3,] 

ver_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[10]))
strong_ver_no_rt_f <- ver_no_rt_f[1,] 
weak_ver_no_rt_f <- ver_no_rt_f[2,] 
neither_ver_no_rt_f <- ver_no_rt_f[3,] 

true_no_rt <- coefs_all %>% 
  select(ends_with(aux[11]))
strong_true_no_rt <- true_no_rt[1,] 
weak_true_no_rt <- true_no_rt[2,] 
neither_true_no_rt <- true_no_rt[3,] 

true_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[12]))
strong_true_no_rt_f <- true_no_rt_f[1,] 
weak_true_no_rt_f <- true_no_rt_f[2,] 
neither_true_no_rt_f <- true_no_rt_f[3,] 

fake_no_rt <- coefs_all %>% 
  select(ends_with(aux[13]))
strong_fake_no_rt <- fake_no_rt[1,] 
weak_fake_no_rt <- fake_no_rt[2,] 
neither_fake_no_rt <- fake_no_rt[3,] 

fake_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[14]))
strong_fake_no_rt_f <- fake_no_rt_f[1,] 
weak_fake_no_rt_f <- fake_no_rt_f[2,] 
neither_fake_no_rt_f <- fake_no_rt_f[3,] 

n_posts_no_rt <- coefs_all %>% 
  select(ends_with(aux[15]))
strong_n_posts_no_rt <- n_posts_no_rt[1,] 
weak_n_posts_no_rt <- n_posts_no_rt[2,] 
neither_n_posts_no_rt <- n_posts_no_rt[3,] 

n_posts_no_rt_f <- coefs_all %>% 
  select(ends_with(aux[16]))
strong_n_posts_no_rt_f <- n_posts_no_rt_f[1,] 
weak_n_posts_no_rt_f <- n_posts_no_rt_f[2,] 
neither_n_posts_no_rt_f <- n_posts_no_rt_f[3,] 

coefs_perm <- data.frame(strong_ver_rt, weak_ver_rt, neither_ver_rt,
                         strong_ver_rt_f, weak_ver_rt_f, neither_ver_rt_f,
                         strong_true_rt, weak_true_rt, neither_true_rt,
                         strong_true_rt_f, weak_true_rt_f, neither_true_rt_f,
                         strong_fake_rt, weak_fake_rt, neither_fake_rt,
                         strong_fake_rt_f, weak_fake_rt_f, neither_fake_rt_f,
                         strong_n_posts_rt, weak_n_posts_rt, neither_n_posts_rt,
                         strong_n_posts_rt_f, weak_n_posts_rt_f, 
                         neither_n_posts_rt_f,
                         strong_ver_no_rt, weak_ver_no_rt, neither_ver_no_rt,
                         strong_ver_no_rt_f, weak_ver_no_rt_f, 
                         neither_ver_no_rt_f,
                         strong_true_no_rt, weak_true_no_rt, neither_true_no_rt,
                         strong_true_no_rt_f, weak_true_no_rt_f, 
                         neither_true_no_rt_f,
                         strong_fake_no_rt, weak_fake_no_rt, neither_fake_no_rt,
                         strong_fake_no_rt_f, weak_fake_no_rt_f, 
                         neither_fake_no_rt_f,
                         strong_n_posts_no_rt, weak_n_posts_no_rt, 
                         neither_n_posts_no_rt,
                         strong_n_posts_no_rt_f, weak_n_posts_no_rt_f, 
                         neither_n_posts_no_rt_f)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates_b1_b2_fake.xlsx"))
