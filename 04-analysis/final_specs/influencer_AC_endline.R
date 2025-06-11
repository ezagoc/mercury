rm(list = ls())
library("purrr")
src_path <- c("../../../src/utils/")             
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

country <- 'joint'
stage <- 'AC'

# Influencer Baseline:

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
                          '/AC_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_sa <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> 
  select(-c("__index_level_0__"))

df_sa <- df_sa |> 
  left_join(bots_sa, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'SA')

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
                             '/AC_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_ke <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id)

df_ke <- df_ke |> 
  left_join(bots_ke, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'KE')

df <- rbind(df_sa, df_ke)

df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                   total_influencers = c_t_strong_total + c_t_weak_total + 
                     c_t_neither_total)

# Define the dependent variables
aux <- c('AC')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ total_treated + AC_base | total_influencers"))
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
  reduce(left_join, by = "treatment") |> filter(treatment != 'AC_base')

# Build matrix
AC <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_AC <- AC[1,]

coefs_perm <- data.frame(strong_AC)

write_xlsx(
  coefs_perm, paste0("../../../data/04-analysis/",country,
                     "/",stage,"/pestimates_AC_all.xlsx"))
