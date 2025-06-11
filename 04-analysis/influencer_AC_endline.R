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
stage <- 'AC'
# Read df (Aggregated Data Set)
df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

followers_base1 <- read_parquet('../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/625489039.parquet') 
followers_base2 <- read_parquet('../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/1468955884092936200.parquet') 

followers_base <- rbind(followers_base1 |> select(id, username),
                        followers_base2 |> select(id, username)) |> 
  mutate(AC_base = 1) |> rename(follower_id = id)

followers_base <- followers_base[!duplicated(followers_base$follower_id), ]

followers1 <- read_parquet('../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/625489039.parquet') 
followers2 <- read_parquet('../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-06-13/collect/1468955884092936200.parquet') 

followers <- rbind(followers1 |> select(id, username),
                   followers2 |> select(id, username)) |> 
  mutate(AC = 1) |> rename(follower_id = id)

followers <- followers[!duplicated(followers$follower_id), ]

df <- df |> left_join(followers, by = c('username', 'follower_id')) |> 
  left_join(followers_base, by = c('username', 'follower_id'))

df <- df |> mutate(AC = ifelse(is.na(AC) == T, 0, AC),
                   AC_base = ifelse(is.na(AC_base) == T, 0, AC_base))

write_parquet(df, paste0('../../data/04-analysis/',country,'/', stage,
                         '/AC_final.parquet'))
# Define the dependent variables
aux <- c('AC')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + AC_base | 
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
  reduce(left_join, by = "treatment") |> filter(treatment != 'AC_base')

# Build matrix
AC <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_AC <- AC[1,]
weak_AC <- AC[2,]
neither_AC <- AC[3,]

coefs_perm <- data.frame(strong_AC, weak_AC, neither_AC)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates.xlsx"))
