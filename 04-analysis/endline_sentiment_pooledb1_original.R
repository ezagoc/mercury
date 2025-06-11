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
# Change country/stage
country <- 'KE'
stage <- 'stage1_2'
# Read df (Aggregated Data Set)
df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_sent_b1.parquet'))

# Define the dependent variables
aux <- c('pos_v_rt_covid', 'pos_v_no_rt_covid', 'neutral_v_rt_covid', 
         'neutral_v_no_rt_covid', 'neg_v_rt_covid', 'neg_v_no_rt_covid',
         'n_posts_rt_covid', 'n_posts_no_rt_covid',
         'pos_v_rt_vax', 'pos_v_no_rt_vax', 'neutral_v_rt_vax', 
         'neutral_v_no_rt_vax', 'neg_v_rt_vax', 'neg_v_no_rt_vax', 
         'n_posts_rt_vax', 'n_posts_no_rt_vax')

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
pos_rt_c <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_pos_rt_c <- pos_rt_c[1,]
weak_pos_rt_c <- pos_rt_c[2,]
neither_pos_rt_c <- pos_rt_c[3,]

pos_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[2]))
strong_pos_no_rt_c <- pos_no_rt_c[1,]
weak_pos_no_rt_c <- pos_no_rt_c[2,]
neither_pos_no_rt_c <- pos_no_rt_c[3,]

neu_rt_c <- coefs_all %>% 
  select(ends_with(aux[3]))
strong_neu_rt_c <- neu_rt_c[1,]
weak_neu_rt_c <- neu_rt_c[2,]
neither_neu_rt_c <- neu_rt_c[3,]

neu_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[4]))
strong_neu_no_rt_c <- neu_no_rt_c[1,]
weak_neu_no_rt_c <- neu_no_rt_c[2,]
neither_neu_no_rt_c <- neu_no_rt_c[3,]

neg_rt_c <- coefs_all %>% 
  select(ends_with(aux[5]))
strong_neg_rt_c <- neg_rt_c[1,]
weak_neg_rt_c <- neg_rt_c[2,]
neither_neg_rt_c <- neg_rt_c[3,]

neg_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[6]))
strong_neg_no_rt_c <- neg_no_rt_c[1,]
weak_neg_no_rt_c <- neg_no_rt_c[2,]
neither_neg_no_rt_c <- neg_no_rt_c[3,]

n_posts_rt_c <- coefs_all %>% 
  select(ends_with(aux[7]))
strong_n_posts_rt_c <- n_posts_rt_c[1,]
weak_n_posts_rt_c <- n_posts_rt_c[2,]
neither_n_posts_rt_c <- n_posts_rt_c[3,]

n_posts_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[8]))
strong_n_posts_no_rt_c <- n_posts_no_rt_c[1,]
weak_n_posts_no_rt_c <- n_posts_no_rt_c[2,]
neither_n_posts_no_rt_c <- n_posts_no_rt_c[3,]

pos_rt_v <- coefs_all %>% 
  select(ends_with(aux[9]))
strong_pos_rt_v <- pos_rt_v[1,]
weak_pos_rt_v <- pos_rt_v[2,]
neither_pos_rt_v <- pos_rt_v[3,]

pos_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[10]))
strong_pos_no_rt_v <- pos_no_rt_v[1,]
weak_pos_no_rt_v <- pos_no_rt_v[2,]
neither_pos_no_rt_v <- pos_no_rt_v[3,]

neu_rt_v <- coefs_all %>% 
  select(ends_with(aux[11]))
strong_neu_rt_v <- neu_rt_v[1,]
weak_neu_rt_v <- neu_rt_v[2,]
neither_neu_rt_v <- neu_rt_v[3,]

neu_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[12]))
strong_neu_no_rt_v <- neu_no_rt_v[1,]
weak_neu_no_rt_v <- neu_no_rt_v[2,]
neither_neu_no_rt_v <- neu_no_rt_v[3,]

neg_rt_v <- coefs_all %>% 
  select(ends_with(aux[13]))
strong_neg_rt_v <- neg_rt_v[1,]
weak_neg_rt_v <- neg_rt_v[2,]
neither_neg_rt_v <- neg_rt_v[3,]

neg_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[14]))
strong_neg_no_rt_v <- neg_no_rt_v[1,]
weak_neg_no_rt_v <- neg_no_rt_v[2,]
neither_neg_no_rt_v <- neg_no_rt_v[3,]

n_posts_rt_v <- coefs_all %>% 
  select(ends_with(aux[15]))
strong_n_posts_rt_v <- n_posts_rt_v[1,]
weak_n_posts_rt_v <- n_posts_rt_v[2,]
neither_n_posts_rt_v <- n_posts_rt_v[3,]

n_posts_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[16]))
strong_n_posts_no_rt_v <- n_posts_no_rt_v[1,]
weak_n_posts_no_rt_v <- n_posts_no_rt_v[2,]
neither_n_posts_no_rt_v <- n_posts_no_rt_v[3,]


coefs_perm <- data.frame(strong_pos_rt_c, weak_pos_rt_c, neither_pos_rt_c,
                         strong_pos_no_rt_c, weak_pos_no_rt_c, neither_pos_no_rt_c,
                         strong_neu_rt_c, weak_neu_rt_c, neither_neu_rt_c,
                         strong_neu_no_rt_c, weak_neu_no_rt_c, neither_neu_no_rt_c,
                         strong_neg_rt_c, weak_neg_rt_c, neither_neg_rt_c,
                         strong_neg_no_rt_c, weak_neg_no_rt_c, neither_neg_no_rt_c,
                         strong_n_posts_rt_c, weak_n_posts_rt_c, neither_n_posts_rt_c,
                         strong_n_posts_no_rt_c, weak_n_posts_no_rt_c, neither_n_posts_no_rt_c,
                         strong_pos_rt_v, weak_pos_rt_v, neither_pos_rt_v,
                         strong_pos_no_rt_v, weak_pos_no_rt_v, neither_pos_no_rt_v,
                         strong_neu_rt_v, weak_neu_rt_v, neither_neu_rt_v,
                         strong_neu_no_rt_v, weak_neu_no_rt_v, neither_neu_no_rt_v,
                         strong_neg_rt_v, weak_neg_rt_v, neither_neg_rt_v,
                         strong_neg_no_rt_v, weak_neg_no_rt_v, neither_neg_no_rt_v,
                         strong_n_posts_rt_v, weak_n_posts_rt_v, neither_n_posts_rt_v,
                         strong_n_posts_no_rt_v, weak_n_posts_no_rt_v, neither_n_posts_no_rt_v)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates_sentiment_b1.xlsx"))
