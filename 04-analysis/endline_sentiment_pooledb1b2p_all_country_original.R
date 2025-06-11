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
country <- 'joint'
stage <- 'stage1_2'
# Read df (Aggregated Data Set)
df <- rbind(read_parquet(paste0('../../data/04-analysis/KE/', stage,
                                '/endline_sent_b1_b2_p.parquet')), 
            read_parquet(paste0('../../data/04-analysis/SA/', stage,
                                '/endline_sent_b1_b2_p.parquet')))

df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                   total_influencers = c_t_strong_total + c_t_weak_total + 
                     c_t_neither_total)

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
  fmla1 <- as.formula(paste0(x, "~ total_treated +  ",
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

# Build matrix
pos_rt_c <- coefs_all %>% 
  select(ends_with(aux[1]))
pos_rt_c <- pos_rt_c[1,]

pos_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[2]))
pos_no_rt_c <- pos_no_rt_c[1,]

neu_rt_c <- coefs_all %>% 
  select(ends_with(aux[3]))
neu_rt_c <- neu_rt_c[1,]

neu_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[4]))
neu_no_rt_c <- neu_no_rt_c[1,]

neg_rt_c <- coefs_all %>% 
  select(ends_with(aux[5]))
neg_rt_c <- neg_rt_c[1,]

neg_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[6]))
neg_no_rt_c <- neg_no_rt_c[1,]

n_posts_rt_c <- coefs_all %>% 
  select(ends_with(aux[7]))
n_posts_rt_c <- n_posts_rt_c[1,]

n_posts_no_rt_c <- coefs_all %>% 
  select(ends_with(aux[8]))
n_posts_no_rt_c <- n_posts_no_rt_c[1,]

pos_rt_v <- coefs_all %>% 
  select(ends_with(aux[9]))
pos_rt_v <- pos_rt_v[1,]

pos_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[10]))
pos_no_rt_v <- pos_no_rt_v[1,]

neu_rt_v <- coefs_all %>% 
  select(ends_with(aux[11]))
neu_rt_v <- neu_rt_v[1,]

neu_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[12]))
neu_no_rt_v <- neu_no_rt_v[1,]


neg_rt_v <- coefs_all %>% 
  select(ends_with(aux[13]))
neg_rt_v <- neg_rt_v[1,]

neg_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[14]))
neg_no_rt_v <- neg_no_rt_v[1,]

n_posts_rt_v <- coefs_all %>% 
  select(ends_with(aux[15]))
n_posts_rt_v <- n_posts_rt_v[1,]

n_posts_no_rt_v <- coefs_all %>% 
  select(ends_with(aux[16]))
n_posts_no_rt_v <- n_posts_no_rt_v[1,]

coefs_perm <- data.frame(pos_rt_c, pos_no_rt_c, neu_rt_c, neu_no_rt_c, neg_rt_c, 
                         neg_no_rt_c, n_posts_rt_c, n_posts_no_rt_c, pos_rt_v, 
                         pos_no_rt_v, neu_rt_v, neu_no_rt_v, neg_rt_v, 
                         neg_no_rt_v, n_posts_rt_v, n_posts_no_rt_v)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates_sentiment_b1b2p_all_country.xlsx"))
