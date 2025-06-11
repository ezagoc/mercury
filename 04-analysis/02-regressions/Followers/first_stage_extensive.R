# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# 2.0 Define constants
country <- 'joint'
data_type <- 'Followers'
file_code <- 'first_stage_extensive'
ini <- '../../../../data/04-analysis/joint/'

blocks_ke <- read_parquet(paste0('../../../../data/04-analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../../../data/04-analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe) 

blocks <- rbind(blocks_ke, blocks_sa)

df <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                    initial_path = '../../../../') |> 
  filter(n_posts_base > 0) |> select(username:strat_block2, batch_id, pais) 

df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                   total_influencers = c_t_strong_total + c_t_weak_total + 
                     c_t_neither_total) |> filter(total_influencers == 1) |> 
  left_join(blocks, by = c('follower_id', 'pais', 'batch_id'))

df_s <- read_parquet('../../../../data/06-other/1-retweeters/aggregated/RTs_counts_smi.parquet') |> 
  select( follower_id = id, RTs_smi_treatment)

df_a <- read_parquet('../../../../data/03-experiment/ads/2-retweets/aggregated/RTs_counts_ads.parquet') |> 
  select(follower_id = id, RTs_ads_treatment)

df <- df |> left_join(df_s, by = 'follower_id') |> 
  left_join(df_a, by = 'follower_id')

df <- df |> mutate(ads = ifelse(is.na(RTs_ads_treatment) == T, 0,
                                RTs_ads_treatment), 
                   smi = ifelse(is.na(RTs_smi_treatment) == T, 0,
                                RTs_smi_treatment), 
                   dummy_ads = ifelse(ads>0, 1, 0), 
                   dummy_smi = ifelse(smi>0, 1, 0), 
                   log_ads = log(ads + 1), log_smi = log(smi + 1))

df_p_c <- df |> filter(ads_treatment == 0 & total_treated == 0)

means <- c(mean(df_p_c$dummy_ads), mean(df_p_c$ads), mean(df_p_c$dummy_smi), 
           mean(df_p_c$smi))

aux <- c('dummy_ads', 'ads', 'dummy_smi', 'smi')

### Run the normal estimates and permutations: 

# 4.0 Run original estimates
lm_list_ols <- list()
count <- 1
for (x in aux) {
  
  fmla1 <- as.formula(paste0(x, "~ total_treated | block1_fe"))
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs) |>
    filter(treatment == 'total_treated')
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

# Build matrix
dac <- coefs_all %>% 
  select(ends_with(aux[1]))
dac <- dac[1,]

ac <- coefs_all %>% 
  select(starts_with(aux[2]))
ac <- ac[1,]

dsmi <- coefs_all %>% 
  select(ends_with(aux[3]))
dsmi <- dsmi[1,]

smi <- coefs_all %>% 
  select(starts_with(aux[4]))
smi <- smi[1,]

coefs_perm <- data.frame(dac, ac, dsmi, smi)

write_xlsx(
  coefs_perm, paste0("../../../../data/04-analysis/",country,
                     "/",data_type,"/original/", file_code,
                     ".xlsx"))
### 5.0 Run 1000 Permutations: 

i <- 1
coefs_fin <- tibble()
for (m in 1:1000){
  print(i) 
  followers <- read_parquet(paste0("../../../../data/04-analysis/joint/",
                                   'small_ties_b1b2p', "/small_tie", 
                                   i,".parquet"))
  
  data <- df
  
  c1 = paste0("n_influencers_followed_control_no_weak_tie_p", i)
  c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", i)
  c3 = paste0("n_influencers_followed_treatment_weak_tie_p", i)
  c4 = paste0("n_influencers_followed_control_weak_tie_p", i)
  c5 = paste0("n_influencers_followed_control_strong_tie_p", i)
  c6 = paste0("n_influencers_followed_treatment_strong_tie_p", i)
  c7 = paste0("n_influencers_followed_control_no_strong_tie_p", i)
  c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", i)
  c9 = paste0("n_influencers_followed_control_p", i)
  c10 = paste0("n_influencers_followed_treatment_p", i)
  c11 = paste0("n_influencers_followed_p_", i)
  
  followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                         c5, c6, c7, c8, c9, c10, c11, pais, 
                                         batch_id)
  
  data <- left_join(
    data, 
    followers_iter,
    by = c('follower_id', 'pais', 'batch_id')
  )
  # Pool treatment variables
  data <- poolTreatmentBalance2(data, c10, c11)
  
  # Balance tables 
  aux_data <- data[aux]
  coefs_list <- list()
  lm_list_ols <- list()
  count <- 1
  for (au in aux) {
    fmla1 <- as.formula(paste0(au, "~ total_treated | block1_fe"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, feols(fmla1, data = data))
    coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
      select(Estimate)
    names(coefs) <- paste0(au)
    coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
      filter(treatment != paste0(au, '_base'))
    rownames(coefs) <- 1:nrow(coefs)
    lm_list_ols[[count]] <- coefs
    count <- count + 1
  }
  coefs_list <- append(coefs_list, lm_list_ols)
  coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
  
  # Build matrix
  dac <- coefs_all %>% 
    select(ends_with(aux[1]))
  dac <- dac[1,]
  
  ac <- coefs_all %>% 
    select(starts_with(aux[2]))
  ac <- ac[1,]
  
  dsmi <- coefs_all %>% 
    select(ends_with(aux[3]))
  dsmi <- dsmi[1,]
  
  smi <- coefs_all %>% 
    select(starts_with(aux[4]))
  smi <- smi[1,]
  
  coefs_perm <- data.frame(dac, ac, dsmi, smi)
  
  coefs_fin <- rbind(coefs_fin, coefs_perm)
  
  
  i <- i + 1}
write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", data_type,
                             "/permutations/", file_code, ".xlsx"))


coefs <- readxl::read_excel(paste0("../../../../data/04-analysis/",country,
                                   "/",data_type,"/original/", file_code,
                                   ".xlsx")) |>
  pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef')

perm <- readxl::read_excel(paste0("../../../../data/04-analysis/joint/", data_type,
                                  "/permutations/", file_code, ".xlsx")) |> 
  summarise(across(everything(), ~sd(.x))) |> 
  pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd')

final <- coefs |> left_join(perm, by = c( 'var'))

final <- final |> mutate(ci_1 = coef + 1.645 * sd, ci_1_min = coef - 1.645 * sd, 
                         ci_05 = coef + 1.96 * sd, ci_05_min = coef - 1.96 * sd, 
                         ci_01 = coef + 2.575 * sd, ci_01_min = coef - 2.575 * sd)

ses <- final$sd

ses <- paste(as.character(ses), collapse =" & ")

m <- 'smi, dsmi significant at the 1% sig, ads, dads not significant'


count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ total_treated | block1_fe"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = df))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

table <- stargazer(
  lm_list_ols, # felm regressions
  #se = lm_se, # robust standard errors
  label = paste0("tab:ads_ext"),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = c('Retweeted an Ad', 'Number of Retweeted Ads', 'Retweeted an SMI Post', 
                    'Number of Retweeted SMI Posts'),
  covariate.labels = "Treated",
  keep = c('total_treated'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Baseline control", rep("No", 4)), 
                   c("Block1 FEs", rep("Yes", 4)), 
                   c("Pure control mean", round(means, 4))
  ),
  title = 'First Stage, Extensive Margin Analysis',
  type = "latex")

note.latex <- paste0("\\multicolumn{4}{l} {\\parbox[t]{11cm}{ \\textit{Notes:} 
This table presents the estimates from the Intensive Margin analysis. Real SDs are ",ses, ' ',m , 
                     " The dependent variables are a dummy indicating whether users
followed Africa Check at the end of the intervention and the number of
social media influencers (SMIs) they followed at the end of the intervention. The models control for whether users followed Africa Check at the
beginning of the intervention and the number of SMIs they followed at
the beginning. This analysis focuses exclusively on Batch 1 followers that follow only 1 influencer, as
endpoint data for Batch 2 followers was unavailable. We do not consider followers that followed an influencer who got their account suspended.
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../../results/01-regression_graphs/",data_type,"/extensive_first_stage.tex"))