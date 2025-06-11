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
file_code <- 'intensive'
ini <- '../../../../data/04-analysis/joint/'

fes <- read_parquet('../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet')

df <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                    initial_path = '../../../../') |> 
  filter(n_posts_base > 0) |> select(username:strat_block2, batch_id, pais) |> 
  left_join(fes, by = c('follower_id', 'batch_id', 'pais'))

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

df <- df |> mutate(strat_block1 = paste0(strat_block1, pais), 
                   strat_block2 = paste0(strat_block2, pais))

df <- df |> mutate(total_treated = t_strong + t_weak + t_neither)

df_p_c <- df |> filter(ads_treatment == 0 & total_treated == 0)

means <- c(mean(df_p_c$dummy_ads), mean(df_p_c$ads), mean(df_p_c$dummy_smi), 
           mean(df_p_c$smi))
# Get the Robust S.E.s

aux <- c('dummy_ads', 'ads', 'dummy_smi', 'smi')

lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ ads_treatment | strat_block1"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = df))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

# Get the Robust S.E.s
lm_se <- list()
count2 <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ ads_treatment | strat_block1"))
  nam1 <- paste("lm_", count2, "_se", sep = "")
  assign(nam1, feols(fmla1, vcov = 'HC1', data = df))
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

table <- stargazer(
  lm_list_ols, # felm regressions
  se = lm_se, # robust standard errors
  label = paste0("tab:ads_followers"),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = c('Retweeted an Ad', 'Number of Retweeted Ads', 'Retweeted an SMI Post', 
                    'Number of Retweeted SMI Posts'),
  covariate.labels = "Ads Treatment",
  keep = c('ads_treatment'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Baseline control", rep("No", 4)), 
                   c("Strat. Block1 FE", rep("Yes", 4)), 
                   c("Outcome mean", round(means, 4))
  ),
  title = 'First Stage, X Ads Treatment',
  type = "latex")

note.latex <- paste0("\\multicolumn{4}{l} {\\parbox[t]{11cm}{ \\textit{Notes:} 
This table presents the estimates from the Twitter Ads intervention. The dependent variables are a dummy indicating whether users
followed Africa Check at the end of the intervention and the number of
social media influencers (SMIs) they followed at the end of the intervention. The models control for whether users followed Africa Check at the
beginning of the intervention and the number of SMIs they followed at
the beginning. This analysis focuses exclusively on Batch 1 followers that follow only 1 influencer, as
endpoint data for Batch 2 followers was unavailable. We do not consider followers that followed an influencer who got their account suspended.
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../../results/01-regression_graphs/",data_type,"/ads_first_stage.tex"))