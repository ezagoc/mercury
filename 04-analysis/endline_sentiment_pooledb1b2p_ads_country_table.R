rm(list = ls())
library("purrr")

src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
library(lfe)
library(fixest)

country <- 'joint'
stage <- 'stage1_2'
# Read df (Aggregated Data Set)
df_KE <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                             '/endline_sent_b1_b2_p.parquet'))

df_KE <- df_KE |> mutate(strat_block1 = paste0(strat_block1, 'KE', batch_id))

df_SA <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                             '/endline_sent_b1_b2_p.parquet'))

df_SA <- df_SA |> mutate(strat_block1 = paste0(strat_block1, 'SA', batch_id))

data <- rbind(df_KE, df_SA)

# Define the dependent variables
aux <- c('pos_v_rt_covid', 'pos_v_no_rt_covid', 'neutral_v_rt_covid', 
         'neutral_v_no_rt_covid', 'neg_v_rt_covid', 'neg_v_no_rt_covid',
         'n_posts_rt_covid', 'n_posts_no_rt_covid',
         'pos_v_rt_vax', 'pos_v_no_rt_vax', 'neutral_v_rt_vax', 
         'neutral_v_no_rt_vax', 'neg_v_rt_vax', 'neg_v_no_rt_vax', 
         'n_posts_rt_vax', 'n_posts_no_rt_vax')

dep_var_new <- c("\\shortstack{Positive \\\\ COVID \\\\ RTs}", 
                 "\\shortstack{Positive \\\\ COVID \\\\ Posts}",
                 "\\shortstack{Neutral \\\\ COVID \\\\ RTs}",
                 "\\shortstack{Neutral \\\\ COVID \\\\ Posts}",
                 "\\shortstack{Negative \\\\ COVID \\\\ RTs}",
                 "\\shortstack{Negative \\\\ COVID \\\\ Posts}",
                 "\\shortstack{Total \\\\ COVID \\\\ RTs}",
                 "\\shortstack{Total \\\\  COVID \\\\ Posts}",
                 "\\shortstack{Positive \\\\ Vaccine \\\\ RTs}", 
                 "\\shortstack{Positive \\\\ Vaccine \\\\ Posts}",
                 "\\shortstack{Neutral \\\\ Vaccine \\\\ RTs}",
                 "\\shortstack{Neutral \\\\ Vaccine \\\\ Posts}",
                 "\\shortstack{Negative \\\\ Vaccine \\\\ RTs}",
                 "\\shortstack{Negative \\\\ Vaccine \\\\ Posts}",
                 "\\shortstack{Total \\\\ Vaccine \\\\ RTs}",
                 "\\shortstack{Total \\\\ Vaccine \\\\ Posts}")

if (stage == 'stage1_2'){
  weeks = 'Week 1-4'
}else if (stage == 'stage3_4'){
  weeks = 'Week 5-8'
}else if (stage == 'stage5_6'){
  weeks = 'Week 9-12'
}

aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ ads_treatment + ", x, "_base  | strat_block1"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

# Get the Robust S.E.s
lm_se <- list()
count2 <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ ads_treatment + ", x, "_base  | strat_block1"))
  nam1 <- paste("lm_", count2, "_se", sep = "")
  assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(
  lm_list_ols, # felm regressions
  se = lm_se, # robust standard errors
  label = paste0("tab:endline_ads_sent_b1b2p_",stage,"", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = "Ads Treatment",
  omit = c('strat_block1', paste0(aux, '_base')),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 FE", rep("Yes", 16)),
                   c("Baseline control", rep("Yes", 16)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel B, Sentiment Analysis Results (Pilot, Batch 1 and 2) for the Ads Treatment for Both Countries (', 
                 weeks,')'),
  type = "latex")

note.latex <- paste0("\\multicolumn{17}{l} {\\parbox[t]{23cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/",country,"/endline_ads_sent_b1b2p_", 
                         stage,".tex"))