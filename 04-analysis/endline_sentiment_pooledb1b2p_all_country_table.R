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
df_KE <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                             '/endline_sent_b1_b2_p.parquet'))
df_SA <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                             '/endline_sent_b1_b2_p.parquet'))

data <- rbind(df_KE, df_SA)

data <- data |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)

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
  weeks = 'Week 9-12'}

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0("../../data/04-analysis/", country, 
                          "/",stage,"/agg_pvalues_sentiment_b1b2p_all.xlsx"))

pvals <- paste(as.character(pvals[1,]), collapse =" & ")

# Endline Results: use felm so that we can use stargazer
# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                             x, "_base  | total_influencers"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

# Get the Robust S.E.s
lm_se <- list()
count2 <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                             x, "_base  | total_influencers"))
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
  label = paste0("tab:endline_table_sent_b1b2p_all_",stage),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = c('Total Treated'),
  omit = c('total_influencers', paste0(aux, '_base')),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Baseline control", rep("Yes", 16)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel C, Pooled Sentiment Analysis Results (Pilot, Batch 1 and 2) for the SMI Treatment in  Both Countries', 
                 '(',weeks,')'),
  type = "latex") %>% 
  star_insert_row(insert.after=13, paste0(" & ", pvals, " \\\ "))

note.latex <- paste0("\\multicolumn{17}{l} {\\parbox[t]{24cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each 
strong, weak, and neither tie as controls. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/", country,
                         "/endline_table_sent_b1b2p_all_", stage,".tex"))