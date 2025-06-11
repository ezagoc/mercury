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

country <- 'SA'
stage <- 'stage6'

data <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/endline_final.parquet'))

data <- data |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

data <- data |> mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, 
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
                       verifiability_no_rt_f_base = verifiability_no_rt_base,
                       verifiability_rt_f_base = verifiability_rt_base, 
                       true_rt_f_base = true_rt_base, 
                       true_no_rt_f_base = true_no_rt_base,
                       n_posts_rt_f_base = n_posts_rt_base,
                       n_posts_no_rt_f_base = n_posts_no_rt_base) 

# Define the dependent variables
aux <- c('verifiability_rt', 'verifiability_rt_f', 'true_rt', 'true_rt_f',
         'n_posts_rt', 'n_posts_rt_f', 'verifiability_no_rt', 
         'verifiability_no_rt_f', 'true_no_rt', 'true_no_rt_f', 'n_posts_no_rt',
         'n_posts_no_rt_f')

dep_var_new <- c("\\shortstack{Verifiable \\\\ RTs}", 
                 "\\shortstack{Verifiable \\\\ RTs}",
                 "\\shortstack{True \\\\ RTs}",
                 "\\shortstack{True \\\\ RTs}",
                 "\\shortstack{Total \\\\ RTs}",
                 "\\shortstack{Total \\\\ RTs}",
                 "\\shortstack{Verifiable \\\\ Posts}",
                 "\\shortstack{Verifiable \\\\ Posts}",
                 "\\shortstack{True \\\\ Posts}",
                 "\\shortstack{True \\\\ Posts}",
                 "\\shortstack{Total \\\\ Posts}",
                 "\\shortstack{Total \\\\ Posts}")


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
  label = paste0("tab:endline_ads_f_",stage,"", country),
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
  add.lines = list(c('Baseline Filtered', 'No', 'Yes', 'No', 'Yes', 'No', 'Yes',
                     'No', 'Yes', 'No', 'Yes', 'No'),
                   c("Strat. Block1 FE", rep("Yes", 12)),
                   c("Baseline control", rep("Yes", 12)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel B, Endline Results for the Ads Treatment in ', country,
                 ' (Weeks 1-2)'),
  type = "latex")

note.latex <- paste0("\\multicolumn{13}{l} {\\parbox[t]{19cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/endline_ads_f_", stage,"_", 
                         country,".tex"))