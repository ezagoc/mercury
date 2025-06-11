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

data_smis <- read_parquet(paste0('../../data/04-analysis/',country,
                                 '/SMIs/SMIs_final.parquet'))

data_ac <- read_parquet(paste0('../../data/04-analysis/',country,
                               '/AC/AC_final.parquet'))

# Define the dependent variables

dep_var_new <- c("\\shortstack{Follows Africa\\\\ Check}", 
                 "\\shortstack{Number of Influencers\\\\ Followed}")

## Run the regressions:

reg1 <- felm(AC ~ ads_treatment + AC_base  | 
               strat_block1, 
             data = data_ac)
reg1_se <- feols(AC ~ ads_treatment + AC_base  | 
                   strat_block1, 
                 vcov = 'HC1', data = data_ac)

reg2 <- felm(SMIs ~ ads_treatment  | 
               strat_block1, 
             data = data_smis)
reg2_se <- feols(SMIs ~ ads_treatment  | 
                   strat_block1, 
                 vcov = 'HC1', data = data_smis)

# Endline Results: use felm so that we can use stargazer

aux_data <- tibble(data_ac$AC, data_smis$SMIs)
means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(
  reg1, reg2, 
  se = list(reg1_se$se, reg2_se$se), # robust standard errors
  label = paste0("tab:followers_ads_", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = "Ads Treatment",
  omit = c('strat_block1', 'AC_base'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 FE", rep("Yes", 2)),
                   c("Baseline control", rep("Yes", 2)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel B, Endline Results for the Ads Treatment in ', country),
  type = "latex")

note.latex <- paste0("\\multicolumn{3}{l} {\\parbox[t]{8cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/followers_ads_",
                         country,".tex"))