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
stage <- 'BalanceVer'
data <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/endline_final.parquet'))

data <- data |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

reg1 <- felm(ver_dummy ~ ads_treatment  | 
               strat_block1, 
             data = data)
reg1_se <- feols(ver_dummy ~ ads_treatment  | 
                   strat_block1, 
                 vcov = 'HC1', data = data)

aux_data <- tibble(data$ver_dummy)
means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(
  reg1, 
  se = list(reg1_se$se), # robust standard errors
  label = paste0("tab:balance_ver_ads_", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = '\\shortstack{Verifiable \\\\ Dummy}',
  covariate.labels = "Ads Treatment",
  omit = c('strat_block1'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 FE", rep("Yes", 2)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel B, Balance Baseline Verifiability Dummy for the Ads Treatment in ', country),
  type = "latex")

note.latex <- paste0("\\multicolumn{2}{l} {\\parbox[t]{6cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/balance_ver_ads_",
                         country,".tex"))