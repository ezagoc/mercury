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

country <- 'KE'
stage <- 'BalanceVer'
data <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

data <- data |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

####################### #################

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0("../../data/04-analysis/", country, 
                               "/BalanceVer/agg_pvalues_endline.xlsx"))

pvals_strong <- pvals %>% select(starts_with('strong_'))
pvals_strong <- paste(as.character(pvals_strong[1,]),collapse =" & ")

pvals_weak <- pvals %>% select(starts_with('weak_'))
pvals_weak <- paste(as.character(pvals_weak[1,]), collapse =" & ")

pvals_neither <- pvals %>% select(starts_with('neither_'))
pvals_neither <- paste(as.character(pvals_neither[1,]), collapse =" & ")


reg1 <- felm(ver_dummy ~ t_strong + t_weak + t_neither  | 
               c_t_strong_total + c_t_weak_total + c_t_neither_total, 
             data = data)
reg1_se <- feols(ver_dummy ~ t_strong + t_weak + t_neither  | 
                   c_t_strong_total + c_t_weak_total + c_t_neither_total, 
                 vcov = 'HC1', data = data)

# Endline Results: use felm so that we can use stargazer

aux_data <- tibble(data$ver_dummy)
means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer( # felm regressions
  reg1,  
  se = list(reg1_se$se), # robust standard errors
  label = paste0("tab:balance_ver_table_", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = '\\shortstack{Verifiable \\\\ Dummy}',
  covariate.labels = c("Treated strong ties", "Treated weak ties",
                       "Treated absent ties"),
  omit = c(omit_var),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel A, Balance Baseline Verifiability Dummy for the SMI Treatment in ', country),
  type = "latex")  %>% 
  star_insert_row(insert.after=19, paste0(" & ", pvals_neither, " \\\ ")) %>% 
  star_insert_row(insert.after=16, paste0(" & ", pvals_weak, " \\\ ")) %>% 
  star_insert_row(insert.after=13, paste0(" & ", pvals_strong, " \\\ "))

note.latex <- paste0("\\multicolumn{3}{l} {\\parbox[t]{8cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each 
strong, weak, and neither tie as controls. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/balance_ver_table_", 
                         country,".tex"))