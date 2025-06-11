#############################################################################
# RCT: Baseline on Experiment followers Kenya and South Africa
# 
#############################################################################

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

data <- read_parquet(paste0('../../data/04-analysis/',country,
                            '/baseline_features.parquet'))

####################### Balance Normal Vars #################

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0(
  "../../data/04-analysis/", country,"/agg_pvalues_joint_balance.xlsx"
))
pvals_strong <- pvals %>% select(starts_with('strong_'))
pvals_strong <- paste(
  as.character(pvals_strong[1,]),
  collapse =" & "
)
pvals_weak <- pvals %>% select(starts_with('weak_'))
pvals_weak <- paste(
  as.character(pvals_weak[1,]),
  collapse =" & "
)
pvals_neither <- pvals %>% select(starts_with('neither_'))
pvals_neither <- paste(
  as.character(pvals_neither[1,]),
  collapse =" & "
)

aux_baseline <- aux
#aux_endline <- aux_endlinef

# Balance tables 
aux_data <- data[aux_baseline]
lm_list_ols <- list()
count <- 1
for (x in aux_baseline) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

lm_se <- list()
count2 <- 1
for (x in aux_baseline) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
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
  lm_list_ols,
  se = lm_se,
  label = paste0("tab:balance_table_", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var,
  covariate.labels = covariates,
  omit = omit_var,
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 Fixed Effects", rep("Yes", 6)),
                   c("Outcome mean", means ),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)
  ),
  title = title,
  type = "latex"
)  %>%
  star_insert_row(
    insert.after=19,
    paste0(" & ", pvals_neither, " \\\ ")
  ) %>% 
  star_insert_row(
    insert.after=16,
    paste0(" & ", pvals_weak, " \\\ ")
  ) %>% 
  star_insert_row(
    insert.after=13,
    paste0(" & ", pvals_strong, " \\\ ")
  )

note.latex <- paste0("\\multicolumn{8}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each strong, weak, and neither tie as controls.
Robust standard errors are in parentheses. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/balance_table_",country,
                         ".tex"))

################################################################################

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

data <- read_parquet(paste0('../../data/04-analysis/',country,
                            '/baseline_features.parquet')) |> 
  rename(w_reactions_sum = w_reactions,
         w_shares_sum = w_shares,
         w_comments_sum = w_comments,
         log_w_reactions_sum = log_w_reactions,
         log_w_shares_sum = log_w_shares,
         log_w_comments_sum = log_w_comments)

####################### Balance Normal Vars #################

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0(
  "../../data/04-analysis/", country,"/agg_pvalues_joint_balance_l.xlsx"
))
pvals_strong <- pvals %>% select(starts_with('strong_'))
pvals_strong <- paste(
  as.character(pvals_strong[1,]),
  collapse =" & "
)
pvals_weak <- pvals %>% select(starts_with('weak_'))
pvals_weak <- paste(
  as.character(pvals_weak[1,]),
  collapse =" & "
)
pvals_neither <- pvals %>% select(starts_with('neither_'))
pvals_neither <- paste(
  as.character(pvals_neither[1,]),
  collapse =" & "
)

aux_baseline <- aux_log
#aux_endline <- aux_endlinef

# Balance tables 
aux_data <- data[aux_baseline]
lm_list_ols <- list()
count <- 1
for (x in aux_baseline) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

lm_se <- list()
count2 <- 1
for (x in aux_baseline) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
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
  lm_list_ols,
  se = lm_se,
  label = paste0("tab:balance_table_log", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_log,
  covariate.labels = covariates,
  omit = omit_var,
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 Fixed Effects", rep("Yes", 6)),
                   c("Outcome mean", means ),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)
  ),
  title = title,
  type = "latex"
)  %>%
  star_insert_row(
    insert.after=19,
    paste0(" & ", pvals_neither, " \\\ ")
  ) %>% 
  star_insert_row(
    insert.after=16,
    paste0(" & ", pvals_weak, " \\\ ")
  ) %>% 
  star_insert_row(
    insert.after=13,
    paste0(" & ", pvals_strong, " \\\ ")
  )

note.latex <- paste0("\\multicolumn{8}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each strong, weak, and neither tie as controls.
Robust standard errors are in parentheses. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/balance_table_log_",country,
                         ".tex"))

##################################################################################
# Filtered

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

data <- read_parquet(paste0('../../data/04-analysis/',country,
                            '/baseline_features_filter.parquet'), as_tibble = T)

####################### Balance Normal Vars #################

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0(
  "../../data/04-analysis/", country,"/agg_pvalues_joint_balance_filter.xlsx"
))
pvals_strong <- pvals %>% select(starts_with('strong_'))
pvals_strong <- paste(
  as.character(pvals_strong[1,]),
  collapse =" & "
)
pvals_weak <- pvals %>% select(starts_with('weak_'))
pvals_weak <- paste(
  as.character(pvals_weak[1,]),
  collapse =" & "
)
pvals_neither <- pvals %>% select(starts_with('neither_'))
pvals_neither <- paste(
  as.character(pvals_neither[1,]),
  collapse =" & "
)

aux_baseline <- aux
#aux_endline <- aux_endlinef

# Balance tables 
aux_data <- data[aux_baseline]
lm_list_ols <- list()
count <- 1
for (x in aux_baseline) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

lm_se <- list()
count2 <- 1
for (x in aux_baseline) {
  fmla1 <- as.formula(paste0(x, fixed_effects))
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
  lm_list_ols,
  se = lm_se,
  label = paste0("tab:balance_table_filter", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var,
  covariate.labels = covariates,
  omit = omit_var,
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 Fixed Effects", rep("Yes", 6)),
                   c("Outcome mean", means ),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)
  ),
  title = title,
  type = "latex"
)  %>%
  star_insert_row(
    insert.after=19,
    paste0(" & ", pvals_neither, " \\\ ")
  ) %>% 
  star_insert_row(
    insert.after=16,
    paste0(" & ", pvals_weak, " \\\ ")
  ) %>% 
  star_insert_row(
    insert.after=13,
    paste0(" & ", pvals_strong, " \\\ ")
  )

note.latex <- paste0("\\multicolumn{8}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each strong, weak, and neither tie as controls.
Robust standard errors are in parentheses. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/balance_table_filter",country,
                         ".tex"))