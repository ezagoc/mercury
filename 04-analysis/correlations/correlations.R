rm(list = ls())
library(lfe)
library(fixest)
library(tidyverse)
library(arrow)
library(stargazer)
library(DescTools)

####

data <- read_parquet( '../../../data/04-analysis/joint/correlations/df_analysis.parquet')

data <- data |> mutate(across(c(n_posts_no_rt.x, n_posts_rt.x, 
                                verifiability_no_rt, verifiability_rt, true_rt,
                                true_no_rt, fake_rt, fake_no_rt), 
                              ~Winsorize(.x, na.rm = T)))

data_log <- data |> mutate(across(c(n_posts_no_rt.x, n_posts_rt.x, 
                                    verifiability_no_rt, verifiability_rt), 
                                  ~log(.x + 1)))

### Verify 1

aux <- rep(paste0(c('verify_1_'), c(1:5)), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ verifiability_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ verifiability_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ verifiability_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ verifiability_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
  se = lm_se, # robust standard errors
  label = paste0("tab:corr_ver1"),
  header = FALSE,
  font.size = "tiny",
  dep.var.caption = "",
  omit = c('Constant'),
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = rep(c('Never', 'Rarely', 'Sometimes', 'Often', 'Always'), 2),
  covariate.labels = c("Verifiability Posts", 'Verifiability RTs'),
  omit.stat=c("f", "ser", "adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Outcome mean", means, means),
                   c("Outcome std. dev.", sds, sds),
                   c("Outcome range", range, range)),
  title = paste0('verify1: When you receive content from platforms like WhatsApp, Facebook, and Twitter, 
                 how often do you try to verify whether it is true or fake?'),
  type = "latex")

note.latex <- paste0("\\multicolumn{11}{l} {\\parbox[t]{17cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/verify_1.tex"))


#
### Verify 1 Log

aux <- rep(paste0(c('verify_1_'), c(1:5)), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_no_rt + 1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_rt + 1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_no_rt + 1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_rt + 1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_log_ver1"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Never', 'Rarely', 'Sometimes', 'Often', 'Always'), 2),
                   covariate.labels = c("Log Verifiability Posts", 'Log Verifiability RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('verify1: When you receive content from platforms like WhatsApp, Facebook, and Twitter, 
                 how often do you try to verify whether it is true or fake?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{11}{l} {\\parbox[t]{17cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/log_verify_1.tex"))

#####

# Verify 2

aux <- rep(c('num_right_ver2', 'num_wrong_ver2', 'D_ver2'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ verifiability_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ verifiability_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ verifiability_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ verifiability_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_ver2"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("Verifiability Posts", 'Verifiability RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('verify2: If you were to verify whether content is true or false, 
                                  which of the following would help you determine if the content is true or fake? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/verify_2.tex"))

## Ver 2

# Verify 2

aux <- rep(c('num_right_ver2', 'num_wrong_ver2', 'D_ver2'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_no_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_no_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:log_corr_ver2"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("Log Verifiability Posts", 'Log Verifiability RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('verify2: If you were to verify whether content is true or false, 
                                  which of the following would help you determine if the content is true or fake? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/log_verify_2.tex"))

#### Verify 3

aux <- rep(c('num_right_ver3', 'num_wrong_ver3', 'D_ver3'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ verifiability_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ verifiability_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ verifiability_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ verifiability_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_ver3"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("Verifiability Posts", 'Verifiability RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('verify3: Which of the following are strategies that professional
                                  fact checkers use to verify news? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/verify_3.tex"))


###

aux <- rep(c('num_right_ver3', 'num_wrong_ver3', 'D_ver3'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_no_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_no_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(verifiability_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:log_corr_ver3"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("Log Verifiability Posts", 
                                        'Log Verifiability RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('verify3: Which of the following are strategies that professional
                                  fact checkers use to verify news? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/log_verify_3.tex"))

# Misinfor:

aux <- rep(c('num_right_mis3', 'num_wrong_mis3', 'D_mis3'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ true_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ true_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ true_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ true_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_mis_true"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("True Posts", 'True RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('misinfor_3: Which types of posts on social media are more likely to contain
                                  misinformation or be fake news? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/misinfor_true.tex"))

####

aux <- rep(c('num_right_mis3', 'num_wrong_mis3', 'D_mis3'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(true_no_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(true_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(true_no_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(true_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:log_corr_mis_true"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("True Posts", 'True RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('misinfor_3: Which types of posts on social media are more likely to contain
                                  misinformation or be fake news? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/log_misinfor_true.tex"))

### Log

aux <- rep(c('num_right_mis3', 'num_wrong_mis3', 'D_mis3'), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(fake_no_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(fake_rt+1)"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ log(fake_no_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ log(fake_rt+1)"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:log_corr_mis_fake"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Correct', 'Wrong', 'Division'), 2),
                   covariate.labels = c("Log Fake Posts", 'Log Fake RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('misinfor_3: Which types of posts on social media are more likely to contain
                                  misinformation or be fake news? Select all that apply.'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/log_misinfor_fake.tex"))


### Sharing Behaviour:

### Verify 1

aux <- rep(paste0(c('csb_2_'), c(1:5)), 2)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ n_posts_no_rt.x"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ n_posts_rt.x"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ n_posts_no_rt.x"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else{
    fmla1 <- as.formula(paste0(x, "~ n_posts_rt.x"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_share1"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Never', 'Rarely', 'Sometimes', 'Often', 'Always'), 2),
                   covariate.labels = c("Number of Posts", 'Number of RTs'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('When someone shares a news story with you that they saw on platforms like WhatsApp,
                                  Facebook, Twitter, or Youtube, how often do you share it with others?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{11}{l} {\\parbox[t]{17cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/sharing_1.tex"))

#################

aux <- rep(c('d_COVID_Q1', 'd_COVID_Q2', 'd_COVID_Q3'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Q1', 'Q2', 'Q3'), 3),
                   covariate.labels = c("Positive COVID RT", 'Neutral COVID RT',
                                        'Negative COVID RT'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q1, COVID Q2, COVID Q3'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
COVID Q1: To what extent do you agree with the following statement: 
'COVID-19 is not a real disease and a serious health problem in Kenya. 
It is a fake disease made up by powerful actors to fool and harm Kenyans.' 
COVID Q2: To what extent do you agree with the following statement: 
'COVID-19 vaccines have not been rigorously tested to ensure that they are safe. 
Some safety protocols were circumvented to fast track the authorization of COVID-19 vaccine for use.'
COVID Q3: To what extent do you agree with the following statement:
'mRNA COVID-19 vaccines like Pfizer and Moderna change a person's DNA.'
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covid.tex"))

###################

aux <- rep(c('d_COVID_Q1', 'd_COVID_Q2', 'd_COVID_Q3'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Q1', 'Q2', 'Q3'), 3),
                   covariate.labels = c("Positive COVID Posts", 'Neutral COVID Posts',
                                        'Negative COVID Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q1, COVID Q2, COVID Q3'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
COVID Q1: To what extent do you agree with the following statement: 
'COVID-19 is not a real disease and a serious health problem in Kenya. 
It is a fake disease made up by powerful actors to fool and harm Kenyans.' 
COVID Q2: To what extent do you agree with the following statement: 
'COVID-19 vaccines have not been rigorously tested to ensure that they are safe. 
Some safety protocols were circumvented to fast track the authorization of COVID-19 vaccine for use.'
COVID Q3: To what extent do you agree with the following statement:
'mRNA COVID-19 vaccines like Pfizer and Moderna change a person's DNA.'
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covidposts.tex"))

####### COVID Q6

aux <- rep(c('COVID_Q6_1', 'COVID_Q6_2', 'COVID_Q6_3', 'COVID_Q6_4', 'COVID_Q6_5'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid6"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Not at all', 'A little bit', 
                                         'Somewhat', 'Mostly', 'Completely'), 3),
                   covariate.labels = c("Positive COVID RT", 'Neutral COVID RT',
                                        'Negative COVID RT'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q6: To what extent do you believe that the COVID-19 vaccine currently available 
                                  at a location near you is safe and effective?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{16}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covid6.tex"))


###############

aux <- rep(c('COVID_Q6_1', 'COVID_Q6_2', 'COVID_Q6_3', 'COVID_Q6_4', 'COVID_Q6_5'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid6posts"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Not at all', 'A little bit', 
                                         'Somewhat', 'Mostly', 'Completely'), 3),
                   covariate.labels = c("Positive COVID Posts", 'Neutral COVID Posts',
                                        'Negative COVID Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q6: To what extent do you believe that the COVID-19 vaccine currently available 
                                  at a location near you is safe and effective?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{16}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covid6posts.tex"))
