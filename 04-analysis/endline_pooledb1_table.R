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
stage <- 'stage3_4'

df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_b1.parquet'))

fake <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                            '/endline_fake_b1.parquet'))

df <- df |> left_join(fake, by = c('follower_id', 'batch_id'))

df <- df |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

df <- df |> mutate(verifiability_rt_f = ifelse(ver_dummy == 0, NA, 
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
                   fake_rt_f = ifelse(ver_dummy == 0, NA, 
                                      fake_rt),
                   fake_no_rt_f = ifelse(ver_dummy == 0, NA, 
                                         fake_no_rt),
                   fake_rt_f_base = fake_rt_base,
                   fake_no_rt_f_base = fake_no_rt_base,
                   verifiability_no_rt_f_base = verifiability_no_rt_base,
                   verifiability_rt_f_base = verifiability_rt_base, 
                   true_rt_f_base = true_rt_base, 
                   true_no_rt_f_base = true_no_rt_base,
                   n_posts_rt_f_base = n_posts_rt_base,
                   n_posts_no_rt_f_base = n_posts_no_rt_base) 

data <- df
# Define the dependent variables
aux <- c('verifiability_rt', 'verifiability_rt_f', 'true_rt', 'true_rt_f', 
         'fake_rt', 'fake_rt_f',
         'n_posts_rt', 'n_posts_rt_f', 'verifiability_no_rt', 
         'verifiability_no_rt_f', 'true_no_rt', 'true_no_rt_f', 
         'fake_no_rt', 'fake_no_rt_f', 'n_posts_no_rt',
         'n_posts_no_rt_f')

dep_var_new <- c("\\shortstack{Verifiable \\\\ RTs}", 
                 "\\shortstack{Verifiable \\\\ RTs}",
                 "\\shortstack{True \\\\ RTs}",
                 "\\shortstack{True \\\\ RTs}",
                 "\\shortstack{Fake \\\\ RTs}",
                 "\\shortstack{Fake \\\\ RTs}",
                 "\\shortstack{Total \\\\ RTs}",
                 "\\shortstack{Total \\\\ RTs}",
                 "\\shortstack{Verifiable \\\\ Posts}",
                 "\\shortstack{Verifiable \\\\ Posts}",
                 "\\shortstack{True \\\\ Posts}",
                 "\\shortstack{True \\\\ Posts}",
                 "\\shortstack{Fake \\\\ Posts}",
                 "\\shortstack{Fake \\\\ Posts}",
                 "\\shortstack{Total \\\\ Posts}",
                 "\\shortstack{Total \\\\ Posts}")

####################### #################


if (stage == 'stage1_2'){
  weeks = 'Week 1-4'
}else if (stage == 'stage3_4'){
  weeks = 'Week 5-8'
}else if (stage == 'stage5_6'){
  weeks = 'Week 9-12'}

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0("../../data/04-analysis/", country, 
                          "/",stage,"/agg_pvalues_fake_b1.xlsx"))

pvals_strong <- pvals %>% select(starts_with('strong_'))
pvals_strong <- paste(as.character(pvals_strong[1,]),collapse =" & ")

pvals_weak <- pvals %>% select(starts_with('weak_'))
pvals_weak <- paste(as.character(pvals_weak[1,]), collapse =" & ")

pvals_neither <- pvals %>% select(starts_with('neither_'))
pvals_neither <- paste(as.character(pvals_neither[1,]), collapse =" & ")

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ",
                             x, "_base  | c_t_strong_total + c_t_weak_total + 
                             c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

# Get the Robust S.E.s
lm_se <- list()
count2 <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ",
                             x, "_base  | c_t_strong_total + c_t_weak_total + 
                             c_t_neither_total"))
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
  label = paste0("tab:endline_table_b1_fake_",stage,"", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = c("Treated strong ties", "Treated weak ties",
                       "Treated absent ties"),
  omit = c(omit_var, paste0(aux, '_base')),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c('Baseline Filtered', rep(c('No', 'Yes'), 8)),
                   c("Baseline control", rep("Yes", 16)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel A, Pooled (Batch 1) Endline Results for the SMI Treatment in ', 
                 country,
                 ' , (', weeks,')'),
  type = "latex")  %>% 
  star_insert_row(insert.after=19, paste0(" & ", pvals_neither, " \\\ ")) %>% 
  star_insert_row(insert.after=16, paste0(" & ", pvals_weak, " \\\ ")) %>% 
  star_insert_row(insert.after=13, paste0(" & ", pvals_strong, " \\\ "))

note.latex <- paste0("\\multicolumn{17}{l} {\\parbox[t]{24cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each 
strong, weak, and neither tie as controls. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/",stage,"/endline_table_b1_fake_", 
                         country,".tex"))
