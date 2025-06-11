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

stage <- 'baseline'
country <- 'KE'

path_pilot <- '../../../social-media-influencers-africa/data/04-analysis/'
path_normal <- '../../data/04-analysis/'

get_panel_stages <- function(stage, path, batch, country){
  df <- read_parquet(paste0(path, country, '/',stage,
                            '/endline_', batch,'.parquet'))
  df <- df |> mutate(stage_id = stage)
  df
}

df_stages_b2 <- paste0(rep('stage', 2), c(1:2)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'batch2', country))

df_stages_agg <- df_stages_b2 |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b2')

base1 <- read_parquet(paste0(path_normal, country, 
                             '/stage1/endline_batch2.parquet')) |>
  select(username:n_posts_no_rt_base) |> select(-c(id, blockid1, blockid2))

base2 <- read_parquet(paste0(path_normal, country, 
                             '/baseline/baseline_batch2_april.parquet')) |>
  select(username:n_posts_no_rt_base) |> select(-c(id, blockid1, blockid2))

colnames(base2)[12:length(base2)] <- paste0(colnames(base2)[12:length(base2)],'_april')

base2 <- base2 |> select(follower_id, total_shares_rt_base_april:n_posts_no_rt_base_april)

final_batch2 <- base1 |> left_join(df_stages_agg, by = 'follower_id')

final_batch2 <- final_batch2 |> left_join(base2, by = 'follower_id')

df <- final_batch2

# Define the dependent variables
aux <- c('verifiability_rt', 'true_rt', 'n_posts_rt', 'verifiability_no_rt',
         'true_no_rt', 'n_posts_no_rt')

dep_var_new <- c("\\shortstack{Verifiable \\\\ RTs}", 
                 "\\shortstack{True \\\\ RTs}",
                 "\\shortstack{Total \\\\ RTs}",
                 "\\shortstack{Verifiable \\\\ Posts}",
                 "\\shortstack{True \\\\ Posts}",
                 "\\shortstack{Total \\\\ Posts}")

####################### #################

data <- df
# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0("../../data/04-analysis/", country, 
                          "/",stage,"/agg_pvalues_prueba_april.xlsx"))

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
                             x, "_base_april  | c_t_strong_total + c_t_weak_total + 
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
                             x, "_base_april  | c_t_strong_total + c_t_weak_total + 
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
  label = paste0("tab:endline_prueba_b2_",stage,"", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = c("Treated strong ties", "Treated weak ties",
                       "Treated absent ties"),
  omit = c(omit_var),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Endline Results for Batch 2 (First Month) with 1 Month of Baseline, ', 
                 country),
  type = "latex")  %>% 
  star_insert_row(insert.after=19, paste0(" & ", pvals_neither, " \\\ ")) %>% 
  star_insert_row(insert.after=16, paste0(" & ", pvals_weak, " \\\ ")) %>% 
  star_insert_row(insert.after=13, paste0(" & ", pvals_strong, " \\\ "))

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each 
strong, weak, and neither tie as controls. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/endline_prueba_april_", 
                         stage,"_", 
                         country,".tex"))


pvals = read_excel(paste0("../../data/04-analysis/", country, 
                          "/",stage,"/agg_pvalues_prueba.xlsx"))

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
  label = paste0("tab:endline_prueba_all_b2_",stage,"", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = c("Treated strong ties", "Treated weak ties",
                       "Treated absent ties"),
  omit = c(omit_var),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Endline Results for Batch 2 (First Month) with 2 Months of Baseline, ', 
                 country),
  type = "latex")  %>% 
  star_insert_row(insert.after=19, paste0(" & ", pvals_neither, " \\\ ")) %>% 
  star_insert_row(insert.after=16, paste0(" & ", pvals_weak, " \\\ ")) %>% 
  star_insert_row(insert.after=13, paste0(" & ", pvals_strong, " \\\ "))

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each 
strong, weak, and neither tie as controls. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../results/04-analysis/endline_prueba_all_", 
                         stage,"_", 
                         country,".tex"))