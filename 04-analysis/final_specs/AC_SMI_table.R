rm(list = ls())
library("purrr")

src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
library(lfe)
library(fixest)

# Read df (Aggregated Data Set):

stage <- 'SMIs'

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
                             '/SMIs_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_sa <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> 
  select(-c("__index_level_0__"))

df_sa <- df_sa |> 
  left_join(bots_sa, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'SA')

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
                             '/SMIs_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_ke <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id)

df_ke <- df_ke |> 
  left_join(bots_ke, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'KE')

df_SMI <- rbind(df_sa, df_ke)

df_SMI <- df_SMI |> mutate(total_treated = t_strong + t_weak + t_neither,
                   total_influencers = c_t_strong_total + c_t_weak_total + 
                     c_t_neither_total)

###

stage <- 'AC'

# Influencer Baseline:

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
                             '/AC_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_sa <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> 
  select(-c("__index_level_0__"))

df_sa <- df_sa |> 
  left_join(bots_sa, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'SA')

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
                             '/AC_final.parquet')) |> 
  select(-c("__index_level_0__"))

bots_ke <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id)

df_ke <- df_ke |> 
  left_join(bots_ke, by = 'follower_id') |> 
  filter(dummy_95 == 0) |>
  mutate(pais = 'KE')

df_AC <- rbind(df_sa, df_ke)

df_AC <- df_AC |> mutate(total_treated = t_strong + t_weak + t_neither,
                           total_influencers = c_t_strong_total + c_t_weak_total + 
                             c_t_neither_total)

# Define the dependent variables

dep_var_new <- c("\\shortstack{Number of Influencers\\\\ Followed}",
                 "\\shortstack{Follows Africa\\\\ Check}")

pvals_smis = read_excel(paste0("../../../data/04-analysis/joint/SMIs/agg_pvalues_all.xlsx"))
pvals_AC = read_excel(paste0("../../../data/04-analysis/joint/AC/agg_pvalues_all.xlsx"))

pvals = cbind(pvals_smis, pvals_AC)

## Run the regressions:

reg2 <- felm(AC ~ total_treated + AC_base  | 
               total_influencers, 
             data = df_AC)
reg2_se <- feols(AC ~ total_treated + AC_base  | 
                   total_influencers, 
                 vcov = 'HC1', data = df_AC)

reg1 <- felm(SMIs ~ total_treated | total_influencers, 
             data = df_SMI)
reg1_se <- feols(SMIs ~ total_treated  | total_influencers, 
                 vcov = 'HC1', data = df_SMI)

# Endline Results: use felm so that we can use stargazer

aux_data <- df_SMI['SMIs']
aux_data1 <- df_AC['AC']
means <- c(round(colMeans(aux_data, na.rm = T), 3), 
           round(colMeans(aux_data1, na.rm = T), 3))
sds <- c(round(colSds(as.matrix(aux_data), na.rm = T), 3),
         round(colSds(as.matrix(aux_data1), na.rm = T), 3))
maximums <- c(round(sapply(aux_data, max, na.rm = T), 1),
              round(sapply(aux_data1, max, na.rm = T), 1))
minimums <- c(round(sapply(aux_data, min, na.rm = T), 1),
              round(sapply(aux_data1, min, na.rm = T), 1))
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(
  reg1, reg2, 
  se = list(reg1_se$se, reg2_se$se), # robust standard errors
  label = paste0("tab:followers_smis_ac_joint"),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var_new,
  covariate.labels = "Total Treated",
  omit = c('strat_block1', 'AC_base'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Baseline control", rep("Yes", 2)),
                   c("Outcome mean", means),
                   c("Outcome std. dev.", sds),
                   c("Outcome range", range)),
  title = paste0('Panel C, Endline Results for the Treatment in Both Countries'),
  type = "latex")  %>% 
  star_insert_row(insert.after=13, paste0(" & ", pvals[1], " & ", pvals[2], '\\'))

note.latex <- paste0("\\multicolumn{3}{l} {\\parbox[t]{8cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/new/followers_smis_ac_joint.tex"))
