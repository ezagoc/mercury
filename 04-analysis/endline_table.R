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

#Load the data
data <- read_parquet(paste0('../../data/03-experiment/',country,
                            '/treatment/followers/01-preprocess/aggregated/stage2.parquet'))

data <- data |> rename(total_shares_base = shares_base, 
                       total_reactions_base = reactions_base,
                       total_comments_base = comments_base)
#data <- data |> filter(bot_account == 0)

# Generate the indexes
w_9_int_base <- data[
  c(
    'total_shares_base', 'total_reactions_base', 'total_comments_base'
  )
]
w_9_index_int <- icwIndex(w_9_int_base |> as.matrix())
data$index_int_base <- scale(w_9_index_int$index)

w_9_int_base <- data[
  c(
    'total_shares', 'total_comments', 'total_reactions'
  )
]
w_9_index_int <- icwIndex(w_9_int_base |> as.matrix())
data$index_int <- scale(w_9_index_int$index)

# Define the dependent variables
aux <- c('total_shares', 'total_reactions', 'total_comments', 'index_int',
         'verifiability', 'true', 'n_posts')

omit_2 <- c('total_shares_base', 'total_reactions_base', 'total_comments_base', 
            'index_int_base', 'verifiability_base', 'true_base', 'n_posts_base')
####################### #################

# Load pvalues obtained through 1000 permutations of assigments
pvals = read_excel(paste0(
  "../../data/04-analysis/", country,"/stage2/agg_pvalues_joint_endline_filter.xlsx"
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

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ads_treatment + ",
                             x, "_base  | strat_block1 + c_t_strong_total + c_t_weak_total + c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = data))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

# Get the Robust S.E.s
lm_se <- list()
count2 <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither + ads_treatment + ",
                             x, "_base  | strat_block1 + c_t_strong_total + c_t_weak_total + c_t_neither_total"))
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
  label = paste0("tab:endline_table_stage2_filter_", country),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = dep_var,
  covariate.labels = covariates,
  omit = c(omit_var, omit_2),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Strat. Block1 Fixed Effects", rep("Yes", 6)),
                   c("Baseline control", rep("Yes", 6)),
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
cat(table, file = paste0("../../results/04-analysis/endline_table_stage2_filter",country,
                         ".tex"))