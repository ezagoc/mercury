# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# 2.0 Define constants
country <- 'joint'
data_type <- 'Followers'
file_code <- 'ads'
ini <- '../../../../data/04-analysis/joint/'


# 3.0 Import data and manipulate
df <- get_analysis_followers(batches = 'b1b2', initial_path = '../../../../')

df <- df |> mutate(strat_block1 = paste0(strat_block1, pais), 
                   strat_block2 = paste0(strat_block2, pais))

df <- df |> filter(n_posts_base>0)

means <- c(mean(df$AC), mean(df$SMIs, na.rm=T))

aux <- c('AC', 'SMIs')

# Get the Robust S.E.s

lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (x == 'AC'){
    fmla1 <- as.formula(paste0(x, "~ ads_treatment + ", x, 
                               "_base  | strat_block1"))}
  else {
    fmla1 <- as.formula(paste0(x, "~ ads_treatment | strat_block1 + total_influencers"))
  }
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = df))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

# Get the Robust S.E.s
lm_se <- list()
count2 <- 1
for (x in aux) {
  if (x == 'AC'){
    fmla1 <- as.formula(paste0(x, "~ ads_treatment + ", x, 
                               "_base  | strat_block1"))}
  else {
    fmla1 <- as.formula(paste0(x, "~ ads_treatment | strat_block1 + total_influencers"))
  }
  nam1 <- paste("lm_", count2, "_se", sep = "")
  assign(nam1, feols(fmla1, vcov = 'HC1', data = df))
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

table <- stargazer(
  lm_list_ols, # felm regressions
  se = lm_se, # robust standard errors
  label = paste0("tab:ads_followers"),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = c('Follows Africa Check', 'Number of SMIs Followed'),
  covariate.labels = "Ads Treatment",
  keep = c('ads_treatment'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Baseline control", rep("Yes", 2)), 
                   c("Strat. Block1 FE", rep("Yes", 2)), 
                   c("Outcome mean", means)
                   ),
  title = 'X Ads Treatment',
  type = "latex")

note.latex <- paste0("\\multicolumn{3}{l} {\\parbox[t]{9cm}{ \\textit{Notes:} 
This table presents the estimates from the Twitter Ads intervention. The dependent variables are a dummy indicating whether users
followed Africa Check at the end of the intervention and the number of
social media influencers (SMIs) they followed at the end of the intervention. The models control for whether users followed Africa Check at the
beginning of the intervention and the number of SMIs they followed at
the beginning. This analysis focuses exclusively on Batch 1 followers that follow only 1 influencer, as
endpoint data for Batch 2 followers was unavailable. We do not consider followers that followed an influencer who got their account suspended.
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../../results/01-regression_graphs/",data_type,"/ads.tex"))
