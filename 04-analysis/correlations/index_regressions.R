rm(list = ls())
library(lfe)
library(fixest)
library(tidyverse)
library(arrow)
library(stargazer)
library(DescTools)
library(matrixStats)
library(estimatr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)
####

pval_asterix_gen <- function(pvalue){
  if(pvalue <= 0.01) {
    aster <- '***'
  }else if(pvalue > 0.01 & pvalue <= 0.05){
    aster <- '**'
  }else if(pvalue > 0.05 & pvalue < 0.1){
    aster <- '*'
  }else{
    aster <- ''
  }
  return(aster)
}

data <- read_parquet( '../../../data/04-analysis/joint/correlations/df_analysis.parquet')

data <- data |> mutate(across(c(COVID_Q1, COVID_Q2, COVID_Q3, COVID_Q6), 
                              ~ifelse(.x == 6 | .x == 7, NA, .x)))

data <- data |> mutate(agg_measure = neg_b_rt + neg_b_no_rt - pos_b_no_rt - pos_b_rt)

data_log <- data |> mutate(across(c(pos_b_rt, pos_b_no_rt, neg_b_rt, neg_b_no_rt,
                                    neutral_b_rt, neutral_b_no_rt), 
                                  ~log(.x + 1)),
                           agg_measure = neg_b_rt + neg_b_no_rt - pos_b_no_rt - pos_b_rt)

int_base <- data[c('COVID_Q1', 'COVID_Q2', 'COVID_Q3', 'COVID_Q6')]
index_int_base <- icwIndex(int_base |> as.matrix())
data$index_covid <- scale(index_int_base$index)


int_base <- data_log[c('COVID_Q1', 'COVID_Q2', 'COVID_Q3', 'COVID_Q6')]
index_int_base <- icwIndex(int_base |> as.matrix())
data_log$index_covid <- scale(index_int_base$index)

# Mapeo

x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

#

reg_cov1 <- feols(index_covid ~ agg_measure, vcov = 'HC1', 
                  data = data)

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data,
             aes(x=index_covid, y=agg_measure)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=index_covid, y=agg_measure),
              method=lm_robust, se = T) + theme_minimal() +
  geom_text(x=.5, y=-3, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'ICW Index of Survey Answers', 
       y = 'Neg. RTs + Neg. Posts - Pos. RTs - Pos. Posts', 
       title = 'Aggregate Measures')

p3
ggsave(p3, filename = 'icw_covid_neg_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


#

reg_cov1 <- feols(index_covid ~ agg_measure, vcov = 'HC1', 
                  data = data_log)

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log,
             aes(x=index_covid, y=agg_measure)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=index_covid, y=agg_measure),
              method=lm_robust, se = T) + theme_minimal() +
  geom_text(x=.5, y=-2, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                   pval_asterix_gen(pvalue))) +
  labs(x = 'ICW Index of Survey Answers', y = 'Neg. RTs + Neg. Posts - Pos. RTs - Pos. Posts', 
       title = 'Aggregate Measures (Logs)')

p3
ggsave(p3, filename = 'log_icw_covid_neg_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


## agg measures table

reg_1 <- felm(index_covid ~ agg_measure, 
              data = data)
reg_1_se <- feols(index_covid ~ agg_measure, vcov = 'HC1', 
                  data = data)

reg_2 <- felm(index_covid ~ agg, 
              data = data_log |> rename(agg = agg_measure))
reg_2_se <- feols(index_covid ~ agg, vcov = 'HC1', 
                  data = data_log |> rename(agg = agg_measure))

lm_se = list(reg_1_se$se, reg_2_se$se)
table <- stargazer(reg_1, reg_2, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covall_full_icw"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = c('ICW Index COVID'),
                   covariate.labels = c("Aggregate Measure",
                                        'Log Aggregate Measure'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0("Aggregate Measure of Twitter Outcomes vs. ICW Index of Q1, Q2, Q3 and Q6"),
                   type = "latex")

note.latex <- paste0("\\multicolumn{5}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
Answers such as 'Do not know' and 'Vaccination is not available' are excluded. 
Aggregate measure refers to the sum of negative posts and RTs, minus the sum of positive posts and RTs.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_covid_agg_icw.tex"))