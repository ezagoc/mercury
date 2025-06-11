rm(list = ls())
library(lfe)
library(fixest)
library(tidyverse)
library(arrow)
library(stargazer)
library(DescTools)
library(matrixStats)
library(estimatr)
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


data_log <- data |> mutate(across(c(pos_b_rt, pos_b_no_rt, neg_b_rt, neg_b_no_rt,
                                    neutral_b_rt, neutral_b_no_rt), 
                                  ~log(.x + 1)))

# Mapeo

reg_1 <- felm(COVID_Q3 ~ pos_b_rt, 
              data = data |> filter(COVID_Q3 != 6))
reg_1_se <- feols(COVID_Q3 ~ pos_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

reg_2 <- felm(COVID_Q3 ~ pos_b_no_rt, 
              data = data |> filter(COVID_Q3 != 6))
reg_2_se <- feols(COVID_Q3 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

reg_3 <- felm(COVID_Q3 ~ neg_b_rt, 
              data = data |> filter(COVID_Q3 != 6))
reg_3_se <- feols(COVID_Q3 ~ neg_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

reg_4 <- felm(COVID_Q3 ~ neg_b_no_rt, 
              data = data |> filter(COVID_Q3 != 6))
reg_4_se <- feols(COVID_Q3 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

reg_5 <- felm(COVID_Q3 ~ neutral_b_rt, 
              data = data |> filter(COVID_Q3 != 6))
reg_5_se <- feols(COVID_Q3 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

reg_6 <- felm(COVID_Q3 ~ neutral_b_no_rt, 
              data = data |> filter(COVID_Q3 != 6))
reg_6_se <- feols(COVID_Q3 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se, 
             reg_5_se$se, reg_6_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_cov3_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('COVID3'), 6),
                   covariate.labels = c("Positive RTs", 'Positive Posts',
                                        "Negative RTs", 'Negative Posts',
                                        "Neutral RTs", 'Neutral Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0("To what extent do you agree with the following statement:
                   mRNA COVID-19 vaccines like Pfizer and Moderna change a person's DNA."),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
Full scale is: \textit{Not At All}, \textit{A little bit}, \textit{Somewhat}, \textit{Mostly}, \textit{Completely} and \textit{Do not know}. 
Answers such as 'Do not know' are excluded.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_covid3.tex"))


### lOG

reg_1 <- felm(COVID_Q1 ~ pos_b_rt, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_1_se <- feols(COVID_Q1 ~ pos_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

reg_2 <- felm(COVID_Q1 ~ pos_b_no_rt, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_2_se <- feols(COVID_Q1 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

reg_3 <- felm(COVID_Q1 ~ neg_b_rt, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_3_se <- feols(COVID_Q1 ~ neg_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

reg_4 <- felm(COVID_Q1 ~ neg_b_no_rt, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_4_se <- feols(COVID_Q1 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

reg_5 <- felm(COVID_Q1 ~ neutral_b_rt, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_5_se <- feols(COVID_Q1 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

reg_6 <- felm(COVID_Q1 ~ neutral_b_no_rt, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_6_se <- feols(COVID_Q1 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se, 
             reg_5_se$se, reg_6_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:log_corr_cov1_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('COVID1'), 6),
                   covariate.labels = c("Log Positive RTs", 'Log Positive Posts',
                                        "Log Negative RTs", 'Log Negative Posts',
                                        "Log Neutral RTs", 'Log Neutral Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0("To what extent do you agree with the following statement: 
                                  COVID-19 is not a real disease and a serious health problem in Kenya. 
                                  It is a fake disease made up by powerful actors to fool and harm Kenyans."),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
Full scale is: \textit{Not At All}, \textit{A little bit}, \textit{Somewhat}, \textit{Mostly}, \textit{Completely} and \textit{Do not know}. 
Answers such as 'Do not know' are excluded.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/log_full_range_covid1.tex"))