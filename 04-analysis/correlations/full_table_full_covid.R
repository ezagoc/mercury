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

data <- data |> mutate(agg_measure = neg_b_rt + neg_b_no_rt - pos_b_no_rt - pos_b_rt)

data_log <- data |> mutate(across(c(pos_b_rt, pos_b_no_rt, neg_b_rt, neg_b_no_rt,
                                    neutral_b_rt, neutral_b_no_rt), 
                                  ~log(.x + 1)),
                           agg_measure = neg_b_rt + neg_b_no_rt - pos_b_no_rt - pos_b_rt)

# Mapeo

x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

# COVID1 POSITIVE

reg_cov1 <- feols(COVID_Q1 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=agg_measure)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=agg_measure),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=-1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neg. RTs + Neg. Posts - Pos. RTs - Pos. Posts', 
       title = 'COVID_Q1 (Logs)',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_log_agg.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# Q2

reg_cov1 <- feols(COVID_Q2 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q2 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q2 != 6),
             aes(x=as.character(COVID_Q2), y=agg_measure)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q2, y=agg_measure),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=-1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                     pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = ' Neg. RTs + Neg. Posts - Pos. RTs - Pos. Posts', 
       title = 'COVID_Q2 (Logs)',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 vaccines have not been rigorously tested to ensure that they are safe. 
       Some safety protocols were circumvented to fast track the authorization of COVID-19 
       vaccine for use.')

p3
ggsave(p3, filename = 'covid2_log_agg.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Q3

reg_cov1 <- feols(COVID_Q3 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q3 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q3 != 6),
             aes(x=as.character(COVID_Q3), y=agg_measure)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q3, y=agg_measure),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=-1.5, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                     pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neg. RTs + Neg. Posts - Pos. RTs - Pos. Posts', 
       title = 'COVID_Q3 (Logs)',
       subtitle = 'To what extent do you agree with the following statement: 
       mRNA COVID-19 vaccines like Pfizer and Moderna change a person’s DNA.')

p3
ggsave(p3, filename = 'covid3_logs_agg.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Q6

reg_cov1 <- feols(COVID_Q6 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=agg_measure)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=agg_measure),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=-2, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                     pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neg. RTs + Neg. Posts - Pos. RTs - Pos. Posts', 
       title = 'COVID_Q6 (Logs)',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine currently available
       at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_log_agg.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')



#### Tables

reg_1 <- felm(COVID_Q1 ~ agg_measure, 
              data = data |> filter(COVID_Q1 != 6))
reg_1_se <- feols(COVID_Q1 ~ agg_measure, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

reg_2 <- felm(COVID_Q2 ~ agg_measure, 
              data = data |> filter(COVID_Q2 != 6))
reg_2_se <- feols(COVID_Q2 ~ agg_measure, vcov = 'HC1', 
                  data = data |> filter(COVID_Q2 != 6))

reg_3 <- felm(COVID_Q3 ~ agg_measure, 
              data = data |> filter(COVID_Q3 != 6))
reg_3_se <- feols(COVID_Q3 ~ agg_measure, vcov = 'HC1', 
                  data = data |> filter(COVID_Q3 != 6))

reg_4 <- felm(COVID_Q6 ~ agg_measure, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_4_se <- feols(COVID_Q6 ~ agg_measure, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covall_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = c('Q1', 'Q2', 'Q3', 'Q6'),
                   covariate.labels = c("Aggregate Measure"),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0("Correlation Aggregate Measure of Twitter Outcomes vs. Full Scale Q1-Q6"),
                   type = "latex")

note.latex <- paste0("\\multicolumn{5}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
Answers such as 'Do not know' are excluded. Aggregate measure refers to the sum of negative posts and RTs, minus the sum of positive posts and RTs.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_covid_agg.tex"))


### lOG

reg_1 <- felm(COVID_Q1 ~ agg_measure, 
              data = data_log |> filter(COVID_Q1 != 6))
reg_1_se <- feols(COVID_Q1 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

reg_2 <- felm(COVID_Q2 ~ agg_measure, 
              data = data_log |> filter(COVID_Q2 != 6))
reg_2_se <- feols(COVID_Q2 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q2 != 6))

reg_3 <- felm(COVID_Q3 ~ agg_measure, 
              data = data_log |> filter(COVID_Q3 != 6))
reg_3_se <- feols(COVID_Q3 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q3 != 6))

reg_4 <- felm(COVID_Q6 ~ agg_measure, 
              data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_4_se <- feols(COVID_Q6 ~ agg_measure, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:log_corr_covall_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = c('Q1', 'Q2', 'Q3', 'Q6'),
                   covariate.labels = c("Aggregate Measure (Logs)"),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0("Correlation Aggregate Measure of Twitter Outcomes (Logs) vs. Full Scale Q1-Q6"),
                   type = "latex")

note.latex <- paste0("\\multicolumn{5}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
Answers such as 'Do not know' are excluded. Aggregate measure refers to the sum of log negative posts and RTs, minus the sum of log positive posts and RTs.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_covid_log_agg.tex"))
