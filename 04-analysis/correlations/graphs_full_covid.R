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

x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

# COVID1 POSITIVE

reg_cov1 <- feols(COVID_Q1 ~ pos_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=pos_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=pos_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Positive RTs', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_pos_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Log

reg_cov1 <- feols(COVID_Q1 ~ pos_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=pos_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=pos_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Positive RTs', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'log_covid1_pos_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

#
# COVID1 Positive Posts

reg_cov1 <- feols(COVID_Q1 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=pos_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=pos_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Positive Posts', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_pos_no_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Log

reg_cov1 <- feols(COVID_Q1 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=pos_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=pos_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Positive Posts', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'log_covid1_pos_no_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


### Neg RTs

reg_cov1 <- feols(COVID_Q1 ~ neg_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neg_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neg_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Negative RTs', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_neg_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Log

reg_cov1 <- feols(COVID_Q1 ~ neg_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neg_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neg_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Negative RTs', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'log_covid1_neg_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# Neg

# COVID1 Neg Posts

reg_cov1 <- feols(COVID_Q1 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neg_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neg_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Negative Posts', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_neg_no_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Log

reg_cov1 <- feols(COVID_Q1 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neg_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neg_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Negative Posts', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'log_covid1_neg_no_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


### Neutral COVID Q1

reg_cov1 <- feols(COVID_Q1 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neutral_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neutral_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neutral RTs', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_neu_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Log

reg_cov1 <- feols(COVID_Q1 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neutral_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neutral_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Neutral RTs', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'log_covid1_neu_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


# COVID1 Neu

reg_cov1 <- feols(COVID_Q1 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neutral_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neutral_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neutral Posts', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'covid1_neu_no_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## Log

reg_cov1 <- feols(COVID_Q1 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q1 != 6))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q1 != 6),
             aes(x=as.character(COVID_Q1), y=neutral_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q1, y=neutral_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Neutral Posts', 
       title = 'COVID_Q1',
       subtitle = 'To what extent do you agree with the following statement: 
       COVID-19 is not a real disease and a serious health problem in Kenya. 
       It is a fake disease made up by powerful actors to fool and harm Kenyans.')

p3
ggsave(p3, filename = 'log_covid1_neu_no_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')
