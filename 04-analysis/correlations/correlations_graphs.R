rm(list = ls())
library(lfe)
library(fixest)
library(tidyverse)
library(arrow)
library(stargazer)
library(DescTools)

src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)

####

data <- read_parquet( '../../../data/04-analysis/joint/correlations/df_analysis.parquet')

###

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


list_vars <- c('csb_2', 'verify_1', 'COVID_Q6', 'COVID_Q7')

names_csb3 <- c('Never', 'Rarely', 'Sometimes', 'Often', 'Always', 'Do not Know')

## Winsorized

data <- data |> mutate(across(c(n_posts_no_rt.x, n_posts_rt.x, 
                                verifiability_no_rt, verifiability_rt), 
                              ~Winsorize(.x, na.rm = T)))

data_log <- data |> mutate(across(c(n_posts_no_rt.x, n_posts_rt.x, 
                                verifiability_no_rt, verifiability_rt), 
                              ~log(.x + 1)))
###

reg_csb1 <- feols(csb_2 ~ n_posts_no_rt.x, vcov = 'HC1', 
                  data = data |> filter(csb_2 != 6))

corr_csb1 <- reg_csb1$coefficients[2]
pvalue_csb1 <- pvalue(reg_csb1)[2]

reg_csb2 <- feols(csb_2 ~ n_posts_rt.x, vcov = 'HC1', data |> filter(csb_2 != 6))

corr_csb2 <- reg_csb2$coefficients[2]
pvalue_csb2 <- pvalue(reg_csb2)[2]

x_value_mapping <- c("1" = "Never", "2" = "Rarely", 
                     "3" = "Sometimes", "4" = "Often", 
                     "5" = 'Always')

p1 <- ggplot(data |> filter(csb_2 != 6), aes(x=as.character(csb_2), y=n_posts_no_rt.x)) + 
  geom_point() + geom_smooth(aes(x = csb_2, y = n_posts_no_rt.x),
                             method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=200, color = 'blue',label=paste0("Corr: ", round(corr_csb1, 7),
                                        pval_asterix_gen(pvalue_csb1))) +
  labs(x = 'Survey Answers', y = 'Number of Posts', 
       title = 'Sharing Behaviour',
       subtitle = 'When someone shares a news story with you that they saw on platforms like
       WhatsApp, Facebook, Twitter, or Youtube, how often do you share it with others?')
p1
ggsave(p1, filename = 'csb2_posts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# rts

p2 <- ggplot(data |> filter(csb_2 != 6), aes(x=as.character(csb_2), y=n_posts_rt.x)) + 
  geom_point() + geom_smooth(aes(x = csb_2, y = n_posts_rt.x),
                             method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=200, color = 'blue',label=paste0("Corr: ", round(corr_csb2, 8),
                                                      pval_asterix_gen(pvalue_csb2))) +
  labs(x = 'Survey Answers', y = 'Number of RTs', 
       title = 'Sharing Behaviour',
       subtitle = 'When someone shares a news story with you that they saw on platforms like
       WhatsApp, Facebook, Twitter, or Youtube, how often do you share it with others?')
p2
ggsave(p2, filename = 'csb2_RTs.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# 

reg_csb1 <- feols(csb_2 ~ n_posts_no_rt.x, vcov = 'HC1', 
                  data = data_log |> filter(csb_2 != 6))

corr_csb1 <- reg_csb1$coefficients[2]
pvalue_csb1 <- pvalue(reg_csb1)[2]

reg_csb2 <- feols(csb_2 ~ n_posts_rt.x, vcov = 'HC1', data_log |> filter(csb_2 != 6))

corr_csb2 <- reg_csb2$coefficients[2]
pvalue_csb2 <- pvalue(reg_csb2)[2]

p1 <- ggplot(data_log |> filter(csb_2 != 6), aes(x=as.character(csb_2), y=n_posts_no_rt.x)) + 
  geom_point() + geom_smooth(aes(x = csb_2, y = n_posts_no_rt.x),
                             method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1.5, color = 'blue',label=paste0("Corr: ", round(corr_csb1, 7),
                                                      pval_asterix_gen(pvalue_csb1))) +
  labs(x = 'Survey Answers', y = 'Log Number of Posts', 
       title = 'Sharing Behaviour',
       subtitle = 'When someone shares a news story with you that they saw on platforms like
       WhatsApp, Facebook, Twitter, or Youtube, how often do you share it with others?')
p1
ggsave(p1, filename = 'log_csb2_posts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# rts

p2 <- ggplot(data_log |> filter(csb_2 != 6), aes(x=as.character(csb_2), y=n_posts_rt.x)) + 
  geom_point() + geom_smooth(aes(x = csb_2, y = n_posts_rt.x),
                             method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr_csb2, 8),
                                                      pval_asterix_gen(pvalue_csb2))) +
  labs(x = 'Survey Answers', y = 'Log Number of RTs', 
       title = 'Sharing Behaviour',
       subtitle = 'When someone shares a news story with you that they saw on platforms like
       WhatsApp, Facebook, Twitter, or Youtube, how often do you share it with others?')
p2
ggsave(p2, filename = 'log_csb2_RTs.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

#### Table:
reg_1 <- felm(csb_2 ~ log(n_posts_rt.x + 1), 
                  data = data |> filter(csb_2 != 6))
reg_1_se <- feols(csb_2 ~ log(n_posts_rt.x + 1), vcov = 'HC1', 
                  data |> filter(csb_2 != 6))

reg_2 <- felm(csb_2 ~ log(n_posts_no_rt.x + 1),  
                  data = data |> filter(csb_2 != 6))
reg_2_se <- feols(csb_2 ~ log(n_posts_no_rt.x + 1), vcov = 'HC1', 
                  data |> filter(csb_2 != 6))

reg_3 <- felm(csb_2 ~ n_posts_rt.x, 
              data = data |> filter(csb_2 != 6))
reg_3_se <- feols(csb_2 ~ n_posts_rt.x, vcov = 'HC1', 
                  data |> filter(csb_2 != 6))

reg_4 <- felm(csb_2 ~ n_posts_no_rt.x,  
              data = data |> filter(csb_2 != 6))
reg_4_se <- feols(csb_2 ~ n_posts_no_rt.x, vcov = 'HC1', 
                  data |> filter(csb_2 != 6))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_posts_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('CSB2'), 4),
                   covariate.labels = c("Log Total RTs", 'Log Total Posts',
                                        'Total RTs', 'Total Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0('When someone shares a news story with you that they saw on platforms like
       WhatsApp, Facebook, Twitter, or Youtube, how often do you share it with others?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{5}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
All variables are winsorized at the 99$%$ level. Answers such as 'Do not know' are excluded.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_posts.tex"))

#### Verify

reg_csb1 <- feols(verify_1 ~ verifiability_no_rt, vcov = 'HC1', 
                  data = data |> filter(verify_1 != 6))

corr_csb1 <- reg_csb1$coefficients[2]
pvalue_csb1 <- pvalue(reg_csb1)[2]

reg_csb2 <- feols(verify_1 ~ verifiability_rt, vcov = 'HC1', 
                  data |> filter(verify_1 != 6))

corr_csb2 <- reg_csb2$coefficients[2]
pvalue_csb2 <- pvalue(reg_csb2)[2]

x_value_mapping <- c("1" = "Never", "2" = "Rarely", 
                     "3" = "Sometimes", "4" = "Often", 
                     "5" = 'Always')

p1 <- ggplot(data |> filter(verify_1 != 6), aes(x=as.character(verify_1), 
                                                y=verifiability_no_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=verify_1, 
                                 y=verifiability_no_rt),
                             method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=10, color = 'blue',label=paste0("Corr: ", round(corr_csb1, 7),
                                                      pval_asterix_gen(pvalue_csb1))) +
  labs(x = 'Survey Answers', y = 'Number of Verifiable Posts', 
       title = 'Verify 1',
       subtitle = 'When you receive content from platforms like WhatsApp, Facebook, 
       and Twitter, how often do you try to verify whether it is true or fake?')
p1
ggsave(p1, filename = 'ver_posts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# rts

p1 <- ggplot(data |> filter(verify_1 != 6), aes(x=as.character(verify_1), 
                                                y=verifiability_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=verify_1, 
                                                    y=verifiability_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=40, color = 'blue',label=paste0("Corr: ", round(corr_csb2, 7),
                                                     pval_asterix_gen(pvalue_csb2))) +
  labs(x = 'Survey Answers', y = 'Number of Verifiable RTs', 
       title = 'Verify 1',
       subtitle = 'When you receive content from platforms like WhatsApp, Facebook, 
       and Twitter, how often do you try to verify whether it is true or fake?')
p1
ggsave(p1, filename = 'ver_rts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# log 

reg_csb1 <- feols(verify_1 ~ verifiability_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(verify_1 != 6))

corr_csb1 <- reg_csb1$coefficients[2]
pvalue_csb1 <- pvalue(reg_csb1)[2]

reg_csb2 <- feols(verify_1 ~ verifiability_rt, vcov = 'HC1', 
                  data_log |> filter(verify_1 != 6))

corr_csb2 <- reg_csb2$coefficients[2]
pvalue_csb2 <- pvalue(reg_csb2)[2]

p1 <- ggplot(data_log |> filter(verify_1 != 6), aes(x=as.character(verify_1), 
                                                y=verifiability_no_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=verify_1, 
                                                    y=verifiability_no_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=.2, color = 'blue',label=paste0("Corr: ", round(corr_csb1, 7),
                                                     pval_asterix_gen(pvalue_csb1))) +
  labs(x = 'Survey Answers', y = 'Log Number of Verifiable Posts', 
       title = 'Verify 1',
       subtitle = 'When you receive content from platforms like WhatsApp, Facebook, 
       and Twitter, how often do you try to verify whether it is true or fake?')
p1
ggsave(p1, filename = 'log_ver_posts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

# rts

p1 <- ggplot(data_log |> filter(verify_1 != 6), aes(x=as.character(verify_1), 
                                                y=verifiability_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=verify_1, 
                                                    y=verifiability_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=.5, color = 'blue',label=paste0("Corr: ", round(corr_csb2, 7),
                                                     pval_asterix_gen(pvalue_csb2))) +
  labs(x = 'Survey Answers', y = 'Log Number of Verifiable RTs', 
       title = 'Verify 1',
       subtitle = 'When you receive content from platforms like WhatsApp, Facebook, 
       and Twitter, how often do you try to verify whether it is true or fake?')
p1
ggsave(p1, filename = 'logver_rts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


reg_1 <- felm(verify_1 ~ log(verifiability_rt + 1), 
              data = data |> filter(verify_1 != 6))
reg_1_se <- feols(verify_1 ~ log(verifiability_rt + 1), vcov = 'HC1', 
                  data |> filter(verify_1 != 6))

reg_2 <- felm(verify_1 ~ log(verifiability_no_rt + 1),  
              data = data |> filter(verify_1 != 6))
reg_2_se <- feols(verify_1 ~ log(verifiability_no_rt + 1), vcov = 'HC1', 
                  data |> filter(verify_1 != 6))

reg_3 <- felm(verify_1 ~ verifiability_rt, 
              data = data |> filter(verify_1 != 6))
reg_3_se <- feols(verify_1 ~ verifiability_rt, vcov = 'HC1', 
                  data |> filter(verify_1 != 6))

reg_4 <- felm(verify_1 ~ verifiability_no_rt,  
              data = data |> filter(verify_1 != 6))
reg_4_se <- feols(verify_1 ~ verifiability_no_rt, vcov = 'HC1', 
                  data |> filter(verify_1 != 6))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_ver_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('CSB2'), 4),
                   covariate.labels = c("Log Ver. RTs", 'Log Ver. Posts',
                                        'Ver. RTs', 'Ver. Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0('When you receive content from platforms like WhatsApp, Facebook, 
       and Twitter, how often do you try to verify whether it is true or fake?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{5}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
All variables are winsorized at the 99$%$ level. Answers such as 'Do not know' are excluded.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_ver.tex"))


###

x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

reg_cov1 <- feols(COVID_Q6 ~ pos_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=pos_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=pos_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                      pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Positive RTs', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_pos_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


reg_cov1 <- feols(COVID_Q6 ~ neg_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neg_b_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=COVID_Q6, y=neg_b_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                   pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Negative RTs', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_neg_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

reg_cov1 <- feols(COVID_Q6 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neutral_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=neutral_b_rt), 
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                   pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neutral RTs', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_neutral_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## POSTS:

x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

reg_cov1 <- feols(COVID_Q6 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=pos_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=pos_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=2.5, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Positive Posts', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_pos_post.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


reg_cov1 <- feols(COVID_Q6 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neg_b_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=COVID_Q6, y=neg_b_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=4, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Negative Posts', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_neg_post.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

reg_cov1 <- feols(COVID_Q6 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neutral_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=neutral_b_no_rt), 
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=2, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Neutral Posts', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'covid6_neutral_posts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

###

#### Table:
reg_1 <- felm(COVID_Q6 ~ pos_b_rt, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_1_se <- feols(COVID_Q6 ~ pos_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_2 <- felm(COVID_Q6 ~ pos_b_no_rt, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_2_se <- feols(COVID_Q6 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_3 <- felm(COVID_Q6 ~ neg_b_rt, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_3_se <- feols(COVID_Q6 ~ neg_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_4 <- felm(COVID_Q6 ~ neg_b_no_rt, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_4_se <- feols(COVID_Q6 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_5 <- felm(COVID_Q6 ~ neutral_b_rt, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_5_se <- feols(COVID_Q6 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_6 <- felm(COVID_Q6 ~ neutral_b_no_rt, 
              data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_6_se <- feols(COVID_Q6 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se, 
             reg_5_se$se, reg_6_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_cov_full"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('COVID6'), 6),
                   covariate.labels = c("Positive RTs", 'Positive Posts',
                                        "Negative RTs", 'Negative Posts',
                                        "Neutral RTs", 'Neutral Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0('To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
All variables are winsorized at the 99$%$ level. Answers such as 'Do not know' and 'Vaccination is not available' are excluded.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_covid.tex"))