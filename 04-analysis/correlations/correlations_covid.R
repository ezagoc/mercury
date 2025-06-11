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
aux <- rep(c('d_COVID_Q1', 'd_COVID_Q2', 'd_COVID_Q3'), 3)

###### 

aux_data <- data_log[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid_win_log"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Q1', 'Q2', 'Q3'), 3),
                   covariate.labels = c("Log Positive COVID RT", 'Log Neutral COVID RT',
                                        'Log Negative COVID RT'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q1, COVID Q2, COVID Q3'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
COVID Q1: To what extent do you agree with the following statement: 
'COVID-19 is not a real disease and a serious health problem in Kenya. 
It is a fake disease made up by powerful actors to fool and harm Kenyans.' 
COVID Q2: To what extent do you agree with the following statement: 
'COVID-19 vaccines have not been rigorously tested to ensure that they are safe. 
Some safety protocols were circumvented to fast track the authorization of COVID-19 vaccine for use.'
COVID Q3: To what extent do you agree with the following statement:
'mRNA COVID-19 vaccines like Pfizer and Moderna change a person's DNA.'
The unit of observation is an influencer's follower.
We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covid_win_log.tex"))


###################

aux <- rep(c('d_COVID_Q1', 'd_COVID_Q2', 'd_COVID_Q3'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 4){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  } else if(count > 3 & count < 7){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Q1', 'Q2', 'Q3'), 3),
                   covariate.labels = c("Log Positive COVID Posts", 
                                        'Log Neutral COVID Posts',
                                        'Log Negative COVID Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q1, COVID Q2, COVID Q3'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{10}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
COVID Q1: To what extent do you agree with the following statement: 
'COVID-19 is not a real disease and a serious health problem in Kenya. 
It is a fake disease made up by powerful actors to fool and harm Kenyans.' 
COVID Q2: To what extent do you agree with the following statement: 
'COVID-19 vaccines have not been rigorously tested to ensure that they are safe. 
Some safety protocols were circumvented to fast track the authorization of COVID-19 vaccine for use.'
COVID Q3: To what extent do you agree with the following statement:
'mRNA COVID-19 vaccines like Pfizer and Moderna change a person's DNA.'
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covidposts_win_log.tex"))

####### COVID Q6

aux <- rep(c('COVID_Q6_1', 'COVID_Q6_2', 'COVID_Q6_3', 'COVID_Q6_4', 'COVID_Q6_5'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid6_log"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Not at all', 'A little bit', 
                                         'Somewhat', 'Mostly', 'Completely'), 3),
                   covariate.labels = c("Log Positive COVID RT", 
                                        'Log Neutral COVID RT',
                                        'Log Negative COVID RT'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q6: To what extent do you believe that the COVID-19 vaccine currently available 
                                  at a location near you is safe and effective?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{16}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covid6_log.tex"))


###############

aux <- rep(c('COVID_Q6_1', 'COVID_Q6_2', 'COVID_Q6_3', 'COVID_Q6_4', 'COVID_Q6_5'), 3)

# Endline Results: use felm so that we can use stargazer
aux_data <- data[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, felm(fmla1, data = data_log))
  }
  
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}


lm_se <- list()
count2 <- 1
for (x in aux) {
  if (count< 6){
    fmla1 <- as.formula(paste0(x, "~ pos_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  } else if(count > 5 & count < 11){
    fmla1 <- as.formula(paste0(x, "~ neutral_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }else {
    fmla1 <- as.formula(paste0(x, "~ neg_b_no_rt"))
    nam1 <- paste("lm_", count2, "_se", sep = "")
    assign(nam1, feols(fmla1, vcov = 'HC1', data = data_log))
  }
  
  lm_se[[count2]] <- get(nam1, envir = globalenv())$se
  count2 <- count2 + 1
}

means <- round(colMeans(aux_data, na.rm = T), 3)
sds <- round(colSds(as.matrix(aux_data), na.rm = T), 3)
maximums <- round(sapply(aux_data, max, na.rm = T), 1)
minimums <- round(sapply(aux_data, min, na.rm = T), 1)
range <- paste0("[", minimums, ",", maximums, "]")

table <- stargazer(lm_list_ols, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_covid6posts_log"),
                   header = FALSE,
                   font.size = "tiny",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('Not at all', 'A little bit', 
                                         'Somewhat', 'Mostly', 'Completely'), 3),
                   covariate.labels = c("Log Positive COVID Posts", 'Log Neutral COVID Posts',
                                        'Log Negative COVID Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   add.lines = list(c("Outcome mean", means, means),
                                    c("Outcome std. dev.", sds, sds),
                                    c("Outcome range", range, range)),
                   title = paste0('COVID Q6: To what extent do you believe that the COVID-19 vaccine currently available 
                                  at a location near you is safe and effective?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{16}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/covid6posts_log.tex"))


##### Graphs:


x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

reg_cov1 <- feols(COVID_Q6 ~ pos_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]


p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=pos_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=pos_b_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Positive RTs', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'log_covid6_pos_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


reg_cov1 <- feols(COVID_Q6 ~ neg_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neg_b_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=COVID_Q6, y=neg_b_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Negative RTs', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'log_covid6_neg_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

reg_cov1 <- feols(COVID_Q6 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neutral_b_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=neutral_b_rt), 
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Neutral RTs', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'log_covid6_neutral_rt.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

## POSTS:

x_value_mapping <- c("1" = "Not at all", "2" = "A little bit", 
                     "3" = "Somewhat", "4" = "Mostly", 
                     "5" = 'Completely')

reg_cov1 <- feols(COVID_Q6 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=pos_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=pos_b_no_rt),
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                      pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Positive Posts', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'log_covid6_pos_post.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')


reg_cov1 <- feols(COVID_Q6 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neg_b_rt)) + 
  geom_point(position = 'jitter') + geom_smooth(aes(x=COVID_Q6, y=neg_b_rt),
                                                method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='4', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Negative Posts', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'log_covid6_neg_post.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

reg_cov1 <- feols(COVID_Q6 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

corr <- reg_cov1$coefficients[2]
pvalue <- pvalue(reg_cov1)[2]

p3 <- ggplot(data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7),
             aes(x=as.character(COVID_Q6), y=neutral_b_no_rt)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(aes(x=COVID_Q6, y=neutral_b_no_rt), 
              method=lm_robust, se = T) + theme_minimal() + 
  scale_x_discrete(breaks = names(x_value_mapping), labels = x_value_mapping) + 
  geom_text(x='5', y=1, color = 'blue',label=paste0("Corr: ", round(corr, 5),
                                                    pval_asterix_gen(pvalue))) +
  labs(x = 'Survey Answers', y = 'Log Neutral Posts', 
       title = 'COVID_Q6',
       subtitle = 'To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?')

p3
ggsave(p3, filename = 'log_covid6_neutral_posts.pdf', device = cairo_pdf,
       dpi = 300, width = 6.07, height = 5.65, units = 'in')

###

#### Table:
reg_1 <- felm(COVID_Q6 ~ pos_b_rt, 
              data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_1_se <- feols(COVID_Q6 ~ pos_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_2 <- felm(COVID_Q6 ~ pos_b_no_rt, 
              data = data_log|> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_2_se <- feols(COVID_Q6 ~ pos_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_3 <- felm(COVID_Q6 ~ neg_b_rt, 
              data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_3_se <- feols(COVID_Q6 ~ neg_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_4 <- felm(COVID_Q6 ~ neg_b_no_rt, 
              data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_4_se <- feols(COVID_Q6 ~ neg_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_5 <- felm(COVID_Q6 ~ neutral_b_rt, 
              data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_5_se <- feols(COVID_Q6 ~ neutral_b_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

reg_6 <- felm(COVID_Q6 ~ neutral_b_no_rt, 
              data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))
reg_6_se <- feols(COVID_Q6 ~ neutral_b_no_rt, vcov = 'HC1', 
                  data = data_log |> filter(COVID_Q6 != 6 & COVID_Q6 != 7))

lm_se = list(reg_1_se$se, reg_2_se$se, reg_3_se$se, reg_4_se$se, 
             reg_5_se$se, reg_6_se$se)
table <- stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6, # felm regressions
                   se = lm_se, # robust standard errors
                   label = paste0("tab:corr_cov_full_log"),
                   header = FALSE,
                   font.size = "scriptsize",
                   dep.var.caption = "",
                   omit = c('Constant'),
                   dep.var.labels.include = FALSE,
                   table.placement = "!htpb",
                   column.labels = rep(c('COVID6'), 6),
                   covariate.labels = c("Log Positive RTs", 'Log Positive Posts',
                                        "Log Negative RTs", 'Log Negative Posts',
                                        "Log Neutral RTs", 'Log Neutral Posts'),
                   omit.stat=c("f", "ser", "adj.rsq"),
                   column.sep.width = "0pt",
                   title = paste0('To what extent do you believe that the COVID-19 vaccine 
       currently available at a location near you is safe and effective?'),
                   type = "latex")

note.latex <- paste0("\\multicolumn{7}{l} {\\parbox[t]{10cm}{ \\textit{Notes:}
Answers such as 'Do not know' and 'Vaccination is not available' are excluded.
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../results/04-analysis/correlations/full_range_covid_log.tex"))