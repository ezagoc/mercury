rm(list = ls())
library("purrr")
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

library(lfe)
library(fixest)

# ORIGINAL ---------------------------------------------
# Change country/stage
country <- 'joint'

# Read df (Aggregated Data Set)

for (stage in list('baseline')){
  for (type in list('', 'log_', 'arc_')){ 
    
    aux <- c('pos_b_rt_covid', 'pos_b_no_rt_covid', 'neutral_b_rt_covid', 
             'neutral_b_no_rt_covid', 'neg_b_rt_covid', 'neg_b_no_rt_covid',
             'n_posts_rt_covid', 'n_posts_no_rt_covid',
             'pos_b_rt_vax', 'pos_b_no_rt_vax', 'neutral_b_rt_vax', 
             'neutral_b_no_rt_vax', 'neg_b_rt_vax', 'neg_b_no_rt_vax', 
             'n_posts_rt_vax', 'n_posts_no_rt_vax')
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', 'stage1_2', 
                                 '/final_data_b1b2p_sent_bert.parquet'))
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage1_2', 
                                 '/final_data_b1b2p_sent_bert.parquet'))
    
    df <- rbind(df_ke, df_sa)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           pos_b_rt_base:n_posts_no_rt_vax) |> 
      mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), ~log(.x + 1)))
    
    colnames(df_log)[4:length(df_log)] <- paste0('log_', 
                                                 colnames(df_log)[4:length(df_log)])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              pos_b_rt_base:n_posts_no_rt_vax) |> 
      mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[4:length(df_arcsin)] <- paste0('arc_', 
                                                       colnames(df_arcsin)[4:length(df_arcsin)])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    aux <- paste0(type, aux, '_base')
    
    data <- df
    
    if (type == 'log_'){
      dep_var_new <- c("\\shortstack{log(Pos. \\\\ COVID \\\\ RTs)}", 
                       "\\shortstack{log(Pos. \\\\ COVID \\\\ Posts)}",
                       "\\shortstack{log(Neu. \\\\ COVID \\\\ RTs)}",
                       "\\shortstack{log(Neu. \\\\ COVID \\\\ Posts)}",
                       "\\shortstack{log(Neg. \\\\ COVID \\\\ RTs)}",
                       "\\shortstack{log(Neg. \\\\ COVID \\\\ Posts)}",
                       "\\shortstack{log(Total \\\\ COVID \\\\ RTs)}",
                       "\\shortstack{log(Total \\\\  COVID \\\\ Posts)}",
                       "\\shortstack{log(Pos. \\\\ Vaccine \\\\ RTs)}", 
                       "\\shortstack{log(Pos. \\\\ Vaccine \\\\ Posts)}",
                       "\\shortstack{log(Neu. \\\\ Vaccine \\\\ RTs)}",
                       "\\shortstack{log(Neu. \\\\ Vaccine \\\\ Posts)}",
                       "\\shortstack{log(Neg. \\\\ Vaccine \\\\ RTs)}",
                       "\\shortstack{log(Neg. \\\\ Vaccine \\\\ Posts)}",
                       "\\shortstack{log(Total \\\\ Vaccine \\\\ RTs)}",
                       "\\shortstack{log(Total \\\\ Vaccine \\\\ Posts)}")
    }else if (type == 'arc_'){
      dep_var_new <- c("\\shortstack{arc(Pos. \\\\ COVID \\\\ RTs)}", 
                       "\\shortstack{arc(Pos. \\\\ COVID \\\\ Posts)}",
                       "\\shortstack{arc(Neu. \\\\ COVID \\\\ RTs)}",
                       "\\shortstack{arc(Neu. \\\\ COVID \\\\ Posts)}",
                       "\\shortstack{arc(Neg. \\\\ COVID \\\\ RTs)}",
                       "\\shortstack{arc(Neg. \\\\ COVID \\\\ Posts)}",
                       "\\shortstack{arc(Total \\\\ COVID \\\\ RTs)}",
                       "\\shortstack{arc(Total \\\\  COVID \\\\ Posts)}",
                       "\\shortstack{arc(Pos. \\\\ Vaccine \\\\ RTs)}", 
                       "\\shortstack{arc(Pos. \\\\ Vaccine \\\\ Posts)}",
                       "\\shortstack{arc(Neu. \\\\ Vaccine \\\\ RTs)}",
                       "\\shortstack{arc(Neu. \\\\ Vaccine \\\\ Posts)}",
                       "\\shortstack{arc(Neg. \\\\ Vaccine \\\\ RTs)}",
                       "\\shortstack{arc(Neg. \\\\ Vaccine \\\\ Posts)}",
                       "\\shortstack{arc(Total \\\\ Vaccine \\\\ RTs)}",
                       "\\shortstack{arc(Total \\\\ Vaccine \\\\ Posts)}")
    }else if (type == ''){
      dep_var_new <- c("\\shortstack{Positive \\\\ COVID \\\\ RTs}", 
                       "\\shortstack{Positive \\\\ COVID \\\\ Posts}",
                       "\\shortstack{Neutral \\\\ COVID \\\\ RTs}",
                       "\\shortstack{Neutral \\\\ COVID \\\\ Posts}",
                       "\\shortstack{Negative \\\\ COVID \\\\ RTs}",
                       "\\shortstack{Negative \\\\ COVID \\\\ Posts}",
                       "\\shortstack{Total \\\\ COVID \\\\ RTs}",
                       "\\shortstack{Total \\\\  COVID \\\\ Posts}",
                       "\\shortstack{Positive \\\\ Vaccine \\\\ RTs}", 
                       "\\shortstack{Positive \\\\ Vaccine \\\\ Posts}",
                       "\\shortstack{Neutral \\\\ Vaccine \\\\ RTs}",
                       "\\shortstack{Neutral \\\\ Vaccine \\\\ Posts}",
                       "\\shortstack{Negative \\\\ Vaccine \\\\ RTs}",
                       "\\shortstack{Negative \\\\ Vaccine \\\\ Posts}",
                       "\\shortstack{Total \\\\ Vaccine \\\\ RTs}",
                       "\\shortstack{Total \\\\ Vaccine \\\\ Posts}")
    }
    
    # Load pvalues obtained through 1000 permutations of assigments
    pvals = read_excel(paste0("../../../data/04-analysis/", country, 
                              "/",stage,"/",type,
                              "agg_pvalues_all_country_b1b2p_sent_bert.xlsx"))
    
    pvals <- paste(as.character(pvals[1,]), collapse =" & ")
    
    # Endline Results: use felm so that we can use stargazer
    # Endline Results: use felm so that we can use stargazer
    aux_data <- data[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated  | total_influencers"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, felm(fmla1, data = data))
      lm_list_ols[[count]] <- get(nam1, envir = globalenv())
      count <- count + 1
    }
    
    # Get the Robust S.E.s
    lm_se <- list()
    count2 <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated | total_influencers"))
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
      label = paste0("tab:baseline_table_sent_bert_b1b2p_all_",type,stage),
      header = FALSE,
      font.size = "scriptsize",
      dep.var.caption = "",
      dep.var.labels.include = FALSE,
      table.placement = "!htpb",
      column.labels = dep_var_new,
      covariate.labels = c('Total Treated'),
      omit = c('total_influencers'),
      omit.stat=c("f", "ser","adj.rsq"),
      column.sep.width = "0pt",
      add.lines = list(c("Outcome mean", means),
                       c("Outcome std. dev.", sds),
                       c("Outcome range", range)),
      title = paste0('Panel C, Pooled Sentiment (BERT) Analysis Baseline Results (Pilot, Batch 1 and 2) for the SMI Treatment in Both Countries'),
      type = "latex") %>% 
      star_insert_row(insert.after=13, paste0(" & ", pvals, " \\\ "))
    
    note.latex <- paste0("\\multicolumn{17}{l} {\\parbox[t]{24cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows for each 
strong, weak, and neither tie as controls. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
    table[grepl("Note", table)] <- note.latex
    print(table)
    cat(table, file = paste0("../../../results/04-analysis/new/", type,
                             "baseline_table_sent_bert_b1b2p_all.tex"))
    
  }
  
}