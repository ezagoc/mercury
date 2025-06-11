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

# Define round and set of dependent variables:
country <- 'joint'


for (stage in list('stage1_2')){
  for (type in list('', 'log_', 'arc_')){
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage, 
                                 '/final_data_b1b2p_urls.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage, 
                                 '/final_data_b1b2p_urls.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           fact_check_base:total_info) |> 
      mutate(across(c(fact_check_base:total_info), ~log(.x + 1)))
    
    colnames(df_log)[4:length(df_log)] <- paste0('log_', colnames(df_log)[4:length(df_log)])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              fact_check_base:total_info) |> 
      mutate(across(c(fact_check_base:total_info), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[4:length(df_arcsin)] <- paste0('arc_', 
                                                       colnames(df_arcsin)[4:length(df_arcsin)])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    data <- df
    
    aux <- c('total_urls', 'total_info', 'total_news', 'fact_check', 
             'rel_news', 'non_rel_news')
    
    if (stage == 'stage1_2'){
      weeks = 'Week 1-4'
    }else if (stage == 'stage3_4'){
      weeks = 'Week 5-8'
    }else if (stage == 'stage5_6'){
      weeks = 'Week 9-12'}
    
    if (type == 'log_'){
      dep_var_new <- c("\\shortstack{Log Total \\\\ URLs}", 
                       "\\shortstack{Log Total \\\\ Info.}",
                       "\\shortstack{Log Total \\\\ News}",
                       "\\shortstack{Log Fact-check \\\\ URL}",
                       "\\shortstack{Log Reliable \\\\ News}",
                       "\\shortstack{Log Non-Reliable \\\\ News}")
    }else if (type == 'arc_'){
      dep_var_new <- c("\\shortstack{arc Total \\\\ URLs}", 
                       "\\shortstack{arc Total \\\\ Info.}",
                       "\\shortstack{arc Total \\\\ News}",
                       "\\shortstack{arc Fact-check \\\\ URL}",
                       "\\shortstack{arc Reliable \\\\ News}",
                       "\\shortstack{arc Non-Reliable \\\\ News}")
    }else if (type == ''){
      dep_var_new <- c("\\shortstack{Total \\\\ URLs}", 
                       "\\shortstack{Total \\\\ Info.}",
                       "\\shortstack{Total \\\\ News}",
                       "\\shortstack{Fact-check \\\\ URL}",
                       "\\shortstack{Reliable \\\\ News}",
                       "\\shortstack{Non-Reliable \\\\ News}")
    }
    
    
    # Load pvalues obtained through 1000 permutations of assigments
    pvals = read_excel(paste0("../../../data/04-analysis/", country, 
                              "/",stage,"/", type,
                              "agg_pvalues_b1b2p_urls.xlsx")) |> select(-nrel)
    
    pvals <- paste(as.character(pvals[1,]), collapse =" & ")
    
    # Endline Results: use felm so that we can use stargazer
    aux_data <- data[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                 x, "_base  | total_influencers"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, felm(fmla1, data = data))
      lm_list_ols[[count]] <- get(nam1, envir = globalenv())
      count <- count + 1
    }
    
    # Get the Robust S.E.s
    lm_se <- list()
    count2 <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                 x, "_base  | total_influencers"))
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
      label = paste0("tab:endline_table_b1b2p_urls",stage,"", country),
      header = FALSE,
      font.size = "scriptsize",
      dep.var.caption = "",
      dep.var.labels.include = FALSE,
      table.placement = "!htpb",
      column.labels = dep_var_new,
      covariate.labels = c("Total Treated"),
      omit = c('strat_block1', paste0(aux, '_base')),
      omit.stat=c("f", "ser","adj.rsq"),
      column.sep.width = "0pt",
      add.lines = list(c("Baseline control", rep("Yes", 6)),
                       c("Strat Block1 FEs", rep("Yes", 6)),
                       c("Outcome mean", means),
                       c("Outcome std. dev.", sds),
                       c("Outcome range", range)),
      title = paste0('Panel A, Pooled (Batch 1, 2 and Pilot) Endline Results for the SMI Treatment for Both Countries, (',
                     weeks,')'),
      type = "latex")  %>% 
      star_insert_row(insert.after=13, paste0(" & ", pvals, " \\\ "))
    
    note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. Total URLs is the sum of all posts that contained at least one URL that was not a Twitter link (pictures, RTs) or other Social Media link.
Total info is the sum of all posts that contain links to information, this could be blogs, newsites and organizations. Totla news is the sum of all posts that contained links to newsites.
We report estimates from OLS regression. Specifications further include block1 randomization of the influencers fixed effects. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
    table[grepl("Note", table)] <- note.latex
    print(table)
    cat(table, file = paste0("../../../results/04-analysis/urls/",type,
                             "endline_table_b1b2p_urls_", 
                             stage,".tex"))
    
  }
}