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

for (stage in list('baseline')){
  for (type in list('', 'log_', 'arc_')){
    
    aux <- c('verifiability_rt', 'true_rt', 'fake_rt', 'n_posts_rt', 
             'verifiability_no_rt', 'true_no_rt',
             'fake_no_rt',  'n_posts_no_rt')
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/','stage1_2', 
                                 '/final_data_b1b2p.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage1_2', 
                                 '/final_data_b1b2p.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_shares_base:fake_no_rt) |> 
      mutate(across(c(total_shares_base:fake_no_rt), ~log(.x + 1)))
    
    colnames(df_log)[4:43] <- paste0('log_', colnames(df_log)[4:43])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_shares_base:fake_no_rt) |> 
      mutate(across(c(total_shares_base:fake_no_rt), ~asinh(.x + 1)))
    
    colnames(df_arcsin)[4:43] <- paste0('arc_', colnames(df_arcsin)[4:43])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    aux <- paste0(type, aux, '_base')
    
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    data <- df
    
    
    if (type == 'log_'){
      dep_var_new <- c("\\shortstack{Log Ver. \\\\ RTs}", 
                       "\\shortstack{Log True \\\\ RTs}",
                       "\\shortstack{Log Fake \\\\ RTs}",
                       "\\shortstack{Log Total \\\\ RTs}",
                       "\\shortstack{Log Ver. \\\\ Posts}",
                       "\\shortstack{Log True \\\\ Posts}",
                       "\\shortstack{Log Fake \\\\ Posts}",
                       "\\shortstack{Log Total \\\\ Posts}")
    }else if (type == 'arc_'){
      dep_var_new <- c("\\shortstack{arcsinh(Ver. \\\\ RTs)}", 
                       "\\shortstack{arcsinh(True \\\\ RTs)}",
                       "\\shortstack{arcsinh(Fake \\\\ RTs)}",
                       "\\shortstack{arcsinh(Total \\\\ RTs)}",
                       "\\shortstack{arcsinh(Ver. \\\\ Posts)}",
                       "\\shortstack{arcsinh(True \\\\ Posts)}",
                       "\\shortstack{arcsinh(Fake \\\\ Posts)}",
                       "\\shortstack{arcsinh(Total \\\\ Posts)}")
    }else if (type == ''){
      dep_var_new <- c("\\shortstack{Verifiable \\\\ RTs}", 
                       "\\shortstack{True\\\\ RTs}",
                       "\\shortstack{Fake \\\\ RTs}",
                       "\\shortstack{Total \\\\ RTs}",
                       "\\shortstack{Verifiable \\\\ Posts}",
                       "\\shortstack{True \\\\ Posts}",
                       "\\shortstack{Fake \\\\  Posts}",
                       "\\shortstack{Total \\\\ Posts}")
    }
    
    
    # Load pvalues obtained through 1000 permutations of assigments
    pvals = read_excel(paste0("../../../data/04-analysis/", country, 
                              "/",stage,"/", type,
                              "agg_pvalues_b1b2p_all.xlsx"))
    
    pvals <- paste(as.character(pvals[1,]), collapse =" & ")
    
    # Endline Results: use felm so that we can use stargazer
    aux_data <- data[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated | total_influencers"))
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
      label = paste0("tab:baseline_table_b1b2p_all",stage,"", country),
      header = FALSE,
      font.size = "scriptsize",
      dep.var.caption = "",
      dep.var.labels.include = FALSE,
      table.placement = "!htpb",
      column.labels = dep_var_new,
      covariate.labels = c("Total Treated"),
      omit = c('total_influencers'),
      omit.stat=c("f", "ser","adj.rsq"),
      column.sep.width = "0pt",
      add.lines = list(c("Outcome mean", means),
                       c("Outcome std. dev.", sds),
                       c("Outcome range", range)),
      title = paste0('Panel A, Pooled (Batch 1, 2 and Pilot) Baseline Results for the SMI Treatment for Both Countries'),
      type = "latex")  %>% 
      star_insert_row(insert.after=13, paste0(" & ", pvals, " \\\ "))
    
    note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows as control. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
    table[grepl("Note", table)] <- note.latex
    print(table)
    cat(table, file = paste0("../../../results/04-analysis/new/",type,
                             "baseline_table_b1b2p_joint.tex"))
    
  }
}

