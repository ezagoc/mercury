rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)



for (stage in list('stage1_2', 'stage3_4' ,'stage5_6')){
  for (base in list('')){
    for (type in list('log_', 'arc_', '')){
      country <- 'joint'
      
      # Define the dependent variables
      aux <- c('verifiability_rt', 'true_rt', 'fake_rt', 'n_posts_rt', 
               'verifiability_no_rt', 'true_no_rt',  'fake_no_rt', 'n_posts_no_rt')
      
      df_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                                   '/endline_b1.parquet'))
      
      fake_ke <- read_parquet(paste0('../../data/04-analysis/KE/', stage,
                                     '/endline_fake_b1.parquet'))
      
      bots_ke <- read_parquet(paste0('../../data/04-analysis/KE/bots_batch1.parquet')) |>
        rename(follower_id = author_id)
      
      baseline_ke <- read_parquet('../../data/04-analysis/KE/baseline_months.parquet')
      
      df_ke <- df_ke |> left_join(fake_ke, by = c('follower_id', 'batch_id')) |> 
        left_join(bots_ke, by = 'follower_id') |> 
        left_join(baseline_ke, by = c('follower_id', 'username')) |> 
        filter(dummy_95 == 0) |>
        mutate(pais = 'KE')
      
      df_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                                   '/endline_b1.parquet'))
      
      fake_sa <- read_parquet(paste0('../../data/04-analysis/SA/', stage,
                                     '/endline_fake_b1.parquet'))
      
      bots_sa <- read_parquet(paste0('../../data/04-analysis/SA/bots_batch1.parquet')) |>
        rename(follower_id = author_id) |> select(-c("__index_level_0__"))
      
      baseline_sa <- read_parquet('../../data/04-analysis/SA/baseline_months.parquet')
      
      df_sa <- df_sa |> left_join(fake_sa, by = c('follower_id', 'batch_id')) |> 
        left_join(bots_sa, by = 'follower_id') |> 
        left_join(baseline_sa, by = c('follower_id', 'username')) |> 
        filter(dummy_95 == 0) |>
        mutate(pais = 'SA') 
      
      df <- rbind(df_ke, df_sa)
      
      df_log <- df |> select(follower_id, pais, batch_id, 
                             total_shares_base:n_posts_no_rt_base_2_month) |> 
        mutate(across(c(total_shares_base:n_posts_no_rt_base_2_month), ~log(.x + 1)))
      
      colnames(df_log)[4:60] <- paste0('log_', colnames(df_log)[4:60])
      
      df_arcsin <- df |> select(follower_id, pais, batch_id, 
                                total_shares_base:n_posts_no_rt_base_2_month) |> 
        mutate(across(c(total_shares_base:n_posts_no_rt_base_2_month), ~asinh(.x + 1)))
      
      colnames(df_arcsin)[4:60] <- paste0('arc_', colnames(df_arcsin)[4:60])
      
      df_share <- df |> select(follower_id, pais, batch_id, 
                               total_shares_base:n_posts_no_rt_base_2_month)
      
      df_share <- df_share |> 
        mutate(across(c(total_shares_base:n_posts_no_rt_base_2_month), 
                      ~.x/sum(df_share$.x)))
      
      colnames(df_share)[4:60] <- paste0('share_', colnames(df_share)[4:60])
      
      df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
        left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id')) |> 
        left_join(df_share, by = c('follower_id', 'pais', 'batch_id'))
      
      df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                         total_influencers = c_t_strong_total + c_t_weak_total + 
                           c_t_neither_total)
      
      pvals = read_excel(paste0("../../data/04-analysis/", country, 
                                "/",stage,"/agg_pvalues_b1_b2_all", base, 
                                type,".xlsx"))
      
      pvals <- paste(as.character(pvals[1,]), collapse =" & ")
      
      if (stage == 'stage1_2'){
        weeks = 'Week 1-4'
      }else if (stage == 'stage3_4'){
        weeks = 'Week 5-8'
      }else if (stage == 'stage5_6'){
        weeks = 'Week 9-12'}
      
      if (type == 'log_'){
        dep_var_new <- c("\\shortstack{Log Verifiable \\\\ RTs}", 
                         "\\shortstack{Log True \\\\ RTs}",
                         "\\shortstack{Log Fake \\\\ RTs}",
                         "\\shortstack{Log Total \\\\ RTs}",
                         "\\shortstack{Log Verifiable \\\\ Posts}",
                         "\\shortstack{Log True \\\\ Posts}",
                         "\\shortstack{Log Fake \\\\ Posts}",
                         "\\shortstack{Log Total \\\\ Posts}")
      }else if (type == 'arc_'){
        dep_var_new <- c("\\shortstack{arcsinh(Ver \\\\ RTs)}", 
                         "\\shortstack{arcsinh(True \\\\ RTs)}",
                         "\\shortstack{arcsinh(Fake \\\\ RTs)}",
                         "\\shortstack{arcsinh(Total \\\\ RTs)}",
                         "\\shortstack{arcsinh(Verifiable \\\\ Posts)}",
                         "\\shortstack{arcsinh(True \\\\ Posts)}",
                         "\\shortstack{arcsinh(Fake \\\\ Posts)}",
                         "\\shortstack{arcsinh(Total \\\\ Posts)}")
      }else if (type == ''){
        dep_var_new <- c("\\shortstack{Verifiable \\\\ RTs}", 
                         "\\shortstack{True \\\\ RTs}",
                         "\\shortstack{Fake \\\\ RTs}",
                         "\\shortstack{Total \\\\ RTs}",
                         "\\shortstack{Verifiable \\\\ Posts}",
                         "\\shortstack{True \\\\ Posts}",
                         "\\shortstack{Fake \\\\ Posts}",
                         "\\shortstack{Total \\\\ Posts}")
      }
      
      else if (type == 'share_'){
        dep_var_new <- c("\\shortstack{Share of Total \\\\ Ver. RTs }", 
                         "\\shortstack{Share of Total \\\\ True RTs )}",
                         "\\shortstack{Share of Total \\\\ Fake RTs )}",
                         "\\shortstack{Share of Total \\\\ Total RTs )}",
                         "\\shortstack{Share of Total \\\\ Ver. Posts )}",
                         "\\shortstack{Share of Total \\\\True Posts )}",
                         "\\shortstack{Share of Total \\\\  Fake Posts )}",
                         "\\shortstack{Share of Total \\\\ Total Posts )}")
      }
      
      data <- df
      
      aux <- paste0(type, aux)
      
      aux_data <- data[aux]
      lm_list_ols <- list()
      count <- 1
      for (x in aux) {
        fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                   x, "_base" ,base,"  | total_influencers"))
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
                                   x, "_base", base ,"| total_influencers"))
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
        label = paste0("tab:endline_table_b1_",stage,"", type, base),
        header = FALSE,
        font.size = "scriptsize",
        dep.var.caption = "",
        dep.var.labels.include = FALSE,
        table.placement = "!htpb",
        column.labels = dep_var_new,
        covariate.labels = c("Total Treated"),
        omit = c('total_influencers', paste0(aux, '_base')),
        omit.stat=c("f", "ser","adj.rsq"),
        column.sep.width = "0pt",
        add.lines = list(c("Baseline control", rep("Yes", 8)),
                         c("Outcome mean", means),
                         c("Outcome std. dev.", sds),
                         c("Outcome range", range)),
        title = paste0('Panel C, Pooled (Batch 1) Endline Results for the SMI Treatment in Both Countries', 
                       base, ' , (', weeks,')'),
        type = "latex")  %>% 
        star_insert_row(insert.after=13, paste0(" & ", pvals, " \\\ "))
      
      note.latex <- paste0("\\multicolumn{9}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
The unit of observation is an influencer's follower. We report estimates from OLS regression.
Specifications further include the total number of influencers each individual follows as control. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
      table[grepl("Note", table)] <- note.latex
      print(table)
      cat(table, file = paste0("../../results/04-analysis/new/endline_table_b1_", stage,  
                               base, type, ".tex"))
    }
  }
}