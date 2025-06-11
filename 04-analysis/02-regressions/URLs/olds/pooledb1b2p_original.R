rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

country <- 'joint'

# Define round and set of dependent variables:
# Define the dependent variables

for (stage in list( 'stage5_6')){
  for (type in list('')){
    
    df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage, 
                                 '/final_data_b1b2p_urls.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage, 
                                 '/final_data_b1b2p_urls.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    df <- df |> filter(n_posts_base > 0)
    
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
    
    aux <- c('total_urls', 'total_info', 'total_news', 'fact_check', 
             'rel_news', 'non_rel_news', 'other')
    
    aux <- paste0(type, aux)
    
    aux_data <- df[aux]
    lm_list_ols <- list()
    count <- 1
    for (x in aux) {
      fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                                 x, "_base | total_influencers"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = df))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0(x)
      coefs <- cbind('treatment' = rownames(coefs), coefs) |>
        filter(treatment != paste0(x, '_base'))
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_all <- lm_list_ols %>% 
      reduce(left_join, by = "treatment")
    
    # Build matrix
    tot_urls <- coefs_all %>% 
      select(ends_with(aux[1]))
    tot_urls <- tot_urls[1,]
    
    tot_info <- coefs_all %>% 
      select(ends_with(aux[2]))
    tot_info <- tot_info[1,]
    
    tot_news <- coefs_all %>% 
      select(ends_with(aux[3]))
    tot_news <- tot_news[1,]
    
    fc <- coefs_all %>% 
      select(ends_with(aux[4]))
    fc <- fc[1,]
    
    rel <- coefs_all %>% 
      select(ends_with(aux[5]))
    rel <- rel[1,]
    
    nrel <- coefs_all %>% 
      select(ends_with(aux[6]))
     nrel <- nrel[1,]
     
     other <- coefs_all %>% 
       select(ends_with(aux[7]))
     other <- other[1,]
    
    coefs_perm <- data.frame(tot_urls, tot_info, tot_news, fc, rel, nrel, other)
    
    write_xlsx(
      coefs_perm, paste0("../../../data/04-analysis/",country,
                         "/",stage,"/", type, "pestimates_b1b2p_urls.xlsx"))
  }
}
