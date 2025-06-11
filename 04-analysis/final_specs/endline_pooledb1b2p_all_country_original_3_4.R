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

for (stage in list('stage5_6')){
  df <- get_analysis_data_ver(stage = stage)
  for (type in list('', 'log_', 'arc_')){
    
    aux <- c('verifiability_rt', 'true_rt', 'fake_rt', 'n_posts_rt', 
             'verifiability_no_rt', 'true_no_rt',
             'fake_no_rt',  'n_posts_no_rt')
    
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
    ver_rt <- coefs_all %>% 
      select(ends_with(aux[1]))
    ver_rt <- ver_rt[1,]
    
    true_rt <- coefs_all %>% 
      select(ends_with(aux[2]))
    true_rt <- true_rt[1,]
    
    fake_rt <- coefs_all %>% 
      select(ends_with(aux[3]))
    fake_rt <- fake_rt[1,]
    
    n_posts_rt <- coefs_all %>% 
      select(ends_with(aux[4]))
    n_posts_rt <- n_posts_rt[1,]
    
    ver_no_rt <- coefs_all %>% 
      select(ends_with(aux[5]))
    ver_no_rt <- ver_no_rt[1,]
    
    true_no_rt <- coefs_all %>% 
      select(ends_with(aux[6]))
    true_no_rt <- true_no_rt[1,]
    
    fake_no_rt <- coefs_all %>% 
      select(ends_with(aux[7]))
    fake_no_rt <- fake_no_rt[1,]
    
    n_posts_no_rt <- coefs_all %>% 
      select(ends_with(aux[8]))
    n_posts_no_rt <- n_posts_no_rt[1,]
    
    coefs_perm <- data.frame(ver_rt, true_rt, fake_rt, n_posts_rt, ver_no_rt, 
                             true_no_rt, fake_no_rt,
                             n_posts_no_rt)
    
    write_xlsx(
      coefs_perm, paste0("../../../data/04-analysis/",country,
                         "/",stage,"/", type, "pestimates_b1b2p_all.xlsx"))
  }
}
