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

# Read df (Aggregated Data Set)

index_gen_stages <- function(stage, country){
  df <- read_parquet(paste0('../../../data/03-experiment/',country,
                            '/treatment/followers/01-preprocess/aggregated/',stage,
                            '_rt_batch2.parquet'))
  
  int_base <- df[c('total_shares_base', 'total_reactions_base', 
                   'total_comments_base')]
  index_int_base <- icwIndex(int_base |> as.matrix())
  df$index_int_base <- scale(index_int_base$index)
  
  int <- df[c('total_shares', 'total_reactions', 
              'total_comments')]
  index_int <- icwIndex(int |> as.matrix())
  df$index_int <- scale(index_int$index)
  
  int_no_rt_base <- df[c('total_shares_no_rt_base', 'total_reactions_no_rt_base', 
                         'total_comments_no_rt_base')]
  index_int_no_rt_base <- icwIndex(int_no_rt_base |> as.matrix())
  df$index_int_no_rt_base <- scale(index_int_no_rt_base$index)
  
  int_no_rt <- df[c('total_shares_no_rt', 'total_reactions_no_rt', 
                    'total_comments_no_rt')]
  index_int_no_rt <- icwIndex(int_no_rt |> as.matrix())
  df$index_int_no_rt <- scale(index_int_no_rt$index)
  
  write_parquet(df, paste0('../../../data/04-analysis/',country,'/',stage,
                       '/endline_batch2.parquet'))
}

c('stage1', 'stage2') |> map(~index_gen_stages(.x, country = 'SA'))

#c('stage1', 'stage2') |> map(~index_gen_stages(.x, country = 'KE'))

