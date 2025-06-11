rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# Define round and set of dependent variables:

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/stage1_2',
                             '/endline_b1_b2_p.parquet'))

bots_ke_b1 <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1')

bots_ke_b2 <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch2.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b2') |>
  select(-c("__index_level_0__"))

bots_ke_p <- read_parquet(paste0('../../../data/04-analysis/KE/new_sample_bots_pilot.parquet')) |>
  select(follower_id, dummy_95) |> mutate(dummy_sample = 1, batch_id = 'p')

bots <- rbind(bots_ke_b1, bots_ke_b2, bots_ke_p)

df_ke <- df_ke |> mutate(pais = 'KE') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0) |> 
  select(follower_id, batch_id, n_posts_base)

get_stages <- function(stage, country){
  url <- read_parquet(paste0('../../../data/04-analysis/', country, '/', 
                             stage,'/urls.parquet')) |> 
    mutate(batch_id = 'b1')
  
  url_p <- read_parquet(paste0('../../../data/04-analysis/', country, '/', 
                               stage,'/urls_pilot.parquet')) |> 
    mutate(batch_id = 'p')
  
  url_b2 <- read_parquet(paste0('../../../data/04-analysis/', country, '/', 
                                stage,'/urls_batch2.parquet'))|> 
    mutate(batch_id = 'b2')
  
  final <- rbind(url, url_b2, url_p)
  
  return(final)
}

get_stages2 <- function(stage, country){
  url <- read_parquet(paste0('../../../data/04-analysis/', country, '/', 
                             stage,'/urls.parquet')) |> 
    mutate(batch_id = 'b1')

  url_b2 <- read_parquet(paste0('../../../data/04-analysis/', country, '/', 
                                stage,'/urls_batch2.parquet'))|> 
    mutate(batch_id = 'b2')
  
  final <- rbind(url, url_b2)
  
  return(final)
}


for (stage in list('stage1_2', 'stage3_4', 'stage5_6')){
  if (stage %in% c('stage1_2', 'stage3_4')){
    urls_final <- get_stages(stage = stage, country = 'KE')
  }else{
    urls_final <- get_stages2(stage = stage, country = 'KE')
  }
  
  
  urls_final <- urls_final |> mutate(pais = 'KE') |> 
    left_join(bots, by = c('follower_id', 'batch_id')) |> 
    filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0) |> 
    left_join(df_ke, by = c('follower_id', 'batch_id'))
  
  write_parquet(urls_final, paste0('../../../data/04-analysis/KE/', stage, 
                                   '/final_data_b1b2p_urls.parquet'))
  
}



############################

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/stage1_2',
                             '/endline_b1_b2_p.parquet'))

bots_sa_b1 <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1') |>
  select(-c("__index_level_0__"))

bots_sa_b2 <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch2.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b2') |>
  select(-c("__index_level_0__"))

bots_sa_p <- read_parquet(paste0('../../../data/04-analysis/SA/new_sample_bots_pilot.parquet')) |>
  select(follower_id, dummy_95) |> mutate(dummy_sample = 1, batch_id = 'p') 

bots <- rbind(bots_sa_b1, bots_sa_b2, bots_sa_p)

df_sa <- df_sa |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0) |> 
  select(follower_id, batch_id, n_posts_base)

for (stage in list('stage1_2', 'stage3_4', 'stage5_6')){
  if (stage %in% c('stage1_2', 'stage3_4')){
    urls_final <- get_stages(stage = stage, country = 'SA')
  }else{
    urls_final <- get_stages2(stage = stage, country = 'SA')
  }
  
  urls_final <- urls_final |> mutate(pais = 'SA') |> 
    left_join(bots, by = c('follower_id', 'batch_id')) |> 
    filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0) |> 
    left_join(df_ke, by = c('follower_id', 'batch_id'))
  
  write_parquet(urls_final, paste0('../../../data/04-analysis/SA/', stage, 
                                   '/final_data_b1b2p_urls.parquet'))
  
}
