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

bots_ke_b1 <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1')

bots_ke_b2 <- read_parquet(paste0('../../../data/04-analysis/KE/bots_batch2.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b2') |>
  select(-c("__index_level_0__"))

bots_ke_p <- read_parquet(paste0('../../../data/04-analysis/KE/new_sample_bots_pilot.parquet')) |>
  select(follower_id, dummy_95) |> mutate(dummy_sample = 1, batch_id = 'p')

bots <- rbind(bots_ke_b1, bots_ke_b2, bots_ke_p)

for (stage in list('stage5_6')){
  # df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
  #                              '/endline_b1_b2_p.parquet'))
  # 
  # fake_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
  #                                '/endline_fake_b1_b2_p.parquet'))
  # 
  # df_ke <- df_ke |> left_join(fake_ke, by = c('follower_id', 'batch_id')) |>
  #   mutate(pais = 'KE') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
  #   filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0)
  
  df_sent_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
                               '/endline_sent_b1_b2_p.parquet')) |>
    mutate(pais = 'KE') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
    filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0)
  
  df_sent_bert_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', stage,
                                    '/endline_sent_bert_b1_b2_p.parquet')) |>
    mutate(pais = 'KE') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
    filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0)
  
  # write_parquet(df_ke, paste0('../../../data/04-analysis/KE/', stage, 
  #                             '/final_data_b1b2p.parquet'))
  
  write_parquet(df_sent_bert_ke, paste0('../../../data/04-analysis/KE/', stage, 
                              '/final_data_b1b2p_sent_bert.parquet'))
 
  write_parquet(df_sent_ke, paste0('../../../data/04-analysis/KE/', stage, 
                                   '/final_data_b1b2p_sent.parquet'))
}


############################

bots_sa_b1 <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1') |>
  select(-c("__index_level_0__"))

bots_sa_b2 <- read_parquet(paste0('../../../data/04-analysis/SA/bots_batch2.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b2') |>
  select(-c("__index_level_0__"))

bots_sa_p <- read_parquet(paste0('../../../data/04-analysis/SA/new_sample_bots_pilot.parquet')) |>
  select(follower_id, dummy_95) |> mutate(dummy_sample = 1, batch_id = 'p') 

bots <- rbind(bots_sa_b1, bots_sa_b2, bots_sa_p)

for (stage in list('stage5_6')){
  # df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
  #                              '/endline_b1_b2_p.parquet'))
  # 
  # fake_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
  #                                '/endline_fake_b1_b2_p.parquet'))
  # 
  # df_sa <- df_sa |> left_join(fake_sa, by = c('follower_id', 'batch_id')) |>
  #   mutate(pais = 'SA') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
  #   filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0)
  
  df_sent_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
                                    '/endline_sent_b1_b2_p.parquet')) |>
    mutate(pais = 'SA') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
    filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0)
  
  df_sent_bert_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', stage,
                                         '/endline_sent_bert_b1_b2_p.parquet')) |>
    mutate(pais = 'SA') |> left_join(bots, by = c('follower_id', 'batch_id')) |> 
    filter(is.na(dummy_sample) == F) |> filter(dummy_95 == 0)
  
  # write_parquet(df_sa, paste0('../../../data/04-analysis/SA/', stage,
  #                             '/final_data_b1b2p.parquet'))
  
  write_parquet(df_sent_bert_sa, paste0('../../../data/04-analysis/SA/', stage, 
                                        '/final_data_b1b2p_sent_bert.parquet'))
  
  write_parquet(df_sent_sa, paste0('../../../data/04-analysis/SA/', stage, 
                                   '/final_data_b1b2p_sent.parquet'))
}






  