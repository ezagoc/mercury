## Aggregate features 

library(tidyverse)
library(arrow)

## Paths

path_pilot <- '../../../social-media-influencers-africa/data/04-analysis/'
path_normal <- '../../data/04-analysis/'

get_panel_stages <- function(stage, path, batch, country){
  df <- read_parquet(paste0(path, country, '/',stage,
                             '/endline_', batch,'.parquet'))
  df <- df |> mutate(stage_id = stage)
  df
}

### Generate panel:
country <- 'KE'

df_stages <- paste0(rep('stage', 6), c(1:6)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'final', country))

write_parquet(df_stages, paste0(path_normal, country, '/endline_panel.parquet'))

df_stages_b2 <- paste0(rep('stage', 2), c(1:2)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'batch2', country))

write_parquet(df_stages_b2, paste0(path_normal, country, 
                                   '/endline_panel_b2_v1.parquet'))

df_stages_p <- paste0(rep('stage', 4), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_pilot, 'final', country))

write_parquet(df_stages_p, paste0(path_normal, country, 
                                   '/endline_panel_p_v1.parquet'))


## Aggregating the stages:

base <- read_parquet(paste0(path_normal, country, '/baseline_rt.parquet')) |> 
  select(-c(bot_account, '__index_level_0__')) |> 
  rename(total_shares_base = shares_base, total_comments_base = comments_base,
         total_reactions_base = reactions_base)

df_stages_agg <- df_stages |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b1')

final_batch1 <- base |> left_join(df_stages_agg, by = 'follower_id')

write_parquet(final_batch1, paste0(path_normal, country, 
                                '/stage1_2/endline_b1.parquet'))

df_stages_agg1 <- df_stages |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b1')

base <- read_parquet(paste0(path_normal, country, '/baseline_rt.parquet')) |> 
  select(-c(bot_account, '__index_level_0__')) |> 
  rename(total_shares_base = shares_base, total_comments_base = comments_base,
         total_reactions_base = reactions_base)

final_stage34 <- base |> left_join(df_stages_agg1, by = 'follower_id')

write_parquet(final_stage34, paste0(path_normal, country, 
                                   '/stage3_4/endline_b1.parquet'))

df_stages_agg2 <- df_stages |> 
  filter(stage_id == 'stage5' | stage_id == 'stage6') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b1')

base <- read_parquet(paste0(path_normal, country, '/baseline_rt.parquet')) |> 
  select(-c(bot_account, '__index_level_0__')) |> 
  rename(total_shares_base = shares_base, total_comments_base = comments_base,
         total_reactions_base = reactions_base)

final_stage34 <- base |> left_join(df_stages_agg2, by = 'follower_id')

write_parquet(final_stage34, paste0(path_normal, country, 
                                    '/stage5_6/endline_b1.parquet'))

df_stages_agg <- df_stages_b2 |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b2')

base1 <- read_parquet(paste0(path_normal, country, 
                            '/stage1/endline_batch2.parquet')) |>
  select(username:n_posts_no_rt_base) |> select(-c(id, blockid1, blockid2))

final_batch2 <- base1 |> left_join(df_stages_agg, by = 'follower_id')

final_1_2 <- rbind(final_batch1, final_batch2)

write_parquet(final_1_2, paste0(path_normal, country, 
                                '/stage1_2/endline_b1_b2.parquet'))


df_stages_agg <- df_stages_p |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'p')

base1 <- read_parquet(paste0(path_pilot, country, 
                             '/stage1/endline_final.parquet')) |> 
  select(username:n_posts_base)

final_p <- base1 |> left_join(df_stages_agg, by = 'follower_id')

final_1_2_p <- rbind(final_1_2, final_p)

write_parquet(final_1_2_p, paste0(path_normal, country, 
                                '/stage1_2/endline_b1_b2_p.parquet'))

