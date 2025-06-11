library(tidyverse)
rm(list = ls())
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
country <- 'SA'
#country <- 'KE'

df_stages <- paste0(rep('stage', 6), c(1:6)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'fake', country))

write_parquet(df_stages, paste0(path_normal, country, '/endline_fake_panel.parquet'))

df_stages_b2 <- paste0(rep('stage', 2), c(1:2)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'fake_batch2', country))

write_parquet(df_stages_b2, paste0(path_normal, country, 
                                   '/endline_fake_panel_b2_v1.parquet'))

df_stages_p <- paste0(rep('stage', 4), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'fake_pilot', country))

write_parquet(df_stages_p, paste0(path_normal, country, 
                                  '/endline_fake_panel_p_v1.parquet'))


## Aggregating the stages:


df_stages_agg <- df_stages |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b1')

write_parquet(df_stages_agg, paste0(path_normal, country, 
                                   '/stage1_2/endline_fake_b1.parquet'))

df_stages_agg1 <- df_stages |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b1')

write_parquet(df_stages_agg1, paste0(path_normal, country, 
                                    '/stage3_4/endline_fake_b1.parquet'))

df_stages_agg2 <- df_stages |> 
  filter(stage_id == 'stage5' | stage_id == 'stage6') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b1')

write_parquet(df_stages_agg2, paste0(path_normal, country, 
                                    '/stage5_6/endline_fake_b1.parquet'))

df_stages_agg_b2 <- df_stages_b2 |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'b2')


final_1_2 <- rbind(df_stages_agg, df_stages_agg_b2)

write_parquet(final_1_2, paste0(path_normal, country, 
                                '/stage1_2/endline_fake_b1_b2.parquet'))


df_stages_agg_p <- df_stages_p |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'p')

final_1_2_p <- rbind(final_1_2, df_stages_agg_p)

write_parquet(final_1_2_p, paste0(path_normal, country, 
                                  '/stage1_2/endline_fake_b1_b2_p.parquet'))

