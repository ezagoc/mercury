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
country <- 'SA'

df_stages <- paste0(rep('stage', 6), c(1:6)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'sent', country))

df_stages <- df_stages |> select(-c("__index_level_0__"))

write_parquet(df_stages, paste0(path_normal, country, '/sentiment_panel.parquet'))

base <- read_parquet(paste0(path_normal, 
                            country, '/stage1/endline_sent.parquet')) |>
  select(username:n_posts_no_rt_vax_base)

### Divide by Stages:
## 1-2

stage1_2 <- df_stages |> select(follower_id, pos_v_rt:stage_id) |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> group_by(follower_id) |>
  summarise(across(c(pos_v_rt:n_posts_no_rt_vax), ~sum(.x))) |> ungroup()

stage1_2 <- base |> left_join(stage1_2, by = 'follower_id')

write_parquet(stage1_2, paste0(path_normal, country, 
                               '/stage1_2/endline_sent_b1.parquet'))

stage1_2 <- stage1_2 |> mutate(batch_id = 'b1')
## 3-4

stage3_4 <- df_stages |> select(follower_id, pos_v_rt:stage_id) |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> group_by(follower_id) |>
  summarise(across(c(pos_v_rt:n_posts_no_rt_vax), ~sum(.x))) |> ungroup()

stage3_4 <- base |> left_join(stage3_4, by = 'follower_id')

write_parquet(stage3_4, paste0(path_normal, country, 
                               '/stage3_4/endline_sent_b1.parquet'))

stage3_4 <- stage3_4 |> mutate(batch_id = 'b1')
## 5-6

stage5_6 <- df_stages |> select(follower_id, pos_v_rt:stage_id) |> 
  filter(stage_id == 'stage5' | stage_id == 'stage6') |> group_by(follower_id) |>
  summarise(across(c(pos_v_rt:n_posts_no_rt_vax), ~sum(.x))) |> ungroup()

stage5_6 <- base |> left_join(stage5_6, by = 'follower_id')

write_parquet(stage5_6, paste0(path_normal, country, 
                               '/stage5_6/endline_sent_b1.parquet'))

stage5_6 <- stage5_6 |> mutate(batch_id = 'b1')
## Batch 2

### Generate panel:

df_stages <- paste0(rep('stage', 2), c(1:2)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'sent_batch2', country))

stage1_2_b2 <- df_stages |> select(follower_id, pos_v_rt:stage_id) |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> group_by(follower_id) |>
  summarise(across(c(pos_v_rt:n_posts_no_rt_vax), ~sum(.x))) |> ungroup()

base <- read_parquet(paste0(path_normal, 
                            country, '/baseline/baseline_sent_batch2.parquet')) |>
  select(username:n_posts_no_rt_vax_base)

stage1_2_b2 <- base |> left_join(stage1_2_b2, by = 'follower_id')

stage1_2_b2 <- stage1_2_b2 |> mutate(batch_id = "b2")

stage1_2_3 <- rbind(stage1_2, stage1_2_b2)


write_parquet(stage1_2_3, paste0(path_normal, country, 
                               '/stage1_2/endline_sent_b1_b2.parquet'))


df_stages <- paste0(rep('stage', 2), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'sent_pilot', country))

stage1_2_p <- df_stages |> select(follower_id, pos_v_rt:stage_id) |> 
  filter(stage_id == 'stage1' | stage_id == 'stage2') |> group_by(follower_id) |>
  summarise(across(c(pos_v_rt:n_posts_no_rt_vax), ~sum(.x))) |> ungroup()

base <- read_parquet(paste0(path_normal, 
                            country, '/baseline/baseline_sent_pilot.parquet')) |>
  select(username:n_posts_no_rt_vax_base)

stage1_2_p <- base |> left_join(stage1_2_p, by = 'follower_id')

stage1_2_p <- stage1_2_p |> mutate(batch_id = "p")

stage1_2_3 <- rbind(stage1_2_3, stage1_2_p)

write_parquet(stage1_2_3, paste0(path_normal, country, 
                                 '/stage1_2/endline_sent_b1_b2_p.parquet'))

###

stage3_4_p <- df_stages |> select(follower_id, pos_v_rt:stage_id) |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> group_by(follower_id) |>
  summarise(across(c(pos_v_rt:n_posts_no_rt_vax), ~sum(.x))) |> ungroup()

base <- read_parquet(paste0(path_normal, 
                            country, '/baseline/baseline_sent_pilot.parquet')) |>
  select(username:n_posts_no_rt_vax_base)

stage3_4_p <- base |> left_join(stage3_4_p, by = 'follower_id')

stage3_4_p <- stage3_4_p |> mutate(batch_id = "p")

stage3_4_batch2 <- read_parquet(paste0(path_normal, country, 
                                       '/stage3_4/endline_sent_batch2.parquet')) |> 
  select(-c("__index_level_0__")) |> mutate(batch_id = 'b2')

stage1_2_3 <- rbind(stage3_4_batch2, stage3_4_p, stage3_4)

write_parquet(stage1_2_3, paste0(path_normal, country, 
                                 '/stage3_4/endline_sent_b1_b2_p.parquet'))

###

stage5_6_batch2 <- read_parquet(paste0(path_normal, country, 
                                       '/stage5_6/endline_sent_batch2.parquet')) |> 
  select(-c("__index_level_0__")) |> mutate(batch_id = 'b2')

stage5_6_1 <- rbind(stage5_6, stage5_6_batch2)


## Here add pilot

#stage5_6_2 <- rbind(stage5_6_1, stagep)

write_parquet(stage5_6_1, paste0(path_normal, country, 
                                 '/stage5_6/endline_sent_b1_b2_p.parquet'))
