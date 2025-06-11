library(tidyverse)
library(lfe)
library(fixest)
library(arrow)
library(stargazer)
library(tidyr)
# Set WD

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Functions:

convert_to_numeric <- function(characters) {
  # Remove 'K' and convert to numeric
  numeric_values <- as.numeric(gsub("K", "", characters))
  # Multiply by 1000 if 'K' is present
  numeric_values <- numeric_values * ifelse(grepl("K", characters), 1000, 1)
  return(numeric_values)
}

agg_fun <- function(df){
  rt <- df |> filter(RT == 1)
  rt <- rt |> group_by(follower_handle) |> summarise(n_posts = n(),
                                                     across(c(replies:likes, 
                                                              verifiability:fake),
                                                            ~sum(.x, na.rm = T))) |>
    ungroup()
  
  colnames(rt)[2:length(rt)] <- paste0(colnames(rt)[2:length(rt)], '_rt')
  
  post <- df |> filter(post == 1)
  post <- post |> group_by(follower_handle) |> summarise(n_posts = n(),
                                                         across(c(replies:likes, 
                                                                  verifiability:fake),
                                                                ~sum(.x, na.rm = T))) |>
    ungroup()
  colnames(post)[2:length(post)] <- paste0(colnames(post)[2:length(post)], '_no_rt')
  
  df_final <- post |> full_join(rt) |> mutate(across(c(n_posts_no_rt:fake_rt),
                                                     ~ifelse(is.na(.x) == T, 0, .x)))
  return(df_final)
}

path_pilot <- '../../../social-media-influencers-africa/data/04-analysis/'
path_normal <- '../../data/04-analysis/'

get_panel_stages <- function(stage, path, batch, country){
  df <- read_parquet(paste0(path, country, '/',stage,
                            '/endline_', batch,'.parquet'))
  df <- df |> mutate(stage_id = stage)
  df
}


# Baseline:
# 'Kenya' (differences in the baseline information we have up to date)
country = 'KE'

# Import the final data base for batch 2, without bots, etc

base1 = read_parquet(paste0('../../data/04-analysis/', country, 
                            '/stage1_2/final_data_b1b2p.parquet')) |> 
  filter(batch_id == 'b2') |>
  select(username:n_posts_no_rt_base, fake_rt_base, fake_no_rt_base)

cor <- read_parquet(paste0('../../data/03-experiment/', country, 
                           '/treatment/followers/01-preprocess/correct_cases_predicted.parquet.gzip'))

# Clean cor

cor <- cor |> mutate(post = ifelse(is.na(type) == T, 1, 0), 
                     RT = ifelse(post == 0, 1, 0)) |> 
  mutate(date = as.Date(substr(TimeStamp, 1, 10))) |> rename(fake = false)

# Define stages:



cor <- cor |> mutate(stage_3_4 = ifelse(date > '2023-05-28' & date < '2023-06-26', 
                     1, 0), 
                     stage_5_6 = ifelse(date > '2023-06-25' & date < '2023-07-23', 
                                        1, 0), 
                     base_new = ifelse(date < '2023-03-01', 1, 0))

cor <- cor |> mutate(across(c(replies:likes), ~ifelse(.x == '', 0, .x)),
                     across(c(replies:likes), ~gsub(',', '', .x)),
                     across(c(replies:likes), ~convert_to_numeric(.x)),
                     across(c(replies:likes), ~as.numeric(.x))) 

# Stage3

cor_3 <- cor |> filter(stage_3_4 == 1)

cor_3_agg <- agg_fun(cor_3)

cor_5 <- cor |> filter(stage_5_6 == 1)

cor_5_agg <-  agg_fun(cor_5)

base <- cor |> filter(base_new == 1)

base_agg <- agg_fun(base)


# Generate files, merge first with baseline and then remove NAs to 0s and export to the respective folder

stage3_4 <- base1 |> left_join(cor_3_agg, by = c('username' = 'follower_handle')) |> 
  mutate(across(c(n_posts_no_rt:fake_rt), ~ifelse(is.na(.x) == T, 0, .x)))

stage5_6 <- base1 |> left_join(cor_5_agg, by = c('username' = 'follower_handle')) |> 
  mutate(across(c(n_posts_no_rt:fake_rt), ~ifelse(is.na(.x) == T, 0, .x)))

base_new <- base1 |> left_join(base_agg, by = c('username' = 'follower_handle')) |> 
  mutate(across(c(n_posts_no_rt:fake_rt), ~ifelse(is.na(.x) == T, 0, .x)))

colnames(base_new)[15:length(base_new)] <- paste0(colnames(base_new)[15:length(base_new)], 
                                                  '_base_new')


# Export to respective folders:

write_parquet(stage3_4, paste0('../../data/04-analysis/', country, 
                               '/stage3_4/endline_b2.parquet'))

write_parquet(stage5_6, paste0('../../data/04-analysis/', country, 
                               '/stage5_6/endline_b2.parquet'))

write_parquet(base_new, paste0('../../data/04-analysis/', country, 
                               '/baseline/baseline_b2_new.parquet'))

## Now let's aggregate with the pilot and batch 1 data bases to generate the final one:

stage3_4 <- stage3_4 |> mutate(batch_id = 'b2')

b1 <- read_parquet(paste0(path_normal, country, 
                          '/stage3_4/endline_b1.parquet'))

b1_fake <- read_parquet(paste0(path_normal, country, 
                               '/stage3_4/endline_fake_b1.parquet'))

b1 <- b1 |> left_join(b1_fake, by = c('follower_id', 'batch_id'))

bots_ke_b1 <- read_parquet(paste0('../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1')

b1 <- b1 |> left_join(bots_ke_b1, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F)  |> filter(dummy_95 == 0)

b1_b2 <- bind_rows(stage3_4 %>% select(intersect(names(b1), names(stage3_4))), 
                   b1 %>% select(intersect(names(b1), names(stage3_4))))

# Now Pilot:

df_stages_p <- paste0(rep('stage', 4), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_pilot, 'final', country))

df_stages_agg <- df_stages_p |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'p')

base1 <- read_parquet(paste0(path_pilot, country, 
                             '/stage1/endline_final.parquet')) |> 
  select(username:n_posts_base)

final_p <- base1 |> left_join(df_stages_agg, by = 'follower_id')

df_stages_p <- paste0(rep('stage', 4), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'fake_pilot', country))

df_stages_agg_p <- df_stages_p |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'p')

bots_ke_p <- read_parquet(paste0('../../data/04-analysis/KE/new_sample_bots_pilot.parquet')) |>
  select(follower_id, dummy_95) |> mutate(dummy_sample = 1, batch_id = 'p')

final_p <- final_p |> left_join(df_stages_agg_p, by = c('follower_id', 'batch_id')) |> 
  left_join(bots_ke_p, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F)  |> filter(dummy_95 == 0)
  

final_1_2_p <- bind_rows(final_p %>% select(intersect(names(final_p), names(b1_b2))), 
                         b1_b2 %>% select(intersect(names(final_p), names(b1_b2))))

write_parquet(final_1_2_p, '../../data/04-analysis/KE/stage3_4/final_data_b1b2p.parquet')

# Stage 5 6

stage5_6 <- stage5_6 |> mutate(batch_id = 'b2')

b1 <- read_parquet(paste0(path_normal, country, 
                          '/stage5_6/endline_b1.parquet'))

b1_fake <- read_parquet(paste0(path_normal, country, 
                               '/stage5_6/endline_fake_b1.parquet'))

b1 <- b1 |> left_join(b1_fake, by = c('follower_id', 'batch_id'))

bots_ke_b1 <- read_parquet(paste0('../../data/04-analysis/KE/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1')

b1 <- b1 |> left_join(bots_ke_b1, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F)  |> filter(dummy_95 == 0)

b1_b2 <- bind_rows(stage5_6 %>% select(intersect(names(b1), names(stage5_6))), 
                   b1 %>% select(intersect(names(b1), names(stage5_6))))

## Here add the pilot part::

b1_b2_ke <- b1_b2

write_parquet(b1_b2, '../../data/04-analysis/KE/stage5_6/final_data_b1b2p.parquet')

#### SA

# Baseline:
# 'Kenya' (differences in the baseline information we have up to date)
country = 'SA'

# Import the final data base for batch 2, without bots, etc

base1 = read_parquet(paste0('../../data/04-analysis/', country, 
                            '/stage1_2/final_data_b1b2p.parquet')) |> 
  filter(batch_id == 'b2') |>
  select(username:n_posts_no_rt_base, fake_rt_base, fake_no_rt_base)

cor <- read_parquet(paste0('../../data/03-experiment/', country, 
                           '/treatment/followers/01-preprocess/correct_cases_predicted.parquet.gzip'))

# Clean cor

cor <- cor |> mutate(post = ifelse(is.na(type) == T, 1, 0), 
                     RT = ifelse(post == 0, 1, 0)) |> 
  mutate(date = as.Date(substr(TimeStamp, 1, 10))) |> rename(fake = false)

# Define stages:



cor <- cor |> mutate(stage_3_4 = ifelse(date > '2023-05-28' & date < '2023-06-26', 
                                        1, 0), 
                     stage_5_6 = ifelse(date > '2023-06-25' & date < '2023-07-23', 
                                        1, 0), 
                     base_new = ifelse(date < '2023-04-01', 1, 0))

cor <- cor |> mutate(across(c(replies:likes), ~ifelse(.x == '', 0, .x)),
                     across(c(replies:likes), ~gsub(',', '', .x)),
                     across(c(replies:likes), ~convert_to_numeric(.x)),
                     across(c(replies:likes), ~as.numeric(.x))) 

# Stage3

cor_3 <- cor |> filter(stage_3_4 == 1)

cor_3_agg <- agg_fun(cor_3)

cor_5 <- cor |> filter(stage_5_6 == 1)

cor_5_agg <-  agg_fun(cor_5)

base <- cor |> filter(base_new == 1)

base_agg <- agg_fun(base)


# Generate files, merge first with baseline and then remove NAs to 0s and export to the respective folder

stage3_4 <- base1 |> left_join(cor_3_agg, by = c('username' = 'follower_handle')) |> 
  mutate(across(c(n_posts_no_rt:fake_rt), ~ifelse(is.na(.x) == T, 0, .x)))

stage5_6 <- base1 |> left_join(cor_5_agg, by = c('username' = 'follower_handle')) |> 
  mutate(across(c(n_posts_no_rt:fake_rt), ~ifelse(is.na(.x) == T, 0, .x)))

base_new <- base1 |> left_join(base_agg, by = c('username' = 'follower_handle')) |> 
  mutate(across(c(n_posts_no_rt:fake_rt), ~ifelse(is.na(.x) == T, 0, .x)))

colnames(base_new)[15:length(base_new)] <- paste0(colnames(base_new)[15:length(base_new)], 
                                                  '_base_new')


# Export to respective folders:

write_parquet(stage3_4, paste0('../../data/04-analysis/', country, 
                               '/stage3_4/endline_b2.parquet'))

write_parquet(stage5_6, paste0('../../data/04-analysis/', country, 
                               '/stage5_6/endline_b2.parquet'))

write_parquet(base_new, paste0('../../data/04-analysis/', country, 
                               '/baseline/baseline_b2_new.parquet'))


stage3_4 <- stage3_4 |> mutate(batch_id = 'b2')

b1 <- read_parquet(paste0(path_normal, country, 
                          '/stage3_4/endline_b1.parquet'))

b1_fake <- read_parquet(paste0(path_normal, country, 
                               '/stage3_4/endline_fake_b1.parquet'))

b1 <- b1 |> left_join(b1_fake, by = c('follower_id', 'batch_id'))

bots_ke_b1 <- read_parquet(paste0('../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1')

b1 <- b1 |> left_join(bots_ke_b1, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F)  |> filter(dummy_95 == 0)

b1_b2 <- bind_rows(stage3_4 %>% select(intersect(names(b1), names(stage3_4))), 
                   b1 %>% select(intersect(names(b1), names(stage3_4))))

# Now Pilot:

df_stages_p <- paste0(rep('stage', 4), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_pilot, 'final', country))

df_stages_agg <- df_stages_p |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> 
  select(follower_id, total_shares_rt:n_posts) |>
  group_by(follower_id) |> summarise(across(c(total_shares_rt:n_posts), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'p')

base1 <- read_parquet(paste0(path_pilot, country, 
                             '/stage1/endline_final.parquet')) |> 
  select(username:n_posts_base)

final_p <- base1 |> left_join(df_stages_agg, by = 'follower_id')

df_stages_p <- paste0(rep('stage', 4), c(1:4)) |> 
  map_dfr(~get_panel_stages(.x, path_normal, 'fake_pilot', country))

df_stages_agg_p <- df_stages_p |> 
  filter(stage_id == 'stage3' | stage_id == 'stage4') |> 
  select(follower_id, fake_rt_base:fake_no_rt) |>
  group_by(follower_id) |> summarise(across(c(fake_rt_base:fake_no_rt), ~sum(.x))) |>
  ungroup() |> mutate(batch_id = 'p')

bots_ke_p <- read_parquet(paste0('../../data/04-analysis/SA/new_sample_bots_pilot.parquet')) |>
  select(follower_id, dummy_95) |> mutate(dummy_sample = 1, batch_id = 'p')

final_p <- final_p |> left_join(df_stages_agg_p, by = c('follower_id', 'batch_id')) |> 
  left_join(bots_ke_p, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F)  |> filter(dummy_95 == 0)


final_1_2_p <- bind_rows(final_p %>% select(intersect(names(final_p), names(b1_b2)), 
                                            fake_rt_base, fake_no_rt_base), 
                         b1_b2 %>% select(intersect(names(final_p), names(b1_b2)), 
                                          fake_rt_base, fake_no_rt_base))

write_parquet(final_1_2_p, '../../data/04-analysis/SA/stage3_4/final_data_b1b2p.parquet')

# Stage 5 6

stage5_6 <- stage5_6 |> mutate(batch_id = 'b2')

b1 <- read_parquet(paste0(path_normal, country, 
                          '/stage5_6/endline_b1.parquet'))

b1_fake <- read_parquet(paste0(path_normal, country, 
                               '/stage5_6/endline_fake_b1.parquet'))

b1 <- b1 |> left_join(b1_fake, by = c('follower_id', 'batch_id'))

bots_ke_b1 <- read_parquet(paste0('../../data/04-analysis/SA/bots_batch1.parquet')) |>
  rename(follower_id = author_id) |> mutate(dummy_sample = 1, batch_id = 'b1')

b1 <- b1 |> left_join(bots_ke_b1, by = c('follower_id', 'batch_id')) |> 
  filter(is.na(dummy_sample) == F)  |> filter(dummy_95 == 0)

b1_b2 <- bind_rows(stage5_6 %>% select(intersect(names(b1), names(stage5_6))), 
                   b1 %>% select(intersect(names(b1), names(stage5_6))))

## Here add the pilot part::


write_parquet(b1_b2, '../../data/04-analysis/SA/stage5_6/final_data_b1b2p.parquet')
