rm(list = ls())
library('fastDummies')
library("purrr")
library(lfe)
library(fixest)
library(tidyverse)
library(arrow)

survey <- read_parquet( '../../../data/04-analysis/joint/correlations/pooled_survey.parquet')

s_corr <- survey |> select(username, batch, country, misinfor_1a:COVID_Q7)

s_corr <- s_corr |> rename(batch_id = batch, pais = country) |>
  mutate(batch_id = case_when(batch_id == 0 ~ 'p', batch_id == 1 ~ 'b1', 
                              batch_id == 2 ~ 'b2'))

# Read Sentiment:

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', 'stage1_2', 
                             '/final_data_b1b2p_sent_bert.parquet'))

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage1_2', 
                             '/final_data_b1b2p_sent_bert.parquet'))

sent <- rbind(df_ke, df_sa) |> select(username, batch_id, pais,
                                      pos_b_rt:n_posts_no_rt_vax)

# Read normal

df_ke <- read_parquet(paste0('../../../data/04-analysis/KE/', 'stage1_2', 
                             '/final_data_b1b2p.parquet'))

df_sa <- read_parquet(paste0('../../../data/04-analysis/SA/', 'stage1_2', 
                             '/final_data_b1b2p.parquet'))

ver <- rbind(df_ke, df_sa) |> select(username, batch_id, pais,
                                      total_shares_rt:n_posts, fake_rt,
                                     fake_no_rt)
# Final 

# Create dummies:

s_corr <- dummy_cols(s_corr, select_columns = c('csb_2', 'verify_1', 'COVID_Q6', 
                                                'COVID_Q7'))

# Create right answers:

right_answers_ver_2 <- tibble(
  answer = c('1', '2', '3', '4', '5', '6', '7','8'),
  correct = c('wrong', 'wrong', 'correct', 'correct', 'correct', 
              'wrong', 'wrong', 'wrong')
)

right_answers_ver_3 <- tibble(
  answer = c('1', '2', '3', '4', '5', '6', '7', '8', '9'),
  correct = c('correct', 'wrong', 'correct', 'wrong', 'wrong', 
              'correct', 'correct', 'wrong', 'wrong')
)

right_answers_mis_3 <- tibble(
  answer = c('1', '2', '3','4', '5', '6', '7'),
  correct = c('wrong', 'correct','wrong', 'correct', 'wrong', 'correct', 'wrong')
)


#### count number of rights
num_right_v_2 <- s_corr %>% 
  mutate(verify_2 = as.character(verify_2), 
         verify_2 = strsplit(verify_2, "", fixed = TRUE)) |>
  unnest(verify_2) %>% left_join(right_answers_ver_2, by = c('verify_2' = 'answer')) %>% 
  group_by(username, pais, batch_id) %>% 
  summarise(num_right_ver2 = sum(correct == 'correct'), 
           num_wrong_ver2 = sum(correct == 'wrong'))

num_right_v_3 <- s_corr %>%
  mutate(verify_3 = as.character(verify_3), 
         verify_3 = strsplit(verify_3, "", fixed = TRUE)) |>
  unnest(verify_3) %>% left_join(right_answers_ver_3, by = c('verify_3' = 'answer')) %>% 
  group_by(username, pais, batch_id) %>% 
  summarise(num_right_ver3 = sum(correct == 'correct'), 
            num_wrong_ver3 = sum(correct == 'wrong'))

num_right_m_3 <- s_corr %>%
  mutate(misinfor_3 = as.character(misinfor_3), 
         misinfor_3 = strsplit(misinfor_3, "", fixed = TRUE)) |>
  unnest(misinfor_3) %>% left_join(right_answers_mis_3, by = c('misinfor_3' = 'answer')) %>% 
  group_by(username, pais, batch_id) %>% 
  summarise(num_right_mis3 = sum(correct == 'correct'), 
            num_wrong_mis3 = sum(correct == 'wrong'))


#### merge data
s_corr <- s_corr %>% left_join(num_right_v_3, by = c('username', 'pais', 'batch_id')) |> 
  left_join(num_right_v_2, by = c('username', 'pais', 'batch_id')) |> 
  left_join(num_right_m_3, by = c('username', 'pais', 'batch_id'))

s_corr_disc <- s_corr |> select(username, pais, batch_id,
                                Discernment_1_1:Conspiracy_1_3, 
                                COVID_Q3:COVID_Q1) |>
  mutate(across(c(Discernment_1_1, Discernment_1_2, COVID_Q3:COVID_Q1,
                  Conspiracy_1_1:Conspiracy_1_3), 
                ~ifelse(.x == 1, 1, 0)),
         across(c(Discernment_1_3, Discernment_1_4), ~ifelse(.x == 5, 1, 0)))

colnames(s_corr_disc)[4:13] <- paste0('d_', colnames(s_corr_disc)[4:13])

s_corr <- s_corr %>% left_join(s_corr_disc, by = c('username', 'batch_id', 'pais'))

s_corr <- s_corr |> mutate(D_ver2 = num_right_ver2/4 - num_wrong_ver2/2,
                           D_ver3 = num_right_ver3/4 - num_wrong_ver3/2,
                           D_mis3 = num_right_mis3/4 - num_wrong_mis3/2)

s_corr <-s_corr |> left_join(ver, by = c('username', 'pais', 'batch_id')) |>
  left_join(sent, by = c('username', 'pais', 'batch_id'))

write_parquet(s_corr, '../../../data/04-analysis/joint/correlations/df_analysis.parquet')
