library(arrow)
library(academictwitteR)
library(tidyverse)
library(readxl)

rm(list = ls()) # Clean out elements in R environment
setwd("../../data/")
#source("../../code/02-randomize/funcs.R")

df <- readxl::read_excel('./03-experiment/ads/0-analytics/2023-04-01-to-2023-07-01-agg_b2.xlsx') %>% 
  distinct(`Tweet ID`, .keep_all = TRUE)

tweets <- df$`Tweet ID`
bearer <- 'AAAAAAAAAAAAAAAAAAAAABR%2FUQEAAAAAHS0JZPRE%2BzA3NYrldLvJ37ulcNE%3DZh2Y4yUoA6EtSjtdLzC6urLqD11YOclklNx4x466a6VVYoyZwe'
analytics <- hydrate_tweets(tweets, bearer_token = bearer)

analytics <- analytics |> unnest(public_metrics)

analytics <- analytics |> select(author_id, id, lang, conversation_id, created_at,
                                 text, retweet_count:impression_count)


df <- df |> rename(id = `Tweet ID`) |> left_join(analytics, by = 'id')

write_parquet(df, './03-experiment/ads/0-analytics/2023-04-01-to-2023-07-01-agg_b2.parquet')
writexl::write_xlsx(df, './03-experiment/ads/0-analytics/2023-04-01-to-2023-07-01-agg_b2.xlsx')
