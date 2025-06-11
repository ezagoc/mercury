# Get User Timeline:
rm(list = ls())
library(tidyverse)
library(academictwitteR)
library(arrow)
library(purrr)

# Paths:
#country <- SA
country <- 'KE'
path_out <- paste0('../../data/03-experiment/',country,
                   '/treatment/followers/00-raw/tweets/april2/')
path_no_tweets <- paste0('../../data/03-experiment/',country,
                         '/treatment/followers/00-raw/tweets/no_tweets_april2/')

rand <- paste0('../../data/02-randomize/',country,
               '/04-stratification/integrate/followers_randomized.parquet')

# Dates:

start =  '2023-04-23T00:00:00Z'
end = '2023-05-08T00:00:00Z'

# Bearer:

bearer <- "HERE_YOUR_TOKEN"

# File with ids:

df <- read_parquet(rand)

# Ids:

ids_initial <- df$follower_id

# Set difference:

onlyfiles <- list.files(path_out)
onlyfiles2 <- list.files(path_no_tweets)
onlyfiles <- gsub('.parquet', '', onlyfiles)
onlyfiles2 <- gsub('.parquet', '', onlyfiles2)

files_difference <- c(onlyfiles, onlyfiles2)
ids <- setdiff(unique(ids_initial), files_difference)
ids <- sort(ids)


# Get user timeline:

get_time_export <- function(id_user){
  tryCatch({
    df_prueba <- get_all_tweets(users = id_user, start_tweets = start, end_tweets = end,
                                bearer_token = bearer, n = Inf, 
                                bind_tweets = T)
    if (length(df_prueba)==0){
      print(paste0('No tweets available for ', id_user))
      df_out <- tibble(id = id_user)
      write_parquet(df_out, paste0(path_no_tweets, id_user, '.parquet'))
    }else{
      df_prueba <- df_prueba |> unnest(public_metrics) |> 
        select(author_id, id, conversation_id, created_at, text, lang, 
               retweet_count, reply_count, like_count, quote_count, impression_count)
      write_parquet(df_prueba, paste0(path_out, id_user, '.parquet'))}
  },
  error=function(cond) {
    print(paste0('INVALIDS', id_user))
    df_out <- tibble(id = id_user)
    write_parquet(df_out, paste0(path_invalids, id_user, '.parquet'))
    return(NA)
  })
}

# Run the loop:

ids |> map(~ get_time_export(.x))
