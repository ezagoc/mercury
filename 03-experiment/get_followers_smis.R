### Get Followers from Geo Located Twitter Users to Build the Network in Mexico
# Author: Eduardo Zago

library(tidyverse)
library(arrow)
library(academictwitteR)
country = 'SA'
batch = ''
#batch = '_batch2'

path_ids <-  paste0('../../data/02-randomize/', country, 
                    '/03-assignment/output/RandomizedTwitterSample',country, '.xlsx')
path_out <- paste0('../../data/07-followers/',country,
                   '/batch1/2023-06-13/collect/')
path_invalids <- paste0('../../data/07-followers/',country,
                        '/batch1/2023-06-13/invalids/')

# Tokens: 
# HL (Zago)
bearer <- "AAAAAAAAAAAAAAAAAAAAAEEXcQEAAAAANiZf0Y2n%2B2nT%2FCfdCz0kqkpep%2Fs%3Dy0yeu7cSyURT6fVUr1MbobyC57R6aLbHUkwzWRCMJERJhwVf1u"

# Read the file of ids:

df <- readxl::read_xlsx(path_ids)

ids <- df$author_id

onlyfiles <- list.files(path_out)
onlyfiles <- gsub('.parquet', '', onlyfiles)
ids_final <- setdiff(unique(ids), onlyfiles)
ids_final <- sort(ids_final)

# Function to obtain the followers and the ones that do not have or are blocked

get_followers <- function(id_user){
  followers <- get_user_followers(id_user, bearer_token = bearer)
  
  if (length(colnames(followers)) == 1) {
    print(paste0('No followers available for ', id_user))
    df_out <- tibble(id = id_user)
    write_parquet(df_out, paste0(path_invalids, id_user, '.parquet'))
    
  }else {
    if (c('location') %in% colnames(followers)){
      followers <- followers |> unnest(public_metrics) |> 
        select(from_id, id, username, description, created_at, location, 
               followers_count, following_count, tweet_count, listed_count)
      
      write_parquet(followers, paste0(path_out, id_user, '.parquet'))
    } else{
      followers <- followers |> unnest(public_metrics) |> mutate(location = NA) |>
        select(from_id, id, username, description, created_at, location, 
               followers_count, following_count, tweet_count, listed_count)
      
      write_parquet(followers, paste0(path_out, id_user, '.parquet'))
    }
  }
}

ids_final |> map(~ get_followers(.x))
