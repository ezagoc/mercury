#############################################################################
# RCT: Social Media Influencers Based in Africa
# Activity: Creating Indexes
#           Preparing for randomization
# Date: Nov 2022
#############################################################################

######################## Influencers ########################

# General Configurations ----------------------------------------------------

rm(list=ls()) # Clean out elements in R environment
setwd('../../data/02-randomize/')
source('../../code/02-randomize/funcs.R')

# Install and load packages

packages <- ipak(c("dplyr","tidyverse"))

# Covariates 

cov0 <- c(
  "username",
  "name",
  "author_id",
  "followers_count", 
  "listed_count",   
  "n_tweets.na",                   
  "n_tweets",                    
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account"
  )

cov1 = c(
  "like_count_sum",               
  "quote_count_sum",              
  "reply_count_sum"  
)

### KE

twitter = arrow::read_parquet(
  'KE/02-variables/variables_batch2.parquet'
  )

# Faulty profile:

# f <- twitter |> filter(is.na(created_at) == T)
# 
# f <- f |> select(-c(created_at, followers_count, following_count, listed_count,
#                     username, name, tweet_count))
# 
# library(academictwitteR)
# 
# bearer <- 'AAAAAAAAAAAAAAAAAAAAAAB8lgEAAAAAtHuFxjMbRwl7WNHEOpMvzf7%2BGrc%3DATF52dZ90jRf9u9qxVvuiC7WLYCte5c9U4HrWfsuz9RK59Girq'
# 
# 
# final <- get_user_profile('1197307644', bearer_token = bearer)
# final <- final |> unnest(public_metrics) |>
#   select(username, name, created_at,
#          followers_count, tweet_count,
#          following_count, listed_count)
# 
# f <- cbind(f, final)
# 
# f <- f |> mutate(days_old_account = as.numeric(difftime("2023-03-08",
#                                                         "2013-02-19",
#                                                         units = "days")))
# 
# twitter <- twitter |> filter(is.na(created_at) == F)
# 
# twitter <- rbind(twitter, f)
# 
# arrow::write_parquet(twitter, 'KE/02-variables/variables.parquet')

twitter_ties = arrow::read_parquet(
  'KE/02-variables/variables_ties_batch2.parquet'
)

twitter = twitter |> left_join(twitter_ties |> rename(author_id = influencer_id),
                               by = 'author_id') |> select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))


# Save data

save(twitter, file = 'KE/03-assignment/input/twitter_batch2.Rda')


### SA

twitter = arrow::read_parquet(
  'SA/02-variables/variables_batch2.parquet'
) |> mutate(author_id = as.character(author_id))

twitter_ties = arrow::read_parquet(
  'SA/02-variables/variables_ties_batch2.parquet'
)

twitter = twitter |> left_join(twitter_ties |> rename(author_id = influencer_id),
                               by = 'author_id') |> select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))


# Save data

save(twitter, file = 'SA/03-assignment/input/twitter_batch2.Rda')


######################## Followers ########################

# General Configurations ----------------------------------------------------

rm(list=ls()) # Clean out elements in R environment
setwd('../../data/02-randomize/')
source('../../code/02-randomize/funcs.R')

# Install and load packages

packages <- ipak(c("dplyr","tidyverse"))

# Covariates 

cov0 <- c(
  "username",
  "follower_id",
  "n_strong",   
  "n_weak",                   
  "n_absent",                    
  "n_strong_treated",
  "n_weak_treated",
  "n_absent_treated",
  "days_old_account"
)

cov1 <- c(
  "followers_count",               
  "following_count",              
  "listed_count",
  "tweet_count"
)

#batch <- ''
batch <- '_batch2'
country <- 'KE'

###
library(arrow)
twitter = read_parquet(paste0(country,
  '/02-variables/variables_followers', batch,'.parquet'), skip_nul = T) %>% 
  select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

# Save data

save(twitter, file = paste0(country, 
                            '/03-assignment/input/twitter_followers',batch,
                            '.Rda'))

## BATCH 1 SA, FIXING FAULTY PROFILES:

twitter = read_parquet(
  'SA/02-variables/variables_followers_batch2.parquet'
) %>% select(c(cov0, cov1))

tw <- twitter |> filter(is.na(listed_count) == T)

list_faulty <- tw$follower_id

get_profs <- function(ids){
  df <- get_user_profile(ids, bearer)
}
library(academictwitteR)
bearer <- 'AAAAAAAAAAAAAAAAAAAAAAB8lgEAAAAAtHuFxjMbRwl7WNHEOpMvzf7%2BGrc%3DATF52dZ90jRf9u9qxVvuiC7WLYCte5c9U4HrWfsuz9RK59Girq'
final <- list_faulty %>% map_dfr(function(x){get_profs(x)})

final <- final |> unnest(public_metrics)

final <- final |> rename(follower_id = id) |> select(follower_id, 
                                                     followers_count:listed_count)

tw <- tw |> select(-c(followers_count:tweet_count)) |> left_join(final)

twitter <- twitter |> filter(is.na(listed_count) == F)

twitter <- rbind(twitter, tw)

write_parquet(twitter, 'SA/02-variables/variables_followers.parquet')
# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

# Save data

save(twitter, file = 'SA/03-assignment/input/twitter_followers.Rda')

