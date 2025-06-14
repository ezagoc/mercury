# Get Profile Participants

library(academictwitteR)
library(tidyverse)

country = 'KE'

df <- arrow::read_parquet(paste0('../../data/01-characterize/influencers/',
                                 country,'/confirmed_influencers.parquet'))

get_profs <- function(ids){
  df <- get_user_profile(ids, bearer)
}

ids <- df$id

bearer <- 'AAAAAAAAAAAAAAAAAAAAAAB8lgEAAAAAtHuFxjMbRwl7WNHEOpMvzf7%2BGrc%3DATF52dZ90jRf9u9qxVvuiC7WLYCte5c9U4HrWfsuz9RK59Girq'
final <- ids %>% map_dfr(function(x){get_profs(x)})

final <- final |> unnest(public_metrics) |> 
  select(author_id = id, handle = username, name, created_at, location,
         followers_count, 
         following_count, tweet_count, listed_count)

writexl::write_xlsx(final, paste0('../../data/02-randomize/',
                                   country,'/01-profiles/profiles.xlsx'))
