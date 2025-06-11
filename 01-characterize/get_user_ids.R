### Get User ID

library(academictwitteR)
library(tidyverse)
library(arrow)
library(purrr)

country <- 'KE'

df <- readxl::read_xlsx(paste0('../../data/01-characterize/influencers/',
                               country,'/confirmed_batch2.xlsx'))

#df <- read_parquet(paste0('../../data/01-characterize/influencers/',
                          #country,'/confirmed.parquet'))

ids <- df$handle

get_id <- function(handle, token){
  id <- get_user_id(handle, bearer_token = token)
  df <- tibble(handle, id)
  df
}

bearer <- 'AAAAAAAAAAAAAAAAAAAAAAB8lgEAAAAAtHuFxjMbRwl7WNHEOpMvzf7%2BGrc%3DATF52dZ90jRf9u9qxVvuiC7WLYCte5c9U4HrWfsuz9RK59Girq'
final <- ids %>% map_dfr(function(x){get_id(x, bearer)})

final <- final |> filter(is.na(id) == F)

write_parquet(final, paste0('../../data/01-characterize/influencers/',
                            country,'/confirmed_influencers_batch2.parquet'))
