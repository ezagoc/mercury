rm(list = ls())
library("purrr")
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

library(lfe)
library(fixest)

# ORIGINAL ---------------------------------------------

country <- 'SA'
stage <- 'AC'
# Read df (Aggregated Data Set)
df <- read_parquet(paste0('../../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

followers_base1 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/625489039.parquet') 
followers_base2 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/1468955884092936200.parquet') 

followers_base <- rbind(followers_base1 |> select(id, username),
                        followers_base2 |> select(id, username)) |> 
  mutate(AC_base = 1) |> rename(follower_id = id)

followers_base <- followers_base[!duplicated(followers_base$follower_id), ]

followers1 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/625489039.parquet') 
followers2 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-06-13/collect/1468955884092936200.parquet') 

followers <- rbind(followers1 |> select(id, username),
                   followers2 |> select(id, username)) |> 
  mutate(AC = 1) |> rename(follower_id = id)

followers <- followers[!duplicated(followers$follower_id), ]

df <- df |> left_join(followers, by = c('username', 'follower_id')) |> 
  left_join(followers_base, by = c('username', 'follower_id'))

df <- df |> mutate(AC = ifelse(is.na(AC) == T, 0, AC),
                   AC_base = ifelse(is.na(AC_base) == T, 0, AC_base))

write_parquet(df, paste0('../../../data/04-analysis/',country,'/', stage,
                         '/AC_final.parquet'))

### SMI Creation

rm(list = ls())
library("purrr")
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

library(lfe)
library(fixest)

stage <- 'SMIs'

## Paths

path_ids <-  paste0('../../../data/02-randomize/', country, 
                    '/03-assignment/output/RandomizedTwitterSample',country, '.xlsx')
path_out <- paste0('../../../data/07-followers/',country,
                   '/batch1/2023-06-13/collect/')

# Read df (Aggregated Data Set):
df <- read_parquet(paste0('../../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

# Read Followers at baseline:
followers_base <- read_parquet(paste0('../../../data/07-followers/', country,
                                      '/batch1/2023-02-20/integrate/followers.parquet.gzip'))


# Read the file of ids:

data <- readxl::read_xlsx(path_ids)

ids <- data$author_id

onlyfiles <- list.files(path_out)
onlyfiles <- gsub('.parquet', '', onlyfiles)
ids_final <- setdiff(unique(ids), onlyfiles)
ids_final <- sort(ids_final)

# Filter the followers at baseline that follow the influencers that got accounts suspended:

followers_cancelled <- followers_base |> 
  select(author_id_following, follower_id = id) |> 
  filter(author_id_following %in% ids_final) |> select(follower_id) |>
  mutate(SMI_na = 1)

followers_cancelled <- followers_cancelled[!duplicated(followers_cancelled$follower_id), ] 

read_followers <- function(id){
  df <- read_parquet(paste0(path_out, id, '.parquet'))
  df
}

followers <- onlyfiles |> map_dfr(~read_followers(.x))

followers_agg <- followers |> group_by(id) |> summarise(SMIs = n()) |>
  ungroup()

df <- df |> left_join(followers_agg |> rename(follower_id = id)) |> 
  left_join(followers_cancelled)

df <- df |> mutate(SMIs = ifelse(is.na(SMIs) == T, 0, SMIs),
                   SMI_na = ifelse(is.na(SMI_na) == T, 0, SMI_na))

df <- df |> mutate(SMIs = ifelse(SMI_na == 1, NA, SMIs))

write_parquet(df, paste0('../../../data/04-analysis/',country,'/', stage,
                         '/SMIs_final.parquet'))