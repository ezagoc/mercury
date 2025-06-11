# They have the same blocks, assignment is what changes but inside the blocks

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(arrow)

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "import_data.R"
)
map(paste0(src_path, source_files), source)

# Import datasets with blocks and bind on top:

#

foll <- read_parquet( '../../../data/04-analysis/joint/BlocksIntensive/followers_filtered.parquet')

df <- get_analysis_english_winsor(stage = 'stage1_2', batches = 'b1b2',
                                  initial_path = '../../../') |> 
  select(follower_id, batch_id, pais)


for (i in 1:1000){
  index <- paste0('_p', i)
  country <- 'SA'
  inf1 <- readxl::read_excel(paste0('../../../data/04-analysis/', country, 
                                    '/assignments_permutations.xlsx')) |>
    select(author_id, ends_with(index)) |> mutate(pais = country, batch_id = 'b1')
  
  inf2 <- readxl::read_excel(paste0('../../../data/04-analysis/', country, 
                                    '/assignments_permutations_batch2.xlsx')) |>
    select(author_id, ends_with(index)) |> mutate(pais = country, batch_id = 'b2')
  
  country <- 'KE'
  inf3 <- readxl::read_excel(paste0('../../../data/04-analysis/', country, 
                                    '/assignments_permutations.xlsx')) |>
    select(author_id, ends_with(index)) |> mutate(pais = country, batch_id = 'b1')
  
  inf4 <- readxl::read_excel(paste0('../../../data/04-analysis/', country, 
                                    '/assignments_permutations_batch2.xlsx')) |>
    select(author_id, ends_with(index)) |> mutate(pais = country, batch_id = 'b2')
  
  inf <- rbind(inf1, inf2, inf3, inf4)
  
  colnames(inf)[2] <- 'block1_fe'
  
  inf <- inf |> mutate(block1_fe = paste0(block1_fe, pais, batch_id))
  
  rm(inf1, inf2, inf3, inf4)
  
  final <- foll |> left_join(inf |> select(author_id, block1_fe))
  
  final <- final |> 
    select(follower_id, block1_fe)
  
  # Convert to wide format
  wide_f <- final %>% mutate(value = 1) |>
    pivot_wider(
      names_from = block1_fe,
      values_from = value,
      values_fill = list(value = 0),
      values_fn = list(value = max)
    )
  
  wide_f[is.na(wide_f)] <- 0
  
  df1 <- df |> left_join(wide_f)
  
  write_parquet(df1, paste0('../../../data/04-analysis/joint/BlocksIntensive/permutations/intensive_fe', 
                            i, '.parquet'))
  
  print(i)
}

c <- read_parquet(paste0('../../../data/04-analysis/joint/BlocksIntensive/permutations/intensive_fe', 
                         2, '.parquet'))
c1 <- read_parquet(paste0('../../../data/04-analysis/joint/BlocksIntensive/permutations/intensive_fe', 
                               4, '.parquet'))


# Final list of analysis:











# Merge


