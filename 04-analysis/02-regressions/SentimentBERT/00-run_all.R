# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)


codes <- list(
  "final_ads.R",
  "final_int_nbase0.R",
  "final_int_nbase0_rts_posts.R",
  "final_int_nbase0_rts_posts_ads.R", 
  "final_int_covid_fes.R", # This is going to be for tests
  "final_int_covid.R", # this has no FEs
  "final_int_nbase0_rts_posts_both_ads.R"
)
map(paste0(codes), source)

# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

blocks_ke <- read_parquet(paste0('../../../../data/04-analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../../../data/04-analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks <- rbind(blocks_ke, blocks_sa)

# Intensive:

codes <- list(
  "final_ext_nbase0.R",
  "final_ext_nbase0_rts_posts.R",
  'final_ext_rts_posts_both.R'
)
map(paste0(codes), source)