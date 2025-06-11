# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(fastDummies)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)


stage <- 'stage3_4'
f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |>
  filter(n_posts_base>0) |> select(follower_id, pais, batch_id, total_treated, 
                                   total_influencers)


f_tab <- f |> group_by(total_influencers) |> summarise(n_obs = n())

writexl::write_xlsx(f_tab, 'influencer_tabulation.xlsx')

f_tab_matrix <- f |> select(total_influencers, total_treated)

f_tab_matrix <- dummy_cols(f_tab_matrix, select_columns = "total_treated", 
                   remove_first_dummy = FALSE)

f_tab_matrix <- f_tab_matrix |> group_by(total_influencers) |> 
  summarise(across(starts_with('total_treated_'), ~sum(.x))) |> ungroup()

writexl::write_xlsx(f_tab_matrix, 'influencer_treated_matrix.xlsx')

