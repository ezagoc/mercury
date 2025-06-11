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

# 2.0 Define constants
country <- 'joint'
data_type <- 'Verifiability'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
ini <- '../../../../data/04-analysis/joint/'


final1 <- tibble()

for (stage in list_stages){
  # 3.0 Import data and manipulate
  df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../')
  df <- df |> select(id, n_posts_base, n_posts, pais, batch_id) |> 
    mutate(n_posts_f = n_posts + n_posts_base)
  
  agg <- df |> group_by(pais, batch_id) |> 
    summarise(across(c(n_posts_f), ~sum(.x)))
  
  final1 <- rbind(final1, agg)
}

writexl::write_xlsx(final1, 'aggregate_n_posts.xlsx')

final_final <- final1 |> summarise(s = sum(n_posts_f))
