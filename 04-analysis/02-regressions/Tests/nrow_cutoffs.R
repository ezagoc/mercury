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
data_type <- 'Tests'
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')
list_types <- list('','log_')
file_code <- 'next_nbase0'
ini <- '../../../../data/04-analysis/joint/'
list_cutoffs <- list('0','2', '11')
for (li in list_cutoffs){
  l <- as.numeric(li)# 3.0 Import data and manipulate
    
  df <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                        initial_path = '../../../../')
    df <- df |> filter(n_posts_base>l) 
    print(nrow(df))
}
