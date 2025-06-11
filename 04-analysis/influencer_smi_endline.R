# Influencer Baseline:

rm(list = ls())
library("purrr")
src_path <- c("../../src/utils/")             
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

country <- 'KE'
stage <- 'SMIs'

## Paths

path_ids <-  paste0('../../data/02-randomize/', country, 
                    '/03-assignment/output/RandomizedTwitterSample',country, '.xlsx')
path_out <- paste0('../../data/07-followers/',country,
                   '/batch1/2023-06-13/collect/')

# Read df (Aggregated Data Set):
df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

# Read Followers at baseline:
followers_base <- read_parquet(paste0('../../data/07-followers/', country,
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

write_parquet(df, paste0('../../data/04-analysis/',country,'/', stage,
                         '/SMIs_final.parquet'))

# Define the dependent variables
aux <- c('SMIs')

# Obtaining the original coefficients
aux_data <- df[aux]
lm_list_ols <- list()
count <- 1
for (x in aux) {
  fmla1 <- as.formula(paste0(x, "~ t_strong + t_weak + t_neither | 
                             c_t_strong_total + c_t_weak_total + c_t_neither_total"))
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs)
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

# Build matrix
smi <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_smi <- smi[1,]
weak_smi <- smi[2,]
neither_smi <- smi[3,]

coefs_perm <- data.frame(strong_smi, weak_smi, neither_smi)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates.xlsx"))
