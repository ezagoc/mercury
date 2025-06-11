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

country <- 'SA'
stage <- 'BalanceVer'

##

# Read df (Aggregated Data Set):
df <- read_parquet(paste0('../../data/04-analysis/',country,'/', stage,
                          '/endline_final.parquet'))

df <- df |> mutate(ver_dummy = ifelse(verifiability_base>0, 1, 0))

##

# Define the dependent variables
aux <- c('ver_dummy')

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
dum <- coefs_all %>% 
  select(ends_with(aux[1]))
strong_dum <- dum[1,]
weak_dum <- dum[2,]
neither_dum <- dum[3,]

coefs_perm <- data.frame(strong_dum, weak_dum, neither_dum)

write_xlsx(
  coefs_perm, paste0("../../data/04-analysis/",country,
                     "/",stage,"/pestimates.xlsx"))
