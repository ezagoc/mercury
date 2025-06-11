# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

df <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                    initial_path = '../../../')

df <- df |> filter(n_posts_base>0)

df_strong <- df |> filter(c_t_strong_total > 0)

df_weak <- df |> filter(c_t_weak_total > 0 & c_t_strong_total == 0)

df_absent <- df |> filter(c_t_strong_total == 0 & c_t_weak_total == 0)


ggplot(data = df, aes(x = c_t_strong_total)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() + 
  labs(title = "Distribution Very Strong",
       x = "Number of Influencers Followed (Very Strong)",
       y = "Density")

ggplot(data = df, aes(x = c_t_weak_total)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() + 
  labs(title = "Distribution Strong",
       x = "Number of Influencers Followed (Strong)",
       y = "Density")

ggplot(data = df, aes(x = c_t_neither_total)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() + 
  labs(title = "Distribution Weak",
       x = "Number of Influencers Followed (Weak)",
       y = "Density")



