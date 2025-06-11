# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(DescTools)
library(tidyverse)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "import_data.R"
)
map(paste0(src_path, source_files), source)
`%!in%` = Negate(`%in%`)


# 2.0 Define constants
list_stages <- list('stage1_2', 'stage3_4', 'stage5_6')


# 3.0 Import data and manipulate
df <- get_analysis_ver_final(stage = 'stage1_2', batches = 'b1b2',
                             initial_path = '../../../../')

df <- df |> filter(n_posts_base > 0)

df <- df |> select(username, follower_id, pais, batch_id, ends_with('rt'))

df_normal <- df |> select(-c(starts_with('log'), starts_with('arc'))) |> 
  mutate(Type = 'Levels')

df_log <- df_normal |> mutate(across(c(ends_with('rt')), ~log(.x+1))) |> 
  mutate(Type = 'Logs')

df_winsor <- df_normal |> mutate(across(c(ends_with('rt')), ~Winsorize(.x, 
                                                                       na.rm = T))) |> 
  mutate(Type = 'Wisorized 95%')


df_log_winsor <- df_winsor |> mutate(across(c(ends_with('rt')), ~log(.x+1))) |> 
  mutate(Type = 'Log Winsorized 95%')

df_graph <- rbind(df_log, df_log_winsor)


ggplot(df_graph, aes(x = n_posts_rt, fill = Type)) +
  geom_histogram(aes(y=..density..), alpha = .5, binwidth = .5) +
  scale_fill_manual(values = c("Logs" = "blue", "Log Winsorized 95%" = "red")) +
  labs(title = "Distribution of Number of RTs for Weeks 1 to 4",
       x = "Number of RTs",
       y = "Density") +
  theme_minimal()

ggplot(df_winsor, aes(x = n_posts_rt)) +
  geom_histogram(bins = 35, fill = 'darkred') +
  labs(title = "Distribution of Number of RTs (Winsorized)",
       x = "Number of RTs",
       y = "Count") + # Annotate max and median values for DataFrame2
  annotate("text", x = 120, y = 75000, label = paste("Max:", round(max(df_winsor$n_posts_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 120, y = 70000, label = paste("Median:", round(median(df_winsor$n_posts_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal() 

ggplot(df_normal, aes(x = n_posts_rt)) +
  geom_histogram( bins = 30, fill = 'darkred') +
  labs(title = "Distribution of Number of RTs (Levels)",
       x = "Number of RTs",
       y = "Count") +  # Annotate max and median values for DataFrame2
  annotate("text", x = 6000, y = 75000, label = paste("Max:", round(max(df_normal$n_posts_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 6000, y = 70000, label = paste("Median:", round(median(df_normal$n_posts_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal() 

### Number of posts:

ggplot(df_graph, aes(x = n_posts_no_rt, fill = Type)) +
  geom_histogram(aes(y=..density..), alpha = .5, binwidth = .5) +
  scale_fill_manual(values = c("Logs" = "blue", "Log Winsorized 95%" = "red")) +
  labs(title = "Distribution of Number of Posts for Weeks 1 to 4",
       x = "Number of Posts",
       y = "Density") +
  theme_minimal()

ggplot(df_winsor, aes(x = n_posts_rt)) +
  geom_histogram(bins = 35, fill = 'darkred') +
  labs(title = "Distribution of Number of Posts (Winsorized)",
       x = "Number of Posts",
       y = "Count") + # Annotate max and median values for DataFrame2
  annotate("text", x = 120, y = 75000, label = paste("Max:", round(max(df_winsor$n_posts_no_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 120, y = 70000, label = paste("Median:", round(median(df_winsor$n_posts_no_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal()

ggplot(df_normal, aes(x = n_posts_no_rt)) +
  geom_histogram( bins = 30, fill = 'darkred') +
  labs(title = "Distribution of Number of Posts (Levels)",
       x = "Number of Posts",
       y = "Count") +  # Annotate max and median values for DataFrame2
  annotate("text", x = 6000, y = 75000, label = paste("Max:", round(max(df_normal$n_posts_no_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 6000, y = 70000, label = paste("Median:", round(median(df_normal$n_posts_no_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal()


### Verfiable RT: 

ggplot(df_graph, aes(x = verifiability_rt, fill = Type)) +
  geom_histogram(aes(y=..density..), alpha = .5, binwidth = .5) +
  scale_fill_manual(values = c("Logs" = "blue", "Log Winsorized 95%" = "red")) +
  labs(title = "Distribution of Number of Verifiable RTs for Weeks 1 to 4",
       x = "Number of Ver. RTs",
       y = "Density") +
  theme_minimal()

ggplot(df_winsor, aes(x = verifiability_rt)) +
  geom_histogram(bins = 30, fill = 'darkred') +
  labs(title = "Distribution of Number of Verifiable RTs (Winsorized)",
       x = "Number of Ver. RTs",
       y = "Count") + # Annotate max and median values for DataFrame2
  annotate("text", x = 20, y = 75000, label = paste("Max:", round(max(df_winsor$verifiability_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 20, y = 70000, label = paste("Median:", round(median(df_winsor$verifiability_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal()

ggplot(df_normal, aes(x = verifiability_rt)) +
  geom_histogram( bins = 30, fill = 'darkred') +
  labs(title = "Distribution of Number of Verifiable RTs (Levels)",
       x = "Number of Ver. RTs",
       y = "Count") +  # Annotate max and median values for DataFrame2
  annotate("text", x = 2000, y = 75000, label = paste("Max:", round(max(df_normal$verifiability_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 2000, y = 70000, label = paste("Median:", round(median(df_normal$verifiability_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal()

# Verifiable posts

ggplot(df_graph, aes(x = verifiability_no_rt, fill = Type)) +
  geom_histogram(aes(y=..density..), alpha = .5, binwidth = .5) +
  scale_fill_manual(values = c("Logs" = "blue", "Log Winsorized 95%" = "red")) +
  labs(title = "Distribution of Number of Verifiable Posts for Weeks 1 to 4",
       x = "Number of Ver. Posts",
       y = "Density") +
  theme_minimal()

ggplot(df_winsor, aes(x = verifiability_no_rt)) +
  geom_histogram(bins = 10, fill = 'darkred') +
  labs(title = "Distribution of Number of Verifiable Posts (Winsorized)",
       x = "Number of Ver. Posts",
       y = "Count") + # Annotate max and median values for DataFrame2
  annotate("text", x = 5, y = 75000, label = paste("Max:", round(max(df_winsor$verifiability_no_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 5, y = 70000, label = paste("Median:", round(median(df_winsor$verifiability_no_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal()

ggplot(df_normal, aes(x = verifiability_no_rt)) +
  geom_histogram( bins = 30, fill = 'darkred') +
  labs(title = "Distribution of Number of Verifiable Posts (Levels)",
       x = "Number of Posts",
       y = "Count") +  # Annotate max and median values for DataFrame2
  annotate("text", x = 600, y = 75000, label = paste("Max:", round(max(df_normal$verifiability_no_rt), 2)), 
           color = "black", hjust = -0.1, vjust = 2) +
  annotate("text", x = 600, y = 70000, label = paste("Median:", round(median(df_normal$verifiability_no_rt), 2)),
           color = "black", hjust = -0.1, vjust = 2) + 
  theme_minimal()
