#############################################################################
# RCT: Social Media Influencers Based in Africa
# Activity: Randomnization Within Blocks (2,4)
# Date: November 2022
#############################################################################

######################## Influencers ########################

# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/02-randomize/")
source("../../code/02-randomize/funcs.R")

# Install and load packages

#url <- "https://cran.r-project.org/src/contrib/Archive/blockTools/blockTools_0.6-3.tar.gz"
#pkgFile <- "blockTools_0.6-3.tar.gz"
#download.file(url = url, destfile = pkgFile)
#install.packages(pkgs=pkgFile, type="source", repos=NULL)
#unlink(pkgFile)

packages <- c(
  "foreign",
  "dplyr",
  "tidyverse",
  "blockTools",
  "writexl",
  "lmtest",
  "sandwich"
  )

ipak(packages)
library(arrow)

###KE

# Read in the data to randomize ----------------

load("KE/03-assignment/input/twitter_batch2.Rda")


# List covariates for determining blocks -------

cov0 <- c(
  "followers_count",
  "listed_count",
  "n_tweets.na",
  "n_tweets",
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account",
  "index_influence"
)



##################### 1. TWITTER

id <- "author_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
  )

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
  )

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
  )

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

# Seed for 1st batch:
# cluster_assign0 <- assignment(block0, seed = 12345)

# Second
cluster_assign0 <- assignment(block0, seed = 1253)

treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
  )

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
  )

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "KE/03-assignment/output/RandomizedTwitterSampleKE_batch2_2.xlsx"
  )

#randomization <- readxl::read_excel("KE/03-assignment/output/RandomizedTwitterSampleKE.xlsx")
# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat <- balance_table |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table <- balance_table |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table, caption = "Balance Table Influencers - Kenya",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

# Logs:

check1 <- t.test(log(followers_count + 1)~ treatment, 
                 data=baseline_treatment)$p.value
check2 <- t.test(log(listed_count+1) ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(log(n_tweets+1) ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(log(n_tweets.na + 1)~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(log(n_strong+1) ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(log(n_weak + 1)~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(log(n_absent+1) ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(log(index_influence+1) ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(log(days_old_account+1) ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table2 <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> 
  mutate(across(c(followers_count:index_influence), ~log(.x + 1))) |>
  group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat2 <- balance_table2 |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table2 <- balance_table2 |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat2) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table2, caption = "Balance Table Influencers - Kenya (Logs)",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))



###SA

# Read in the data to randomize ----------------

load("SA/03-assignment/input/twitter_batch2.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "followers_count",
  "listed_count",
  "n_tweets.na",
  "n_tweets",
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account",
  "index_influence"
)

##################### 1. TWITTER

id <- "author_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
)

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
)

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
)

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

#Seed 1st batch
#cluster_assign0 <- assignment(block0, seed = 12345)

# Seed 2nd batch
#cluster_assign0 <- assignment(block0, seed = 123)
cluster_assign0 <- assignment(block0, seed = 1239)
treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
)

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
)

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "SA/03-assignment/output/RandomizedTwitterSampleSA_batch2_2.xlsx"
)

#randomization <- readxl::read_excel("SA/03-assignment/output/RandomizedTwitterSampleSA.xlsx")

# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat <- balance_table |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table <- balance_table |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table, caption = "Balance Table Influencers - South Africa",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

##### 

check1 <- t.test(log(followers_count + 1)~ treatment, 
                 data=baseline_treatment)$p.value
check2 <- t.test(log(listed_count+1) ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(log(n_tweets+1) ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(log(n_tweets.na + 1)~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(log(n_strong+1) ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(log(n_weak + 1)~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(log(n_absent+1) ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(log(index_influence+1) ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(log(days_old_account+1) ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table2 <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> 
  mutate(across(c(followers_count:index_influence), ~log(.x + 1))) |>
  group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat2 <- balance_table2 |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table2 <- balance_table2 |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat2) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table2, caption = "Balance Table Influencers - Kenya (Logs)",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))



######################## Followers ########################

# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/02-randomize/")
source("../../code/02-randomize/funcs.R")

# Install and load packages
install.packages('RTools')
url <- "https://cran.r-project.org/src/contrib/Archive/blockTools/blockTools_0.6-3.tar.gz"
pkgFile <- "blockTools_0.6-3.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
unlink(pkgFile)

install.packages('blockTools')

packages <- c(
  "foreign",
  "dplyr",
  "tidyverse",
  "blockTools",
  "writexl",
  "lmtest",
  "sandwich"
)


ipak(packages)

###KE

# Read in the data to randomize ----------------

load("KE/03-assignment/input/twitter_followers.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "n_strong",   
  "n_weak",                   
  "n_absent",                    
  "n_strong_treated",
  "n_weak_treated",
  "n_absent_treated",
  "days_old_account",
  "index_influence"
)


##################### 1. TWITTER

id <- "follower_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
)

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
)

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
)

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

cluster_assign0 <- assignment(block0, seed = 12345)

treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
)

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
)

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "KE/03-assignment/output/RandomizedTwitterFollowers.xlsx"
)

# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(combined, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value


###SA

# Read in the data to randomize ----------------

load("SA/03-assignment/input/twitter_followers.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "n_strong",   
  "n_weak",                   
  "n_absent",                    
  "n_strong_treated",
  "n_weak_treated",
  "n_absent_treated",
  "days_old_account",
  "index_influence"
)

##################### 1. TWITTER

id <- "follower_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
)

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
)

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
)

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

cluster_assign0 <- assignment(block0, seed = 12345)

treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
)

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
)

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "SA/03-assignment/output/RandomizedTwitterFollowers.xlsx"
)

# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(combined, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value


