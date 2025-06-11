rm(list = ls())
library('fastDummies')
library(ggplot2)
library(ggrepel)
library(readxl)
library(tidyverse)
library(magrittr)
library(lubridate)
library(gridExtra)

library("purrr")

src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance2.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)


library(scales)
### Clean all letters from age

ads <- read_excel("../../data/03-experiment/ads/0-analytics/2023-03-01-to-2023-05-19-agg.xlsx")

ads_2 <- read_excel("../../data/03-experiment/ads/0-analytics/2023-04-01-to-2023-07-02-agg_b2.xlsx")

ads_p <- read_excel("../../../social-media-influencers-africa/data/03-experiment/ads/0-analytics/2022-12-01-to-2023-01-31-agg.xlsx")


ads_p$Retweets <- as.numeric(as.character(ads_p$Retweets))
ads$Retweets <- as.numeric(as.character(ads$Retweets))
ads_2$Retweets <- as.numeric(as.character(ads_2$Retweets))

ads_p <- ads_p %>%
  mutate(Retweets = replace_na(na_if(Retweets, NA), 0))

ads_2 <- ads_2 %>%
  mutate(Retweets = replace_na(na_if(Retweets, NA), 0))

# Get the common column names
common_cols <- Reduce(intersect, list(names(ads_p), names(ads), names(ads_2)))

# Subset the data frames to keep only the common columns
ads_subset_p <- ads_p[, common_cols]
ads_subset <- ads[, common_cols]
ads_subset_2<- ads_2[, common_cols]

# Combine the data frames using rbind
ads_pooled <- rbind(ads_subset_p, ads_subset, ads_subset_2)


#get means
ads_means <- ads_pooled %>% group_by(Country, Week, Ad) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')


p_p<-ggplot(ads_p, aes(x=Week, y=Impressions, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")

p<-ggplot(ads, aes(x=Week, y=Impressions, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(ads_2, aes(x=Week, y=Impressions, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/impressions.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")

#pooled
p<-ggplot(ads_means, aes(x=Week, y=Impressions, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Mean Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")

p

ggsave(filename = "../../results/06-plots/impressions_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")


p_p<-ggplot(ads_p, aes(x=Week, y=Spend, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='GBP Spent', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")

p<-ggplot(ads, aes(x=Week, y=Spend, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='GBP Spent', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(ads_2, aes(x=Week, y=Spend, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='GBP Spent', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 6)) +
  theme(legend.position = c(.93, .7),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/spent.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")

#pooled
p<-ggplot(ads_means, aes(x=Week, y=Spend, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Mean GBP Spent', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 6)) +
  theme(legend.position = c(.93, .7),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")

p

ggsave(filename = "../../results/06-plots/spent_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")


p_p<-ggplot(ads_p, aes(x=Week, y=Likes, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Likes', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 6)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")

p<-ggplot(ads, aes(x=Week, y=Likes, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Likes', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 7)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(ads_2, aes(x=Week, y=Likes, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Likes', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/likes.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")

#pooled

p<-ggplot(ads_means, aes(x=Week, y=Likes, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Mean Likes', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")

p

ggsave(filename = "../../results/06-plots/likes_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")


ads_p$Retweets <- as.numeric(as.character(ads_p$Retweets))
ads$Retweets <- as.numeric(as.character(ads$Retweets))
ads_2$Retweets <- as.numeric(as.character(ads_2$Retweets))

ads_p <- ads_p %>%
  mutate(Retweets = replace_na(na_if(Retweets, NA), 0))

ads_2 <- ads_2 %>%
  mutate(Retweets = replace_na(na_if(Retweets, NA), 0))

p_p<-ggplot(ads_p, aes(x=Week, y=Retweets, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Retweets', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")

p<-ggplot(ads, aes(x=Week, y=Retweets, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Retweets', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(ads_2, aes(x=Week, y=Retweets, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Retweets', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .99),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")
p2

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/retweets.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")



#pooled
p<-ggplot(ads_means, aes(x=Week, y=Retweets, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Mean Retweets', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .99),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")
p


ggsave(filename = "../../results/06-plots/retweets_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")


#ggsave("../../results/06-plots/retweets.png")

videos_p <- ads_p %>% filter(Ad=='Ad 1')
videos <- ads %>% filter(Ad=='Ad 1')
videos_2 <- ads_2 %>% filter(Ad=='Ad 1')

videos_pooled <- ads_means %>% filter(Ad=='Ad 1')

p_p<-ggplot(videos_p, aes(x=Week, y=`Video completions`, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Video completions', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11)) 
p_p <- p_p+scale_color_brewer(palette="Dark2") + geom_label_repel(aes(label = `Video completions`),
                                                              box.padding   = 0.35, 
                                                              point.padding = 0.5,
                                                              size = 3,
                                                              segment.color = 'grey50') +
  ggtitle("Pilot")

p<-ggplot(videos, aes(x=Week, y=`Video completions`, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Video completions', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11)) 
p <- p+scale_color_brewer(palette="Dark2") + geom_label_repel(aes(label = `Video completions`),
                                                           box.padding   = 0.35, 
                                                           point.padding = 0.5,
                                                         size = 3,
                                                           segment.color = 'grey50') +
  ggtitle("Batch 1")

p2<-ggplot(videos_2, aes(x=Week, y=`Video completions`, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Video completions', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11)) 
p2 <- p2+scale_color_brewer(palette="Dark2") + geom_label_repel(aes(label = `Video completions`),
                                                              box.padding   = 0.35, 
                                                              point.padding = 0.5,
                                                              size = 3,
                                                              segment.color = 'grey50') +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/videos.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")



#pooled
p<-ggplot(videos_pooled, aes(x=Week, y=`Video completions`, group= Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country)) + theme_minimal() + labs(y='Mean Video completions', x='Week') +
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) + facet_grid(Ad ~ .) +
  theme(strip.text.x = element_text(size = 11)) 
p <- p+scale_color_brewer(palette="Dark2") + geom_label_repel(aes(label = sprintf("%0.2f", round(`Video completions`, digits = 2))),
                                                                box.padding   = 0.35, 
                                                                point.padding = 0.5,
                                                                size = 3,
                                                                segment.color = 'grey50') +
  ggtitle("Pooled")

p

ggsave(filename = "../../results/06-plots/videos_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")

#ggsave("../../results/06-plots/videos.png")




#### Plots posts influencers

countries <- c("KE", "SA")
cols <- c("attachments.media_keys", "edit_history_tweet_ids", "entities.annotations",
          "attachments.poll_ids", "entities.cashtags", "withheld.copyright", 
          "withheld.country_codes", "withheld.scope")

for (country in countries) {
  
  # Specify the path where all the subfolders are located
  path <- sprintf("../../data/03-experiment/%s/treatment/influencers/01-preprocessed/batch1/content", 
                  country)
  
  # Get a list of the subdirectories in the specified path
  subdirs <- list.dirs(path, recursive = FALSE)
  
  # Function to read and merge dataframes with the same name in each subdirectory
  read_and_merge <- function(dirname) {
    # Get a list of the csv files in the specified subdirectory
    files <- list.files(dirname, pattern = "*.xlsx", full.names = TRUE)
    
    # Read and merge the dataframes with the same name in the specified subdirectory
    dfs <- lapply(unique(basename(files)), function(fname) {
      df_list <- lapply(files[grep(fname, files)], function(file) {
        # Read the excel file and select only the desired columns
        readxl::read_excel(file, col_types = NULL, col_names = TRUE, sheet = NULL, range = NULL) %>% 
          select(-one_of(cols))
      })
      do.call(rbind, df_list)
    })
    
    # Return a list with the merged dataframes
    dfs
  }
  
  if (country == 'KE') {
    # Apply the read_and_merge function to each subdirectory
    merged_dfs_ke <- Map(read_and_merge, subdirs) 
    
    # Merge the dataframes from each subdirectory into a single dataframe
    final_df_ke <- dplyr::bind_rows(merged_dfs_ke)%>% 
      mutate(country = country)
    
    } else {
      # Apply the read_and_merge function to each subdirectory
      merged_dfs_sa <- Map(read_and_merge, subdirs)
      # Merge the dataframes from each subdirectory into a single dataframe
      final_df_sa <- dplyr::bind_rows(merged_dfs_sa)%>% 
        mutate(country = country)
      }
}
  
#merge both dfs
final_df <- rbind(final_df_ke, final_df_sa)


final_df$created_at <- as.POSIXct(final_df$created_at, tz = "UTC" )



library(timetk)

final_df <- final_df %>% 
  mutate(
    week = case_when(
      created_at %>% between_time('2023-03-10 00:00:00','2023-03-20 00:00:00') ~ 'Week 1',
      created_at %>% between_time('2023-03-20 00:00:00','2023-03-27 00:00:00') ~ 'Week 2',
      created_at %>% between_time('2023-03-27 00:00:00','2023-04-03 00:00:00') ~ 'Week 3',
      created_at %>% between_time('2023-04-03 00:00:00','2023-04-10 00:00:00') ~ 'Week 4',
      created_at %>% between_time('2023-04-10 00:00:00','2023-04-17 00:00:00') ~ 'Week 5',
      created_at %>% between_time('2023-04-17 00:00:00','2023-04-24 00:00:00') ~ 'Week 6',
      created_at %>% between_time('2023-04-24 00:00:00','2023-05-01 00:00:00') ~ 'Week 7',
      created_at %>% between_time('2023-05-01 00:00:00','2023-05-08 00:00:00') ~ 'Week 8',
    )
  )

names(final_df)
final_df <- final_df %>% mutate(batch = '1')

### Batch 2

for (country in countries) {
  
  # Specify the path where all the subfolders are located
  path <- sprintf("../../data/03-experiment/%s/treatment/influencers/01-preprocessed/batch2/content", 
                  country)
  
  # Get a list of the subdirectories in the specified path
  subdirs <- list.dirs(path, recursive = FALSE)
  
  # Function to read and merge dataframes with the same name in each subdirectory
  read_and_merge <- function(dirname) {
    # Get a list of the csv files in the specified subdirectory
    files <- list.files(dirname, pattern = "*.xlsx", full.names = TRUE)
    
    # Read and merge the dataframes with the same name in the specified subdirectory
    dfs <- lapply(unique(basename(files)), function(fname) {
      df_list <- lapply(files[grep(fname, files)], function(file) {
        # Read the excel file and select only the desired columns
        readxl::read_excel(file, col_types = NULL, col_names = TRUE, sheet = NULL, range = NULL) %>% 
          select(-one_of(cols))
      })
      do.call(rbind, df_list)
    })
    
    # Return a list with the merged dataframes
    dfs
  }
  
  if (country == 'KE') {
    # Apply the read_and_merge function to each subdirectory
    merged_dfs_ke_2 <- Map(read_and_merge, subdirs) 
    
    # Merge the dataframes from each subdirectory into a single dataframe
    final_df_ke_2 <- dplyr::bind_rows(merged_dfs_ke_2)%>% 
      mutate(country = country)
    
  } else {
    # Apply the read_and_merge function to each subdirectory
    merged_dfs_sa_2 <- Map(read_and_merge, subdirs)
    # Merge the dataframes from each subdirectory into a single dataframe
    final_df_sa_2 <- dplyr::bind_rows(merged_dfs_sa_2)%>% 
      mutate(country = country)
  }
}

#merge both dfs
final_df_2 <- rbind(final_df_ke_2, final_df_sa_2)


final_df_2$created_at <- as.POSIXct(final_df_2$created_at, tz = "UTC" )



library(timetk)

final_df_2 <- final_df_2 %>% 
  mutate(
    week = case_when(
      created_at %>% between_time('2023-04-28 00:00:00','2023-05-08 00:00:00') ~ 'Week 1',
      created_at %>% between_time('2023-05-08 00:00:00','2023-05-15 00:00:00') ~ 'Week 2',
      created_at %>% between_time('2023-05-15 00:00:00','2023-05-22 00:00:00') ~ 'Week 3',
      created_at %>% between_time('2023-05-22 00:00:00','2023-05-29 00:00:00') ~ 'Week 4',
      created_at %>% between_time('2023-05-29 00:00:00','2023-06-05 00:00:00') ~ 'Week 5',
      created_at %>% between_time('2023-06-05 00:00:00','2023-06-12 00:00:00') ~ 'Week 6',
      created_at %>% between_time('2023-06-12 00:00:00','2023-06-19 00:00:00') ~ 'Week 7',
      created_at %>% between_time('2023-06-19 00:00:00','2023-06-27 00:00:00') ~ 'Week 8',
    )
  )

final_df_2 <- final_df_2 %>% mutate(batch = '2')
names(final_df_2)



### Pilot

for (country in countries) {
  
  # Specify the path where all the subfolders are located
  path <- sprintf("../../../social-media-influencers-africa/data/03-experiment/%s/treatment/influencers/01-preprocessed/content", 
                  country)
  
  # Get a list of the subdirectories in the specified path
  subdirs <- list.dirs(path, recursive = FALSE)
  
  # Function to read and merge dataframes with the same name in each subdirectory
  read_and_merge <- function(dirname) {
    # Get a list of the csv files in the specified subdirectory
    files <- list.files(dirname, pattern = "*.xlsx", full.names = TRUE)
    
    # Read and merge the dataframes with the same name in the specified subdirectory
    dfs <- lapply(unique(basename(files)), function(fname) {
      df_list <- lapply(files[grep(fname, files)], function(file) {
        # Read the excel file and select only the desired columns
        readxl::read_excel(file, col_types = NULL, col_names = TRUE, sheet = NULL, range = NULL) %>% 
          select(-one_of(cols))
      })
      do.call(rbind, df_list)
    })
    
    # Return a list with the merged dataframes
    dfs
  }
  
  if (country == 'KE') {
    # Apply the read_and_merge function to each subdirectory
    merged_dfs_ke_p <- Map(read_and_merge, subdirs) 
    
    # Merge the dataframes from each subdirectory into a single dataframe
    final_df_ke_p <- dplyr::bind_rows(merged_dfs_ke_p)%>% 
      mutate(country = country)
    
  } else {
    # Apply the read_and_merge function to each subdirectory
    merged_dfs_sa_p <- Map(read_and_merge, subdirs)
    # Merge the dataframes from each subdirectory into a single dataframe
    final_df_sa_p <- dplyr::bind_rows(merged_dfs_sa_p)%>% 
      mutate(country = country)
  }
}

#merge both dfs
final_df_p <- rbind(final_df_ke_p, final_df_sa_p)


final_df_p$created_at <- as.POSIXct(final_df_p$created_at, tz = "UTC" )



library(timetk)

final_df_p <- final_df_p %>% 
  mutate(
    week = case_when(
      created_at %>% between_time('2022-12-01 00:00:00','2022-12-12 00:00:00') ~ 'Week 1',
      created_at %>% between_time('2022-12-12 00:00:00','2022-12-19 00:00:00') ~ 'Week 2',
      created_at %>% between_time('2022-12-19 00:00:00','2022-12-26 00:00:00') ~ 'Week 3',
      created_at %>% between_time('2022-12-26 00:00:00','2023-01-02 00:00:00') ~ 'Week 4',
      created_at %>% between_time('2023-01-02 00:00:00','2023-01-09 00:00:00') ~ 'Week 5',
      created_at %>% between_time('2023-01-09 00:00:00','2023-01-16 00:00:00') ~ 'Week 6',
      created_at %>% between_time('2023-01-16 00:00:00','2023-01-23 00:00:00') ~ 'Week 7',
      created_at %>% between_time('2023-01-23 00:00:00','2023-02-06 00:00:00') ~ 'Week 8',
    )
  )

final_df_p <- final_df_p %>% mutate(batch = 'pilot')
names(final_df_p)


#merge final dfs
# Get the common column names
common_cols <- Reduce(intersect, list(names(final_df_p), names(final_df), names(final_df_2)))

# Subset the data frames to keep only the common columns
final_df_p_subset <- final_df_p[, common_cols]
final_df_subset <- final_df[, common_cols]
final_df_2_subset <- final_df_2[, common_cols]

# Combine the data frames using rbind
final_df_pooled <- rbind(final_df_p_subset, final_df_subset, final_df_2_subset)

#writexl::write_xlsx(final_df_pooled, "../../data/06-other/merged_treatment_tweets.xlsx")


#group by and get means
final_df_means <- final_df %>% group_by(country, week) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')

final_df_means_2 <- final_df_2 %>% group_by(country, week) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')

final_df_means_p <- final_df_p %>% group_by(country, week) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')

final_df_means_pooled <- final_df_pooled %>% group_by(country, week) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')

# Filter out NA observations in column A
filtered_df_pooled <- final_df_pooled[complete.cases(final_df_pooled$public_metrics.impression_count), ]

final_df_means_pooled_imp <- filtered_df_pooled %>% group_by(country, week) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')


p_p<-ggplot(final_df_means_p, aes(x=week, y=public_metrics.impression_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")


p<-ggplot(final_df_means, aes(x=week, y=public_metrics.impression_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(final_df_means_2, aes(x=week, y=public_metrics.impression_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/impressions_smi.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")

#pooled

p<-ggplot(final_df_means_pooled_imp, aes(x=week, y=public_metrics.impression_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Impressions', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.02, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")


ggsave(filename = "../../results/06-plots/impressions_smi_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")


#ggsave("../../results/06-plots/impressions_smi.png")

p_p<-ggplot(final_df_means_p, aes(x=week, y=public_metrics.like_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Likes', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")


p<-ggplot(final_df_means, aes(x=week, y=public_metrics.like_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Likes', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(final_df_means_2, aes(x=week, y=public_metrics.like_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Likes', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside",
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/likes_smi.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")


p<-ggplot(final_df_means_pooled, aes(x=week, y=public_metrics.like_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Likes', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside",
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")

ggsave(filename = "../../results/06-plots/likes_smi_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")


p_p<-ggplot(final_df_means_p, aes(x=week, y=public_metrics.retweet_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Retweets', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p_p <- p_p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pilot")



p<-ggplot(final_df_means, aes(x=week, y=public_metrics.retweet_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Retweets', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 8)) +
  theme(legend.position = "none",legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 1")

p2<-ggplot(final_df_means_2, aes(x=week, y=public_metrics.retweet_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Retweets', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 9)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p2 <- p2+scale_color_brewer(palette="Dark2") +
  ggtitle("Batch 2")

grid.arrange(p_p, p, p2, ncol = 1)

ggsave(filename = "../../results/06-plots/retweets_smi.png", 
       plot = grid.arrange(p_p, p, p2, ncol = 1), width = 10, height = 6, units = "in")

#pooled
p<-ggplot(final_df_means_pooled, aes(x=week, y=public_metrics.retweet_count, group= country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country)) + theme_minimal() + labs(y='Mean Retweets', x='Week') + 
  scale_y_continuous(expand = expansion(mult = c(0.05, .05)), breaks=scales::pretty_breaks(n = 9)) +
  theme(legend.position = c(.93, .94),legend.title = element_text(size=11),
        legend.key.size = unit(.5, "cm"),
        panel.spacing.x = unit(0, "pt"), 
        strip.placement = "outside", 
        strip.background.x = element_blank(),
        axis.line.x = element_line(size = .1),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11))
p <- p+scale_color_brewer(palette="Dark2") +
  ggtitle("Pooled")

ggsave(filename = "../../results/06-plots/retweets_smi_pooled.png", 
       plot = p, width = 10, height = 6, units = "in")




