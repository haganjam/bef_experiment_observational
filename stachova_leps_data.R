
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Re-analysis of data extracted from Stachova and Leps (2010)

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(viridis)
library(here)
library(vegan)
library(ggpubr)


# load the species pool data
spp_pool <- read_csv(here("data/stachova_leps_fig_1_data.csv"))

# round off the species pool and realised diversity data to the nearest integer

spp_pool <- 
  spp_pool %>%
  mutate(unique_id = 1:n(),
         five_num = rep(LETTERS[1:5], times = 10),
         spp_pool_round = rep(seq(from = 10, to = 100, by = 10), each = 5)) %>%
  select(-spp_pool)

# biomass
bio_graph <- 
  spp_pool %>%
  select(-realised_diversity, -unique_id) %>%
  spread(key = "five_num", value = "biomass")

f1 <- 
  ggplot(data = bio_graph) +
  geom_boxplot(aes(x = spp_pool_round, ymin = A, lower = B, middle = C, upper = D, ymax = E),
               stat = "identity",
               width = 4) +
  scale_x_continuous(breaks = c(seq(from = 0, to = 100, by = 10)) ) +
  ylab("total biomass") +
  xlab("initial diversity") +
  theme_classic()


# realised diversity
realised_graph <- 
  spp_pool %>%
  select(-biomass, -unique_id) %>%
  spread(key = "five_num", value = "realised_diversity")

f2 <- 
  ggplot(data = realised_graph) +
  geom_boxplot(aes(x = spp_pool_round, ymin = A, lower = B, middle = C, upper = D, ymax = E),
               stat = "identity",
               width = 4) +
  scale_y_continuous(limits = c(1, 7.1)) +
  scale_x_continuous(breaks = c(seq(from = 0, to = 100, by = 10)) ) +
  ylab("realised diversity") +
  xlab("initial diversity") +
  theme_classic() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))


# load the constant species pool data
real_dat <- read_csv(here("data/stachova_leps_fig_2_data.csv"))

insets <- 
  real_dat %>%
  split(.$spp_pool) %>%
  map(~ ggplot(data = .,
               aes(x = realised_div, y = biomass)) +
        geom_jitter(width = 0.1 ) +
        geom_smooth(method = "lm", alpha = 0.2, size = 0.1, colour = "black") +
        scale_x_continuous(limits = c(0, 12), breaks = c(0:12)) +
        ylab("total biomass") +
        xlab("realised diversity") +
        theme_classic() +
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)))


box_1_figs <- list(f1, f2, insets[[1]], insets[[2]])
names(box_1_figs) <- c(1:4)

for (i in seq_along(1:length(box_1_figs))) {
  
  ggsave(filename = paste0(here("figures"), "/box1_fig_", names(box_1_figs)[i], ".png"), 
         plot = box_1_figs[[i]], dpi = 300)
}





