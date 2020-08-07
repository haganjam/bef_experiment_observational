
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

# create customised plotting theme
theme_meta <- function(base_size = 12, base_family = "") {
  theme(panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill="NA", color="black", size=0.75, linetype="solid"),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.ticks.length = unit(-0.16, "cm"),
        axis.title.x = element_text(colour ="black", size = 12, face = "plain", margin=margin(5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 12, face = "plain", margin=margin(0,5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=12, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=12, face = "plain", margin=margin(0,10,0,0,"pt")),
        axis.ticks.x = element_line(colour = "black", size = 0.4),
        axis.ticks.y = element_line(colour = "black", size = 0.4))
}


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

box_1_fig_1a <- 
  ggplot(data = bio_graph) +
  geom_boxplot(mapping = aes(x = spp_pool_round, group = spp_pool_round,
                             ymin = A, lower = B, middle = C, upper = D, ymax = E),
               stat = "identity",
               width = 4, fill = "grey88") +
  scale_x_continuous(breaks = c(seq(from = 0, to = 100, by = 10)) ) +
  ylab("community biomass") +
  xlab(expression(paste("inoculated ", alpha, " diversity", sep = ""))) +
  theme_meta()

box_1_fig_1a


# realised diversity
realised_graph <- 
  spp_pool %>%
  select(-biomass, -unique_id) %>%
  spread(key = "five_num", value = "realised_diversity")

box_1_fig_1a_inset <- 
  ggplot(data = realised_graph) +
  geom_boxplot(aes(x = spp_pool_round, group = spp_pool_round,
                   ymin = A, lower = B, middle = C, upper = D, ymax = E),
               stat = "identity",
               width = 4, fill = "grey88") +
  scale_y_continuous(limits = c(1, 7.1)) +
  scale_x_continuous(breaks = c(seq(from = 0, to = 100, by = 20)) ) +
  ylab(expression(paste("realised ", alpha, " diversity", sep = ""))) +
  xlab(expression(paste("inoculated ", alpha, " diversity", sep = ""))) +
  theme_meta() +
  theme(axis.title.x = element_text(colour ="black", size = 8, face = "plain", margin=margin(2.5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 8, face = "plain", margin=margin(0,2.5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=8, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=8, face = "plain", margin=margin(0,10,0,0,"pt")))

# add the inset to the fig 1a
box_1_fig_1a <- 
  box_1_fig_1a +
  annotation_custom(ggplotGrob(box_1_fig_1a_inset), 
                    xmin = 55, xmax = 105, 
                    ymin = 130, ymax = 170)

ggsave(filename = here("figures/box_1_fig1a.png"), plot = box_1_fig_1a,
       dpi = 500, units = "cm", width = 11, height = 10)


# load the constant species pool data
real_dat <- read_csv(here("data/stachova_leps_fig_2_data.csv"))

box_1_fig_1ab <- 
  real_dat %>%
  split(.$spp_pool) %>%
  map(~ ggplot(data = .,
               aes(x = realised_div, y = biomass)) +
        geom_jitter(width = 0.1, size = 1) +
        geom_smooth(method = "lm", alpha = 0.2, size = 0.1, colour = "black") +
        scale_x_continuous(limits = c(0, 12), breaks = seq(from = 2, to = 12, by = 2) ) +
        ylab("community biomass") +
        xlab(expression(paste("realised ", alpha, " diversity", sep = ""))) +
        theme_meta() +
        theme(axis.title.y = element_text(size = 9),
              axis.title.x = element_text(size = 9),
              axis.text.y = element_text(size = 9),
              axis.text.x = element_text(size = 9)))

box_1_fig_1ab_list <- list(box_1_fig_1ab[[1]], box_1_fig_1ab[[2]])
names(box_1_fig_1ab_list) <- c(1:length(box_1_fig_1ab_list))

for (i in seq_along(1:length(box_1_fig_1ab_list))) {
  
  ggsave(filename = paste0(here("figures"), "/box1_fig_1bc", names(box_1_fig_1ab_list)[i], ".png"), 
         plot = box_1_fig_1ab_list[[i]], dpi = 500, units = "cm", width = 5, height = 5)
}





