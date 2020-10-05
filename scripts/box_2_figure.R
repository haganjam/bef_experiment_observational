
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Conceptual figure for box 1

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


# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

if(! dir.exists(here("figures/box_2"))){
  dir.create(here("figures/box_2"))
}


# plot a niche-competitive ability figure

# simulate normally distributed niches
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  stats::qnorm(seq(1 / n, 1 - 1 / n, length.out = n), mean, sd)
}

# set the seed
set.seed(555)

# set up species abundances
spp_abun <- c(200000, 25000, 30000, 10000)

# set up species locations (i.e. means)
spp_means <- c(0, 20, 35, -15)

# set up species standard deviations
spp_sd <- c(25, 5, 5, 3)

# run a loop to 
spp <- vector("list", length = length(spp_abun))
names(spp) <- LETTERS[1:length(spp_abun)]

for (i in seq_along(1:length(spp_abun))) {
  
  spp[[i]] <- data.frame(spp = rnorm_perfect(n = spp_abun[i], mean = spp_means[i], sd = spp_sd[i]))
  
}

spp <- 
  bind_rows(spp, .id = "species") %>%
  as_tibble()

# create a niche-competitive ability plot
p1 <- 
  ggplot(data = spp, 
         mapping = aes(x = spp, y = ..count.., fill = species)) +
  geom_density(alpha = 0.75, colour = "white") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  scale_x_continuous(limits = c(-75, 75)) +
  ylab("competitive ability") +
  xlab("niche") +
  scale_fill_viridis_d(option = "C", end = 0.9, direction = -1) +
  theme_meta() +
  theme(legend.position = "right", 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin=unit(c(1.5,1.5,1.5,4),"cm"))


# set up the axis labels

# local species pool terminology
a_term <- expression(paste(alpha, " species pool diversity", sep = " "))

# realised diversity terminology
b_term <- expression(paste("realised ", alpha, " diversity ", (t[n]), sep = " "))

# ecosystem function terminology
eco_func <- expression(paste("ecosystem function  ", (t[n]), sep = " "))


# case 1

c1l <- 
  ggplot(data = ) +
  ylab(eco_func) +
  xlab(a_term) +
  theme_meta() +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(1, 2)) +
  ggtitle("(a) case 1") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))

c1r <- 
  ggplot(data = ) +
  ylab("") +
  xlab(b_term) +
  theme_meta() +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(1, 2)) +
  ggtitle("") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))

cs1 <- ggarrange(c1l, c1r)


# case 2

c2l <- 
  ggplot(data = ) +
  ylab(eco_func) +
  xlab(a_term) +
  theme_meta() +
  scale_x_continuous(limits = c(2.5, 4.5), breaks = c(3, 4)) +
  ggtitle("(b) case 2") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))

c2r <- 
  ggplot(data = ) +
  ylab("") +
  xlab(b_term) +
  theme_meta() +
  scale_x_continuous(limits = c(1.5, 3.5), breaks = c(2, 3)) +
  ggtitle("") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))

cs2 <- ggarrange(c2l, c2r)


# case 3

c3l <- 
  ggplot(data = ) +
  ylab(eco_func) +
  xlab(a_term) +
  theme_meta() +
  scale_x_continuous(limits = c(2.5, 4.5), breaks = c(3, 4)) +
  ggtitle("(c) case 3") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))

c3r <- 
  ggplot(data = ) +
  ylab("") +
  xlab(b_term) +
  theme_meta() +
  scale_x_continuous(limits = c(2.5, 4.5), breaks = c(3, 4)) +
  ggtitle("") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, margin=margin(15,0,0,0,"pt")),
        axis.title.y = element_text(size = 11, margin=margin(0,15,0,0,"pt")),
        plot.title = element_text(hjust = -0.2, vjust = 2, size = 11, face = "bold"))

cs3 <- ggarrange(c3l, c3r)


# bind these three cases together with the niche plot

box_2_f1 <- ggarrange(cs1, p1, cs2, cs3, ncol = 2, nrow = 2)

ggsave(filename = here("figures/box_2/fig_1_t.png"), 
       plot = box_2_f1, width = 21, height = 14, units = "cm",
       dpi = 450)









