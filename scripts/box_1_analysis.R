
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Box 1 analysis (model and Jena data)

# load relevant libraries generally
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))
source(here("scripts/realised_div_slope_function.R"))

# load the model data
mod_dat <- read_delim(here("data/stachova_leps_model_data.csv"), delim = ",")

# subset out the first run
mod_dat <- 
  mod_dat %>%
  filter(run == first(run))

mod_dat <- 
  mod_dat %>%
  filter(species_pool %in% c(10, 20, 30, 40))

# load the Jena data
jena_dat <- read_delim(here("data/jena_analysis_data.csv"), delim = ",")

# combine these datasets into a list
box1_dat <- list(mod_dat, jena_dat)


# plot species pool diversity versus function

# set the y axis labels
ylabs1 <- 
  list(c("community biomass"), 
       expression(paste("community biomass (g ",  " m"^"-2", ")") ) )

box1_fig1ab <- vector("list")
for (i in 1:length(box1_dat)) {
  
  box1_fig1ab[[i]] <- 
    ggplot(data = box1_dat[[i]],
         mapping = aes(x = species_pool, y = community_biomass)) +
    geom_jitter(width = 0.5, size = 1.5) +
    geom_smooth(method = "lm", size = 0.5, colour = "black", alpha = 0.3) +
    ylab(ylabs1[[i]]) +
    xlab("initial diversity") +
    theme_meta()
  
}

fig1ab <- 
  ggarrange(box1_fig1ab[[1]], box1_fig1ab[[2]], labels = c("a", "b"),
            font.label = list(size = 12, color = "black", face = "plain", family = NULL))


# plot realised diversity versus function for each species pool

box1_fig1cd <- vector("list")
for (i in 1:length(box1_dat)) {
  
  x <- box1_dat[[i]]
  
  levs <- sort(unique(x$species_pool), decreasing = FALSE)
  
  z <- 
    mutate(x, 
           species_pool = factor(as.factor(species_pool), levels = levs )  )
  
  box1_fig1cd[[i]] <- 
    ggplot(data = z,
           mapping = aes(x = realised_richness, 
                         y = community_biomass,
                         colour = species_pool)) +
    geom_jitter(width = 0.25, size = 1.5) +
    geom_smooth(method = "lm", size = 0.75, se = FALSE) +
    ylab(ylabs1[[i]]) +
    xlab("realised diversity") +
    labs(colour = "initial diversity") +
    scale_colour_viridis_d(option = "C", end = 0.9) +
    theme_meta() +
    theme(legend.position = "bottom",
          legend.key = element_blank())
  
}

fig1cd <- 
  ggarrange(box1_fig1cd[[1]], box1_fig1cd[[2]], labels = c("c", "d"),
            font.label = list(size = 12, color = "black", face = "plain", family = NULL))

# join these two figures together
fig1 <- ggarrange(fig1ab, fig1cd, ncol = 1, nrow = 2, labels = NULL,
                  heights = c(1, 1.25))

ggsave(filename = here("figures/box1_fig1.png"), 
       plot = fig1, width = 19, height = 19, units = "cm",
       dpi = 450)





