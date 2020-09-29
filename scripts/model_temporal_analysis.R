
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Randomly sample time points from the model (like the kelp data)

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# load in the model data
mod_time <- read_delim(file = here("data/stachova_leps_model_data_full.csv"), delim = ",")
head(mod_time)

# get rid of the monoculture data
mod_ana <- 
  mod_time %>%
  filter(species_pool > 2, species_pool < 16)

mod_time$species_pool %>% unique()

# provide a id column for each plot for each time point
mod_ana <- 
  mod_ana %>%
  group_by(run, time) %>%
  mutate(id = 1:n())

maxid <- max(mod_ana$id)

# set up number of time points and number of sites to match with the kelp data
tp <- length(unique(mod_ana$time))
ts <- 17

t_samp <- sample(x = 1:tp, size = ts)

# set up the sites to sample
sites <- 9

s_samp <- sample(x = 1:maxid, size = sites)

# sample a specific set of time points and sites
mod_time_s <- 
  mod_ana %>%
  filter(time %in% t_samp ) %>%
  filter(id %in% s_samp )

mod_time_s %>%
  mutate(time = as.character(time)) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = community_biomass, colour = time) ) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE, method = "lm", size = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab("community biomass") +
  xlab(expression(paste("realised ", alpha, " diversity"))) +
  facet_wrap(~run, scales = "free") +
  theme_meta() +
  theme(legend.position = "none")












