
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Preparation of the stachova and leps model output data

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
source(here("scripts/stachova_leps_model.R"))

# run the model to generate sample data
sl_mod_out <- 
  s_l_2010_mod(reg_pool = 60,
               t_steps = 1000, 
               n0 = 3,
               a_mean = 0.8, a_sd = 0.2, a_min = 0.2, a_max = 1.2, a_spp = 1,
               k_min = 3, k_max = 150,
               r_min = 0.01, r_max = 0.5, 
               lsp = c(2, 4, 8, 16),
               reps = 16)

# get the final time point in the model
sl_mod_an <- 
  sl_mod_out %>%
  filter(time == last(time))

# Box 1 analysis
ggplot(data = sl_mod_an,
       mapping = aes(x = species_pool, y = community_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = sl_mod_an,
       mapping = aes(x = realised_richness, y = community_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = sl_mod_an,
       mapping = aes(x = realised_richness, y = community_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~as.character(species_pool), scales = "free") +
  theme_bw()







