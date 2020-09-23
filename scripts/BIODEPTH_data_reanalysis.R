
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: BIODEPTH data reanalysis and data prep

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)
library(vegan)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))
source(here("scripts/realised_div_slope_function.R"))

# load the BIODEPTH observed species richness data
bio_d_real <- read_tsv(here("data/Observed.Species.Richness.txt"))

# load the BIODEPTH biomass data (shoot biomass)
bio_d_bio <- read_tsv(here("data/Shoots.txt"))

# join these data sets together to output the relevant variables
names(bio_d_real)
names(bio_d_bio)

bio_d <- 
  full_join(select(bio_d_real,
                 year, plot, location, block, species.richness, species.observed),
            select(bio_d_bio,
                 year, plot, location, block, species.richness, biomass),
            by = c("year", "plot", "location", "block", "species.richness"))

# check the number of years available
unique(bio_d$year)
unique(bio_d$location)

bio_d %>%
  group_by(location) %>%
  summarise(years = max(year)-min(year))

# output the data from the final year in each experiment (year = 3)
bio_d <- 
  bio_d %>%
  filter(year == last(year))

# out the data without the monocultures
bio_d <- 
  bio_d %>%
  filter(species.richness > 1)

# check species richness treatments
bio_d %>%
  group_by(location) %>%
  summarise(sowndiv_min = min(species.richness),
            sowndiv_max = max(species.richness))

# check number of replicates for each treatment and location
bio_d %>%
  group_by(location, species.richness) %>%
  summarise(n = n()) %>%
  View()

# select out relevant experiments:
# 1. at least three species richness treatments with at least 8 replicates for that treatment
# this allowed random draws of six plots whilst having at least 20 unique reps:
# gtools::combinations(n = 8, r = 6)

bio_d <- 
  bio_d %>%
  filter(location %in% c(1, 4, 3, 5, 7, 8)) %>%
  filter( !(location == 1 & species.richness == 16),
          !(location == 3 & species.richness == 32),
          !(location == 5 & species.richness == 3),
          !(location == 7 & species.richness == 12) )

ggplot(data = bio_d,
       mapping = aes(x = species.richness, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~location, scales = "free")

ggplot(data = bio_d,
       mapping = aes(x = species.observed, 
                     y = biomass, 
                     colour = as.character(species.richness)) ) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ location, scales = "free")


# only pick the experiments with a positive slope because:
# we want to show that this effect comes out even when species pool richness does affect function

# subset out locations 1, 3 and 7
bio_d <- 
  bio_d %>%
  filter(location %in% c(1, 3, 7))

# rename the variables to match with the function
bio_d <- 
  bio_d %>%
  rename(species_pool = species.richness,
         realised_richness = species.observed,
         community_biomass = biomass)

# write these files into a .csv files

bio_d_list <- split(bio_d, bio_d$location)

for (i in 1:length(bio_d_list) ) {
  
  write_csv(x = ran_bio,
            path = here(paste0("data/", paste0("biodepth_analysis_data_", i, ".csv"))) )
  
}










