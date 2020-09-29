
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


# check species richness treatments
bio_d %>%
  group_by(location) %>%
  summarise(sowndiv_min = min(species.richness),
            sowndiv_max = max(species.richness))

# check number of replicates for each treatment and location
bio_d %>%
  group_by(location, species.richness) %>%
  summarise(n = n())

bio_d %>%
  filter(location == 3) %>%
  group_by(species.richness) %>%
  summarise(n = n())

# remove the highest diversity treatment for location 3 as it has low replication and is far from other treatments
bio_d <- 
  bio_d %>%
  filter(!(location == 3 & species.richness > 30) )

# check for missing values
lapply(bio_d, function(x) { sum(ifelse(is.na(x), 1, 0)) })

bio_d %>%
  filter(is.na(species.richness) | is.na(species.observed))

# remove these data points from location 2 with missing data
bio_d <- 
  bio_d %>%
  filter(!is.na(species.observed))

# check for rows without any species.observed
lapply(bio_d, function(x) { sum(ifelse(x == 0, 1, 0))   })

# remove data points without biomass or any observed species
bio_d <- 
  bio_d %>%
  filter(species.observed > 0 | biomass > 0)

# remove the monoculture data
bio_d <- 
  bio_d %>%
  filter(species.richness > 1)


ggplot(data = bio_d,
       mapping = aes(x = species.richness, y = biomass)) +
  geom_jitter(width = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~location, scales = "free") +
  theme_meta()

ggplot(data = bio_d,
       mapping = aes(x = species.observed, y = biomass)) +
  geom_jitter(width = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~location, scales = "free") +
  theme_meta()

# rename the variables to match with the function
bio_d <- 
  bio_d %>%
  rename(species_pool = species.richness,
         realised_richness = species.observed,
         community_biomass = biomass)

# write these files into a .csv files

bio_d_list <- split(bio_d, bio_d$location)

for (i in 1:length(bio_d_list) ) {
  
  write_csv(x = bio_d_list[[i]],
            path = here(paste0("data/", paste0("biodepth_analysis_data_", i, ".csv"))) )
  
}









