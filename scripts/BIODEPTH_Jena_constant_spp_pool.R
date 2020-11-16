
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: BIODEPTH data reanalysis when species pool is (almost) constant

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


# BIODEPTH data: constant species pool

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


# subset out the constant species pool locations

# minimal species pool variation among highest diversity treatments

# greece (location = 4)
# sweden (location = 6)
# portugal (location = 2)

# unclear how much species pool variation there is among highest diversity treatments

# ireland (location = 5)
# sheffield (location = 7)

bio_con <- 
  bio_d %>%
  filter(location %in% c(4, 6, 2)) %>%
  group_by(location) %>%
  filter(species.richness == max(species.richness)) %>%
  filter(year == last(year)) %>%
  ungroup()


# Jena data: constant species pool

# load the Jena biomass data
jena_bio <- read_delim(here("data/Jena_Biomass_02-08.csv"), delim = ",")
head(jena_bio)
names(jena_bio)

# create a vector of species names
sp_names <- names(jena_bio[, 85:144])

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !(sowndiv %in% c(0)) )

# remove species presence columns
jena_bio <- select(jena_bio, -all_of(paste0("p", sp_names)))

# create a season variable for jena_bio
unique(jena_bio$month)

jena_bio <- 
  jena_bio %>%
  mutate(season = if_else(month %in% c("May", "Jun"), "spring", "summer"))

# subset out the spring data only
jena_bio <- 
  jena_bio %>%
  filter(season == "spring")

# replace the NAs with zeros
jena_bio <- 
  jena_bio %>%
  mutate(across(.cols = all_of(sp_names), ~replace(., is.na(.), 0)))

# remove rows of the data where there are missing values i.e. -9999 values in the sp_names
jena_bio <- 
  jena_bio %>%
  filter_at(all_of(sp_names), all_vars(. >= 0 ) )

# take the first three sub-samples as not all plots have four sub-samples
unique(jena_bio$subsample)

jena_bio <- 
  jena_bio %>%
  filter(subsample %in% c(1, 2, 3))

# select out the relevant columns
jena_bio <- 
  jena_bio %>%
  select(plotcode, season, time, subsample, sowndiv, target.biomass, all_of(sp_names))

# for each species and for target biomass, take the average value
jena_bio <- 
  jena_bio %>%
  group_by(plotcode, time) %>%
  summarise(across(.cols = all_of(c("sowndiv", "target.biomass", sp_names)), ~mean(., na.rm = TRUE) ), .groups = "drop")

# extract site variables
site_bio <- 
  jena_bio %>%
  select(-all_of(sp_names))

# check for NA's in the dataset
lapply(site_bio, function(x) { sum(if_else(is.na(x), 1, 0)) })

# check for any -9999's in the dataset
lapply(site_bio, function(x) { sum(if_else(x < 0, 1, 0)) })

# extract out the individual species' biomass
sp_bio <- 
  jena_bio %>%
  select(all_of(sp_names))

# check for NA's in the dataset
lapply(sp_bio, function(x) { sum(if_else(is.na(x), 1, 0)) })

# check for any -9999's in the dataset
lapply(sp_bio, function(x) { sum(if_else(x < 0, 1, 0), na.rm = TRUE) })

# check if biomass calculated from individual species correlates with target.biomass reported
cor(rowSums(sp_bio), site_bio$target.biomass)

# add species-specific biomass and observed species richness to the site_dat data
site_bio <- 
  site_bio %>%
  mutate(comm_biomass = rowSums(sp_bio),
         observed_sr = rowSums(decostand(sp_bio, method = "pa")) )


# subset out the 60 species treatment (i.e. constant species pool)
site_con <- 
  site_bio %>%
  filter(sowndiv == 60)

# take the final time-point
site_con <- 
  filter(site_con, time == max(time))


# BIODEPTH: plot out the results
ggplot(data = bio_con,
       mapping = aes(x = species.observed, y = biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~location, scales = "free") +
  theme_meta()

# Jena: plot out the results
ggplot(data = site_con,
       mapping = aes(x = observed_sr, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_meta()

cor(site_con$observed_sr, site_con$comm_biomass)



