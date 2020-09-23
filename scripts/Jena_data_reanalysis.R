
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Jena data reanalysis

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
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

# only take the first sub-sample
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

# remove the 60 species treatment and the monocultures as it is not really relevant
site_bio <- 
  site_bio %>%
  filter(sowndiv > 1, sowndiv < 60)


# Box 1 analysis
ggplot(data = filter(site_bio, time == max(time)),
       mapping = aes(x = sowndiv, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = filter(site_bio, time == max(time)),
       mapping = aes(x = observed_sr, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = filter(site_bio, time == max(time)),
       mapping = aes(x = observed_sr, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~as.character(sowndiv), scales = "free") +
  theme_bw()


# Random sampling of data at final time-point
ran_bio <- 
  filter(site_bio, time == max(time))

# rename the variables to match with the function: slope_est_func
ran_bio <- 
  rename(ran_bio,
         community_biomass = comm_biomass,
         realised_richness = observed_sr,
         species_pool = sowndiv)

# use the function to get the realised-diversity function slopes
mod_out <- slope_est_func(data = df, reps = 5, plots = 12)

ggplot(data = mod_out,
       mapping = aes(x = estimate)) +
  geom_histogram(alpha = 0.3, colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1.25) +
  ylab("count") +
  xlab("realised diversity-function estimate") +
  theme_meta()






















