
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: California kelp data from (Gerung et al. 2020)

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

# load the biomass data
kelp_raw <- read_csv(here("data/Annual_All_Species_Biomass_at_transect_20200108.csv"))

# check for unique sites
kelp_raw$SITE %>%
  unique()

# remove the two sites on the channel islands
# SCDI
# SCTW
kelp_raw <- 
  kelp_raw %>%
  filter( !(SITE %in% c("SCDI", "SCTW")) )

# subset out the algae data
names(kelp_raw)

unique(kelp_raw$GROUP)

kelp_raw <- 
  kelp_raw %>%
  filter(GROUP == c("ALGAE"))

View(kelp_raw)

# explore the data
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(transect = length(unique(TRANSECT))) %>%
  filter(transect > 2)

kelp_raw %>%
  group_by(SITE) %>%
  summarise(year = length(unique(YEAR)))

kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(n_transects = length(unique(TRANSECT)) ) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = SITE, y = YEAR, colour = n_transects)) +
  geom_point()
  
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(n_transects = length(unique(TRANSECT)) ) %>%
  ungroup() %>%
  group_by(SITE) %>%
  summarise(min_transects = min(n_transects))

kelp_raw %>%
  group_by(SITE, YEAR, TRANSECT) %>%
  summarise(transect = min(TRANSECT) )  %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = YEAR, y = transect)) +
  geom_point() +
  facet_wrap(~SITE)


# based on this exploration:
# (1) remove years before 2001 (i.e. only 2000) because this is only available at some sites
# (2) select the transects with the lowest number ID at each site because these are consistent across years

kelp_raw <- 
  kelp_raw %>%
  filter(YEAR > 2000) %>%
  group_by(SITE) %>%
  mutate(min_1 = min(unique(TRANSECT), na.rm = TRUE),
         min_2 = sort(unique(TRANSECT) )[2]) %>%
  ungroup() %>%
  filter(TRANSECT %in% c(min_1, min_2) ) %>%
  select(-min_1, -min_2)


# analysis data

kelp_ana <- 
  kelp_raw %>%
  select(-PERCENT_COVER, -DENSITY, -WM_GM2, -DRY_GM2,
         -SFDM)

# sum up the transects at each site-year combination

# check if there are missing values
kelp_ana %>%
  filter_at(vars(c("AFDM", "SP_CODE")), any_vars(. == -99999))

# remove the rows with missing values
kelp_ana <- 
  kelp_ana %>%
  filter_at(vars(c("AFDM", "SP_CODE")), any_vars(. != -99999))

# summarise for the two transects
kelp_ana_sum <- 
  kelp_ana %>%
  group_by(SITE, YEAR, SP_CODE) %>%
  summarise(AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup()


# work with the summarised data

# add the local alpha diversity for each year
# add the gamma diversity across years

kelp_ana_sum %>%
  








