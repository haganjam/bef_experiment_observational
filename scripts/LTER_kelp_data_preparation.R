
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Clean the California kelp data (LTER)

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# load the biomass data
# this can take a few seconds
kelp_raw <- read_csv(url("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sbc.50.8&entityid=24d18d9ebe4f6e8b94e222840096963c"))

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
unique(kelp_raw$GROUP)

kelp_raw <- 
  kelp_raw %>%
  filter(GROUP == c("ALGAE"))

# explore the data
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(transect = length(unique(TRANSECT))) %>%
  filter(transect > 2)

kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(transect = length(unique(TRANSECT))) %>%
  filter(transect == 2)

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

# check if all species codes are recorded for each site
kelp_ana %>%
  group_by(SITE, YEAR) %>%
  summarise(spp = length(unique(SP_CODE))) %>%
  pull(spp)

# all species codes are recorded each year at each site and just ticked off

# check for NAs in the data
lapply(kelp_ana, function(x) {
  
  if_else(is.na(x), 1, 0) %>%
    sum()
})


# sum up the transects at each site-year combination

# check if there are missing values
kelp_ana %>%
  filter(AFDM == -99999) %>%
  View()

kelp_ana$COMMON_NAME %>%
  unique()

kelp_ana$SP_CODE %>%
  unique()

kelp_ana %>%
  filter(is.na(SP_CODE) ) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

kelp_ana %>%
  filter(is.na(SP_CODE) ) %>%
  nrow()

# remove the rows with missing values for ash free dry mass
kelp_ana <- 
  kelp_ana %>%
  filter_at(vars(c("AFDM")), any_vars(. != -99999))

# replace the NAs for species code with NIAN as this is the scientific name
kelp_ana <- 
  kelp_ana %>%
  mutate(SP_CODE = if_else(is.na(SP_CODE), "NIAN", SP_CODE))

# check general summary statistics
kelp_ana %>%
  summary()

# check for more missing values for the AFDM
kelp_ana %>%
  filter(AFDM < 0)

kelp_ana %>%
  filter(SP_CODE != "MAPY") %>%
  filter(grepl("kelp", COMMON_NAME)) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

kelp_ana$TAXON_GENUS %>%
  unique()

kelp_ana %>%
  filter(grepl("Macrocystis", TAXON_GENUS)) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

# summarise for the two transects at each site for each year
kelp_ana_sum <- 
  kelp_ana %>%
  group_by(SITE, YEAR, SP_CODE) %>%
  summarise(AFDM = sum(AFDM, na.rm = TRUE), .groups = "drop")

# there are two years at the AHND site 2017 and 2019 with zeros for all algae species
# remove these for now
kelp_ana_sum %>%
  filter((SITE == "AHND" & YEAR %in% c(2017, 2019)) )

# remove these two years and do the analysis without them
kelp_ana_sum <- 
  kelp_ana_sum %>%
  filter(!(YEAR %in% c(2017, 2019)))

kelp_ana_sum$YEAR %>%
  unique() %>%
  length()

# cleaned kelp data
kelp_ana_sum

# output an analysis data file
write_csv(x = kelp_ana_sum, file = here("analysis_data/kelp_data_cleaned.csv"))

### END