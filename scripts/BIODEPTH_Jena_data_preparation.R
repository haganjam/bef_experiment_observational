
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Prepare the BIODEPTH and Jena data

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)
library(vegan)

# make a folder to export the cleaned data
if(! dir.exists(here("analysis_data"))){
  dir.create(here("analysis_data"))
}


####################################
#                                  #
#         BIODEPTH data            #
#                                  #
####################################


# load the BIODEPTH observed species richness data
# bio_d_real <- read_tsv(here("raw_data/Observed.Species.Richness.txt"))
bio_d_real <- read_tsv(url("https://ndownloader.figshare.com/files/5640243"))

# load the BIODEPTH biomass data (shoot biomass)
# bio_d_bio <- read_tsv(here("raw_data/Shoots.txt"))
bio_d_bio <- read_tsv(url("https://ndownloader.figshare.com/files/5640249"))


# join these data sets together to output the relevant variables
names(bio_d_real)
names(bio_d_bio)

bio_d <- 
  full_join(select(bio_d_real,
                   year, plot, location, block, species.richness, species.observed),
            select(bio_d_bio,
                   year, plot, location, block, species.richness, biomass),
            by = c("year", "plot", "location", "block", "species.richness"))

# subset out the final year of each experiment
bio_con <- 
  bio_d %>%
  group_by(location) %>%
  filter(year == last(year)) %>%
  ungroup()

# rename the experiments and make it a factor
site_names <- 
  c("DEU", "PRT", "CHE", "GRC",
    "IRL", "SWE", "Shef.", "Silw.")

bio_con$location <- as.factor(bio_con$location)
levels(bio_con$location) <- site_names


####################################
#                                  #
#         Jena data                #
#                                  #
####################################

# load the Jena biomass data
# jena_bio <- read_delim(here("raw_data/Jena_Biomass_02-08.csv"), delim = ",")
jena_bio <- read_delim(url("https://ndownloader.figshare.com/files/5608847"), delim = ",")

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

# take the final time-point
site_bio <-
  site_bio %>%
  filter(time == max(time))

# remove the 60 species treatment because this is a positive control
site_bio <- 
  site_bio %>%
  filter(sowndiv < 60)


# merge the BIODEPTH and Jena data
names(site_bio)

# create a location variable for the Jena experiment
site_bio$location <- "Jena"

# select variables and rename columns from each data.frame
var.names <- c("location", "plot", "time", "sown.diversity", "realised.diversity", "biomass")

site_bio <- 
  site_bio %>%
  select(location, plotcode, time, sowndiv, observed_sr, comm_biomass)

bio_con <- 
  bio_con %>%
  select(location, plot, year, species.richness, species.observed, biomass)

names(site_bio) <- var.names
names(bio_con) <- var.names

# stick the Jena data onto the bottom of the BIODEPTH data
exp.dat <- bind_rows(bio_con, site_bio)

# check for missing data
exp.dat %>%
  filter_all(any_vars(is.na(.)) )

# remove the datapoints with missing data
exp.dat <- 
  exp.dat %>%
  filter_all(all_vars(!is.na(.)))

# remove the monoculture data
exp.dat <- 
  exp.dat %>%
  filter(sown.diversity > 1)

# write a csv of this
write_csv(x = exp.dat, file = here("analysis_data/bio_exp_dat.csv"))

### END