
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Duffy et al. (2017) meta-analysis spatial-grain and extent

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

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# load the duffy et al. 2017 data
duf_spat_raw <- read_delim(here("data/duffy_2017_meta_analysis_spatial_extent_grain.csv"), delim = ",")
duf_spat_raw

# remove data points that are multifunctionality measurements
duf_spat_raw$Y_broad %>%
  unique()

# remove the multifunctionality measurements
duf_spat_raw <- 
  duf_spat_raw %>%
  filter(!(grepl(pattern = "Multi", x = Y_broad)))
duf_spat_raw

View(duf_spat_raw)

# copy the duf_spat_raw to manipulate
duf_spat <- duf_spat_raw

# create a reference-specific habitat variable as a unique identifier
duf_spat <-
  duf_spat %>%
  mutate(unique_id = paste(Paper, Author, Year, Taxon_specific, Eco_specific, sep = "_"))

# check references with multiple slopes
mult_slopes <- 
  duf_spat %>%
  group_by(unique_id) %>%
  summarise(n = n()) %>% 
  filter(n > 1) %>%
  pull(unique_id)

# subset out these multiple slope studies
duf_spat %>%
  filter(unique_id %in% mult_slopes) %>%
  select(-Reference) %>%
  View()

# if multiple estimates from same data set are reported: 
# for a given data set (i.e. habitat and taxon) we include most direct (i.e. taxonomic richness)
# productivity, biomass, consumption (preference)

mult_ind <- 
  duf_spat %>%
  filter(unique_id %in% mult_slopes) %>%
  select(unique_id, Estimate) %>%
  filter(Estimate %in% c(2, 4, 5, 6, 13, 14, 15, 19, 30, 32, 33, 34, 38, 57, 59,
                         61, 62, 78:81, 89, 91, 93, 95, 96, 98, 102))

# join this to duf_spat
sub_mult <- 
  left_join(mult_ind, duf_spat, by = c("unique_id", "Estimate"))
 
# bind this to the duf_spat data and create duf_spat_ind
duf_spat_ind <- 
  bind_rows(sub_mult, duf_spat %>%
  filter( !(unique_id %in% mult_slopes) ) %>%
  select(names(sub_mult)) ) %>%
  select(-Reference) %>%
  arrange(Paper, Author, Year, unique_id)

duf_spat_ind <- 
  duf_spat_ind %>%
  mutate(spatial_extent_coarse = if_else(spatial_extent %in% c("continental", "national"), "continental",
                                  if_else(spatial_extent %in% c("landscape", "regional"), "regional", "global" )))

duf_spat_ind %>%
  filter(Rel_after != ".") %>%
  group_by(spatial_extent_coarse) %>%
  summarise(positive = sum(if_else(Rel_after == "pos", 1, 0), na.rm = TRUE),
            negative = sum(if_else(Rel_after == "neg", 1, 0), na.rm = TRUE),
            null = sum(if_else(Rel_after == "ind", 1, 0), na.rm = TRUE)) %>%
  mutate(total = (positive + negative + null)) %>%
  mutate_at(vars(c("positive", "negative", "null")), ~./total) %>%
  gather(positive, negative, null, key = "direction", value = "proportion") %>%
  ggplot(data = .,
         mapping = aes(x = direction, y = proportion, fill = spatial_extent_coarse)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_fill_viridis_d() +
  theme_classic()


