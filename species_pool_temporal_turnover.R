
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Testing the turnover-species pool hypothesis

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
library(ggpubr)


# load the species pool data
bio_t <- read_csv(here("data/rawdata_biotime.csv"))

bio_t %>% 
  View()

# inclusion criteria

# 1. at least 5 years of data

# 2. 


# lakeID_sampleDate_station_towDepth

# check categories
bio_t %>%
  mutate(focal_id = paste(STUDY_ID, SAMPLE_DESC, sep = "_")) %>%
  split(.$STUDY_ID) %>%
  map(~ .x %>% 
        group_by(YEAR) %>%
        summarise(n = length(unique(focal_id))))




