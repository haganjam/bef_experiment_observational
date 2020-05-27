
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Duffy et al. (2017) meta-analysis

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
duf_dat_raw <- read_delim(here("data/duffy_et_al_2017_obs_data.csv"), delim = ",")
head(duf_dat_raw)

View(duf_dat_raw)

# create a copy to clean
duf_dat <- duf_dat_raw

# subset out relevant variables
names(duf_dat)

duf_dat <- 
  duf_dat %>%
  select(names(duf_dat[, c(1:25, 35:51)]))


# subset out rows with log response ratio estimates
duf_dat$LRnet_after %>% unique()

duf_dat_LR <- 
  duf_dat %>%
  filter( !(LRnet_after %in% c(".", ". ") ) ) %>%
  filter(!grepl(pattern = "can", x = LRnet_after)) %>%
  filter(!is.na(LRnet_after)) %>%
  mutate(LRnet_after = as.numeric(LRnet_after))

# subset out rows with binned p-values reported
duf_dat$Pval_after_bin %>% 
  unique()

duf_dat_p <- 
  duf_dat %>%
  filter(Pval_after_bin != ".") %>%
  filter(!is.na(Pval_after_bin))

duf_dat_p$Reference %>%
  unique() %>%
  length()

nrow(duf_dat_p)


# export a .csv file to fill in the spatial grain and extent of these studies

duf_dat_p %>%
  mutate(spatial_grain = c(NA),
         spatial_extent = c(NA),
         grain_extent_notes = c(NA)) %>%
  write_csv(., path = here("data/duffy_2017_meta_analysis_spatial.csv"))




