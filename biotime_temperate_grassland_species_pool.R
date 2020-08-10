# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Species pool estimation from individual composition samples using BIOTIME data

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

# create customised plotting theme
theme_meta <- function(base_size = 12, base_family = "") {
  theme(panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill="NA", color="black", size=0.75, linetype="solid"),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.ticks.length = unit(-0.16, "cm"),
        axis.title.x = element_text(colour ="black", size = 12, face = "plain", margin=margin(5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 12, face = "plain", margin=margin(0,5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=12, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=12, face = "plain", margin=margin(0,10,0,0,"pt")),
        axis.ticks.x = element_line(colour = "black", size = 0.4),
        axis.ticks.y = element_line(colour = "black", size = 0.4))
}

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# load the BIOTIME data
bio_dat_raw <- read_csv(here("data/rawdata_temperate_grassland_BIOTIME.csv"),
                        col_types = cols(
                          PLOT = col_character()
                        ))

ad_dat_raw <- 
  bio_dat_raw %>%
  filter(STUDY_ID == 473)

# calculate temporal gamma diversity
ad_dat_raw %>%
  group_by(PLOT) %>%
  summarise(temp_gamma = length(unique(GENUS_SPECIES)))
  
ad_dat_raw %>%
  group_by(PLOT, YEAR) %>%
  summarise(temp_gamma = length(unique(GENUS_SPECIES)))

# create a species by site matrix
ad_dat_wide <- 
  ad_dat_raw %>%
  select(PLOT, YEAR, GENUS_SPECIES, ABUNDANCE) %>%
  pivot_wider(id_cols = c("PLOT", "YEAR"),
              names_from = "GENUS_SPECIES",
              values_from = "ABUNDANCE") %>%
  mutate(across(.cols = where(is.numeric), ~if_else(is.na(.), 0, .)))

site <- 
  ad_dat_wide %>%
  mutate(row_id = 1:nrow(ad_dat_wide)) %>%
  select(row_id, PLOT, YEAR)

spp <- 
  ad_dat_wide %>%
  select(-PLOT, -YEAR)




