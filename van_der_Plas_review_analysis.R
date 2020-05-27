

# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: van der Plas (2019) systematic review data

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

# load the van der Plas data
vand_dat_raw <- read_delim(here("data/van_der_Plas_2019_systematic_review.csv"), delim = ",")
head(vand_dat_raw)

# check the variables without X's
vand_dat_raw[, (!grepl("X", x = names(vand_dat_raw)))] %>%
  names()

# check the variables with X's
vand_dat_raw[, (grepl("X", x = names(vand_dat_raw)))] %>%
  names()

# remove these variables associated with the .csv file problems
vand_dat_raw <- vand_dat_raw[, (!grepl("X", x = names(vand_dat_raw)))]

# check the variables names
names(vand_dat_raw)

# make a copy of the data for cleaning
vand_dat <- vand_dat_raw

# check unique values for different variables

unique(vand_dat$`Category of function`) # subset out biomass only data points

unique(vand_dat$`abiotic covariate`)

unique(vand_dat$`composition covariate`)

# subset out Category of function: biomass
vand_dat <- 
  vand_dat %>%
  filter(`Category of function` == "Biomass")

# subset out studies where abiotic covariates were controlled for
vand_dat <- 
  vand_dat %>%
  filter(grepl(pattern = "es", `abiotic covariate`) )

vand_dat

# isolate relevant variables
rel_vars <- c( names(vand_dat[, 1:25]), names(vand_dat[, c(32, 34, 38, 39)]) )

# subset out the relevant variables
vand_dat <- 
  vand_dat %>%
  select(rel_vars)

vand_dat

# check which relationships have multiple BEF relationships associated with them

# create a vector of Relationship_nr that have multiple BEF relationships associated with them
mult_BEF <- 
  vand_dat %>%
  group_by(Relationship_nr) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(Relationship_nr)

mult_BEF

# split the vand_dat data into two

# all rows with only one BEF-relationship reported
vand_dat_1 <- 
  vand_dat %>%
  filter(!(Relationship_nr %in% mult_BEF)) %>%
  mutate(bef_multi = c("no"))

# all rows with multiple BEF-relationships reported
vand_dat_2 <- 
  vand_dat %>%
  filter((Relationship_nr %in% mult_BEF)) %>%
  mutate(bef_multi = c("yes"))

# for both datasets, add a new column: BEF relationship
vand_dat_1 <- 
  vand_dat_1 %>%
  mutate(bef_relationship = if_else(is.na(`Relationship overall`), Relationship, `Relationship overall`)) %>%
  select(-Relationship, `Relationship overall`, -`Line number`)

vand_dat_2 <- 
  vand_dat_2 %>%
  mutate(bef_relationship = `Relationship overall`) %>%
  select(-Relationship, `Relationship overall`, -`Line number`) %>%
  distinct() %>% 
  filter(!is.na(bef_relationship))

# bind these datasets together
vand_dat_c <- bind_rows(vand_dat_1, vand_dat_2)

# this dataset is cleaned and can now be searched for spatial grain and extent information
nrow(vand_dat_c)




