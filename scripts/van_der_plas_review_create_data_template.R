
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: van der Plas (2019) systematic review data template preparation

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(here)

### code for creating a template to fill in the grain and extent information

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

# how many bef slopes are there that controlled for abiotic factors?
vand_dat$Relationship_nr %>%
  unique() %>%
  length()

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

# however, we need to add the 'other remarks data' because this actually contains some habitat type information
other_dat <- 
  vand_dat_raw %>%
  select(Relationship_nr, 'Other remarks')

# join these data to the vand_dat_c data
vand_dat_c <- 
  inner_join(vand_dat_c, other_dat, by = c("Relationship_nr") )

View(vand_dat_c)

vand_dat_c %>%
  filter(`paper number` == 125) %>%
  View()


# export an excel file to fill in the spatial grain and spatial extent information from the papers

# how many papers are there to go through?
vand_dat_c$`paper number` %>%
  unique() %>%
  length()

sub_names <- 
  names(vand_dat_c)[c(3, 4, 5, 6, 7, 8, 9, 11, 12, 30, 13, 14, 15, 16, 17, 18, 19, 29)]

# export a file to fill in with the spatial grain information
vand_dat_c %>%
  select(sub_names) %>%
  mutate(min_lat = c("."),
         max_lat = c("."),
         min_lon = c("."),
         max_lon = c("."),
         spatial_grain = c("."),
         spatial_extent = c("."),
         grain_extent_notes = c(".")) %>%
  write_csv(., path = here("data/van_der_Plas_review_spatial_grains.csv"))

# this exported file has duplicates for some bef-slopes
# these were removed while filling in the data


