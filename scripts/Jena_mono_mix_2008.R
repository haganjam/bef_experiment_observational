
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Jena data re-analysis with monocultures from PANGAEA

# Details:
# the monoculture dataset is from 2008 which the data used for Fig. 2 in the first submission
# https://doi.pangaea.de/10.1594/PANGAEA.866317

# the mixture data is also from 2008 and can be downloaded from:
# https://doi.pangaea.de/10.1594/PANGAEA.846532

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


# monocultures

# load the data the biomass data
mono.dat <- read_tsv(url("https://doi.pangaea.de/10.1594/PANGAEA.866317?format=textfile"), skip = 35)

# load the plot information data
mono.info <- read_tsv(url("https://store.pangaea.de/Publications/Jena_Experiment/PlotInformationSmallMonos.txt"))

# clean the biomass data
unique(mono.dat$`Date/time start`)[2]
unique(mono.dat$`Date/time end`)[2]
unique(paste(mono.dat$`Date/time start`, mono.dat$`Date/time end`, sep = "_"))

# subset the August data and relevant columns
mono.dat <- 
  mono.dat %>%
  filter(`Date/time start` == unique(mono.dat$`Date/time start`)[1] ) %>%
  filter(Replicate != "mean") %>%
  select(`Experimental plot`, `Date/time start`, Replicate, `Sown plant biom [g/m**2]`) %>%
  filter(!is.na(`Sown plant biom [g/m**2]`)) # replicate plots were abandoned

# rename the columns
names(mono.dat) <- c("plotcode", "date", "replicate", "sown_plant_biomass_g_m2")

# clean the plot information data
mono.info <- 
  mono.info %>%
  select(plotcode, SpeciesFullName, SpeciesAbbreviation) %>%
  mutate(genus = gsub(pattern = " .*", replacement = "", SpeciesFullName),
         binom = gsub(pattern = ".*? ", replacement = "", SpeciesFullName)) %>%
  select(plotcode, genus, binom, SpeciesFullName, SpeciesAbbreviation)

names(mono.info) <- c("plotcode", "genus", "binom", "species", "species_abbr")

# join the mono.dat to the species identities in the mono.info
unique(mono.info$species) %>% length()
unique(mono.info$plotcode) %>% length()
unique(mono.dat$plotcode) %>% length()

# get plots in the mono.dat data
focal_plots <- unique(mono.dat$plotcode)

# subset the mono.info data
mono.info <- 
  mono.info %>%
  filter(plotcode %in% focal_plots)

unique(mono.info$plotcode) %>% length()
unique(mono.dat$plotcode) %>% length()

# make the join
mono.join <- full_join(mono.dat, mono.info, by = "plotcode")

# summarise the monoculture data over the two replicates
mono.join <- 
  mono.join %>%
  group_by(date, plotcode, genus, binom, species, species_abbr) %>%
  summarise(mean_plant_biomass_g_m2 = mean(sown_plant_biomass_g_m2), .groups = "drop") 

# some species have two plots (i.e. nine species from dominance experiment)
# take one of these at random
mono.join %>%
  group_by(species) %>%
  summarise(n = n())

set.seed(385279845)
mono.join <- 
  mono.join %>%
  group_by(species) %>%
  slice_sample(n = 1) %>%
  ungroup()

# test for missing or negative values
lapply(mono.join, function(x) {sum(if_else(is.na(x), 1, 0)) } )
lapply(mono.join, function(x) {sum(if_else(x < 0, 1, 0)) } )
unique(mono.join$species) %>% length()

# check the monoculture distribution
hist(mono.join$mean_plant_biomass_g_m2)


# mixtures
mix.dat <- read_tsv(url("https://doi.pangaea.de/10.1594/PANGAEA.846532?format=textfile"), skip = 107)
problems(mix.dat)
spec(mix.dat)

# check the columns etc.
head(mix.dat)
names(mix.dat)

# clean the data
unique(mix.dat$`Date/time start`)[1]
unique(mix.dat$`Date/time end`)
unique(paste(mix.dat$`Date/time end`, mix.dat$`Date/time start`, sep = "_") )

# get the species names
j.cols <- names(mix.dat)

# get full species names
sp.names.full <- j.cols[grepl(pattern = "\\[g\\/m\\*\\*2\\]", j.cols) & (!grepl(pattern = "plant|Weeds", j.cols))]

mix.dat <- 
  mix.dat %>%
  filter(`Date/time start` == unique(mix.dat$`Date/time start`)[1]) %>%
  filter(Replicate != "mean") %>%
  select(`Date/time start`, `Experimental plot`, Replicate, `Sown plant biom [g/m**2]`, all_of(sp.names.full))

# simplify species names
sp.names <- gsub(pattern = "\\ biom\\ \\[g\\/m\\*\\*2\\]", replacement = "", x = sp.names.full)
sp.names <- gsub(pattern = "\\.\\ ", replacement = "_", x = sp.names)

names(mix.dat) <- c("date", "plotcode", "replicate", "total_biomass_g_m2", sp.names)

sown.spec <- 
  mix.dat %>%
  pivot_longer(cols = all_of(sp.names),
               names_to = "species",
               values_to = "biomass_g_m2") %>%
  filter(!is.na(biomass_g_m2) ) %>%
  split(., .$plotcode) %>%
  lapply(., function(x){ unique(x$species) })
  

# pull the dataset into the long format
mix.dat <- 
  mix.dat %>%
  pivot_longer(cols = all_of(sp.names),
               names_to = "species",
               values_to = "biomass_g_m2")

# check for -9999 values
lapply(mix.dat, function(x) {sum(if_else(x < 0, 1, 0), na.rm = TRUE) })
lapply(mix.dat, function(x) {sum(if_else(is.na(x), 1, 0), na.rm = TRUE) })

# replace the NA values with 0's
mix.dat <- 
  mix.dat %>%
  mutate(biomass_g_m2 = if_else(is.na(biomass_g_m2), 0, biomass_g_m2))

# summarise the data across the replicates
mix.tot <- 
  mix.dat %>%
  group_by(date, plotcode) %>%
  summarise(total_biomass_g_m2 = mean(total_biomass_g_m2, na.rm = TRUE), .groups = "drop")

head(mix.tot)
  
mix.spec <- 
  mix.dat %>%
  group_by(date, plotcode, species) %>%
  summarise(biomass_g_m2 = mean(biomass_g_m2, na.rm = TRUE), .groups = "drop")

head(mix.spec)

# join these dataset together
mix.sum <- full_join(mix.spec, mix.tot, by = c("date", "plotcode"))

# test if total sown biomass is equal to sum of species specific biomasses
mix.sum %>%
  group_by(date, plotcode) %>%
  summarise(ss_bio = sum(biomass_g_m2, na.rm = TRUE),
            total_bio = mean(total_biomass_g_m2), .groups = "drop") %>%
  mutate(test_bio = if_else(ss_bio != total_bio, 1, 0)) %>%
  ggplot(data = .,
         mapping = aes(x = ss_bio, y = total_bio, colour = test_bio)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# split the mix.sum dataset
mix.list <- split(select(mix.sum, -plotcode), mix.sum$plotcode)

# check if mix.list plotcodes are in the same order as the sown.spec list
any(names(mix.list) != names(sown.spec))

mix.out <- vector("list", length = length(mix.list))
for(i in 1:length(mix.list)) {
  
  mix.out[[i]] <- 
    mix.list[[i]] %>%
    filter(species %in% sown.spec[[i]])
  
}
names(mix.out) <- names(mix.list)

# bind this into a data.frame
mix.out <- bind_rows(mix.out, .id = "plotcode")

# calculate sowndiv and realised div
mix.out <- 
  mix.out %>%
  group_by(date, plotcode) %>%
  mutate(sowndiv_calc = length(unique(species)),
         realised_richness = sum(if_else(biomass_g_m2 > 0, 1, 0)) ) %>%
  ungroup()

# see if results match the figure in the original figure we published
mix.out.sum <- 
  mix.out %>%
  group_by(date, plotcode) %>%
  filter(sowndiv_calc > 1, sowndiv_calc < 60) %>%
  summarise(total_biomass_g_m2 = first(total_biomass_g_m2),
            realised_richness = first(realised_richness),
            sowndiv_calc = as.character(first(sowndiv_calc)), .groups = "drop" )

ggplot(data = mix.out.sum,
       mapping = aes(x = realised_richness, y = total_biomass_g_m2, colour = sowndiv_calc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  theme(legend.position = "bottom")


# join the monoculture data data to the species specific mixtures
mix.part <- 
  mix.out %>%
  select(date, plotcode, species, biomass_g_m2)

mono.join <- 
  mono.join %>% 
  mutate(species = paste(substring(species, 1, 1), binom, sep = "_" ) )

# check the the species names match
any(sort(mono.join$species) != sort(unique(mix.part$species)))
x <- which(sort(mono.join$species) != sort(unique(mix.part$species)))

sort(mono.join$species)[x]
sort(unique(mix.part$species))[x]

# fix the non-matching names
mono.join <-
  mono.join %>%
  mutate(species = if_else(species == "G_agg.", "G_mollugo", species),
         species = if_else(species == "L_agg.", "L_vulgare", species),
         species = if_else(species == "T_agg.", "T_officinale", species))

mix.part <- 
  mix.part %>%
  mutate(species = if_else(species == "M_x varia", "M_varia", species))

# test again
any(sort(mono.join$species) != sort(unique(mix.part$species)))

# join these datasets into the partition format
mono_join <- 
  mono.join %>%
  select(species, species_abbr, mean_plant_biomass_g_m2)

part_dat <- 
  full_join(mix.part, mono_join, by = "species") %>%
  select(plotcode, species, mean_plant_biomass_g_m2, biomass_g_m2)

# rename the columns
names(part_dat) <- c("sample", "species", "M", "Y")

# load the BEF partition scripts
source(here("scripts/functions_BEF_partitions.R"))

# what format should the data be in
f1

n_rye <- 
  part_dat %>%
  group_by(sample) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  arrange(sample)

# convert into the list
part_dat <- 
  part_dat %>%
  filter(sample %in% n_rye$sample) %>%
  arrange(sample, species)

any(FALSE == (sort(unique(part_dat$sample)) == sort(n_rye$sample)))

# convert part_dat into a list
part.list <- split(part_dat, part_dat$sample)

part_out <- vector("list", length = length(part.list)) 
for(i in 1:length(part_out)) {
  
  x <- 1/rep(n_rye$n[i], n_rye$n[i])
  
  part_out[[i]] <- fox.2005.pt(adf = part.list [[i]], RY.exp = x)
  
}

part_out[[10]]











