
# load relevant libraries generally
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)


# load the Jena data
jena_dat <- read_delim(here("analysis_data/jena_analysis_data.csv"), delim = ",")

jena_dat %>%
  filter(species_pool > 1) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = community_biomass)) +
  geom_point() +
  geom_smooth(method = "lm")

lm.dat <- 
  jena_dat %>%
  filter(species_pool > 1)

lm.1 <- lm(community_biomass ~ realised_richness, data = lm.dat) 
summary(lm.1)


dat.in <- read_tsv(here("raw_data/e120_Plant aboveground biomass data.txt"), skip = 79)
View(dat.in)

unique(dat.in$Strip)


