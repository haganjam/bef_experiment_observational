
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Foster data seed addition re-analysis

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

# load the biomass data
fos_bio_raw <- read_csv(here("data/E1 Plant Biomass 6 16.csv"))
summary(fos_bio_raw)
head(fos_bio_raw)

# load the composition data
fos_com_raw <- read_csv(here("data/E1 Plant Species Compostion 6 16.csv"))
summary(fos_com_raw)


### biomass data

fos_bio_1 <- 
  fos_bio_raw %>%
  mutate(Plot = as.character(Plot)) %>%
  filter(Year > 2001, N == 0, P == 0)

ggplot(data = fos_bio_1,
       mapping = aes(x = Year, y = Live_Biomass, colour = Plot)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Seed) +
  theme_classic()

bio_trends <- 
  fos_bio_1 %>%
  split(., .$Plot) %>%
  lapply(., function(x) { 
    lm(Live_Biomass ~ Year, data = x) %>%
      tidy() }) %>%
  bind_rows(.id = "Plot") %>%
  filter(term == "Year") %>%
  select(Plot, estimate) %>%
  mutate(bio_est = estimate) %>%
  select(-estimate)


### community data

spp_names <- 
  names(fos_com_1[, -c(1:10)])

fos_com_1 <- 
  fos_com_raw %>%
  mutate(Plot = as.character(Plot)) %>%
  filter(N == 0, P == 0)

fos_com_1 %>%
  gather(all_of(spp_names), 
         key = "species",
         value = "cover") %>%
  filter(Seed == 1) %>%
  ggplot(data = .,
         mapping = aes(x = Year, y = cover, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Plot) +
  theme_classic() +
  theme(legend.position = "none")

# most species are not changing but a few are increasing and others are decreasing

spp_plot <- 
  fos_com_1 %>%
  gather(all_of(spp_names), 
         key = "species",
         value = "cover") %>%
  split(., .$Plot)

spp_trends <- vector("list", length = length(spp_plot))
names(spp_trends) <- seq_along(1:length(spp_plot))

for (i in seq_along(1:length(spp_plot))) {
  
  spp_trends[[i]] <- 
    spp_plot[[i]] %>%
    split(., .$species) %>%
    lapply(., function(x) { 
      lm(cover ~ Year, data = x) %>%
        tidy() }) %>%
    bind_rows(.id = "species")
}

spp_trends <- bind_rows(spp_trends, .id = "Plot")

# subset out the species that have zero trends
spp_trends <- 
  spp_trends %>%
  filter(term == "Year") %>%
  filter(estimate != 0) %>%
  filter(p.value < 0.05)


### examine which species explains increases or decreases in biomass

bio_spp <- full_join(spp_trends, bio_trends, by = c("Plot"))

bio_spp <- 
  bio_spp %>%
  select(Plot, species) %>%
  split(., .$Plot)

z <- zoo()

for(i in 1:length(bio_spp)) {
  z <- merge(z, bio_spp[[i]] %>% select(species))
  }
names(z) <- names(bio_spp)
z <- as_tibble(z)

z
