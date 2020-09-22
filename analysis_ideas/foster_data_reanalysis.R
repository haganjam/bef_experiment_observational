
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Foster data seed addition re-analysis


# next to do:

# plan the next moves in this story...

# there is something that can be done here but need to think about how to show it properly

# need to see which species are increasing, are they sown and do they contribute biomass?


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
  mutate(Unit = as.character(Unit)) %>%
  filter(Year > 2001, N == 0, P == 0)

ggplot(data = fos_bio_1,
       mapping = aes(x = Year, y = Live_Biomass, colour = Unit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Seed) +
  theme_classic()

ggplot(data = fos_bio_1,
       mapping = aes(x = Seed, y = Live_Biomass, colour = as.character(Year) )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

bio_trends <- 
  fos_bio_1 %>%
  split(., .$Unit) %>%
  lapply(., function(x) { 
    lm(Live_Biomass ~ Year, data = x) %>%
      tidy() }) %>%
  bind_rows(.id = "Unit") %>%
  filter(term == "Year") %>%
  select(Unit, estimate) %>%
  mutate(bio_est = estimate) %>%
  select(-estimate)


### community data

spp_names <- 
  names(fos_com_1[, -c(1:10)])

fos_com_1 %>%
  gather(all_of(spp_names), 
         key = "species",
         value = "cover") %>%
  filter(Seed == 1) %>%
  ggplot(data = .,
         mapping = aes(x = Year, y = cover, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Unit) +
  theme_classic() +
  theme(legend.position = "none")

# most species are not changing but a few are increasing and others are decreasing

spp_plot <- 
  fos_com_1 %>%
  gather(all_of(spp_names), 
         key = "species",
         value = "cover") %>%
  split(., .$Unit)

spp_trends <- vector("list", length = length(spp_plot))
names(spp_trends) <- names(spp_plot)

for (i in seq_along(1:length(spp_plot))) {
  
  spp_trends[[i]] <- 
    spp_plot[[i]] %>%
    split(., .$species) %>%
    lapply(., function(x) { 
      lm(cover ~ Year, data = x) %>%
        tidy() }) %>%
    bind_rows(.id = "species")
}

spp_trends <- bind_rows(spp_trends, .id = "Unit")


# subset out the species that have zero trends
spp_inc <- 
  spp_trends %>%
  filter(term == "Year") %>%
  filter(estimate > 0.5, p.value < 0.05)

spp_dec <- 
  spp_trends %>%
  filter(term == "Year") %>%
  filter(estimate < -0.5, p.value < 0.05)

spp_inc_dec <- 
  bind_rows(spp_inc, spp_dec)

hist(spp_inc_dec$estimate)


### examine which species explains increases or decreases in biomass

bio_spp <- full_join(spp_inc_dec, bio_trends, by = c("Unit"))

mod_vars <- 
  fos_bio_1 %>%
  filter(Year == max(Year)) %>%
  select(Unit, Block, Gradient, Plot, Seed, N, P, Live_Biomass, Litter_Biomass) %>%
  mutate(total_biomass = (Live_Biomass + Litter_Biomass))

trend_dat <- full_join(mod_vars, bio_spp, by = "Unit")

ggplot(data = trend_dat,
       mapping = aes(x = Seed, y = bio_est)) +
  geom_point() +
  theme_classic()

ggplot(data = trend_dat,
       mapping = aes(x = Seed, y = Live_Biomass)) +
  geom_point() +
  theme_classic()


# which species are increasing? Are these species that were added?

sown_spp <- read_csv(here("data/sown_spp.csv"))

# all species names
spp_names # n = 173

# sown species names
sown_spp_names <- unique(sown_spp$Species) 
sown_spp_names # n = 34

# non-sown species names
non_sown_spp <- spp_names[ !(spp_names %in% sown_spp_names) ]
non_sown_spp # n = 145

species_list <- 
  left_join(tibble(species = spp_names), 
          tibble(species = sown_spp_names, sown = c("yes")),
          by = "species") %>%
  mutate(sown = if_else(is.na(sown), "no", sown))


# which species are increasing in the different units?
spp_inc

increasers <- 
  spp_inc %>%
  select(Unit, species) %>%
  split(., .$Unit) %>%
  lapply(., function(x) { x %>% mutate(pres = 1) })

# add the full species list
inc_spp <- species_list

for(i in seq_along(1:length(increasers))) {
  inc_spp <- left_join(inc_spp, 
                         select(increasers[[i]], species, pres), 
                         by = "species")
}

names(inc_spp) <- c("species", "sown", names(increasers))

# are sown increasing more than non-sown species?
inc_spp %>%
  mutate_at(vars(names(increasers)), ~ if_else(is.na(.), 0, 1)) %>%
  gather(names(increasers),
         key = "Unit", value = "presence") %>%
  group_by(Unit, sown) %>%
  summarise(total = sum(presence)) %>%
  ungroup()


# which species are decreasing in the different units?
spp_dec

decreasers <- 
  spp_dec %>%
  select(Unit, species) %>%
  split(., .$Unit) %>%
  lapply(., function(x) { x %>% mutate(pres = 1) })

# add the full species list
all_names <- 
  tibble(Unit = "all", species = unique(spp_dec$species))

for(i in seq_along(1:length(decreasers))) {
  all_names <- left_join(all_names, 
                     select(decreasers[[i]], species, pres), 
                     by = "species")
}

names(all_names) <- c("Unit", "species", names(decreasers))

all_names %>%
  mutate_all(~ if_else(is.na(.), 0, .))





