
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


# load the Cedar Creek data

# plant biomass data
# https://www.cedarcreek.umn.edu/research/data/dataset?ple120
ced.dat <- read_tsv(here("raw_data/e120_Plant aboveground biomass data.txt"), skip = 79)
head(ced.dat)

# remove the NAs at the end of the dataset
ced.dat <- 
  ced.dat %>%
  filter(!is.na(Year))

# note that the 1996 data has no strip labels
ced.dat %>%
  filter(Year == 1996) %>%
  pull(Strip) %>%
  is.na(.) %>%
  any(. == TRUE)

# none of the strip samples from 1997 to 2000 have NAs
ced.dat %>%
  filter(Year %in% c(1997, 1998, 1999, 2000)) %>%
  pull(Strip) %>%
  is.na(.) %>%
  any(. == TRUE)

# subset out those years
ced.early <- 
  ced.dat %>%
  filter(Year %in% c(1997, 1998, 1999, 2000))

summary(ced.sub)
View(ced.sub)

unique(ced.sub$Species)


# plant cover data
# https://www.cedarcreek.umn.edu/research/data/dataset?pce120
ced.pc <- read_tsv(here("raw_data/e120_Plant species percent cover data (1).txt"), skip = 77,
                   n_max = 1000)
problems(ced.pc)
head(ced.pc)




# subset out the later years of the experiment
unique(ced.dat$Year)

ced.dat %>%
  group_by(Exp, Year, Month) %>%
  summarise(sorted_biomass = length(unique(Species)),
            plot_n = length(unique(Plot))) %>%
  View()

ced.late <- 
  ced.dat %>%
  filter(Year == 2001,
         Month == 8)

species_names <- names(ced.late[,18:35])
species_names

seed_species <- c("Achillea millefolium",
                  "Agropyron smithii",
                  "Amorpha canescens",
                  "Andropogon gerardi",
                  "Asclepias tuberosa",
                  "Elymus canadensis",
                  "Koeleria cristata",
                  "Lespedeza capitata",
                  "Liatris aspera",
                  "Lupinus perennis",
                  "Monarda fistulosa",
                  "Panicum virgatum",
                  "Petalostemum purpureum",
                  "Poa pratensis",
                  "Quercus ellipsoidalis",
                  "Quercus macrocarpa",
                  "Schizachyrium scoparium",
                  "Sorghastrum nutans")

ced.late <- 
  ced.late %>%
  filter(Species %in% seed_species) %>%
  pivot_longer(cols = all_of(species_names),
               names_to = "species_code",
               values_to = "planted_not_planted")

ced.late$Species <- as.factor(ced.late$Species)
levels(ced.late$Species) <- sort(species_names, decreasing = FALSE)[-1]
ced.late$Species <- as.character(ced.late$Species)

ced.late <- 
  ced.late %>%
  filter(planted_not_planted > 0) %>%
  filter(Species == species_code)

# calculate realised diversity
rr <- 
  ced.late %>%
  group_by(Year, Month, NumSp, Plot, Strip) %>%
  summarise(species_comp = paste(Species, sep = "."),
            realised_richness = length(unique(Species)),
            biomass = sum( `Biomass (g/m2)` ), .groups = "drop") %>%
  group_by(Year, Month, NumSp, species_comp, Plot) %>%
  summarise(biomass = mean(biomass),
            realised_richness = mean(realised_richness), .groups = "drop") %>%
  mutate(NumSp_char = as.character(NumSp))

rr

rr %>%
  filter(NumSp > 1) %>%
  ggplot(data = rr, 
         mapping = aes(x = realised_richness, y = biomass, colour = NumSp_char)) +
  geom_point() +
  geom_smooth(method = "lm")



