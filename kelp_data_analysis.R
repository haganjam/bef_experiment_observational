
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: California kelp data from (Gerung et al. 2020)

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
kelp_raw <- read_csv(here("data/Annual_All_Species_Biomass_at_transect_20200108.csv"))

# check for unique sites
kelp_raw$SITE %>%
  unique()

# remove the two sites on the channel islands
# SCDI
# SCTW
kelp_raw <- 
  kelp_raw %>%
  filter( !(SITE %in% c("SCDI", "SCTW")) )

# subset out the algae data
names(kelp_raw)

unique(kelp_raw$GROUP)

kelp_raw <- 
  kelp_raw %>%
  filter(GROUP == c("ALGAE"))

View(kelp_raw)

# explore the data
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(transect = length(unique(TRANSECT))) %>%
  filter(transect > 2)

kelp_raw %>%
  group_by(SITE) %>%
  summarise(year = length(unique(YEAR)))

kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(n_transects = length(unique(TRANSECT)) ) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = SITE, y = YEAR, colour = n_transects)) +
  geom_point()
  
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(n_transects = length(unique(TRANSECT)) ) %>%
  ungroup() %>%
  group_by(SITE) %>%
  summarise(min_transects = min(n_transects))

kelp_raw %>%
  group_by(SITE, YEAR, TRANSECT) %>%
  summarise(transect = min(TRANSECT) )  %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = YEAR, y = transect)) +
  geom_point() +
  facet_wrap(~SITE)


# based on this exploration:
# (1) remove years before 2001 (i.e. only 2000) because this is only available at some sites
# (2) select the transects with the lowest number ID at each site because these are consistent across years

kelp_raw <- 
  kelp_raw %>%
  filter(YEAR > 2000) %>%
  group_by(SITE) %>%
  mutate(min_1 = min(unique(TRANSECT), na.rm = TRUE),
         min_2 = sort(unique(TRANSECT) )[2]) %>%
  ungroup() %>%
  filter(TRANSECT %in% c(min_1, min_2) ) %>%
  select(-min_1, -min_2)


# analysis data

kelp_ana <- 
  kelp_raw %>%
  select(-PERCENT_COVER, -DENSITY, -WM_GM2, -DRY_GM2,
         -SFDM)

# sum up the transects at each site-year combination

# check if there are missing values
kelp_ana %>%
  filter_at(vars(c("AFDM", "SP_CODE")), any_vars(. == -99999))

# remove the rows with missing values
kelp_ana <- 
  kelp_ana %>%
  filter_at(vars(c("AFDM", "SP_CODE")), all_vars(. != -99999))

kelp_ana %>%
  summary()

kelp_ana %>%
  filter(AFDM < 0)

kelp_ana %>%
  filter(SP_CODE != "MAPY") %>%
  filter(grepl("kelp", COMMON_NAME)) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

kelp_ana$TAXON_GENUS %>%
  unique()

kelp_ana %>%
  filter(grepl("Macrocystis", TAXON_GENUS)) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

# summarise for the two transects at each site for each year
kelp_ana_sum <- 
  kelp_ana %>%
  group_by(SITE, YEAR, SP_CODE) %>%
  summarise(AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup()


# work with the summarised data

# add the local alpha diversity for each year
# add the gamma diversity across years

kelp_ana_sum %>%
  group_by(SITE, YEAR) %>%
  summarise(alpha_diversity = sum(decostand(AFDM, method = "pa"), na.rm = TRUE),
         comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = alpha_diversity, y = comm_biomass, colour = as.character(YEAR) )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

kelp_ana_sum %>%
  group_by(SITE, YEAR) %>%
  mutate(comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(AFDM > 0) %>%
  group_by(SITE) %>%
  summarise(gamma_diversity = length(unique(SP_CODE)),
            comm_biomass = mean(comm_biomass, na.rm = TRUE)) %>%
  select(-SITE) %>%
  ggplot(data = .,
         mapping = aes(x = gamma_diversity, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()


# calculate relative abundances for each year and each site
kelp_ana_sum %>% 
  group_by(SITE, YEAR) %>%
  mutate(total_AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SP_CODE_rel = (AFDM/total_AFDM)*100 ) %>%
  filter(SP_CODE_rel > 5) %>%
  split(., .$SITE) %>%
  lapply(., function(x) {
    
    ggplot(data = x, mapping = aes(x = YEAR, y = AFDM, colour = SP_CODE)) +
      geom_point() +
      facet_wrap(~SITE) +
      theme_classic() 
    }
  )


# do these analyses without the giant kelp (MAPY) and unidentified juvenile kelp (BLD)

kelp_ana_sum %>%
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  summarise(alpha_diversity = sum(decostand(AFDM, method = "pa"), na.rm = TRUE),
            comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = alpha_diversity, y = comm_biomass, colour = as.character(YEAR) )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gam_no_kelp <- 
  kelp_ana_sum %>%
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  mutate(comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(AFDM > 0) %>%
  group_by(SITE) %>%
  summarise(gamma_diversity = length(unique(SP_CODE)),
            comm_biomass = mean(comm_biomass, na.rm = TRUE))

ggplot(data = gam_no_kelp,
         mapping = aes(x = gamma_diversity, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()



# plot species' biomass through time
kelp_ana_sum %>% 
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  mutate(total_AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SP_CODE_rel = (AFDM/total_AFDM)*100 ) %>%
  filter(SP_CODE_rel > 5) %>%
  split(., .$SITE) %>%
  lapply(., function(x) {
    
    ggplot(data = x, mapping = aes(x = YEAR, y = AFDM, colour = SP_CODE)) +
      geom_point() +
      facet_wrap(~SITE) +
      theme_classic() 
  }
  )

# are the dominant species different across sites in a given year
dom_spp <- 
  kelp_ana_sum %>% 
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  mutate(total_AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SP_CODE_rel = (AFDM/total_AFDM)*100 ) %>%
  group_by(SITE, YEAR) %>%
  mutate(SP_max = max(SP_CODE_rel)) %>%
  ungroup() %>%
  filter(SP_CODE_rel == SP_max)

# do dominant species change in space?
dom_spp %>%
  split(., .$YEAR) %>%
  lapply(., function(x) { unique(x$SP_CODE)})

# do dominant species change in time?
spp_turn <- 
  dom_spp %>%
  split(., .$SITE) %>%
  lapply(., function(x) { unique(x$SP_CODE) %>% length()}) %>%
  unlist() %>%
  tibble::enframe(., name = "SITE", value = "spp_turn")

full_join(gam_no_kelp, spp_turn) %>%
  ggplot(data = .,
         mapping = aes(x = gamma_diversity, y = spp_turn)) +
  geom_point()



