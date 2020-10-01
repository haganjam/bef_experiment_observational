
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Analyse the observational kelp dataset

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# load in the model data
kelp_adat <- read_delim(file = here("data/kelp_analysis_data.csv"), delim = ",")

# examine the data
head(kelp_adat)

# examine replication etc.
length(unique(kelp_adat$YEAR))
length(unique(kelp_adat$SITE))

# calculate alpha diversity and community biomass for each site-year combination
kelp_alpha <- 
  kelp_adat %>%
  group_by(SITE, YEAR) %>%
  summarise(realised_richness = sum(vegan::decostand(x = AFDM, method = "pa"), na.rm = TRUE),
            community_biomass = sum(AFDM, na.rm = TRUE),
            .groups = "drop")

alpha_est <- 
  lapply(split(select(kelp_alpha, -YEAR), kelp_alpha$YEAR ), function(x) {
    
    y <- 
      x %>%
      mutate(community_biomass = as.numeric(scale(sqrt(community_biomass))),
             realised_richness = as.numeric(scale((realised_richness))) )
    
    z <- lm(community_biomass ~ realised_richness, data = y)
    
    coef(z)[2]
    
  }) %>%
  bind_rows(., .id = "YEAR")

ggplot(data = alpha_est,
       mapping = aes(x = realised_richness)) +
  geom_histogram(bins = 8, alpha = 0.5, colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = mean(alpha_est$realised_richness), ) +
  xlab(NULL) +
  ylab(NULL) +
  theme_meta() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

kelp_alpha %>%
  mutate(community_biomass = sqrt(community_biomass),
         YEAR = as.character(YEAR)) %>%
  ggplot(data = .,
       mapping = aes(x = realised_richness, y = sqrt(community_biomass), colour = YEAR) ) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE, method = "lm", size = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(expression(sqrt(paste("community dry mass (g ",  " m"^"-2", ")") ))) +
  xlab(expression(paste("realised ", alpha, " diversity"))) +
  theme_meta() +
  theme(legend.position = "none")


# calculate gamma diversity and mean +- se across years for each year
gam_div <- 
  kelp_adat %>%
  group_by(SITE, SP_CODE) %>%
  summarise(AFDM_sum = sum(AFDM), .groups = "drop") %>%
  group_by(SITE) %>%
  summarise(species_pool = sum(vegan::decostand(x = AFDM_sum, method = "pa"), na.rm = TRUE),
            .groups = "drop") %>%
  pull(species_pool)

kelp_gam <- 
  kelp_adat %>%
  group_by(SITE, YEAR) %>%
  summarise(community_biomass = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
  group_by(SITE) %>%
  summarise(community_biomass_m = mean(sqrt(community_biomass), na.rm = TRUE),
            community_biomass_se = (sd(sqrt(community_biomass), na.rm = TRUE)/sqrt(n())), 
            .groups = "drop") %>%
  mutate(species_pool = gam_div) %>%
  select(SITE, species_pool, community_biomass_m, community_biomass_se)


ggplot(data = kelp_gam,
         mapping = aes(x = species_pool, 
                       y = community_biomass_m) ) +
  geom_smooth(alpha = 0.1, se = TRUE, method = "lm", size = 0.5, colour = "black") +
  geom_errorbar(mapping = aes(ymin = community_biomass_m - community_biomass_se,
                              ymax = community_biomass_m + community_biomass_se),
                width = 0.1) +
  geom_point(alpha = 0.75) +
  ylab(expression(sqrt(paste("community dry mass (g ",  " m"^"-2", ")") ))) +
  xlab(expression(paste("realised ", alpha, " diversity"))) +
  theme_meta() +
  theme(legend.position = "none")






