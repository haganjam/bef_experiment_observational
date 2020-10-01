
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Randomly sample time points from the model (like the kelp data)

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# set up axis labels
l1 <- expression(sqrt(paste("community dry mass (g ",  " m"^"-2", ")") ))
l2 <- c("community biomass")
l3 <- expression(paste(alpha, " species pool", " diversity"))
l4 <- expression(paste("realised ", alpha, " diversity-", "function", " est."))
l5 <- c("model")


### analyse the temporal model data

# load in the model data
mod_time <- read_delim(file = here("data/stachova_leps_model_data_full.csv"), delim = ",")
head(mod_time)

# get rid of the monoculture data
mod_ana <- 
  mod_time %>%
  filter(species_pool > 1)

mod_ana$species_pool %>% unique()

# provide a id column for each plot for each time point
mod_ana <- 
  mod_ana %>%
  group_by(run, time) %>%
  mutate(id = 1:n()) %>%
  ungroup()

maxid <- max(mod_ana$id)

# set up number of time points and number of sites to match with the kelp data
tp <- length(unique(mod_ana$time))
ts <- 17

set.seed(5467)
t_samp <- sample(x = (1:tp), size = ts)

# set up the sites to sample
sites <- 9

set.seed(54997)
s_samp <- sample(x = 1:maxid, size = sites)

# sample a specific set of time points and sites
mod_time_s <- 
  mod_ana %>%
  filter(time %in% t_samp ) %>%
  filter(id %in% s_samp )

# get slopes between realised richness and biomass at each time point in each model run

t_slopes <- 
  lapply(split(mod_time_s, mod_time_s$run), 
       function(df) { 
         
         z <- 
           lapply(split(df, df$time), function(x) {
             
             y <- 
               x %>%
               mutate(realised_richness = as.numeric(scale(realised_richness)),
                      community_biomass = as.numeric(scale(community_biomass)))
             
             lm_y <- lm(community_biomass ~ realised_richness, data = y)
             
             lm_y$coef[2]
             
           }
           )
         
         unlist(z)
         
         })

t_slopes <- 
  as.data.frame(do.call("cbind", t_slopes))

# pull this into a long dataframe
row.names(t_slopes) <- NULL
t_slopes$year <- c(1:nrow(t_slopes))

t_slopes <- 
  t_slopes %>%
  pivot_longer(cols = unique(mod_time_s$run),
               names_to = "run",
               values_to = "est") %>%
  rename(model = run)

g1 <- 
  ggplot(data = t_slopes,
         mapping = aes(x = model, y = est, colour = model) ) +
  geom_jitter(width = 0.1, alpha = 0.75) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  ylab(l4) +
  xlab(l5) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")

g2 <- 
  mod_time_s %>%
  group_by(run, id) %>%
  summarise(species_pool = mean(species_pool, na.rm = TRUE),
            community_biomass_m = mean(community_biomass, na.rm = TRUE),
            community_biomass_se = sd(community_biomass, na.rm = TRUE)/sqrt(n()), .groups = "drop") %>%
  mutate(model = as.character(run)) %>%
  ggplot(data = .,
         mapping = aes(x = species_pool, y = community_biomass_m, colour = model) ) +
  geom_point(alpha = 0.75) +
  ylab(l2) +
  xlab(l3) +
  geom_smooth(se = FALSE, method = "lm", size = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")

# get a legend for this plot
g2l <- 
  mod_time_s %>%
  group_by(run, id) %>%
  summarise(species_pool = mean(species_pool, na.rm = TRUE),
            community_biomass_m = mean(community_biomass, na.rm = TRUE),
            community_biomass_se = sd(community_biomass, na.rm = TRUE)/sqrt(n()), .groups = "drop") %>%
  mutate(model = as.character(run)) %>%
  ggplot(data = .,
         mapping = aes(x = species_pool, y = community_biomass_m, colour = model) ) +
  geom_jitter(width = 0.5, alpha = 0.75) +
  geom_smooth(se = FALSE, method = "lm", size = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  theme(legend.position = "bottom",
      legend.key.size = unit(0.75,"line"),
      legend.text = element_text(size = 8),
      legend.key = element_blank())

g2l <- gglegend(g2l)


### analyse the temporal kelp data

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



g3 <- 
  kelp_alpha %>%
  mutate(community_biomass = sqrt(community_biomass),
         YEAR = as.character(YEAR)) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = sqrt(community_biomass), colour = YEAR) ) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE, method = "lm", size = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(l1) +
  xlab(l4) +
  theme_meta() +
  theme(legend.position = "none")

# get the legend from this plot
g3l <- 
  kelp_alpha %>%
  mutate(community_biomass = sqrt(community_biomass),
         year = as.character(YEAR)) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, y = sqrt(community_biomass), colour = year) ) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE, method = "lm", size = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.75,"line"),
        legend.text = element_text(size = 8),
        legend.key = element_blank())

g3l <- gglegend(g3l)


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


g4 <- 
  ggplot(data = kelp_gam,
       mapping = aes(x = species_pool, 
                     y = community_biomass_m) ) +
  geom_smooth(alpha = 0.1, se = TRUE, method = "lm", size = 0.5, colour = "black") +
  geom_errorbar(mapping = aes(ymin = community_biomass_m - community_biomass_se,
                              ymax = community_biomass_m + community_biomass_se),
                width = 0.1) +
  geom_point(alpha = 0.75) +
  ylab(l1) +
  xlab(l3) +
  theme_meta() +
  theme(legend.position = "none")


# join all these plots together

# combine these plots
f1 <- 
  ggpubr::ggarrange(g2, g1, g2l, ncol = 1, nrow = 3,
                    labels = c("a", "b", ""),
                    heights = c(1, 1, 0.2),
                    font.label = list(size = 12, color = "black", face = "plain", family = NULL))

f2 <- 
  ggpubr::ggarrange(g4, g3, g3l, ncol = 1, nrow = 3,
                    labels = c("c", "d", ""),
                    heights = c(1, 1, 0.2),
                    font.label = list(size = 12, color = "black", face = "plain", family = NULL))

f_comb <- 
  ggpubr::ggarrange(f1, f2,
                    ncol = 2, nrow = 1)


ggsave(filename = here("figures/fig_4.png"), 
       plot = f_comb, width = 15, height = 17, units = "cm",
       dpi = 450)



