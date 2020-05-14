
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Jena data reanalysis

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


# load the Jena community data
jena_comm <- read_delim(here("data/Jena_Community_02-08.csv"), delim = ",")
head(jena_comm)

# remove the first plots that were not sown with any species
jena_comm <- filter(jena_comm, !sowndiv %in% c(0))

# create a vector of species names
sp_names <- names(jena_comm)[51:110]

# seperate the data into spp and site characteristics matrix
site_dat <- select(jena_comm, -sp_names)

sp_dat <- select(jena_comm, sp_names)

# replace NAs in sp_dat with zeros to reflect absence
sp_dat <- 
  sp_dat %>% 
  mutate_all(~replace(., is.na(.), 0))

# check if there are any -9999's in the species data
sp_dat %>% 
    filter_at(vars(sp_names), any_vars(. < 0)) # there are none!

# calculate the number of species in the surveyed 3 x 3 m plots
rowSums(decostand(sp_dat, method = "pa")) # number of species

# calculate the number of species in the surveyed 3 x 3 m plots with more than 1 % cover
mutate_all(sp_dat, ~if_else(. == c(1), 0, .)) %>% 
  decostand(method = "pa") %>%
  rowSums()

# calculate the number of species in the surveyed 3 x 3 m plots with more than 5 % cover
mutate_all(sp_dat, ~if_else(. %in% c(1, 2), 0, .)) %>% 
  decostand(method = "pa") %>%
  rowSums()

# add these observed species richness values to the site_dat dataset
site_dat <- 
  site_dat %>% 
  mutate(observed_species = rowSums(decostand(sp_dat, method = "pa")),
         observed_species_1 = mutate_all(sp_dat, ~if_else(. == c(1), 0, .)) %>% 
           decostand(method = "pa") %>% 
           rowSums(),
         observed_species_5 = mutate_all(sp_dat, ~if_else(. %in% c(1, 2), 0, .)) %>% 
           decostand(method = "pa") %>% 
           rowSums())


# Use the jena_bio dataset to get plot-level (20 x 20 m) target biomass measurements

# load the Jena biomass data
jena_bio <- read_delim(here("data/Jena_Biomass_02-08.csv"), delim = ",")
head(jena_bio)

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !sowndiv %in% c(0))

# examine the data and the variable names
View(jena_bio)
names(jena_bio)

# extract relevant biomass variables
site_bio_dat <- 
  jena_bio %>% 
  select(plotcode, block, plot, subsample, year, month, time, X, Y, sowndiv, target.biomass)

# check for NA's in the dataset
site_bio_dat %>% filter_all(any_vars(is.na(.))) # only NA's in the coordinates

# check for any -9999's in the dataset
site_bio_dat %>% filter_all(any_vars(. < 0)) # yes there is one sub-sample with missing biomass data

# exclude this missing subplot observation from the dataset
site_bio_dat <- 
  site_bio_dat %>% 
  filter(target.biomass >= 0)

# calculate the mean target.biomass for each plot for each time point
site_bio_dat <- 
  site_bio_dat %>% 
  group_by(plotcode, time) %>%
  summarise(target_biomass_m = mean(target.biomass, na.rm = TRUE),
            target_biomass_sd = sd(target.biomass, na.rm = TRUE),
            target_biomass_n = n()) %>%
  ungroup()


# add the biomass measurements to the site_dat data

# first make a season variable in the site_dat dataset
site_dat <- 
  site_dat %>% 
  mutate(season = if_else(month %in% c("May", "June"), "spring", "summer"))

# make a list of relevant variable names
var_names <- c("plotcode", "year", "season", "month", "time", "X", "Y", "sowndiv",
               "numfg", "numgrass", "numsherb", "numtherb", "numleg",
               "observed_species", "observed_species_1", "observed_species_5", 
               "target_biomass_m", "target_biomass_sd", "target_biomass_n")

# join the biomass data to the site data
jena_dat <- 
  full_join(site_dat, site_bio_dat, by = c("plotcode", "time")) %>% 
  select(var_names)

# calculate the total number of species observed across years

df_agg <- 
  bind_cols(jena_dat, sp_dat) %>% 
  group_by(plotcode, season) %>%
  summarise_at(vars(sp_names), ~sum(., na.rm = TRUE)) %>% 
  ungroup()

species_richness <- 
  df_agg %>%
  select(-plotcode, -season) %>% 
  decostand(method = "pa") %>%
  rowSums()

total_spp <- 
  bind_cols(select(df_agg, plotcode, season), 
            total_species = species_richness)
  

# join the total spp calculated to the jena_dat
jena_dat <- full_join(jena_dat, total_spp, by = c("plotcode", "season"))


# examine how sowndiv and target_biomass_m are related

ggplot(data = filter(jena_dat, season == "spring") %>% 
         mutate(year = as.character(year)), 
       mapping = aes(x = sowndiv, y = target_biomass_m, colour = year)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  theme_classic()

ggplot(data = filter(jena_dat, season == "spring") %>% 
         mutate(year = as.character(year)), 
       mapping = aes(x = observed_species_5, y = target_biomass_m, colour = year)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  theme_classic()

ggplot(data = filter(jena_dat, season == "spring") %>% 
         mutate(year = as.character(year),
                sowndiv = as.character(sowndiv)), 
       mapping = aes(x = observed_species_5, y = target_biomass_m, colour = sowndiv)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  facet_wrap(~year, scales = "free") +
  theme_classic()

# how does species pool richness correlate with realised species richness?
ggplot(data = filter(jena_dat, season == "spring") %>% 
         mutate(year = as.character(year)), 
       mapping = aes(x = sowndiv, y = observed_species, colour = year)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0) +
  scale_colour_viridis_d() +
  theme_classic()

ggplot(data = filter(jena_dat, season == "spring") %>% 
         mutate(year = as.character(year)), 
       mapping = aes(x = sowndiv, y = total_species, colour = year)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  theme_classic()


# take the mean across all years in spring
jena_dat_years <- 
  jena_dat %>%
  filter(season == "spring") %>%
  group_by(sowndiv, plotcode) %>%
  summarise_at(vars(c("target_biomass_m", "observed_species", "total_species")),
               list(mean = ~mean(., na.rm = TRUE),
                    sd = ~sd(., na.rm = TRUE))) %>%
  ungroup()

jena_dat_years %>%
  gather(key = "spp", value = "species_number",
         observed_species_mean, sowndiv, total_species_mean) %>%
  mutate(spp = factor(spp, levels = c("sowndiv", "observed_species_mean", "total_species_mean"))) %>%
  ggplot(mapping = aes(x = species_number, y = target_biomass_m_mean, colour = spp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  ylab("community biomass") +
  xlab("species richness") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "none")


# Plot the relationship between species pool diversity and ecosystem function
ggplot(data = jena_dat_years %>% 
         filter(sowndiv < 60, sowndiv > 1),
       mapping = aes(x = sowndiv, y = target_biomass_m_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab("community biomass") +
  xlab("species pool diversity") +
  theme_classic()

# Plot the relationship between species pool diversity and realised species richness
ggplot(data = jena_dat_years %>% 
         filter(sowndiv < 60, sowndiv > 1),
       mapping = aes(x = sowndiv, y = observed_species_m)) +
  geom_point(alpha = 0.6, shape = 16) +
  geom_abline(intercept = c(0), slope = c(1), colour = "black", size = 0.5, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  scale_y_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  ylab("realised diversity") +
  xlab("species pool diversity") +
  theme_classic()



ggplot(data = jena_dat_years %>% filter(sowndiv == 16),
       aes(x = total_species_m, y = target_biomass_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab("community biomass") +
  xlab("realised diversity") +
  theme_classic()

ggplot(data = jena_dat_years %>% filter(sowndiv == 4),
       aes(x = observed_species_m, y = target_biomass_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab("community biomass") +
  xlab("realised diversity") +
  theme_classic()


### Simulate the effect of changing species pool gradients in Jena data

# How many plots are there for different ranges?
jena_dat_years %>% group_by(as.character(sowndiv)) %>%
  summarise(n = n())

# Set up plots without the 60 species plots
jena_dat_samp <- jena_dat_years %>% filter(sowndiv < 60)

# Set-up ranges for the data
sd_vec_asc <- jena_dat_samp %>% 
  pull(sowndiv) %>%
  unique()

sd_vec_des <- sort(sd_vec_asc, decreasing = TRUE)

# Create all possible combinations but when lower > upper and
# remove the 1-1 and 2-2 treatment because there is no possible observed diversity gradient
combs <- crossing(sd_vec_asc, sd_vec_des) %>%
  mutate(remain = if_else(sd_vec_asc <= sd_vec_des, 1, 0)) %>%
  filter(remain == 1) %>% 
  select(-remain) %>%
  filter(!(sd_vec_asc == 1 & sd_vec_des == 1)) %>%
  filter(!(sd_vec_asc == 2 & sd_vec_des == 2))

n_reps <- 100

sp_ranges <- combs[rep(seq_len(nrow(combs)), each = n_reps), ]

n_samples <- 12

samp_dat_ran <- vector("list", length = nrow(sp_ranges))
for (i in seq_along(1:nrow(sp_ranges))) {
  jena_samp <- filter(jena_dat_samp, 
                      sowndiv >= sp_ranges$sd_vec_asc[i],
                      sowndiv <= sp_ranges$sd_vec_des[i])
  samp_dat_ran[[i]] <- jena_samp[sample(x = seq_along(1:nrow(jena_samp)), 
                                        size = n_samples),]
}

# Bind simulated data into a dataframe
sim_dat_all <- bind_rows(samp_dat_ran, .id = "replicate") %>%
  group_by(replicate) %>%
  mutate(start_end_div = paste(min(sowndiv), max(sowndiv), sep = "_"),
         start_div = min(sowndiv),
         end_div = max(sowndiv)) %>%
  mutate(species_pool_range = max(sowndiv) - min(sowndiv),
         observed_div_range = max(observed_species_m) - min(observed_species_m)) %>%
  mutate(species_pool = as.character(species_pool_range)) %>%
  ungroup()

# Fit linear models and get the coefficients when there is some observed gradient
lm_fits <- sim_dat_all %>% group_by(replicate) %>%
  mutate(target_biomass_mean = scale(target_biomass_mean),
         observed_species_m = scale(observed_species_m)) %>% ungroup %>%
  group_by(start_end_div, replicate) %>%
  do(fit_sim = lm(target_biomass_mean ~ observed_species_m, data = .)) %>%
  tidy(fit_sim) %>% ungroup() %>%
  filter(term == "observed_species_m") %>%
  select(-statistic, -p.value)

# Join the lm_fits data to the sim_dats all
sim_dat_all <- full_join(sim_dat_all, lm_fits, 
                         by = c("start_end_div", "replicate"))

# Plot the standardised coefficients against the species pool size
tot_cof <- summary(lm(target_biomass_mean ~ observed_species_m,
                      data = jena_dat_samp %>% 
                        mutate(target_biomass_mean = scale(target_biomass_mean),
                               observed_species_m = scale(observed_species_m))))$coefficients
bind_cols(species_pool_range = 16, estimate = tot_cof[2,1])

x1 <- sim_dat_all %>% 
  select("replicate", "species_pool_range", "start_div", "end_div",
         "observed_div_range", "estimate") %>% 
  distinct() %>%
  ggplot(aes(x = species_pool_range, y = estimate, colour = observed_div_range)) +
  geom_jitter(alpha = 0.5, shape = 16) +
  geom_smooth(method = "lm", colour = "black",
              size = 0.5) +
  #geom_point(data = bind_cols(species_pool_range = 16, estimate = tot_cof[2,1]),
  #aes(x = species_pool_range, y = estimate), colour = "black",
  #shape = 16, size = 2.5) +
  geom_hline(yintercept = 0) +
  xlab("species pool range") +
  ylab("BEF slope") +
  labs(colour = "observed range") +
  scale_colour_viridis_c() +
  theme_classic()

ggsave(x1, 
       filename = "species_pool_size.jpeg",
       dpi = 300, units = "cm",
       width = 12, height = 8)

# Plot the relationship between species pool range and observed diversity range
sim_dat_all %>% 
  select("replicate", "species_pool_range", "observed_div_range", "estimate",) %>% 
  distinct() %>%
  ggplot(aes(x = species_pool_range, y = observed_div_range)) +
  geom_jitter(alpha = 0.5, shape = 16) +
  geom_smooth(method = "lm") +
  theme_classic()




# Example for printing multiple graphs
jena_list <- list(j1, j2)

for (i in seq_along(1:length(jena_list))){
  ggsave(jena_list[[i]], 
         filename = paste(paste("jena_plots_model", (i), sep = "_"), ".jpeg", sep = ""),
         dpi = 300, units = "cm",
         width = 10, height = 8)
}














