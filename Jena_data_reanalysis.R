
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


# plot for intro seminar
intro_plot <- 
  ggplot(data = filter(jena_dat, season == "spring",
                     year == 2003,
                     sowndiv > 1,
                     sowndiv < 60) %>%
         mutate(sowndiv = as.character(sowndiv)), 
       mapping = aes(x = observed_species, y = target_biomass_m, colour = sowndiv)) +
  geom_jitter(width = 0.5, size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("") +
  xlab("") +
  scale_colour_viridis_d() +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 15))
ggsave(filename = here("figures/intro_sem.png"),
       width = 14, height = 12, dpi = 300, units = "cm")


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


# take the values in the last year of the experiment (i.e. 2008)
jena_dat_years <- 
  jena_dat %>%
  filter(season == "spring", year == 2008)

names(jena_dat_years)

jena_dat_years %>%
  gather(key = "spp", value = "species_number",
         observed_species, sowndiv, total_species) %>%
  mutate(spp = factor(spp, levels = c("sowndiv", "observed_species", "total_species"))) %>%
  ggplot(mapping = aes(x = species_number, y = target_biomass_m, colour = spp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  ylab("community biomass") +
  xlab("species richness") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "none")


# plot the relationship between species pool diversity and ecosystem function
fig_s3 <- 
  ggplot(data = jena_dat_years %>% 
         filter(sowndiv < 60, sowndiv > 1),
       mapping = aes(x = sowndiv, y = target_biomass_m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  ylab("community biomass") +
  xlab("initial diversity") +
  theme_classic()

# plot the relationship between species pool diversity and realised species richness
fig_s2 <- 
  ggplot(data = jena_dat_years %>% 
         filter(sowndiv < 60, sowndiv > 1),
       mapping = aes(x = sowndiv, y = observed_species)) +
  geom_abline(intercept = 0, slope = 1, colour = "black", size = 0.1, linetype = "dashed") +
  geom_point(alpha = 0.6, shape = 16) +
  scale_x_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  scale_y_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  ylab("realised diversity") +
  xlab("initial diversity") +
  theme_classic()



# simulate the effect of changing species pool gradients in Jena data

# how many plots are there for different ranges?
jena_dat_years %>% 
  group_by(as.character(sowndiv)) %>%
  summarise(n = n())

# set up plots without the 60 species plots
jena_dat_samp <- 
  jena_dat_years %>% 
  filter(sowndiv < 60)

jena_dat_samp$sowndiv %>%
  unique()

# remove the monocultures
jena_dat_samp <- 
  jena_dat_samp %>%
  filter(sowndiv > 1)

# how many plots are there
jena_dat_samp %>%
  nrow()

jena_dat_samp$sowndiv %>%
  unique()

jena_dat_samp %>%
  group_by(sowndiv) %>%
  summarise(n = n())

# Set-up ranges for the data
spp_ascent <- 
  jena_dat_samp %>% 
  pull(sowndiv) %>%
  unique()

spp_descent <- 
  sort(spp_ascent, decreasing = TRUE)

# create all possible combinations but when lower > upper and
# remove the 1-1 and 2-2 treatment because there is no possible observed diversity gradient
combs <- 
  crossing(spp_ascent, spp_descent) %>%
  mutate(remain = if_else(spp_ascent <= spp_descent, 1, 0)) %>%
  filter(remain == 1) %>% 
  select(-remain) %>%
  filter(!(spp_ascent == 2 & spp_descent == 2))

combs

# set the number of replications to take of each initial diversity range combination
n_reps <- 30

# replicate the initial diversity ranges to match with the replicates
sp_ranges <- combs[ rep(seq(1:nrow(combs)), each = n_reps), ]

# set the number of plots to draw in each run
n_samples <- 12

samp_dat <- vector("list", length = nrow(sp_ranges) )

for (i in seq_along(1:nrow(sp_ranges))) {
  
  jena_samp <- 
    filter(jena_dat_samp, 
           sowndiv >= sp_ranges$spp_ascent[i],
           sowndiv <= sp_ranges$spp_descent[i])
  
  samp_dat[[i]] <- 
    jena_samp[ sample(x = seq_along(1:nrow(jena_samp)), size = n_samples), ]
  
}

samp_dat[[1]]

# collapse the iterated data and generate new modifier variables
sim_out <- 
  samp_dat %>%
  bind_rows(.id = "replicate") %>%
  group_by(replicate) %>%
  mutate(sowndiv_range = paste(min(sowndiv), max(sowndiv), sep = "_"),
         sowndiv_min = min(sowndiv),
         sowndiv_max = max(sowndiv)) %>%
  mutate(initial_div_range = max(sowndiv) - min(sowndiv),
         realised_div_range = max(observed_species) - min(observed_species),
         min_realised = min(observed_species),
         max_realised = max(observed_species)) %>%
  mutate(species_pool = as.character(initial_div_range)) %>%
  ungroup()

# set a minimum realised diversity
min_r <- 1

sim_out <- 
  sim_out %>%
  filter(realised_div_range >= min_r)

# extract linear model slope for the observed diversity gradient
sim_out_slopes <- 
  sim_out %>% 
  group_by(replicate) %>%
  mutate(target_biomass_m = scale(target_biomass_m),
         observed_species = scale(observed_species)) %>% 
  ungroup() %>%
  group_by(sowndiv_range, replicate) %>%
  do(fit_sim = lm(target_biomass_m ~ observed_species, data = .)) %>%
  tidy(fit_sim) %>% 
  ungroup() %>%
  filter(term == "observed_species") %>%
  select(-statistic, -p.value)

# join these slope data to the rest of the data
sim_out <- 
  full_join(sim_out, 
            sim_out_slopes, 
            by = c("sowndiv_range", "replicate"))

# subset relevant variables and remove replicate data points from the join
sim_out <- 
  sim_out %>% 
  select(replicate, initial_div_range, sowndiv_min, sowndiv_max,
         realised_div_range, min_realised, max_realised, estimate) %>% 
  distinct() 

# plot the relationship between initial diversity range and the observed diversity-function slope

ord <- 
  sim_out %>%
  pull(initial_div_range) %>%
  unique() %>%
  sort()
ord

rea <- 
  sim_out %>%
  pull(realised_div_range) %>%
  unique() %>%
  sort()
rea

# check if low initial diversity values are biased
sim_out %>%
  filter(initial_div_range == 0) %>%
  pull(sowndiv_max) %>%
  unique()


fig_2a <- 
  ggplot(data = sim_out,
       mapping = aes(x = initial_div_range, y = estimate, colour = realised_div_range)) +
  geom_jitter(alpha = 0.5, shape = 16, size = 4, width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = ord) +
  xlab("initial diversity range") +
  ylab("realised diversity function slope") +
  labs(colour = "realised diversity range") +
  scale_colour_viridis_c() +
  theme_classic() +
  theme(legend.position="top")

fig_s1 <- 
  ggplot(data = sim_out) +
  geom_jitter(aes(x = initial_div_range, y = sowndiv_min, group = initial_div_range )) +
  geom_boxplot(aes(x = initial_div_range, y = sowndiv_min, group = initial_div_range )) +
  scale_x_continuous(breaks = ord) +
  xlab("initial diversity range") +
  ylab("minimum initial diversity") +
  theme_classic()

# plot insets to show the slope calculation
fig_2i_iii <- 
  samp_dat %>%
  bind_rows(.id = "replicate") %>%
  group_by(replicate) %>%
  mutate(sowndiv_range = paste(min(sowndiv), max(sowndiv), sep = "_"),
         sowndiv_min = min(sowndiv),
         sowndiv_max = max(sowndiv)) %>%
  mutate(initial_div_range = max(sowndiv) - min(sowndiv),
         realised_div_range = max(observed_species) - min(observed_species)) %>%
  mutate(species_pool = as.character(initial_div_range)) %>%
  ungroup() %>%
  filter(realised_div_range >= min_r)

# check the sowndiv values when without any additional diversity range
fig_2i_iii %>%
  filter(initial_div_range == 0) %>%
  pull(sowndiv) %>%
  unique()

# sample 
i_reps <- 
  insets %>%
  filter(initial_div_range %in% c(0, 6, 14) ) %>%
  group_by(initial_div_range) %>%
  sample_n(., size = 1) %>%
  ungroup() %>%
  pull(replicate)

fig_2i_iii <- 
  fig_2i_iii %>%
  filter(replicate %in% i_reps) %>%
  split(., .$replicate) %>%
  lapply(function (x) {
    
    ggplot(data = x,
           mapping = aes(x = observed_species, y = target_biomass_m)) +
      geom_point(size = 4) +
      geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.1) +
      ggtitle(paste0("initial_div_range_", x$initial_div_range[1]) ) +
      ylab("biomass") +
      xlab("realised diversity") +
      theme_classic() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 18))
    
  })

jena_figs <- list(fig_s1, fig_s2, fig_s3, fig_2a, fig_2i_iii[[1]], fig_2i_iii[[2]], fig_2i_iii[[3]])
names(jena_figs) <- c(1:length(jena_figs))

for (i in seq_along(1:length(jena_figs))) {
  
  ggsave(filename = paste0(here("figures"), "/jena_fig_", names(jena_figs)[i], ".png"), 
         plot = jena_figs[[i]], dpi = 300)
}

















