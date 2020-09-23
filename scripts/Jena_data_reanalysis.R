
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Jena data reanalysis

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(viridis)
library(here)
library(vegan)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))


# load the Jena biomass data
jena_bio <- read_delim(here("data/Jena_Biomass_02-08.csv"), delim = ",")
head(jena_bio)
names(jena_bio)

# create a vector of species names
sp_names <- names(jena_bio[, 85:144])

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !(sowndiv %in% c(0)) )

# remove species presence columns
jena_bio <- select(jena_bio, -all_of(paste0("p", sp_names)))

# create a season variable for jena_bio
unique(jena_bio$month)

jena_bio <- 
  jena_bio %>%
  mutate(season = if_else(month %in% c("May", "Jun"), "spring", "summer"))

# subset out the spring data only
jena_bio <- 
  jena_bio %>%
  filter(season == "spring")

# replace the NAs with zeros
jena_bio <- 
  jena_bio %>%
  mutate(across(.cols = all_of(sp_names), ~replace(., is.na(.), 0)))

# remove rows of the data where there are missing values i.e. -9999 values in the sp_names
jena_bio <- 
  jena_bio %>%
  filter_at(all_of(sp_names), all_vars(. >= 0 ) )

# only take the first sub-sample
unique(jena_bio$subsample)

jena_bio <- 
  jena_bio %>%
  filter(subsample %in% c(1, 2, 3))

# select out the relevant columns
jena_bio <- 
  jena_bio %>%
  select(plotcode, season, time, subsample, sowndiv, target.biomass, all_of(sp_names))

# for each species and for target biomass, take the average value
jena_bio <- 
  jena_bio %>%
  group_by(plotcode, time) %>%
  summarise(across(.cols = all_of(c("sowndiv", "target.biomass", sp_names)), ~mean(., na.rm = TRUE) ), .groups = "drop")

# extract site variables
site_bio <- 
  jena_bio %>%
  select(-all_of(sp_names))

# check for NA's in the dataset
lapply(site_bio, function(x) { sum(if_else(is.na(x), 1, 0)) })

# check for any -9999's in the dataset
lapply(site_bio, function(x) { sum(if_else(x < 0, 1, 0)) })

# extract out the individual species' biomass
sp_bio <- 
  jena_bio %>%
  select(all_of(sp_names))

# check for NA's in the dataset
lapply(sp_bio, function(x) { sum(if_else(is.na(x), 1, 0)) })

# check for any -9999's in the dataset
lapply(sp_bio, function(x) { sum(if_else(x < 0, 1, 0), na.rm = TRUE) })

# check if biomass calculated from individual species correlates with target.biomass reported
cor(rowSums(sp_bio), site_bio$target.biomass)

# add species-specific biomass and observed species richness to the site_dat data
site_bio <- 
  site_bio %>%
  mutate(comm_biomass = rowSums(sp_bio),
         observed_sr = rowSums(decostand(sp_bio, method = "pa")) )

# remove the 60 species treatment and the monocultures as it is not really relevant
site_bio <- 
  site_bio %>%
  filter(sowndiv > 1, sowndiv < 60)


# Box 1 analysis
ggplot(data = filter(site_bio, time == max(time)),
       mapping = aes(x = sowndiv, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = filter(site_bio, time == max(time)),
       mapping = aes(x = observed_sr, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = filter(site_bio, time == max(time)),
       mapping = aes(x = observed_sr, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~as.character(sowndiv), scales = "free") +
  theme_bw()


# Random sampling of data at final time-point
ran_bio <- 
  filter(site_bio, time == max(time))

perm_grid <- 
  expand.grid(sort(unique(ran_bio$sowndiv)), 
              sort(unique(ran_bio$sowndiv), decreasing = TRUE) )

perm_grid <- 
  perm_grid %>%
  filter(Var1 <= Var2)

# set the number of replicates
reps <- 30

# set the number of plots
plots <- 12

est_out <- vector("list", length = nrow(perm_grid))

for (s in c(1:nrow(perm_grid)) ) {
  
  x <- 
    ran_bio %>%
    filter(sowndiv >= perm_grid$Var1[s],
           sowndiv <= perm_grid$Var2[s])
  
  
  rep_out <- vector("list", length = reps)
  
  for (i in c(1:reps) ) {
    
    y <- x[sample(1:nrow(x), size = plots), ]
    
    y <- 
      y %>%
      mutate(comm_biomass = as.numeric(scale(comm_biomass, center = TRUE, scale = TRUE)),
             observed_sr = as.numeric(scale(observed_sr, center = TRUE, scale = TRUE)))
    
    lmx <- lm(comm_biomass ~ observed_sr, data = y)
    
    z <- broom::tidy(lmx)
    
    z <-
      z %>%
      dplyr::mutate(r2 = summary(lmx)$r.squared) %>%
      dplyr::mutate(observed_sr_min = min(x$observed_sr),
             observed_sr_max = max(x$observed_sr)) %>%
      dplyr::mutate(observed_sr_range = (observed_sr_max - observed_sr_min) )
    
    rep_out[[i]] <- z
    
  }
  
  est_out[[s]] <- 
    dplyr::bind_rows(rep_out, .id = "rep") %>%
    dplyr::mutate(sowndiv_low = perm_grid$Var1[s],
                  sowndiv_upp = perm_grid$Var2[s]) %>%
    dplyr::mutate(sowndiv_range = (sowndiv_upp - sowndiv_low) )
  
}

# bind this output into a dataframe
ran_est_out <- 
  bind_rows(est_out, .id = "sowndiv_comb")
nrow(ran_est_out)

# remove duplicate rows
ran_est_out <- 
  distinct(ran_est_out)
nrow(ran_est_out)

# get only the species richness slope and remove any where realised diversity range was below 1
est_all <- 
  ran_est_out %>%
  filter(term == "observed_sr", observed_sr_range > 1)

ggplot(data = est_all,
       mapping = aes(x = estimate)) +
  geom_histogram(alpha = 0.3, colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1.25) +
  ylab("count") +
  xlab("realised diversity-function estimate") +
  theme_meta()






















