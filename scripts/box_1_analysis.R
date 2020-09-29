
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Box 1 analysis (model and Jena data)

# load relevant libraries generally
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))
source(here("scripts/realised_div_slope_function.R"))

# load the model data
mod_dat <- read_delim(here("data/stachova_leps_model_data_full.csv"), delim = ",")

# get the final time point in this model
mod_dat_t <- 
  mod_dat %>%
  filter(time == last(time))

# remove the monocultures
mod_dat_t <- 
  mod_dat_t %>%
  filter(species_pool > 1)


# plot the realised diversity function relationship for each model
levs <- sort(unique(mod_dat_t$species_pool), decreasing = FALSE)

dfx <- 
  mod_dat_t %>%
  mutate(species_pool = factor(as.factor(species_pool), levels = levs ),
         run = factor(as.factor(run), levels = 1:length(unique(mod_dat_t$run) )) )

ggplot(data = dfx,
       mapping = aes(x = realised_richness, 
                     y = community_biomass,
                     colour = species_pool)) +
  geom_jitter(width = 0.25, size = 1.5) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  ylab("community biomass") +
  xlab("realised diversity") +
  labs(colour = "initial diversity") +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~run, scales = "free") +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())

# choose the most representative run and then put the rest in the supplementary


# load the Jena data
jena_dat <- read_delim(here("data/jena_analysis_data.csv"), delim = ",")

# combine these datasets into a list
box.1_dat <- list(filter(mod_dat_t, run == 3),
                  jena_dat)


# plot species pool diversity versus function

# set the y axis labels
ylabs1 <- 
  list(c("community biomass"), 
       expression(paste("community biomass (g ",  " m"^"-2", ")") ) )

box.1_fab <- vector("list")
for (i in 1:length(box.1_dat)) {
  
  box.1_fab[[i]] <- 
    ggplot(data = box.1_dat[[i]],
         mapping = aes(x = species_pool, y = community_biomass)) +
    geom_jitter(width = 0.5, size = 1.5) +
    geom_smooth(method = "lm", size = 0.5, colour = "black", alpha = 0.3) +
    ylab(ylabs1[[i]]) +
    xlab("initial diversity") +
    theme_meta()
  
}

f1 <- 
  ggarrange(box.1_fab[[1]], box.1_fab[[2]], labels = c("a", "b"),
            font.label = list(size = 12, color = "black", face = "plain", family = NULL))


# plot realised diversity versus function for each species pool

box.1_fcd <- vector("list")
for (i in 1:length(box.1_dat)) {
  
  x <- box.1_dat[[i]]
  
  levs <- sort(unique(x$species_pool), decreasing = FALSE)
  
  z <- 
    mutate(x, 
           species_pool = factor(as.factor(species_pool), levels = levs )  )
  
  box.1_fcd[[i]] <- 
    ggplot(data = z,
           mapping = aes(x = realised_richness, 
                         y = community_biomass,
                         colour = species_pool)) +
    geom_jitter(width = 0.25, size = 1.5) +
    geom_smooth(method = "lm", size = 0.75, se = FALSE) +
    ylab(ylabs1[[i]]) +
    xlab("realised diversity") +
    labs(colour = "initial diversity") +
    scale_colour_viridis_d(option = "C", end = 0.9) +
    theme_meta() +
    theme(legend.position = "bottom",
          legend.key = element_blank())
  
}


f2 <- ggarrange(box.1_fcd[[1]], box.1_fcd[[2]], labels = c("c", "d"),
            font.label = list(size = 12, color = "black", face = "plain", family = NULL))

# join these two figures together
fig1 <- ggarrange(f1, f2, ncol = 1, nrow = 2, labels = NULL,
                  heights = c(1, 1.25))

ggsave(filename = here("figures/box1_fig1.png"), 
       plot = fig1, width = 19, height = 19, units = "cm",
       dpi = 450)



# do this for the BIODEPTH data as well to show that it is consistent

# load in the biodepth data
biod1 <- read_delim(here("data/biodepth_analysis_data_1.csv"), delim = ",")
biod2 <- read_delim(here("data/biodepth_analysis_data_2.csv"), delim = ",")
biod3 <- read_delim(here("data/biodepth_analysis_data_3.csv"), delim = ",")
biod4 <- read_delim(here("data/biodepth_analysis_data_4.csv"), delim = ",")
biod5 <- read_delim(here("data/biodepth_analysis_data_5.csv"), delim = ",")
biod6 <- read_delim(here("data/biodepth_analysis_data_6.csv"), delim = ",")
biod7 <- read_delim(here("data/biodepth_analysis_data_7.csv"), delim = ",")
biod8 <- read_delim(here("data/biodepth_analysis_data_8.csv"), delim = ",")

# combines these datasets into a list
data_bio <- 
  list(biod1, biod2, biod3, biod4,
       biod5, biod6, biod7, biod8)

# bind this into a single dataframe
data_bio <- bind_rows(data_bio, .id = "exp.")

# plot the realised diversity function relationship for each experiment in BIODEPTH
bio_names <- 
  c("Germany", "Portugal", "Switzerland", "Greece",
    "Ireland", "Sweden", "Sheffield", "Silwood")

data_bio$exp. <- as.factor(data_bio$exp.)
levels(data_bio$exp.) <- bio_names

data_bio <- 
  data_bio %>%
  mutate(species_pool = as.character(species_pool))

ggplot(data = data_bio,
       mapping = aes(x = realised_richness, 
                     y = community_biomass,
                     colour = species_pool)) +
  geom_jitter(width = 0.1, size = 1.5) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  ylab("community biomass") +
  xlab("realised diversity") +
  labs(colour = "initial diversity") +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~exp., scales = "free") +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())






