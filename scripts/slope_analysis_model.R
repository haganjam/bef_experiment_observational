
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Sample slopes from the model data

# load relevant libraries
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

# load in the model data
mod_dat <- read_delim(file = here("data/stachova_leps_model_data_full.csv"),
                      delim = ",")
head(mod_dat)

# get the last time point for each replicate from these data
mod_dat_t <- 
  mod_dat %>%
  filter(time == last(time))

# rename the run column to model
mod_dat_t <- 
  mod_dat_t %>%
  rename(model = run)

# set up axis labels
ylab1 <- c("community biomass")
xlab1 <- c("species pool diversity")

# rename the experiments and make it a factor
mod_names <- 
  c(1:max(mod_dat_t$model))

mod_dat_t$model <- as.factor(mod_dat_t$model)
levels(mod_dat_t$model) <- mod_names

# plot the relationship between species pool diversity and biomass
ggplot(data = mod_dat_t,
       mapping = aes(x = species_pool, y = community_biomass, colour = model) ) +
  geom_jitter(width = 0.5, alpha = 0.6) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(ylab1) +
  xlab(xlab1) +
  theme_meta() +
  theme(legend.position = "none")


# run the slopes function on all the datasets

# convert data to a list first
mod_dat_t_l <- split(select(mod_dat_t, -model), mod_dat_t$model )

est_mod <- vector("list", length = length(mod_names))
for (i in 1:length(data_col)) {
  
  est_mod[[i]] <- slope_est_func(data = mod_dat_t_l[[i]], reps = 100, plots = 0.25 )
  
}

unlist(lapply(est_mod, function(x) { min(x$n) }))

# bind the function output into a dataframe
mod_slopes <- 
  bind_rows(est_mod, .id = "model")

mod_slopes$model <- as.factor(mod_slopes$model)
levels(mod_slopes$model) <- mod_names

# set up the x and y labels for the raw slope plots
ylab2 <- c("density")
xlab2 <- c("realised diversity-function est.")

ggplot(data = mod_slopes,
       mapping = aes(x = estimate, fill = model) ) +
  geom_density(alpha = 0.5, colour = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(ylab2) +
  xlab(xlab2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())






