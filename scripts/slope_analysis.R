
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Sample slopes from the model output, Jena data and relevant BIODEPTH sites

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
mod1 <- read_delim(here("data/stachova_leps_model_data.csv"), delim = ",")

# split the model data by model run
mod1 <- split(mod1, mod1$run)


# load in the biodiversity experiment data

# load in the jena data
jen1 <- read_delim(here("data/jena_analysis_data.csv"), delim = ",")

# load in the biodepth data
biod1 <- read_delim(here("data/biodepth_analysis_data_1.csv"), delim = ",")
biod2 <- read_delim(here("data/biodepth_analysis_data_2.csv"), delim = ",")
biod3 <- read_delim(here("data/biodepth_analysis_data_3.csv"), delim = ",")

# combines these datasets into a list
data_col <- 
  list(mod1[[1]], mod1[[2]], mod1[[3]], mod1[[4]],
       jen1,
       biod1, biod2, biod3)

s_plots <- 
  lapply(data_col, function(x) {
  
  x %>%
    group_by(species_pool) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n) %>%
    min(.)
  
})
s_plots <- unlist(s_plots)


# run the function on all the datasets

est_col <- vector("list", length = length(data_col))
for (i in 1:length(data_col)) {
  
  est_col[[i]] <- slope_est_func(data = data_col[[i]], reps = 20, plots = (s_plots[i]-2) )
  
}

# set up the x and y labels
ylab1 <- c("count")
xlab1 <- c("realised diversity-function est.")

# plot the model slopes 
mod_slopes <- bind_rows(est_col[1:4], .id = "model run")

fig_a <- 
  ggplot(data = mod_slopes,
       mapping = aes(x = estimate, fill = `model run`) ) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(ylab1) +
  xlab(xlab1) +
  theme_meta() +
  theme(legend.position = "bottom")


# do the same for the experimental data
exp_slopes <- 
  bind_rows(est_col[5:8], .id = "experiment") %>%
  mutate(experiment = as.factor(experiment))

# change the levels of the factor
levs <- c("Jena", "Bayreuth", "Sheffield", "Umea")
levels(exp_slopes$experiment) <- levs

fig_b <- 
  ggplot(data = exp_slopes,
       mapping = aes(x = estimate, fill = experiment) ) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(ylab1) +
  xlab(xlab1) +
  theme_meta() +
  theme(legend.position = "bottom")

ggarrange(fig_a, fig_b,
          labels = c("a", "b"),
          font.label = list(size = 12, color = "black", face = "plain", family = NULL),
          ncol = 1)



