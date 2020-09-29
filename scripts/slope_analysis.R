
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

# load in the biodiversity experiment data

# load in the jena data
jen1 <- read_delim(here("data/jena_analysis_data.csv"), delim = ",")

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
data_col <- 
  list(jen1, 
       biod1, biod2, biod3, biod4,
       biod5, biod6, biod7, biod8)

unlist(lapply(data_col, function(x) { nrow(x) }))


# set up axis labels
ylab1 <- expression(paste("community biomass (g ",  " m"^"-2", ")") )
xlab1 <- c("species pool diversity")

# output a dataframe
raw_exp_dat <- bind_rows(data_col, .id = "experiment")

# rename the experiments and make it a factor
site_names <- 
  c("Jena", "Germany", "Portugal", "Switzerland", "Greece",
  "Ireland", "Sweden", "Sheffield", "Silwood")

raw_exp_dat$experiment <- as.factor(raw_exp_dat$experiment)
levels(raw_exp_dat$experiment) <- site_names

# plot the relationship between species pool diversity and biomass
ggplot(data = raw_exp_dat,
         mapping = aes(x = species_pool, y = community_biomass, colour = experiment) ) +
  geom_jitter(width = 0.25, alpha = 0.6) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(ylab1) +
  xlab(xlab1) +
  theme_meta() +
  theme(legend.position = "none")

lapply(data_col, function(x) { 
  
  x <- lm(community_biomass ~ species_pool, data = x)
  
  broom::glance(x)
  
  })


# run the function on all the datasets
est_col <- vector("list", length = length(data_col))
for (i in 1:length(data_col)) {
  
  est_col[[i]] <- slope_est_func(data = data_col[[i]], reps = 100, plots = 0.66 )
  
}

# check how many data points are used in these analyses
unlist(lapply(est_col, function(x) { min(x$n) }))


# bind the function output into a dataframe
exp_slopes <- 
  bind_rows(est_col, .id = "experiment")

exp_slopes$exp. <- as.factor(exp_slopes$experiment)
levels(exp_slopes$exp.) <- site_names

# set up the x and y labels for the raw slope plots
ylab2 <- c("density")
xlab2 <- c("realised diversity-function est.")

ggplot(data = exp_slopes,
       mapping = aes(x = estimate, fill = exp.) ) +
  geom_density(alpha = 0.5, colour = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(ylab2) +
  xlab(xlab2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())

# f_comb <- 
  # ggarrange(f1, f2, ncol = 1, nrow = 2,
            # labels = c("a", "b"),
            # font.label = list(size = 12, color = "black", face = "plain", family = NULL),
            # heights = c(1, 1.25, 1, 1.25))

#ggsave(filename = here("figures/fig_2.png"), 
       # plot = f_comb, width = 19, height = 19, units = "cm",
       # dpi = 450)




