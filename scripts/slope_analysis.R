
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

# set up the x and y labels for the raw slope plots
ylab1 <- c("count")
xlab1 <- c("realised diversity-function est.")

# set up the x and y labels for slope plots vs. the species pool
ylab2 <- c("realised diversity-function est.")
xlab2 <- c("species pool diversity range")

# plot the model slopes 
mod_slopes <- bind_rows(est_col[1:4], .id = "model run")

f1 <- 
  ggplot(data = mod_slopes,
       mapping = aes(x = estimate, fill = `model run`) ) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(ylab1) +
  xlab(xlab1) +
  theme_meta() +
  theme(legend.position = "none")

 
f2 <- 
  ggplot(data = mod_slopes,
         mapping = aes(x = sp_range, y = estimate, colour = `model run`) ) +
  geom_jitter(width = 1, alpha = 0.25) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(ylab2) +
  xlab(xlab2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())


# do the same for the experimental data
exp_slopes <- 
  bind_rows(est_col[5:8], .id = "experiment") %>%
  mutate(experiment = as.factor(experiment))

# change the levels of the factor
levs <- c("Jena", "Bayreuth", "Sheffield", "Umea")
levels(exp_slopes$experiment) <- levs

f3 <- 
  ggplot(data = exp_slopes,
       mapping = aes(x = estimate, fill = experiment) ) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(ylab1) +
  xlab(xlab1) +
  theme_meta() +
  theme(legend.position = "none")

f4 <- 
  ggplot(data = exp_slopes,
       mapping = aes(x = sp_range, y = estimate, colour = experiment) ) +
  geom_jitter(width = 0.25, alpha = 0.6) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(ylab2) +
  xlab(xlab2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())


f_comb <- 
  ggarrange(f1, f3, f2, f4, ncol = 2, nrow = 2,
            labels = c("a", "b", "c", "d"),
            font.label = list(size = 12, color = "black", face = "plain", family = NULL),
            heights = c(1, 1.25, 1, 1.25))

ggsave(filename = here("figures/fig_2.png"), 
       plot = f_comb, width = 19, height = 19, units = "cm",
       dpi = 450)




