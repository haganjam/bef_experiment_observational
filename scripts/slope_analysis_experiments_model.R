
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Sample slopes from Jena data and relevant BIODEPTH sites and from the model

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

# set up axis labels
l1 <- expression(paste("community biomass (g ",  " m"^"-2", ")") )
l2 <- c("community biomass")
l3 <- expression(paste(alpha, " species pool", " diversity"))
l4 <- c("density")
l5 <- expression(paste("realised ", alpha, " diversity-", "function", " est."))


### biodiversity experiment data

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

# output a dataframe
raw_exp_dat <- bind_rows(data_col, .id = "experiment")

neg_mods <- 
  lapply(data_col, function(x) { 
  
  x <- lm(community_biomass ~ species_pool, data = x)
  
  broom::glance(x)
  
}) %>%
  bind_rows(., .id = "model") %>%
  filter(p.value > 0.05) %>%
  pull(model)

raw_exp_dat <- 
  raw_exp_dat %>%
  mutate(pos_slopes = if_else(experiment %in% neg_mods, "neg", "pos"))

# rename the experiments and make it a factor
site_names <- 
  c("Jena", "DEU", "PRT", "CHE", "GRC",
    "IRL.", "SWE", "Shef.", "Silw.")

raw_exp_dat$experiment <- as.factor(raw_exp_dat$experiment)
levels(raw_exp_dat$experiment) <- site_names


# plot the relationship between species pool diversity and biomass

f1 <- 
  lapply(split(raw_exp_dat, raw_exp_dat$pos_slopes),
       function(x) {
         
         ggplot(data = x,
                mapping = aes(x = species_pool, y = community_biomass, colour = experiment) ) +
           geom_jitter(width = 0.25, alpha = 0.6) +
           geom_smooth(method = "lm", size = 0.75, se = FALSE) +
           scale_colour_viridis_d(option = "C", end = 0.9) +
           ylab(l1) +
           xlab(l3) +
           theme_meta() +
           theme(legend.position = "none",
                 axis.title.x = element_text(size = 9),
                       axis.title.y = element_text(size = 9),
                       axis.text.x = element_text(size = 8),
                       axis.text.y = element_text(size = 8))
         
       })


# run the function on all the datasets
est_col <- vector("list", length = length(data_col))
for (i in 1:length(data_col)) {
  
  est_col[[i]] <- slope_est_func(data = data_col[[i]], reps = 150, plots = 0.5)
  
}

# check how many data points are used in these analyses
unlist(lapply(est_col, function(x) { min(x$n) }))

# bind the function output into a dataframe
exp_slopes <- 
  bind_rows(est_col, .id = "experiment")

exp_slopes <- 
  exp_slopes %>%
  mutate(pos_slopes = if_else(experiment %in% neg_mods, "neg", "pos"))

exp_slopes$exp. <- as.factor(exp_slopes$experiment)
levels(exp_slopes$exp.) <- site_names

f2 <- 
  lapply(split(exp_slopes, exp_slopes$pos_slopes),
       function(x) {
         
         ggplot(data = x,
                mapping = aes(x = estimate, fill = exp.) ) +
           geom_density(alpha = 0.5, colour = "white") +
           geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
           scale_fill_viridis_d(option = "C", end = 0.9) +
           # scale_y_continuous(breaks = c(0.0, 0.5, 1.0, 1.5, 2, 2.5)) +
           ylab(l4) +
           xlab(l5) +
           theme_meta() +
           theme(legend.position = "none",
                 legend.key = element_blank(),
                 axis.title.x = element_text(size = 8.5),
                       axis.title.y = element_text(size = 9, margin=margin(0,15,0,0,"pt")),
                       axis.text.x = element_text(size = 8),
                       axis.text.y = element_text(size = 8))
         
       })

# get the legends for these plots separately

f2l <- 
  lapply(split(exp_slopes, exp_slopes$pos_slopes),
         function(x) {
           
           y <- 
             ggplot(data = x,
                  mapping = aes(x = estimate, fill = exp.) ) +
             geom_density(alpha = 0.5, colour = "white") +
             scale_fill_viridis_d(option = "C", end = 0.9) +
             guides(fill = guide_legend(nrow=2,byrow=TRUE)) +
             theme_meta() +
             theme(legend.position = "bottom",
                   legend.key = element_blank(),
                   legend.title = element_blank(),
                   legend.key.size = unit(0.6,"line"),
                   legend.text = element_text(size = 7))
           
           gglegend(y)
           
         })


### model data

# load in the model data
mod_dat <- read_delim(file = here("data/stachova_leps_model_data_full.csv"),
                      delim = ",")
head(mod_dat)

# get the last time point for each replicate from these data
mod_dat_t <- 
  mod_dat %>%
  filter(time == last(time))

mod_dat_t <- 
  mod_dat_t %>%
  filter(species_pool > 1)

# rename the run column to model
mod_dat_t <- 
  mod_dat_t %>%
  rename(model = run)

# rename the experiments and make it a factor
mod_names <- 
  c(1:max(mod_dat_t$model))

mod_dat_t$model <- as.factor(mod_dat_t$model)
levels(mod_dat_t$model) <- mod_names

# plot the relationship between species pool diversity and biomass
m1 <- 
  ggplot(data = mod_dat_t,
       mapping = aes(x = species_pool, y = community_biomass, colour = model) ) +
  geom_jitter(width = 0.5, alpha = 0.6) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab(l2) +
  xlab(l3) +
  theme_meta() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


# run the slopes function on all the datasets

# convert data to a list first
mod_dat_t_l <- split(select(mod_dat_t, -model), mod_dat_t$model )

est_mod <- vector("list", length = length(mod_names))
for (i in 1:length(mod_dat_t_l )) {
  
  est_mod[[i]] <- slope_est_func(data = mod_dat_t_l[[i]], reps = 150, plots = 0.5)
  
}

unlist(lapply(est_mod, function(x) { min(x$n) }))

# bind the function output into a dataframe
mod_slopes <- 
  bind_rows(est_mod, .id = "model")

mod_slopes$model <- as.factor(mod_slopes$model)
levels(mod_slopes$model) <- mod_names

m2x <- 
  ggplot(data = mod_slopes,
       mapping = aes(x = estimate, fill = model) ) +
  geom_density(alpha = 0.5, colour = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab(l4) +
  xlab(l5) +
  theme_meta()

m2 <- 
  m2x +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 9, margin=margin(0,15,0,0,"pt")),
        axis.title.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8))

# get the legend
m2l <- 
  m2x +
  theme(legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.6,"line"),
        legend.text = element_text(size = 7))

m2l <- gglegend(m2l)

# combine these plots
g1 <- 
  ggpubr::ggarrange(m1, f1[[1]], f1[[2]], ncol = 3, nrow = 1,
                  widths = c(1, 1.05, 1.05),
                  labels = c("a", "c", "e"),
                  font.label = list(size = 12, color = "black", face = "plain", family = NULL))

g2 <- 
  ggpubr::ggarrange(m2, f2[[1]], f2[[2]], 
                  ncol = 3, nrow = 1,
                  heights = c(1, 0.2),
                  widths = c(1, 1.05, 1.05),
                  labels = c("b", "d", "f"),
                  font.label = list(size = 12, color = "black", face = "plain", family = NULL))

g3 <- 
  ggpubr::ggarrange(m2l, f2l[[1]], f2l[[2]], 
                    ncol = 3, nrow = 1,
                    labels = NULL,
                    align = "v")

g_comb <- 
  ggpubr::ggarrange(g1, g2, g3,
                  ncol = 1, nrow = 3, 
                  heights = c(1, 1, 0.2))


ggsave(filename = here("figures/fig_2.pdf"), 
       plot = g_comb, width = 17.3, height = 15, units = "cm")





### additional plots

# plot the relationship between species pool range and slope

exp_slopes %>%
  group_by(exp.) %>%
  mutate(samp_pool = if_else(sp_range == min(sp_range), "< median", 
                             if_else(sp_range == max(sp_range), "full gradient", "> median"))) %>%
  ungroup() %>%
  ggplot(data = .,
       mapping = aes(x = estimate, fill = exp. ) ) +
  geom_density(alpha = 0.5, colour = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~samp_pool) +
  # ylab(ylab2) +
  # xlab(xlab2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())

mod_slopes %>%
  group_by(model) %>%
  mutate(samp_pool = if_else(sp_range == min(sp_range), "< median", 
                             if_else(sp_range == max(sp_range), "full gradient", "> median"))) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = estimate, fill = model ) ) +
  geom_density(alpha = 0.5, colour = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~samp_pool) +
  # ylab(ylab2) +
  # xlab(xlab2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())







