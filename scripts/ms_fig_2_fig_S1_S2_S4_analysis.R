
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Fig.2 analysis and plots

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

# set up axis labels
l1 <- expression(paste("Biomass (g ",  " m"^"-2", ")") )
l2 <- c("Biomass")
l3 <- c("Initial diversity")
l4 <- c("Realised diversity")
l5 <- c("Model")

# load the model data
mod_dat <- read_delim(here("analysis_data/stachova_leps_model_data_full.csv"), delim = ",")

# get the final time point in this model
mod_dat_t <- 
  mod_dat %>%
  filter(time == last(time))

# remove the monocultures
mod_dat_t <- 
  mod_dat_t %>%
  filter(species_pool > 1)

# get the first four species pool diversities to match with the Jena data
mod_dat_t <- 
  mod_dat_t %>%
  filter(species_pool %in% c(5, 10, 15, 20))

# convert species pool to levels
levs <- sort(unique(mod_dat_t$species_pool), decreasing = FALSE)

dfx <- 
  mod_dat_t %>%
  mutate(species_pool = factor(as.factor(species_pool), levels = levs ),
         run = factor(as.factor(run), levels = 1:length(unique(mod_dat_t$run) )) )


# fig. s1: plot the realised diversity - initial diversity relationship for all models
cor_mod <-
  dfx %>%
  group_by(run) %>%
  summarise(Pearson_r = cor(as.numeric(species_pool), 
                            realised_richness, 
                            method = "pearson") %>%
              round(., 2)) %>%
  mutate(Pearson_r = paste("r = ", Pearson_r, sep = "")) %>%
ungroup()

fig.s1 <- 
  ggplot(data = dfx,
         mapping = aes(x = as.numeric(species_pool), 
                       y = realised_richness)) +
  geom_jitter(width = 0.1, size = 1.5) +
  geom_smooth(method = "lm", size = 0.5, se = TRUE, colour = "black") +
  ylab(l4) +
  xlab(l3) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~run, scales = "free") +
  theme_meta() +
  geom_text(
    data    = cor_mod,
    mapping = aes(x = -Inf, y = +Inf, label = Pearson_r),
    vjust = +1.4,
    hjust = -0.2,
    size = 3)
fig.s1

ggsave(filename = here("figures/fig_S1.pdf"), 
       plot = fig.s1, width = 17.3, height = 15, units = "cm",
       dpi = 450)


# plot the realised diversity function relationship
ggplot(data = dfx,
       mapping = aes(x = realised_richness, 
                     y = community_biomass,
                     colour = species_pool)) +
  geom_jitter(width = 0.25, size = 1.5) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  ylab(l2) +
  xlab(l4) +
  labs(colour = "Initial diversity") +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~run, scales = "free") +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())

# choose the most representative run and then put the rest in the supplementary

# run 5

# fig. s3: plot the other runs for the supplementary

fig.s4 <- 
  dfx %>%
  filter(run != 5) %>%
  ggplot(data = .,
         mapping = aes(x = realised_richness, 
                       y = community_biomass,
                       colour = species_pool)) +
  geom_jitter(width = 0.25, size = 1.5) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  ylab(l2) +
  xlab(l4) +
  labs(colour = "Initial diversity") +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  facet_wrap(~run, scales = "free", ncol = 4, nrow = 2) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank())

ggsave(filename = here("figures/fig_S4.pdf"), 
       plot = fig.s4, width = 17.3, height = 11, units = "cm",
       dpi = 450)



# load the Jena data
jena_dat <- read_delim(here("analysis_data/jena_analysis_data.csv"), delim = ",")

# combine these datasets into a list
fig.2_dat <- list(filter(mod_dat_t, run == 5), jena_dat)


# plot species pool diversity versus function

# set the y axis labels
ylabs1 <- 
  list(l2, l1 )

fig.2_ab <- vector("list")
for (i in 1:length(fig.2_dat)) {
  
  fig.2_ab[[i]] <- 
    ggplot(data = fig.2_dat[[i]],
         mapping = aes(x = species_pool, y = community_biomass)) +
    geom_jitter(width = 0.5, size = 1.5) +
    geom_smooth(method = "lm", size = 0.5, colour = "black", alpha = 0.3) +
    ylab(ylabs1[[i]]) +
    xlab(l3) +
    theme_meta()
  
}

f2.1 <- 
  ggpubr::ggarrange(fig.2_ab[[1]], fig.2_ab [[2]], labels = c("a", "c"),
            font.label = list(size = 9, color = "black", face = "plain", family = NULL))


# plot realised diversity versus function for each species pool

fig.2_fcd <- vector("list")
for (i in 1:length(fig.2_dat)) {
  
  x <- fig.2_dat[[i]]
  
  levs <- sort(unique(x$species_pool), decreasing = FALSE)
  
  z <- 
    mutate(x, 
           species_pool = factor(as.factor(species_pool), levels = levs )  )
  
  fig.2_fcd[[i]] <- 
    ggplot(data = z,
           mapping = aes(x = realised_richness, 
                         y = community_biomass,
                         colour = species_pool)) +
    geom_jitter(width = 0.25, size = 1.5) +
    geom_smooth(method = "lm", size = 0.75, se = FALSE) +
    ylab(ylabs1[[i]]) +
    xlab(l4) +
    labs(colour = l3) +
    guides(color = guide_legend(override.aes = list(linetype = 0))) +
    scale_colour_viridis_d(option = "C", end = 0.9) +
    theme_meta() +
    theme(legend.position = "bottom",
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.key.size = unit(0.5,"line"))
  
}


f2.2 <- ggpubr::ggarrange(fig.2_fcd[[1]], fig.2_fcd[[2]], labels = c("b", "d"),
            font.label = list(size = 9, color = "black", face = "plain", family = NULL))

# join these two figures together
fig.2 <- ggpubr::ggarrange(f2.1, f2.2, ncol = 1, nrow = 2, labels = NULL,
                  heights = c(1, 1.25))

ggsave(filename = here("figures/fig_2.pdf"), 
       plot = fig.2, width = 11, height = 11, units = "cm",
       dpi = 450)



# fig. s2: plot initial diversity vs. realised diversity
cor_jena <-
  jena_dat %>%
  summarise(Pearson_r = cor(as.numeric(species_pool), 
                           realised_richness,
                           method = "pearson") %>%
              round(., digits = 2)) %>%
  mutate(Pearson_r = paste("r = ", Pearson_r, sep = "")) %>%
  ungroup()

fig.s2 <- 
  ggplot(data = jena_dat,
       mapping = aes(x = as.numeric(species_pool), 
                     y = realised_richness)) +
  geom_jitter(width = 0.1, size = 1.5) +
  geom_smooth(method = "lm", size = 0.5, se = TRUE, colour = "black") +
  ylab(l4) +
  xlab(l3) +
  theme_meta() +
  geom_text(
    data    = cor_jena,
    mapping = aes(x = -Inf, y = +Inf, label = Pearson_r),
    vjust = +1.7,
    hjust = -0.3,
    size = 4)
fig.s2

ggsave(filename = here("figures/fig_S2.pdf"), 
       plot = fig.s2, width = 6, height = 6, units = "cm",
       dpi = 450)


### check exponential shannon to see if it changes the results at all (reviewer 2 comment)
names(jena_dat)

levs <- sort(unique(jena_dat$species_pool), decreasing = FALSE)

fig_rev_2 <- 
  jena_dat %>%
  mutate(species_pool = factor(as.factor(species_pool), levels = levs )  ) %>%
  ggplot(data = .,
       mapping = aes(x = shannon_d, 
                     y = community_biomass,
                     colour = species_pool )) +
  geom_jitter(width = 0.25, size = 1.5) +
  geom_smooth(method = "lm", size = 0.75, se = FALSE) +
  ylab(l1) +
  xlab("Effective number of species (q = 1)") +
  labs(colour = l3) +
  guides(color = guide_legend(override.aes = list(linetype = 0))) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"line"))

ggsave(filename = here("figures/fig_rev_2.pdf"), 
       plot = fig_rev_2, width = 6, height = 6, units = "cm",
       dpi = 450)

cor.test(jena_dat$realised_richness, jena_dat$shannon_d)

### END
