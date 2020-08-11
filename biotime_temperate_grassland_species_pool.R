
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Species pool estimation from individual composition samples using BIOTIME data

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
library(ggpubr)

# create customised plotting theme
theme_meta <- function(base_size = 12, base_family = "") {
  theme(panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill="NA", color="black", size=0.75, linetype="solid"),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.ticks.length = unit(-0.16, "cm"),
        axis.title.x = element_text(colour ="black", size = 12, face = "plain", margin=margin(5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 12, face = "plain", margin=margin(0,5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=12, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=12, face = "plain", margin=margin(0,10,0,0,"pt")),
        axis.ticks.x = element_line(colour = "black", size = 0.4),
        axis.ticks.y = element_line(colour = "black", size = 0.4))
}

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# load the BIOTIME data
bio_dat_raw <- read_csv(here("data/rawdata_temperate_grassland_BIOTIME.csv"),
                        col_types = cols(
                          PLOT = col_character()
                        ))

ad_dat_raw <- 
  bio_dat_raw %>%
  filter(STUDY_ID == 473)

# remove data before 1935 because far fewer quadrats were mapped
ad_dat_raw <- 
  ad_dat_raw %>%
  filter(YEAR > 1934)

# only use quadrats that were mapped each year
plot_list <- 
  split(ad_dat_raw, ad_dat_raw$YEAR) %>%
  lapply(., function(x) {
    
    unique(x$PLOT)
    
  })

plot_list <- Reduce(intersect, plot_list)

ad_dat_raw <- 
  ad_dat_raw %>%
  filter(PLOT %in% plot_list)

# are the number of quadrats consistent through time?
ad_dat_raw %>%
  group_by(YEAR) %>%
  summarise(plots = length(unique(PLOT)))

# calculate temporal gamma diversity
ad_dat_raw %>%
  group_by(PLOT) %>%
  summarise(temp_gamma = length(unique(GENUS_SPECIES)))
  
ad_dat_raw %>%
  group_by(PLOT, YEAR) %>%
  summarise(temp_alpha = length(unique(GENUS_SPECIES))) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  summarise(mean = mean(temp_alpha))

# create a species by site matrix and then replace the NAs with zeros to indicate absence
ad_dat_wide <- 
  ad_dat_raw %>%
  select(PLOT, YEAR, GENUS_SPECIES, ABUNDANCE) %>%
  pivot_wider(id_cols = c("PLOT", "YEAR"),
              names_from = "GENUS_SPECIES",
              values_from = "ABUNDANCE") %>%
  mutate(across(.cols = where(is.numeric), ~if_else(is.na(.), 0, .)))

# split the species by site dataframe into a site and species dataframe separately
site <- 
  ad_dat_wide %>%
  mutate(row_id = 1:nrow(ad_dat_wide)) %>%
  select(row_id, PLOT, YEAR)

spp <- 
  ad_dat_wide %>%
  select(-PLOT, -YEAR)


# randomly sample different years and perform the estimation

# how many individuals years to choose? Here, we just use all of them
year_samps <- length(unique(site$YEAR))

# sample 10 years randomly to do the estimations for
year_s <- sample(x = unique(site$YEAR), size = year_samps, replace = FALSE)

# create an output vector
species_pool_samp <- vector("list", length = year_samps)

for(i in seq_along(1:year_samps)) {
  
  # get row numbers
  rows <- 
    site %>%
    filter(YEAR %in% year_s[i]) %>%
    pull(row_id)
  
  spp_pool_est <- 
    with(site[rows, ], estimateR(spp[rows, ]), PLOT)
  
  spp_pool_list <- vector("list", length = length(rows))
  
  for(k in (seq_along(1:length(rows)))) {
    
    spp_pool_list[[k]] <- spp_pool_est[,k]
    
  }
  
  spp_pool <- 
    as_tibble(do.call(rbind, spp_pool_list)) %>%
    mutate(row_id = rows)
  
  # get data frame with plot id
  plots <- 
    site %>%
    filter(row_id %in% rows) %>%
    select(row_id, PLOT)
  
  spp_pool_est <- 
    left_join(spp_pool, plots, by = "row_id") %>%
    select(-contains("se.")) %>%
    pivot_longer(cols = starts_with("S"),
                 names_to = "estimator",
                 values_to = "estimate")
  
  # add a year identity to this dataframe
  spp_pool_est$year <- year_s[i]
  
  # write this into a list
  species_pool_samp[[i]] <- spp_pool_est
  
}

# bind this list into a dataframe
species_pool_samp <- 
  species_pool_samp %>%
  bind_rows(., .id = "run")


# get temporal gamma diversity
temp_gam <- 
  ad_dat_raw %>%
  group_by(PLOT) %>%
  summarise(temp_gamma = length(unique(GENUS_SPECIES)), .groups = "drop")

# join the temporal gamma data to the estimated data
spp_pool_est <- 
  left_join(species_pool_samp, temp_gam, by = "PLOT")

# how many data points are there for each?
spp_pool_est %>%
  group_by(estimator) %>%
  summarise(n = n())


# what is the error rate?
error_dat <- 
  spp_pool_est  %>%
  mutate(abs_error = abs((temp_gamma-estimate)) ) %>%
  mutate(perc_error = ((abs_error/temp_gamma)*100),
         pos_neg = if_else( (temp_gamma-estimate) > 0, "under", "over"))

# plot error rate between the estimators
ggplot(data = error_dat,
       mapping = aes(x = estimator, y = perc_error)) +
  geom_jitter(mapping = aes(colour = pos_neg), width = 0.2) +
  scale_colour_brewer()
  geom_boxplot(outlier.shape = NA, width = 0.1, fill = "grey") 

# calculate summmary statistics for the error rates
error_dat %>%
  group_by(estimator) %>%
  summarise(m_abs_error = mean(abs_error, na.rm = TRUE),
            se_abs_error = sd(abs_error, na.rm = TRUE)/sqrt(n()),
            m_perc_error = mean(perc_error, na.rm = TRUE),
            se_perc_error = sd(perc_error, na.rm = TRUE)/sqrt(n()),
            .groups = "drop")

spp_pool_est %>%
  filter(is.na(estimate)) %>%
  nrow()

# there are 42 rows with missing data

# calculate the correlation coefficients between the estimate and the actual gamma
corr_out <- 
  spp_pool_est %>%
  nest_by(year, estimator) %>%
  summarise(corr = list(cor(x = data$estimate, y = data$temp_gamma,
                         use = "pairwise.complete.obs") ),
            .groups = "drop") %>%
  unnest(cols = c("corr"))

ggplot(data = corr_out,
       mapping = aes(x = estimator, y = corr)) +
  geom_jitter(width = 0.1) +
  geom_boxplot(outlier.shape = NULL, width = 0.1, fill = "grey") +
  theme_meta()







