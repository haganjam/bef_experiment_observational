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

# are all 44 plots surveyed in each year?
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

# create a species by site matrix
ad_dat_wide <- 
  ad_dat_raw %>%
  select(PLOT, YEAR, GENUS_SPECIES, ABUNDANCE) %>%
  pivot_wider(id_cols = c("PLOT", "YEAR"),
              names_from = "GENUS_SPECIES",
              values_from = "ABUNDANCE") %>%
  mutate(across(.cols = where(is.numeric), ~if_else(is.na(.), 0, .)))

site <- 
  ad_dat_wide %>%
  mutate(row_id = 1:nrow(ad_dat_wide)) %>%
  select(row_id, PLOT, YEAR)

spp <- 
  ad_dat_wide %>%
  select(-PLOT, -YEAR)


# choose a year randomly to estimate the species pool diversity

# how many years to choose?
n_year <- 1

# sample three years at random from the dataset
year_s <- sample(x = unique(site$YEAR), size = n_year)

# get row numbers
rows <- 
  site %>%
  filter(YEAR %in% year_s) %>%
  pull(row_id)

rows %>% length()

spp_pool_est <- 
  with(site[rows, ], estimateR(spp[rows, ]), PLOT)

spp_pool_list <- vector("list", length = length(rows))
for(i in (seq_along(1:length(rows)))) {
  
  spp_pool_list[[i]] <- spp_pool_est[,i]
  
}

spp_pool <- 
  as_tibble(do.call(rbind, spp_pool_list)) %>%
  mutate(row_id = rows)
spp_pool

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

# get temporal gamma diversity
temp_gam <- 
  ad_dat_raw %>%
  group_by(PLOT) %>%
  summarise(temp_gamma = length(unique(GENUS_SPECIES)), .groups = "drop")

# join the temporal gamma data to the estimated data
spp_pool_est <- 
  left_join(spp_pool_est, temp_gam, by = "PLOT")

# how many data points are there for each?
spp_pool_est %>%
  group_by(estimator) %>%
  summarise(n = n())

# what is the error rate?
error_dat <- 
  spp_pool_est  %>%
  mutate(abs_error = abs((temp_gamma-estimate)) ) %>%
  mutate(perc_error = ((abs_error/temp_gamma)*100) )

error_dat %>%
  group_by(estimator) %>%
  summarise(m_abs_error = mean(abs_error, na.rm = TRUE),
            se_abs_error = sd(abs_error, na.rm = TRUE)/sqrt(n()),
            m_perc_error = mean(perc_error, na.rm = TRUE),
            se_perc_error = sd(perc_error, na.rm = TRUE)/sqrt(n()),
            .groups = "drop")


# fit a linear model to get the r2 values
# fit the linear models and get the r2 values
spp_pool_est %>%
  nest_by(estimator) %>%
  mutate(model = list(lm(estimate ~ temp_gamma, data = data))) %>%
  summarise(rsq = summary(model)$r.squared, .groups = "drop")

ggplot(data = spp_pool_est,
       mapping = aes(x = temp_gamma, y = estimate, colour = estimator)) +
  geom_jitter(size = 2)









