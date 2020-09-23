
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: van der Plas (2019) systematic review data reanalysis

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)
library(here)
library(ggpubr)


# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))


### figure 2 analysis

# import the dataset with the spatial extent and grain information filled in

# check whether all relationship numbers were accounted for

# read in the completed data file
spat_comp <- read_delim(here("data/van_der_Plas_2019_spatial_extent_complete.csv"), delim = ",")


### figure 2a

# subset out the bef-relationships that were unknown

fig_2a_raw <- 
  spat_comp %>%
  filter(bef_relationship != "unknown") %>%
  select(`paper number`, Relationship_nr, bef_relationship) %>%
  mutate(direction = if_else( bef_relationship == "Positive", "+",
                              if_else( bef_relationship == "Negative", "-", " ")),
         bef_relationship = if_else( bef_relationship == "Positive", "positive",
                                     if_else( bef_relationship == "Negative", "negative", "neutral")))


# randomly sample single bef slopes from a paper to avoid non-independence
fig_2a_raw %>%
  group_by(`paper number`) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow()

fig_2a_raw$`paper number` %>%
  unique() %>%
  length()

fig_2a_raw$Relationship_nr %>%
  unique() %>%
  length()

# overall there are 246 slopes

# 111 papers and 45 have more than one bef slope


# set the number of replicates
r <- 1000

# set up an output list
fig_2a_ran <- vector("list", length = r)

set.seed(56)
for (i in seq_along(1:r) ) {
  
  x <- 
    fig_2a_raw %>%
    group_by(`paper number`) %>%
    slice_sample(., n = 1) %>%
    ungroup()
  
  fig_2a_ran[[i]] <- 
    x %>%
    group_by(bef_relationship, direction) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(proportion = n/sum(n, na.rm = TRUE))
  
}

fig_2a_ran <- 
  bind_rows(fig_2a_ran, .id = "run")


# get proportions in the observed data

fig_2a_obs <- 
  fig_2a_raw %>%
  group_by(bef_relationship, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(proportion = n/sum(n, na.rm = TRUE))

fig_2a <- 
  ggplot() +
  geom_violin(data = fig_2a_ran,
              mapping = aes(x = bef_relationship, y = proportion),
              alpha = 0.5, bw = 0.005, position = position_dodge(width=0.3),
              fill = "grey") +
  geom_point(data = fig_2a_obs,
             mapping = aes(x = bef_relationship, y = proportion),
             colour = "black", size = 4.5, shape = 21, fill = "white") +
  geom_text(data = fig_2a_obs,
            mapping = aes(x = bef_relationship, y = proportion, label = direction), 
            position = position_dodge(width = 0.3)) +
  scale_y_continuous(limits = (c(0, 0.905)), breaks = seq(from = 0, to = 0.8, by = 0.2)) +
  ylab("proportion of slopes") +
  xlab(NULL) +
  theme_meta() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12))

fig_2a


### figure 2b

# check the spatial extent data

# subset out relationships to include
# create useful variables for plotting
# correct a mistake in the landscape classification

fig_2b_raw <- 
  spat_comp %>%
  filter(include == "yes") %>%
  mutate(bef_relationship = if_else( bef_relationship == "Positive", "positive",
                                     if_else( bef_relationship == "Negative", "negative", "neutral")),
         spatial_extent = if_else(spatial_extent == "landscape ", "landscape", spatial_extent))

fig_2b_raw$`paper number` %>%
  unique() %>%
  length()

fig_2b_raw %>%
  group_by(`paper number`) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow()

fig_2b_raw$Relationship_nr %>%
  unique() %>%
  length()

fig_2b_raw %>%
  group_by(spatial_extent) %>%
  summarise(n = n())



# we obtained information on spatial extent for 231 relationships

# this came from 106 papers and 42 of those had multiple bef slopes associated with it


# does the classification represent spatial extent?
spat_clas <- 
  fig_2b_raw %>%
  mutate(lat_diff = (max_lat - min_lat),
         lon_diff = (max_lon -min_lon))

# check the high landscape spatial extent values
spat_clas %>%
  filter(spatial_extent == "landscape",
         lat_diff > 2) %>%
  View()

# max lat is 40 and not 49
spat_clas %>%
  filter(spatial_extent == "regional",
         lat_diff < -10) %>%
  View()

# lat and lon measurements are switched around
spat_clas %>%
  filter(spatial_extent == "continental",
         lon_diff > 200) %>%
  View()


# correct these mistakes

# for paper 116, replace the max_lat: 49 with max_lat: 40
fig_2b_raw$max_lat[fig_2b_raw$`paper number` == 116] <- 40

# for paper 125, replace max_lat: 7 with max_lat: 53
# for paper 125, replace min_lon: 53 with min_lon: 7
fig_2b_raw$max_lat[fig_2b_raw$`paper number` == 125] <- 53
fig_2b_raw$min_lon[fig_2b_raw$`paper number` == 125] <- 7

# reorder the spatial extent factor so it is ascending
fig_2b_raw$spatial_extent <- 
  factor(fig_2b_raw$spatial_extent, levels = c("landscape", "regional", "continental", "global"))


### figure s2

fig_s2_dat <- 
  fig_2b_raw %>%
  mutate(lat_diff = (max_lat - min_lat),
         lon_diff = (max_lon -min_lon))

fig_s2 <- 
  ggplot(data = rename(fig_s2_dat, `spatial extent` = spatial_extent),
         mapping = aes(x = log(1 + lon_diff), y = log(1 + lat_diff), 
                       colour = `spatial extent`)) +
  geom_jitter(size = 2.5, alpha = 1, shape = 16) +
  scale_colour_viridis_d() +
  ylab("ln - latitude range (DD)") +
  xlab("ln - longitude range (DD)") +
  theme_meta()

ggsave(filename = here("figures/fig_s2.png"), plot = fig_s2, dpi = 500,
       width = 11, height = 7, units = "cm")


# continue with plotting of figure 2b

# add columns for neutral, negative and positive slopes
fig_2b_ana <- 
  fig_2b_raw %>%
  group_by(spatial_extent) %>%
  mutate(pos = if_else(bef_relationship == "positive", 1, 0),
         neu = if_else(bef_relationship == "neutral", 1, 0),
         neg = if_else(bef_relationship == "negative", 1, 0)) %>%
  ungroup() %>%
  select(`paper number`, Relationship_nr, spatial_extent, pos, neu, neg)

fig_2b_ana

# get the observed proportions
fig_2b_obs <- 
  fig_2b_ana %>%
  group_by(spatial_extent) %>%
  summarise_at(vars(c("pos", "neu", "neg")), ~sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_n = pos + neu + neg) %>%
  gather(pos, neu, neg, key = "relationship", value = "slope") %>%
  mutate(slope_proportion = slope/total_n) %>%
  mutate(direction = if_else( relationship == "pos", "+",
                              if_else( relationship == "neg", "-", " ")))



# get the randomised proportions for 1000 different runs

# set the number of replicates
r <- 1000

# set up an output list
fig_2b_ran <- vector("list", length = r)

set.seed(78)
for (i in seq_along(1:r) ) {
  
  x <- 
    fig_2b_ana %>%
    group_by(`paper number`) %>%
    slice_sample(., n = 1) %>%
    ungroup()
  
  fig_2b_ran[[i]] <- 
    x %>%
    group_by(spatial_extent) %>%
    summarise_at(vars(c("pos", "neu", "neg")), ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total_n = pos + neu + neg) %>%
    gather(pos, neu, neg, key = "relationship", value = "slope") %>%
    mutate(slope_proportion = slope/total_n)
  
}

fig_2b_ran <- 
  bind_rows(fig_2b_ran, .id = "run")

fig_2b_ran$slope_proportion %>%
  range()


fig_2b <- 
  ggplot() +
  geom_violin(data = fig_2b_ran,
              mapping = aes(x = spatial_extent, y = slope_proportion, fill = relationship), 
              alpha = 1, bw = 0.025, position = position_dodge(width=0.3)) +
  geom_point(data = fig_2b_obs,
             mapping = aes(x = spatial_extent, y = slope_proportion, group = relationship),
             position = position_dodge(width = 0.3),
             size = 4.5, shape = 21, colour = "black", fill = "white") +
  geom_text(data = fig_2b_obs,
            mapping = aes(x = spatial_extent, y = slope_proportion, group = relationship, label = direction), 
            position = position_dodge(width = 0.3)) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = (c(0, 0.905)), breaks = seq(from = 0, to = 0.8, by = 0.2)) +
  ylab("") +
  xlab(NULL) +
  theme_meta() +
  theme(legend.position = "none")

fig_2b


# export figure 2

fig_2 <- 
  ggarrange(fig_2a, fig_2b, labels = c("(a)", "(b)"),
            font.label = list(size = 10, color = "black", face = "plain", family = NULL),
            widths = c(1, 1.7) )

fig_2

ggsave(filename = here("figures/fig_2.png"), plot = fig_2, dpi = 500,
       width = 16, height = 7, units = "cm")
