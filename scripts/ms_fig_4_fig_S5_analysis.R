
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Fig. 4 and Fig. S4 analysis (van der Plas 2019, systematic review data)

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


# fig. 4

# import the dataset with the spatial extent and grain information filled in
# check whether all relationship numbers were accounted for

# read in the completed data file
vd_dat <- read_delim(here("raw_data/van_der_Plas_2019_spatial_extent_complete.csv"), delim = ",")


# figure 4a

# subset out the bef-relationships that were unknown
# rename bef_relationship entries into signs for plotting

vd_raw <- 
  vd_dat %>%
  filter(bef_relationship != "unknown") %>%
  select(`paper number`, Relationship_nr, bef_relationship) %>%
  mutate(direction = if_else( bef_relationship == "Positive", "+",
                              if_else( bef_relationship == "Negative", "-", " ")))

# how many unique papers do the bef relationships come from?
vd_raw$`paper number` %>%
  unique() %>%
  length()

# how many papers report multiple bef relationships?
vd_raw %>%
  group_by(`paper number`) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow()

# how many bef relationships are there in total?
vd_raw$Relationship_nr %>%
  unique() %>%
  length()

# overall there are 246 slopes
# 111 papers and 45 have more than one bef slope


# randomly sample single bef slopes from a paper to avoid non-independence

# set the number of replicates
r <- 1000

# set up an output list
vd_ran <- vector("list", length = r)

set.seed(56)
for (i in seq_along(1:r) ) {
  
  x <- 
    vd_raw %>%
    group_by(`paper number`) %>%
    slice_sample(., n = 1) %>%
    ungroup()
  
  vd_ran[[i]] <- 
    x %>%
    group_by(bef_relationship, direction) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(proportion = n/sum(n, na.rm = TRUE))
  
}

vd_ran <- 
  bind_rows(vd_ran, .id = "run")


# get proportions in the observed data
vd_obs <- 
  vd_raw %>%
  group_by(bef_relationship, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(proportion = n/sum(n, na.rm = TRUE))

# get range of proportions in the randomised data
vd_range <- 
  vd_ran %>%
  group_by(bef_relationship, direction) %>%
  summarise(min_prop = min(proportion),
            max_prop = max(proportion), .groups = "drop")

# bind the range information to the observed data
sim_dat <- 
  full_join(vd_obs,
            select(vd_range, bef_relationship, min_prop, max_prop),
            by = "bef_relationship")


fig.4a <- 
  ggplot(data = sim_dat,
         mapping = aes(x = bef_relationship, y = proportion, fill = bef_relationship)) +
  geom_bar(stat = "identity", width = 0.2, colour = "white") +
  geom_errorbar(mapping = aes(ymin = min_prop,
                              ymax = max_prop),
                width = 0.1) +
  scale_y_continuous(limits = (c(0, 0.905)), breaks = seq(from = 0, to = 0.8, by = 0.2)) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab("Proportion of slopes") +
  xlab(NULL) +
  theme_meta() +
  theme(legend.position = "none")

# check fig. 4a
fig.4a


# figure 4b and figure s5

# check the spatial extent data

# subset out relationships to include
# create useful variables for plotting
# correct a mistake in the landscape classification

vd2_raw <- 
  vd_dat %>%
  filter(include == "yes") %>%
  mutate(spatial_extent = if_else(spatial_extent == "landscape ", "landscape", spatial_extent))


# capitalise the first letter of each spatial extent
vd2_raw$spatial_extent <- as.factor(vd2_raw$spatial_extent)
levels(vd2_raw$spatial_extent) <- c("Continental", "Global", "Landscape", "Regional")
vd2_raw$spatial_extent <- as.character(vd2_raw$spatial_extent)

# how many papers are the bef relationships from with spatial extent information?
vd2_raw$`paper number` %>%
  unique() %>%
  length()

# how many of these reported multiple bef relationships
vd2_raw %>%
  group_by(`paper number`) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow()

# how many bef relationships in total were there?
vd2_raw$Relationship_nr %>%
  unique() %>%
  length()

# how many bef relationships per spatial extent?
vd2_raw %>%
  group_by(spatial_extent) %>%
  summarise(n = n())


# we obtained information on spatial extent for 231 relationships
# this came from 106 papers and 42 of those had multiple bef slopes associated with it

# test how robust our spatial classification is:

# does the classification represent spatial extent?
spat_clas <- 
  vd2_raw %>%
  mutate(lat_diff = (max_lat - min_lat),
         lon_diff = (max_lon - min_lon))

# examine anomalous values
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
View(vd2_raw[vd2_raw$`paper number` == 116,] )
vd2_raw$max_lat[vd2_raw$`paper number` == 116] <- 40

# for paper 125, replace max_lat: 7 with max_lat: 53
# for paper 125, replace min_lon: 53 with min_lon: 7
vd2_raw$max_lat[vd2_raw$`paper number` == 125] <- 53
vd2_raw$min_lon[vd2_raw$`paper number` == 125] <- 7

# reorder the spatial extent factor so it is ascending
vd2_raw$spatial_extent <- 
  factor(vd2_raw$spatial_extent, levels = c("Landscape", "Regional", "Continental", "Global"))


# plot figure S5

fig_s5_dat <- 
  vd2_raw %>%
  mutate(lat_diff = (max_lat - min_lat),
         lon_diff = (max_lon - min_lon))

fig.s5 <- 
  ggplot(data = rename(fig_s5_dat, `spatial extent` = spatial_extent),
         mapping = aes(x = log(1 + lon_diff), y = log(1 + lat_diff), 
                       colour = `spatial extent`)) +
  geom_jitter(size = 2, alpha = 0.75) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  ylab("ln - latitude range (DD)") +
  xlab("ln - longitude range (DD)") +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank(),
        legend.key.size = unit(0.5,"line"))

ggsave(filename = here("figures/fig_S5.pdf"), 
       plot = fig.s5, width = 8.5, height = 7, units = "cm",
       dpi = 450)


# continue with plotting of figure 4b

# add columns for neutral, negative and positive slopes
vd2_ana <- 
  vd2_raw %>%
  group_by(spatial_extent) %>%
  mutate(pos = if_else(bef_relationship == "Positive", 1, 0),
         neu = if_else(bef_relationship == "Neutral", 1, 0),
         neg = if_else(bef_relationship == "Negative", 1, 0)) %>%
  ungroup() %>%
  select(`paper number`, Relationship_nr, spatial_extent, pos, neu, neg)

# get the observed proportions
vd2_obs <- 
  vd2_ana %>%
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
vd2_ran <- vector("list", length = r)

set.seed(78)
for (i in seq_along(1:r) ) {
  
  x <- 
    vd2_ana %>%
    group_by(`paper number`) %>%
    slice_sample(., n = 1) %>%
    ungroup()
  
  vd2_ran[[i]] <- 
    x %>%
    group_by(spatial_extent) %>%
    summarise_at(vars(c("pos", "neu", "neg")), ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total_n = pos + neu + neg) %>%
    gather(pos, neu, neg, key = "relationship", value = "slope") %>%
    mutate(slope_proportion = slope/total_n)
  
}

vd2_ran <- 
  bind_rows(vd2_ran, .id = "run")

# summarise the randomised data
range_spat <- 
  vd2_ran %>%
  group_by(spatial_extent, relationship) %>%
  summarise(min_slope_proportion = min(slope_proportion),
            max_slope_proportion = max(slope_proportion),
            .groups = "drop")

# join the the randomised data to the observed data
sim_dat_spat <- 
  full_join(vd2_obs,
            select(range_spat, spatial_extent, relationship,
                   min_slope_proportion, max_slope_proportion),
            by = c("spatial_extent", "relationship"))


# plot figure 4b
fig.4b <- 
  ggplot(data = sim_dat_spat,
       mapping = aes(x = spatial_extent, y = slope_proportion, fill = direction)) +
  geom_bar(stat = "identity", width = 0.4, colour = "white",
           position=position_dodge(0.5)) +
  geom_errorbar(mapping = aes(ymin = min_slope_proportion,
                              ymax = max_slope_proportion),
                width = 0.2,
                position=position_dodge(0.5)) +
  scale_y_continuous(limits = (c(0, 0.905)), breaks = seq(from = 0, to = 0.8, by = 0.2)) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  ylab("") +
  xlab(NULL) +
  theme_meta() +
  theme(legend.position = "right")

fig.4 <- 
  ggarrange(fig.4a, fig.4b,
            ncol = 2, nrow = 1,
            labels = c("a", "b"),
            font.label = list(size = 9, color = "black", face = "plain", family = NULL),
            widths = c(1, 1.8))


ggsave(filename = here("figures/fig_4.pdf"), plot = fig.4,
       width = 17.3, height = 7, units = "cm")

### END
