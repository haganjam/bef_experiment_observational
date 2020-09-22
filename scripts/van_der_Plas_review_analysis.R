
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: van der Plas (2019) systematic review data

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

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# create customised plotting theme
theme_meta <- function(base_size = 12, base_family = "") {
  theme(panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill="NA", color="black", size=0.75, linetype="solid"),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.ticks.length = unit(-0.16, "cm"),
        axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 10, face = "plain", margin=margin(0,5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=10, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=10, face = "plain", margin=margin(0,10,0,0,"pt")),
        axis.ticks.x = element_line(colour = "black", size = 0.4),
        axis.ticks.y = element_line(colour = "black", size = 0.4))
}


### code for creating a template to fill in the grain and extent information

# load the van der Plas data
vand_dat_raw <- read_delim(here("data/van_der_Plas_2019_systematic_review.csv"), delim = ",")
head(vand_dat_raw)

# check the variables without X's
vand_dat_raw[, (!grepl("X", x = names(vand_dat_raw)))] %>%
  names()

# check the variables with X's
vand_dat_raw[, (grepl("X", x = names(vand_dat_raw)))] %>%
  names()

# remove these variables associated with the .csv file problems
vand_dat_raw <- vand_dat_raw[, (!grepl("X", x = names(vand_dat_raw)))]

# check the variables names
names(vand_dat_raw)

# make a copy of the data for cleaning
vand_dat <- vand_dat_raw

# check unique values for different variables

unique(vand_dat$`Category of function`) # subset out biomass only data points

unique(vand_dat$`abiotic covariate`)

unique(vand_dat$`composition covariate`)

# subset out Category of function: biomass
vand_dat <- 
  vand_dat %>%
  filter(`Category of function` == "Biomass")

# subset out studies where abiotic covariates were controlled for
vand_dat <- 
  vand_dat %>%
  filter(grepl(pattern = "es", `abiotic covariate`) )

vand_dat

# how many bef slopes are there that controlled for abiotic factors?
vand_dat$Relationship_nr %>%
  unique() %>%
  length()

# isolate relevant variables
rel_vars <- c( names(vand_dat[, 1:25]), names(vand_dat[, c(32, 34, 38, 39)]) )

# subset out the relevant variables
vand_dat <- 
  vand_dat %>%
  select(rel_vars)

vand_dat

# check which relationships have multiple BEF relationships associated with them

# create a vector of Relationship_nr that have multiple BEF relationships associated with them
mult_BEF <- 
  vand_dat %>%
  group_by(Relationship_nr) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(Relationship_nr)

mult_BEF

# split the vand_dat data into two

# all rows with only one BEF-relationship reported
vand_dat_1 <- 
  vand_dat %>%
  filter(!(Relationship_nr %in% mult_BEF)) %>%
  mutate(bef_multi = c("no"))

# all rows with multiple BEF-relationships reported
vand_dat_2 <- 
  vand_dat %>%
  filter((Relationship_nr %in% mult_BEF)) %>%
  mutate(bef_multi = c("yes"))

# for both datasets, add a new column: BEF relationship
vand_dat_1 <- 
  vand_dat_1 %>%
  mutate(bef_relationship = if_else(is.na(`Relationship overall`), Relationship, `Relationship overall`)) %>%
  select(-Relationship, `Relationship overall`, -`Line number`)

vand_dat_2 <- 
  vand_dat_2 %>%
  mutate(bef_relationship = `Relationship overall`) %>%
  select(-Relationship, `Relationship overall`, -`Line number`) %>%
  distinct() %>% 
  filter(!is.na(bef_relationship))

# bind these datasets together
vand_dat_c <- bind_rows(vand_dat_1, vand_dat_2)

# this dataset is cleaned and can now be searched for spatial grain and extent information
nrow(vand_dat_c)

# however, we need to add the 'other remarks data' because this actually contains some habitat type information
other_dat <- 
  vand_dat_raw %>%
  select(Relationship_nr, 'Other remarks')

# join these data to the vand_dat_c data
vand_dat_c <- 
  inner_join(vand_dat_c, other_dat, by = c("Relationship_nr") )

View(vand_dat_c)

vand_dat_c %>%
  filter(`paper number` == 125) %>%
  View()


# export an excel file to fill in the spatial grain and spatial extent information from the papers

# how many papers are there to go through?
vand_dat_c$`paper number` %>%
  unique() %>%
  length()

sub_names <- 
  names(vand_dat_c)[c(3, 4, 5, 6, 7, 8, 9, 11, 12, 30, 13, 14, 15, 16, 17, 18, 19, 29)]

# export a file to fill in with the spatial grain information

# vand_dat_c %>%
  # select(sub_names) %>%
  # mutate(min_lat = c("."),
         # max_lat = c("."),
         # min_lon = c("."),
         # max_lon = c("."),
         # spatial_grain = c("."),
         # spatial_extent = c("."),
         # grain_extent_notes = c(".")) %>%
  # write_csv(., path = here("data/van_der_Plas_review_spatial_grains.csv"))

# this exported file has duplicates for some bef-slopes
# these were removed while filling in the data



### figure 2 analysis

# import the dataset with the spatial extent and grain information filled in

# check whether all relationship numbers were accounted for

# read in the completed data file
spat_comp <- read_delim(here("data/van_der_Plas_2019_spatial_extent_complete.csv"), delim = ",")

# check if all relationship numbers were evaluated
unique(spat_comp$Relationship_nr) %>%
  length()

tibble(x = sort(unique(spat_comp$Relationship_nr)),
       y = sort(unique(vand_dat_c$Relationship_nr))) %>%
  mutate(id = if_else(x == y, 0, 1)) %>%
  pull(id) %>%
  sum(.)

# all relationship numbers were evaluated


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
              mapping = aes(x = bef_relationship, y = proportion, fill = bef_relationship),
              alpha = 1, bw = 0.005, position = position_dodge(width=0.3)) +
  geom_point(data = fig_2a_obs,
             mapping = aes(x = bef_relationship, y = proportion),
             colour = "black", size = 4.5, shape = 21, fill = "white") +
  geom_text(data = fig_2a_obs,
            mapping = aes(x = bef_relationship, y = proportion, label = direction), 
            position = position_dodge(width = 0.3)) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = (c(0, 0.905)), breaks = seq(from = 0, to = 0.8, by = 0.2)) +
  ylab("proportion of slopes") +
  xlab(NULL) +
  theme_meta() +
  theme(legend.position = "none")

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





