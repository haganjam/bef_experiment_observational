
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Jena data reanalysis

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


### fig. 3, fig. s3, fig. s4

# load the Jena community data
jena_comm <- read_delim(here("data/Jena_Community_02-08.csv"), delim = ",")
head(jena_comm)

# remove the first plots that were not sown with any species
jena_comm <- filter(jena_comm, !sowndiv %in% c(0))

# create a vector of species names
sp_names <- names(jena_comm)[51:110]

# seperate the data into spp and site characteristics matrix
site_dat <- select(jena_comm, all_of(-sp_names))

sp_dat <- select(jena_comm, all_of(sp_names))

# replace NAs in sp_dat with zeros to reflect absence
sp_dat <- 
  sp_dat %>% 
  mutate_all(~replace(., is.na(.), 0))

# check if there are any -9999's in the species data
sp_dat %>% 
    filter_at(vars(sp_names), any_vars(. < 0)) # there are none!

# calculate the number of species in the surveyed 3 x 3 m plots
rowSums(decostand(sp_dat, method = "pa")) # number of species

# calculate the number of species in the surveyed 3 x 3 m plots with more than 1 % cover
mutate_all(sp_dat, ~if_else(. == c(1), 0, .)) %>% 
  decostand(method = "pa") %>%
  rowSums()

# calculate the number of species in the surveyed 3 x 3 m plots with more than 5 % cover
mutate_all(sp_dat, ~if_else(. %in% c(1, 2), 0, .)) %>% 
  decostand(method = "pa") %>%
  rowSums()

# add these observed species richness values to the site_dat dataset
site_dat <- 
  site_dat %>% 
  mutate(observed_species = rowSums(decostand(sp_dat, method = "pa")),
         observed_species_1 = mutate_all(sp_dat, ~if_else(. == c(1), 0, .)) %>% 
           decostand(method = "pa") %>% 
           rowSums(),
         observed_species_5 = mutate_all(sp_dat, ~if_else(. %in% c(1, 2), 0, .)) %>% 
           decostand(method = "pa") %>% 
           rowSums())


# use the jena_bio dataset to get plot-level (20 x 20 m) target biomass measurements

# load the Jena biomass data
jena_bio <- read_delim(here("data/Jena_Biomass_02-08.csv"), delim = ",")
head(jena_bio)

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !sowndiv %in% c(0))

# examine the data and the variable names
View(jena_bio)
names(jena_bio)

# extract relevant biomass variables
site_bio_dat <- 
  jena_bio %>% 
  select(plotcode, block, plot, subsample, year, month, time, X, Y, sowndiv, target.biomass)

# check for NA's in the dataset
site_bio_dat %>% filter_all(any_vars(is.na(.))) # only NA's in the coordinates

# check for any -9999's in the dataset
site_bio_dat %>% filter_all(any_vars(. < 0)) # yes there is one sub-sample with missing biomass data

# exclude this missing subplot observation from the dataset
site_bio_dat <- 
  site_bio_dat %>% 
  filter(target.biomass >= 0)

# calculate the mean target.biomass for each plot for each time point
site_bio_dat <- 
  site_bio_dat %>% 
  group_by(plotcode, time) %>%
  summarise(target_biomass_m = mean(target.biomass, na.rm = TRUE),
            target_biomass_sd = sd(target.biomass, na.rm = TRUE),
            target_biomass_n = n()) %>%
  ungroup()


# add the biomass measurements to the site_dat data

# first make a season variable in the site_dat dataset
site_dat <- 
  site_dat %>% 
  mutate(season = if_else(month %in% c("May", "June"), "spring", "summer"))

# make a list of relevant variable names
var_names <- c("plotcode", "year", "season", "month", "time", "X", "Y", "sowndiv",
               "numfg", "numgrass", "numsherb", "numtherb", "numleg",
               "observed_species", "observed_species_1", "observed_species_5", 
               "target_biomass_m", "target_biomass_sd", "target_biomass_n")

# join the biomass data to the site data
jena_dat <- 
  full_join(site_dat, site_bio_dat, by = c("plotcode", "time")) %>% 
  select(var_names)


# calculate the total number of species observed across years
df_agg <- 
  bind_cols(jena_dat, sp_dat) %>% 
  group_by(plotcode, season) %>%
  summarise_at(vars(sp_names), ~sum(., na.rm = TRUE)) %>% 
  ungroup()

species_richness <- 
  df_agg %>%
  select(-plotcode, -season) %>% 
  decostand(method = "pa") %>%
  rowSums()

total_spp <- 
  bind_cols(select(df_agg, plotcode, season), 
            total_species = species_richness)
  

# join the total spp calculated to the jena_dat
jena_dat <- full_join(jena_dat, total_spp, by = c("plotcode", "season"))


### subset the values in the last year of the experiment (i.e. 2008)
jena_dat_years <- 
  jena_dat %>%
  filter(season == "spring", year == 2008)

# plot the relationship between species pool diversity and ecosystem function
fig_s3a <- 
  ggplot(data = jena_dat_years %>% 
         filter(sowndiv < 60, sowndiv > 1),
       mapping = aes(x = sowndiv, y = target_biomass_m)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  scale_x_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  ylab(expression(paste("community biomass g ", "(m"^"-2", ")" )) ) +
  xlab(expression(paste("inoculated ", alpha, " diversity", sep = ""))) +
  theme_meta()

# plot the relationship between species pool diversity and realised species richness
fig_s3b <- 
  ggplot(data = jena_dat_years %>% 
         filter(sowndiv < 60, sowndiv > 1),
       mapping = aes(x = sowndiv, y = observed_species)) +
  geom_abline(intercept = 0, slope = 1, colour = "black", size = 0.5, linetype = "dashed") +
  geom_point(alpha = 1, shape = 16, size = 2) +
  scale_x_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  scale_y_continuous(limits = c(0, 16), breaks = unique(pull(filter(jena_dat_years, sowndiv < 60, sowndiv > 1), sowndiv))) +
  ylab(expression(paste("realised ", alpha, " diversity", sep = ""))) +
  xlab(expression(paste("inoculated ", alpha, " diversity", sep = ""))) +
  theme_meta()

fig_s3 <- 
  ggarrange(fig_s3a, fig_s3b, labels = c("(a)", "(b)"),
            font.label = list(size = 10, color = "black", face = "plain", family = NULL))

ggsave(filename = here("figures/fig_s3.png"), plot = fig_s3,
       dpi = 500, units = "cm", width = 20, height = 10)



### simulate the effect of changing species pool gradients in Jena data

# how many plots are there for different ranges?
jena_dat_years %>% 
  group_by(as.character(sowndiv)) %>%
  summarise(n = n())

# set up plots without the 60 species plots
jena_dat_samp <- 
  jena_dat_years %>% 
  filter(sowndiv < 60)

jena_dat_samp$sowndiv %>%
  unique()

# remove the monocultures
jena_dat_samp <- 
  jena_dat_samp %>%
  filter(sowndiv > 1)

# how many plots are there
jena_dat_samp %>%
  nrow()

jena_dat_samp$sowndiv %>%
  unique()

jena_dat_samp %>%
  group_by(sowndiv) %>%
  summarise(n = n())

# Set-up ranges for the data
spp_ascent <- 
  jena_dat_samp %>% 
  pull(sowndiv) %>%
  unique()

spp_descent <- 
  sort(spp_ascent, decreasing = TRUE)

# create all possible combinations but when lower > upper and
# remove the 1-1 and 2-2 treatment because there is no possible observed diversity gradient
combs <- 
  crossing(spp_ascent, spp_descent) %>%
  mutate(remain = if_else(spp_ascent <= spp_descent, 1, 0)) %>%
  filter(remain == 1) %>% 
  select(-remain) %>%
  filter(!(spp_ascent == 2 & spp_descent == 2))

combs

# set the number of replications to take of each initial diversity range combination
n_reps <- 30

# replicate the initial diversity ranges to match with the replicates
sp_ranges <- combs[ rep(seq(1:nrow(combs)), each = n_reps), ]

# set the number of plots to draw in each run
n_samples <- 12

samp_dat <- vector("list", length = nrow(sp_ranges) )

for (i in seq_along(1:nrow(sp_ranges))) {
  
  jena_samp <- 
    filter(jena_dat_samp, 
           sowndiv >= sp_ranges$spp_ascent[i],
           sowndiv <= sp_ranges$spp_descent[i])
  
  samp_dat[[i]] <- 
    jena_samp[ sample(x = seq_along(1:nrow(jena_samp)), size = n_samples), ]
  
}

# collapse the iterated data and generate new modifier variables
sim_out <- 
  samp_dat %>%
  bind_rows(.id = "replicate") %>%
  group_by(replicate) %>%
  mutate(sowndiv_range = paste(min(sowndiv), max(sowndiv), sep = "_"),
         sowndiv_min = min(sowndiv),
         sowndiv_max = max(sowndiv)) %>%
  mutate(initial_div_range = max(sowndiv) - min(sowndiv),
         realised_div_range = max(observed_species) - min(observed_species),
         min_realised = min(observed_species),
         max_realised = max(observed_species)) %>%
  mutate(species_pool = as.character(initial_div_range)) %>%
  ungroup()

# set a minimum realised diversity
min_r <- 1

sim_out <- 
  sim_out %>%
  filter(realised_div_range >= min_r)

# extract linear model slope for the observed diversity gradient
sim_out_slopes <- 
  sim_out %>% 
  group_by(replicate) %>%
  mutate(target_biomass_m = scale(target_biomass_m),
         observed_species = scale(observed_species)) %>% 
  ungroup() %>%
  group_by(sowndiv_range, replicate) %>%
  do(fit_sim = lm(target_biomass_m ~ observed_species, data = .)) %>%
  tidy(fit_sim) %>% 
  ungroup() %>%
  filter(term == "observed_species") %>%
  select(-statistic, -p.value)

# join these slope data to the rest of the data
sim_out <- 
  full_join(sim_out, 
            sim_out_slopes, 
            by = c("sowndiv_range", "replicate"))

# subset relevant variables and remove replicate data points from the join
sim_out <- 
  sim_out %>% 
  select(replicate, initial_div_range, sowndiv_min, sowndiv_max,
         realised_div_range, min_realised, max_realised, estimate) %>% 
  distinct() 

# plot the relationship between initial diversity range and the observed diversity-function slope

ord <- 
  sim_out %>%
  pull(initial_div_range) %>%
  unique() %>%
  sort()
ord

rea <- 
  sim_out %>%
  pull(realised_div_range) %>%
  unique() %>%
  sort()
rea

# check if low initial diversity values are biased
sim_out %>%
  filter(initial_div_range == 0) %>%
  pull(sowndiv_max) %>%
  unique()

# plot fig. 3
fig_3 <- 
  ggplot(data = sim_out,
       mapping = aes(x = initial_div_range, y = estimate, colour = realised_div_range)) +
  geom_jitter(alpha = 0.5, shape = 16, size = 4, width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = ord) +
  xlab(expression(paste(alpha, " species pool diversity range", sep = ""))) +
  ylab(expression(paste("realised ", alpha, " diversity", " - ", "function slope", sep = ""))) +
  labs(colour = expression(paste("realised ", alpha, " diversity range") ) ) +
  scale_colour_viridis_c() +
  theme_meta() +
  theme(legend.position = "top")

ggsave(filename = here("figures/fig_3.png"), plot = fig_3,
       dpi = 500, units = "cm", width = 11, height = 10)

# plot fig s4
fig_s4 <- 
  ggplot(data = sim_out) +
  geom_jitter(aes(x = initial_div_range, y = sowndiv_min, group = initial_div_range )) +
  geom_boxplot(aes(x = initial_div_range, y = sowndiv_min, group = initial_div_range )) +
  scale_x_continuous(breaks = ord) +
  xlab(expression(paste("inoculated ", alpha, " diversity range", sep = ""))) +
  ylab(expression(paste("minimum inoculated  ", alpha, " diversity", sep = ""))) +
  theme_meta()

ggsave(filename = here("figures/fig_s4.png"), plot = fig_s4,
       dpi = 500, units = "cm", width = 10, height = 10)


# plot insets to show the slope calculation
fig_3i_iii_dat <- 
  samp_dat %>%
  bind_rows(.id = "replicate") %>%
  group_by(replicate) %>%
  mutate(sowndiv_range = paste(min(sowndiv), max(sowndiv), sep = "_"),
         sowndiv_min = min(sowndiv),
         sowndiv_max = max(sowndiv)) %>%
  mutate(initial_div_range = max(sowndiv) - min(sowndiv),
         realised_div_range = max(observed_species) - min(observed_species)) %>%
  mutate(species_pool = as.character(initial_div_range)) %>%
  ungroup() %>%
  filter(realised_div_range >= min_r)

# check the sowndiv values when without any additional diversity range
fig_3i_iii_dat %>%
  filter(initial_div_range == 0) %>%
  pull(sowndiv) %>%
  unique()

# sample randomly a set of plots for the insets of fig. 3i-iii
set.seed(45)
i_reps <- 
  fig_3i_iii_dat %>%
  filter(initial_div_range %in% c(0, 6, 14) ) %>%
  group_by(initial_div_range) %>%
  sample_n(., size = 1) %>%
  ungroup() %>%
  pull(replicate)

fig_3i_iii <- 
  fig_3i_iii_dat %>%
  filter(replicate %in% i_reps) %>%
  split(., .$replicate) %>%
  lapply(function (x) {
    
    ggplot(data = x,
           mapping = aes(x = observed_species, y = target_biomass_m)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
      ylab(expression(paste("community biomass g ", "(m"^"-2", ")" )) ) +
      xlab(expression(paste("realised ", alpha, " diversity", sep = ""))) +
      theme_meta() +
      theme(axis.title.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8))
    
  })

# check plots and their inoculated diversity range
fig_3i_iii_dat %>%
  filter(replicate %in% i_reps) %>%
  group_by(replicate) %>%
  summarise(range = max(sowndiv)-min(sowndiv))

# put the insets into a list
fig_3i_iii_list <- list(fig_3i_iii[[1]], fig_3i_iii[[2]], fig_3i_iii[[3]])
names(fig_3i_iii_list) <- c(1:length(fig_3i_iii_list))

# export the insets
for (i in seq_along(1:length(fig_3i_iii_list))) {
  
  ggsave(filename = paste0(here("figures"), "/fig_3_", names(fig_3i_iii_list)[i], ".png"), 
         plot = fig_3i_iii_list[[i]], dpi = 500, units = "cm", width = 5, height = 5)
  
}



### compare diversity estimators for the alpha species pool diversity

# load the Jena community data
jena_est_raw <- read_delim(here("data/Jena_Community_02-08.csv"), delim = ",")
head(jena_est_raw )

# remove the first plots that were not sown with any species
jena_est_raw  <- filter(jena_est_raw , !sowndiv %in% c(0, 1))

# create a vector of species names
sp_names_est <- names(jena_est_raw)[51:110]

# seperate the data into spp and site characteristics matrix
site_est <- select(jena_est_raw, -all_of(sp_names_est) )

sp_est <- select(jena_est_raw, all_of(sp_names_est ))

# create a season column in site_est
site_est <- 
  site_est %>% 
  mutate(season = if_else(month %in% c("May", "June"), "spring", "summer"))

# create a numeric id column for each data point
site_est <- 
  site_est %>%
  mutate(row_id = seq_along(1:nrow(site_est)))

# replace NAs in sp_dat with zeros to reflect absence
sp_est <- 
  sp_est %>% 
  mutate_all(~replace(., is.na(.), 0))

# check if there are any -9999's in the species data
sp_est %>% 
  filter_at(vars(sp_names_est), any_vars(. < 0)) # there are none!


### estimate diversity for each row of data in sp_est

# choose years to sample
year_n <- 3

# sample three years at random from the dataset
year_s <- sample(x = unique(site_est$year), size = year_n)

# or chooose years directly
year_s <- c(2006, 2007, 2008)

# choose the season
seas <- c("spring")

# get row numbers
row_id <- 
  site_est %>%
  filter(year %in% year_s,
         season == seas) %>%
  pull(row_id)

est_out <- with(site_est[row_id, ], specpool(decostand(sp_est[row_id, ], method = "pa"), plotcode))

# add a plot code column
est_out$plotcode <- row.names(est_out)

# join this to the sowndiv data
est_out <- 
  left_join(as_tibble(est_out), 
            site_est %>%
              filter(time == 5) %>%
              select(plotcode, sowndiv), 
            by = "plotcode")

# make each estimator a variable
est_out <- 
  est_out %>%
  select(-contains(match = "se")) %>%
  pivot_longer(cols = c(Species, chao, jack1, jack2, boot), 
               names_to = "estimator",
               values_to = "estimate")

# estimate the absolute and percent deviation of each estimate from the sowndiv
est_out <- 
  est_out %>%
  mutate(abs_error = (sowndiv-estimate)) %>%
  mutate(perc_error = ((abs_error/sowndiv)*100) )

# fit the linear models and get the r2 values

models <- mtcars %>%
  nest_by(cyl) %>%
  mutate(mod = list(lm(mpg ~ disp, data = data)))
models %>% summarise(rsq = summary(mod)$r.squared)


















