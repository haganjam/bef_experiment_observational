
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: California kelp data from (Gerung et al. 2020)

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)
library(viridis)
library(here)
library(vegan)
library(piecewiseSEM)
library(ggpubr)

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# load the biomass data
kelp_raw <- read_csv(here("data/Annual_All_Species_Biomass_at_transect_20200108.csv"))

# check for unique sites
kelp_raw$SITE %>%
  unique()

# remove the two sites on the channel islands
# SCDI
# SCTW
kelp_raw <- 
  kelp_raw %>%
  filter( !(SITE %in% c("SCDI", "SCTW")) )

# subset out the algae data
names(kelp_raw)

unique(kelp_raw$GROUP)

kelp_raw <- 
  kelp_raw %>%
  filter(GROUP == c("ALGAE"))

# explore the data
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(transect = length(unique(TRANSECT))) %>%
  filter(transect > 2)

kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(transect = length(unique(TRANSECT))) %>%
  filter(transect == 2)

kelp_raw %>%
  group_by(SITE) %>%
  summarise(year = length(unique(YEAR)))

kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(n_transects = length(unique(TRANSECT)) ) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = SITE, y = YEAR, colour = n_transects)) +
  geom_point()
  
kelp_raw %>%
  group_by(SITE, YEAR) %>%
  summarise(n_transects = length(unique(TRANSECT)) ) %>%
  ungroup() %>%
  group_by(SITE) %>%
  summarise(min_transects = min(n_transects))

kelp_raw %>%
  group_by(SITE, YEAR, TRANSECT) %>%
  summarise(transect = min(TRANSECT) )  %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = YEAR, y = transect)) +
  geom_point() +
  facet_wrap(~SITE)


# based on this exploration:
# (1) remove years before 2001 (i.e. only 2000) because this is only available at some sites
# (2) select the transects with the lowest number ID at each site because these are consistent across years

kelp_raw <- 
  kelp_raw %>%
  filter(YEAR > 2000) %>%
  group_by(SITE) %>%
  mutate(min_1 = min(unique(TRANSECT), na.rm = TRUE),
         min_2 = sort(unique(TRANSECT) )[2]) %>%
  ungroup() %>%
  filter(TRANSECT %in% c(min_1, min_2) ) %>%
  select(-min_1, -min_2)


# analysis data

kelp_ana <- 
  kelp_raw %>%
  select(-PERCENT_COVER, -DENSITY, -WM_GM2, -DRY_GM2,
         -SFDM)

# check if all species codes are recorded for each site
kelp_ana %>%
  group_by(SITE, YEAR) %>%
  summarise(spp = length(unique(SP_CODE))) %>%
  pull(spp)

# all species codes are recorded each year at each site and just ticked off


# check for NAs in the data
lapply(kelp_ana, function(x) {
  
  if_else(is.na(x), 1, 0) %>%
    sum()
  
})


# sum up the transects at each site-year combination

# check if there are missing values
kelp_ana %>%
  filter(AFDM == -99999) %>%
  View()

kelp_ana$COMMON_NAME %>%
  unique()

kelp_ana$SP_CODE %>%
  unique()

kelp_ana %>%
  filter(is.na(SP_CODE) ) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

kelp_ana %>%
  filter(is.na(SP_CODE) ) %>%
  nrow()

# remove the rows with missing values for ash free dry mass
kelp_ana <- 
  kelp_ana %>%
  filter_at(vars(c("AFDM")), any_vars(. != -99999))

# replace the NAs for species code with NIAN as this is the scientific name
kelp_ana <- 
  kelp_ana %>%
  mutate(SP_CODE = if_else(is.na(SP_CODE), "NIAN", SP_CODE))

# check general summary statistics
kelp_ana %>%
  summary()

# check for more missing values for the AFDM
kelp_ana %>%
  filter(AFDM < 0)

kelp_ana %>%
  filter(SP_CODE != "MAPY") %>%
  filter(grepl("kelp", COMMON_NAME)) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()

kelp_ana$TAXON_GENUS %>%
  unique()

kelp_ana %>%
  filter(grepl("Macrocystis", TAXON_GENUS)) %>%
  pull(SCIENTIFIC_NAME) %>%
  unique()


# summarise for the two transects at each site for each year
kelp_ana_sum <- 
  kelp_ana %>%
  group_by(SITE, YEAR, SP_CODE) %>%
  summarise(AFDM = sum(AFDM, na.rm = TRUE), .groups = "drop")

# there are two years at the AHND site 2017 and 2019 with zeros for all algae species
# remove these for now
kelp_ana_sum %>%
  filter((SITE == "AHND" & YEAR %in% c(2017, 2019)) )

# remove these two years and do the analysis without them
kelp_ana_sum <- 
  kelp_ana_sum %>%
  filter(!(YEAR %in% c(2017, 2019)))

kelp_ana_sum$YEAR %>%
  unique() %>%
  length()


# work with the summarised data
# examine the relationships between alpha diversity and function
alpha_div <- 
  kelp_ana_sum %>%
  group_by(SITE, YEAR) %>%
  summarise(alpha_diversity = sum(decostand(AFDM, method = "pa"), na.rm = TRUE),
            comm_biomass = sum(AFDM, na.rm = TRUE),
            .groups = "drop")

ggplot(data = alpha_div,
         mapping = aes(x = alpha_diversity, y = sqrt(comm_biomass), colour = as.character(YEAR) )) +
  geom_point(alpha = 0.5, shape = 16, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.1) +
  scale_colour_viridis_d() +
  theme_meta() +
  ylab("sqrt( community biomass )") +
  xlab("realised diversity") +
  theme(legend.position = "none")


# get a slope for each year by fitting a simple linear regression
# then plot a distribution of slopes
alpha_slopes <- 
  split(select(alpha_div, -YEAR), alpha_div$YEAR ) %>%
  lapply(., function(x) {
    
    z <- lm(sqrt(comm_biomass) ~ (alpha_diversity), data = x)
    
    coef(z)[2]
    
  }) %>%
  
  bind_rows(., .id = "YEAR")

fig_4a_inset <- 
  ggplot(data = alpha_slopes,
       mapping = aes(x = alpha_diversity)) +
  geom_histogram(bins = 15, alpha = 0.5, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(alpha_slopes$alpha_diversity), 
             colour = "red") +
  xlab(NULL) +
  ylab(NULL) +
  theme_meta() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

# plot this histogram as an inset graph

fig_4a <- 
  ggplot(data = alpha_div,
  mapping = aes(x = alpha_diversity, y = sqrt(comm_biomass), colour = as.character(YEAR) )) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  theme_meta() +
  ylab(expression(sqrt(paste("community dry mass (g ",  "m"^"-2", ")") ))) +
  xlab(expression(paste("realised ", alpha, " diversity"))) +
  scale_y_continuous(limits = c(0, 53)) +
  annotation_custom(ggplotGrob(fig_4a_inset), xmin = 1, xmax = 12, 
                    ymin = 34, ymax = 57) +
  theme(legend.position = "none")

fig_4a

# examine the relationship between temporal gamma diversity function
kelp_ana_sum %>%
  group_by(SITE) %>%
  summarise(years = length(unique(YEAR)))


# we fit the model with the square-root of community biomass
# do the calculations for biomass with the square-root

# calculate mean biomass
mean_bio <- 
  kelp_ana_sum %>%
  group_by(SITE, YEAR) %>%
  summarise(comm_biomass = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
  group_by(SITE) %>%
  summarise(comm_biomass_mean = mean(sqrt(comm_biomass), na.rm = TRUE),
            comm_biomass_se = (sd(sqrt(comm_biomass), na.rm = TRUE)/sqrt(n())), .groups = "drop")

# calculate temporal gamma diversity
temporal_gamma <- 
  kelp_ana_sum %>%
  pivot_wider(names_from = SP_CODE, values_from = AFDM) %>%
  select(-YEAR) %>%
  group_by(SITE) %>%
  summarise(across(.cols = everything(), ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
  mutate(across(.cols = everything(), ~as.numeric(decostand(.x, method = "pa")) )) %>%
  ungroup() %>%
  rowwise(SITE) %>%
  summarise(temporal_gamma = sum(c_across(where(is.numeric)))) %>%
  ungroup()

gamma_div <- full_join(mean_bio, temporal_gamma, by = "SITE") 

# test this with a linear model

# examine the variable distributions
hist(gamma_div$comm_biomass_mean)
hist((gamma_div$temporal_gamma) )

# fit the linear model
lm_1 <- lm(comm_biomass_mean ~ (temporal_gamma), data = gamma_div)
plot(lm_1)
hist(residuals(lm_1))

# check the model output
summary(lm_1)
lm_1_sum <- summary(lm_1)
lm_1_sum

# plot the graph
fig_4b <- 
  ggplot() +
  geom_point(data = gamma_div,
             mapping = aes(x = temporal_gamma, y = comm_biomass_mean), 
             size = 1.5) +
  geom_errorbar(data = gamma_div,
                mapping = aes(x = temporal_gamma, 
                              ymin = comm_biomass_mean - comm_biomass_se, 
                              ymax = comm_biomass_mean + comm_biomass_se),
                width = 0.1) +
  geom_smooth(data = gamma_div,
              mapping = aes(x = temporal_gamma, y = comm_biomass_mean),
              method = "lm", size = 0.5, colour = "black", alpha = 0.3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("r^2 == ", round(lm_1_sum$r.squared, 2)), parse = TRUE,
           vjust = 1.1, hjust = -0.5, size = 3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("F == ", round(lm_1_sum$fstatistic[1], 2)), parse = TRUE,
           vjust = 3.3, hjust = -0.5, size = 3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("P == ", round(lm_1_sum$coefficients[2, 4], 3)), parse = TRUE,
           vjust = 5.1, hjust = -0.45, size = 3) +
  xlab(expression(paste(alpha, " species pool diversity", sep = ""))) +
  ylab(expression(sqrt(paste("community dry mass (g ",  "m"^"-2", ")") ))) +
  theme_meta()

fig_4b

fig_4 <- 
  ggarrange(fig_4a, fig_4b, labels = c("a", "b"),
          font.label = list(size = 12, color = "black", face = "plain", family = NULL),
          widths = c(1, 1))

ggsave(filename = here("figures/fig_4.png"), plot = fig_4, dpi = 500,
       width = 16, height = 7, units = "cm")




