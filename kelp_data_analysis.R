
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: California kelp data from (Gerung et al. 2020)

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lme4)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(viridis)
library(here)
library(vegan)
library(piecewiseSEM)
library(ggpubr)

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

View(kelp_raw)

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
  summarise(AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup()

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
            comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() 

ggplot(data = alpha_div,
         mapping = aes(x = alpha_diversity, y = sqrt(comm_biomass), colour = as.character(YEAR) )) +
  geom_point(alpha = 0.5, shape = 16, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.1) +
  scale_colour_viridis_d() +
  theme_classic() +
  ylab("sqrt( community biomass )") +
  xlab("realised diversity") +
  theme(legend.position = "none")

# test this with a linear mixed model

# explore the data
ggplot(data = alpha_div,
       mapping = aes(x = alpha_diversity)) +
  geom_histogram() +
  facet_wrap(~YEAR, scales = "free") +
  theme_classic()

ggplot(data = alpha_div,
       mapping = aes(x = sqrt(comm_biomass) )) +
  geom_histogram() +
  facet_wrap(~YEAR, scales = "free") +
  theme_classic()

# square-root could work if plots are not conforming to assumptions

lmm_1 <- lmer(sqrt(comm_biomass) ~ alpha_diversity + (0 + alpha_diversity|YEAR), 
              data = mutate(alpha_div, YEAR = as.character(YEAR)), REML = TRUE)

# check the assumptions
plot(lmm_1)
hist(residuals(lmm_1))

# check the model output
lmm_1_sum <- summary(lmm_1)
lmm_1_sum

# get the r2 value
rsquared(lmm_1, method = "nagelkerke")

# check for significant fixed effect of alpha diversity
drop1(lmm_1, test = "Chisq")


# get a slope for each year by fitting a simple linear regression
# then plot a distribution of slopes

alpha_slopes <- 
  split(select(alpha_div, -YEAR), alpha_div$YEAR ) %>%
  lapply(., function(x) {
    
    z <- lm(sqrt(comm_biomass) ~ (alpha_diversity), data = x)
    
    coef(z)[2]
    
  }) %>%
  
  bind_rows(., .id = "YEAR")

i1 <- 
  ggplot(data = alpha_slopes,
       mapping = aes(x = alpha_diversity)) +
  geom_histogram(bins = 15, alpha = 0.5, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(alpha_slopes$alpha_diversity), 
             colour = "red") +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic()

# plot this histogram as an inset graph

fig_4a <- 
  ggplot(data = alpha_div,
  mapping = aes(x = alpha_diversity, y = sqrt(comm_biomass), colour = as.character(YEAR) )) +
  geom_point(alpha = 0.5, shape = 16, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ylab(expression(sqrt("community biomass"))) +
  xlab("realised diversity") +
  scale_y_continuous(limits = c(0, 53)) +
  annotation_custom(ggplotGrob(i1), xmin = 1, xmax = 12, 
                    ymin = 35, ymax = 58) +
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
             mapping = aes(x = temporal_gamma, y = comm_biomass_mean)) +
  geom_errorbar(data = gamma_div,
                mapping = aes(x = temporal_gamma, 
                              ymin = comm_biomass_mean - comm_biomass_se, 
                              ymax = comm_biomass_mean + comm_biomass_se),
                width = 0.1) +
  geom_smooth(data = gamma_div,
              mapping = aes(x = temporal_gamma, y = comm_biomass_mean),
              method = "lm", alpha = 0.1, colour = "black", size = 0.5) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("R^2 == ", round(lm_1_sum$r.squared, 2)), parse = TRUE,
           vjust = 1, hjust = -0.4, size = 3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("F == ", round(lm_1_sum$fstatistic[1], 2)), parse = TRUE,
           vjust = 3.2, hjust = -0.5, size = 3) +
  annotate("text", x = -Inf, y = Inf, 
           label = paste("P == ", round(lm_1_sum$coefficients[2, 4], 3)), parse = TRUE,
           vjust = 5, hjust = -0.45, size = 3) +
  xlab("species pool diversity (temporal)") +
  ylab(expression(sqrt("community biomass"))) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 9, colour = "black"),
        axis.text.y = element_text(size = 9, colour = "black"))
fig_4b

fig_4 <- 
  ggarrange(fig_4a, fig_4b, labels = c("(a)", "(b)"),
          font.label = list(size = 10, color = "black", face = "plain", family = NULL),
          widths = c(1, 1))

ggsave(filename = here("figures/fig_4.png"), plot = fig_4, dpi = 300,
       width = 16, height = 7, units = "cm")




# plot mean alpha against mean community biomass (perhaps)

# add mean alpha diversity to this dataset
mean_alpha <- 
  alpha_div %>%
  group_by(SITE) %>%
  summarise(mean_alpha = mean(alpha_diversity, na.rm = TRUE),
            se_alpha = (sd(alpha_diversity, na.rm = TRUE)/sqrt(n())),
            alpha_bio_mean = mean(comm_biomass, na.rm = TRUE),
            alpha_bio_se = (sd(comm_biomass, na.rm = TRUE)/sqrt(n())),
            .groups = "drop")

gamma_div <- full_join(gamma_div, mean_alpha, by = "SITE") 

ggplot() +
  geom_point(data = gamma_div,
             mapping = aes(x = mean_alpha, y = alpha_bio_mean)) +
  geom_errorbar(data = gamma_div,
                mapping = aes(x = mean_alpha, 
                              ymin = alpha_bio_mean - alpha_bio_se, 
                              ymax = alpha_bio_mean + alpha_bio_se),
                width = 0.1) +
  geom_smooth(data = gamma_div,
              mapping = aes(x = mean_alpha, y = alpha_bio_mean),
              method = "lm", alpha = 0.2) +
  theme_classic()


# plot alpha and gamma relationships on the same axis

p +
  geom_point(data = gamma_div,
              mapping = aes(x = mean_alpha, y = alpha_bio_mean)) +
  geom_errorbar(data = gamma_div,
                mapping = aes(x = mean_alpha, 
                              ymin = alpha_bio_mean - alpha_bio_se, 
                              ymax = alpha_bio_mean + alpha_bio_se),
                width = 0.1) +
  geom_smooth(data = gamma_div,
              mapping = aes(x = mean_alpha, y = alpha_bio_mean),
              method = "lm", alpha = 0.2) +
  theme_classic()
  





# if we decide to keep the 2017 and 2019 years, then we need to correct with this code

# one site has only 17 years of data so we need to correct for this:

# calculate temporal gamma diversity
min_years <- 
  kelp_ana_sum %>%
  group_by(SITE) %>%
  summarise(n_years = length(unique(YEAR))) %>%
  pull(n_years) %>%
  min()


# set-up number of randomisations
r <- 100

# set-up output lists

com_bio <- vector("list", length = r)
temp_gamma <- vector("list", length = r)

for(i in seq_along(1:r)) {
  
  # calculate community biomass in each year at each site
  com_bio[[i]] <-
    kelp_ana_sum %>%
    group_by(SITE, YEAR) %>%
    summarise(comm_biomass = sum(AFDM, na.rm = TRUE), .groups = "drop") %>%
    group_by(SITE) %>%
    slice_sample(., n = min_years, replace = TRUE) %>%
    summarise(comm_biomass = mean(comm_biomass, na.rm = TRUE), .groups = "drop")
  
  z <- 
    kelp_ana_sum %>%
    spread(key = SP_CODE, value = AFDM) %>%
    select(-YEAR) %>%
    group_by(SITE) %>%
    slice_sample(., n = min_years, replace = TRUE) %>%
    summarise(across(.cols = everything(), ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
    mutate(across(.cols = everything(), ~as.numeric(decostand(.x, method = "pa")) )) %>%
    ungroup()
  
  temp_gamma[[i]] <- 
    tibble(SITE = z$SITE,
         temporal_gamma = rowSums(select(z, -SITE)))
}

gamma_bio <- 
  full_join(bind_rows(com_bio, .id = "replicate"),
          bind_rows(temp_gamma, .id = "replicate"),
          by = c("replicate", "SITE") ) %>%
  group_by(SITE) %>%
  summarise(biomass = mean(comm_biomass, na.rm = TRUE),
            biomass_sd = sd(comm_biomass, na.rm = TRUE),
            temporal_gamma = mean(temporal_gamma, na.rm = TRUE))

ggplot() +
  geom_point(data = gamma_bio,
             mapping = aes(x = temporal_gamma, y = biomass)) +
  geom_errorbar(data = gamma_bio,
                mapping = aes(x = temporal_gamma, ymin = biomass-biomass_sd, ymax = biomass+biomass_sd),
                width = 0.1) +
  geom_smooth(data = gamma_bio,
              mapping = aes(x = temporal_gamma, y = biomass ),
              method = "lm", alpha = 0.2) +
  theme_classic()


# calculate relative abundances for each year and each site
kelp_ana_sum %>% 
  group_by(SITE, YEAR) %>%
  mutate(total_AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SP_CODE_rel = (AFDM/total_AFDM)*100 ) %>%
  filter(SP_CODE_rel > 5) %>%
  split(., .$SITE) %>%
  lapply(., function(x) {
    
    ggplot(data = x, mapping = aes(x = YEAR, y = AFDM, colour = SP_CODE)) +
      geom_point() +
      facet_wrap(~SITE) +
      theme_classic() 
    }
  )


# do these analyses without the giant kelp (MAPY) and unidentified juvenile kelp (BLD)

kelp_ana_sum %>%
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  summarise(alpha_diversity = sum(decostand(AFDM, method = "pa"), na.rm = TRUE),
            comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(data = .,
         mapping = aes(x = alpha_diversity, y = comm_biomass, colour = as.character(YEAR) )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gam_no_kelp <- 
  kelp_ana_sum %>%
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  mutate(comm_biomass = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(AFDM > 0) %>%
  group_by(SITE) %>%
  summarise(gamma_diversity = length(unique(SP_CODE)),
            comm_biomass = mean(comm_biomass, na.rm = TRUE))

ggplot(data = gam_no_kelp,
         mapping = aes(x = gamma_diversity, y = comm_biomass)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()



# plot species' biomass through time
kelp_ana_sum %>% 
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  mutate(total_AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SP_CODE_rel = (AFDM/total_AFDM)*100 ) %>%
  filter(SP_CODE_rel > 5) %>%
  split(., .$SITE) %>%
  lapply(., function(x) {
    
    ggplot(data = x, mapping = aes(x = YEAR, y = AFDM, colour = SP_CODE)) +
      geom_point() +
      facet_wrap(~SITE) +
      theme_classic() 
  }
  )

# are the dominant species different across sites in a given year
dom_spp <- 
  kelp_ana_sum %>% 
  filter(!(SP_CODE %in% c("MAPY", "BLD"))) %>%
  group_by(SITE, YEAR) %>%
  mutate(total_AFDM = sum(AFDM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(SP_CODE_rel = (AFDM/total_AFDM)*100 ) %>%
  group_by(SITE, YEAR) %>%
  mutate(SP_max = max(SP_CODE_rel)) %>%
  ungroup() %>%
  filter(SP_CODE_rel == SP_max)

# do dominant species change in space?
dom_spp %>%
  split(., .$YEAR) %>%
  lapply(., function(x) { unique(x$SP_CODE)})

# do dominant species change in time?
spp_turn <- 
  dom_spp %>%
  split(., .$SITE) %>%
  lapply(., function(x) { unique(x$SP_CODE) %>% length()}) %>%
  unlist() %>%
  tibble::enframe(., name = "SITE", value = "spp_turn")

full_join(gam_no_kelp, spp_turn) %>%
  ggplot(data = .,
         mapping = aes(x = gamma_diversity, y = spp_turn)) +
  geom_point()



