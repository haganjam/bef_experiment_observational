
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Analyse Adler et al.'s (2018) data on competition coefficients of plants

# load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# load in the data
ad.dat.raw <- read_csv(here("raw_data/CompetitionRegressionData071517.csv") )
spec(ad.dat.raw)

View(ad.dat.raw)

# select relevant columns
ad.dat <- 
  ad.dat.raw %>%
  select(`Paper Key #`, Focal.Species, Comp.Species, 
         `competition coefficient`, `Negative coefs mean competition`)

# make a new column of intra vs. interspecific competition
ad.dat <- 
  ad.dat %>%
  mutate(intra.inter = if_else(Focal.Species == Comp.Species, "intra", "inter"))

# convert the negative competition coefficients to positive if they mean competition
ad.dat <- 
  ad.dat %>%
  mutate(`Negative coefs mean competition` = if_else(`Negative coefs mean competition` == "Yes", -1, 1))

ad.dat <- 
  ad.dat %>%
  mutate(`competition coefficient` = `competition coefficient`*`Negative coefs mean competition`)

summary(ad.dat)

# examine these extreme values
ad.dat.raw %>%
  filter(`competition coefficient` > 5 | `competition coefficient` < -5) %>%
  View()

# get rid of the very extreme values
ad.dat <- 
  ad.dat %>%
  filter(`competition coefficient` < 1) %>%
  filter(`competition coefficient` > -1)

# check the summary statistics
ad.dat %>%
  group_by(intra.inter) %>%
  summarise(mean.a = mean(`competition coefficient`, na.rm = TRUE))

# plot the distribution of competition coefficients for intra vs. inter species
ggplot(data = ad.dat %>% filter(intra.inter == "inter"),
       mapping = aes(x = `competition coefficient`)) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 0) +
  theme_bw()





