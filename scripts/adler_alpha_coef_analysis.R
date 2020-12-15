
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
ad.dat <- read_csv(here("data/CompetitionRegressionData071517.csv") )
spec(ad.dat)

View(ad.dat)

# select relevant columns
ad.dat <- 
  ad.dat %>%
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

# get rid of the very extreme values
ad.dat <- 
  ad.dat %>%
  filter(`competition coefficient` < 5, `competition coefficient` > -5)

# check the summary statistics
ad.dat %>%
  group_by(intra.inter) %>%
  summarise(mean.a = mean(`competition coefficient`, na.rm = TRUE))

# plot the distribution of competition coefficients for intra vs. inter species
ggplot(data = ad.dat,
       mapping = aes(x = `competition coefficient`, colour = intra.inter, fill = intra.inter)) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 0) +
  theme_bw()





