
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Plot the figures for the manuscript

# load plotting libraries
library(ggplot2)
library(ggpubr)
library(viridis)
library(here)
library(readr)
library(dplyr)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))

# axis labels
x1 <- c("realised diversity")
x2 <- c("initial diversity")
x3 <- c("realised div. - function est.")
y1 <- c("function")
y2 <- expression(paste("biomass (g ",  " m"^"-2", ")") )
y3 <- c("frequency")


# Jena and BIODEPTH data

# load the data
exp.dat <- read_csv(file = "analysis_data/bio_exp_dat.csv")

# remove the Jena data
exp.dat <- 
  exp.dat %>%
  filter(location != "Jena")


# plot an experiment legend
y <- 
  ggplot(data = exp.dat,
       mapping = aes(x = sown.diversity, y = biomass, fill = location, colour = location) ) +
  geom_point() +
  scale_colour_viridis_d(option = "C", end = 0.9) +
  guides(fill = guide_legend(nrow=2,byrow=TRUE)) +
  theme_meta() +
  theme(legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.6,"line"),
        legend.text = element_text(colour = "black", size=6, face = "plain"),
        legend.spacing.x = unit(1.25, 'mm'))

exp.l <- gglegend(y)


# prepare the data
data.sc.12.exp <- 
  exp.dat %>%
  filter(realised.diversity > 0) %>%
  group_by(location, sown.diversity) %>%
  filter( (max(realised.diversity)-min(realised.diversity)) >= 1 ) %>%
  ungroup() %>%
  mutate(loc.sown.div = paste(location, sown.diversity, sep = "."))

# if there are only two sown.diversity levels, then we want at least multiple replicates at each realised diversity level
data.sc.12.exp <- 
  data.sc.12.exp %>%
  group_by(location, sown.diversity) %>%
  mutate(n.sd = length(unique(realised.diversity))) %>%
  ungroup() %>%
  group_by(location, sown.diversity, realised.diversity) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter( !(n.sd == 2 & n == 1) ) %>%
  group_by(location, sown.diversity) %>%
  filter( (max(realised.diversity)-min(realised.diversity)) >= 1 ) %>%
  ungroup()
  
  
f.sc2.exp.1 <- 
  ggplot(data = data.sc.12.exp,
       mapping = aes(x = (realised.diversity), y = sqrt(biomass), group = loc.sown.div,
                     colour = location)) +
  geom_point(shape = 16, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x1) +
  ylab(y2) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  # facet_wrap(~loc.sown.div, scales = "free") +
  theme_meta() + 
  theme(legend.position = "none")


# get the realised diversity - function slopes
est.sc.2.exp <- 
  sapply(split(data.sc.12.exp, data.sc.12.exp$loc.sown.div), function(x) {
    
    x1 <- x$realised.diversity
    y1 <- x$biomass
    
    lm1 <- lm(scale(y1)[,1] ~ scale(x1)[,1] )
    
    return(as.numeric(lm1$coefficients[2]))
    
  }
  )

f.sc2.exp.2 <- 
  ggplot(mapping = aes(x = est.sc.2.exp)) +
  geom_histogram(fill = viridis::viridis(n = 1, begin = 0.5, end = 0.5, option = "C")) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_colour_viridis_d(option = "C") +
  xlab(x3) +
  ylab(y3) +
  theme_meta()

f.sc2.exp <- 
  ggarrange(f.sc2.exp.1, f.sc2.exp.2, exp.l, nrow = 2, ncol = 2,
          labels = c("a", "b", ""),
          font.label = list(size = 9, color = "black", face = "plain", family = NULL),
          widths = c(1.3, 1),
          heights = c(1, 0.2))

ggsave(filename = here("figures/ms_fig_3.pdf"), 
       plot = f.sc2.exp, width = 11, height = 7, units = "cm")







