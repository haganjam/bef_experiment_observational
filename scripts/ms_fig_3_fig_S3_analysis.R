
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Plot Fig. 3

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
x1 <- c("Realised diversity")
x3 <- c("Realised div. - biomass est.")
y2 <- expression( sqrt(paste("Biomass (g ",  " m"^"-2", ")")) )
y3 <- c("Frequency")


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


# prepare the data:
# remove plots where realised diversity is zero
# for each sown diversity, subset the treatments where there is at least a realised diversity gradient of one
plot.dat <- 
  exp.dat %>%
  filter(realised.diversity > 0) %>%
  group_by(location, sown.diversity) %>%
  filter( (max(realised.diversity)-min(realised.diversity)) >= 1 ) %>%
  ungroup() %>%
  mutate(loc.sown.div = paste(location, sown.diversity, sep = "."))

# if there are only two realised diversity levels:
# then we want at least multiple replicates at each realised diversity level
# i.e. at least two replicate plots per realised diversity value unless there are multiple realised diversity values
plot.dat <- 
  plot.dat %>%
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
  

# plot figure 3a
  
fig.3a <- 
  ggplot(data = plot.dat,
       mapping = aes(x = (realised.diversity), y = sqrt(biomass), group = loc.sown.div,
                     colour = location)) +
  geom_point(shape = 16, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x1) +
  ylab(y2) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() + 
  theme(legend.position = "none")

fig.3a

# get the realised diversity - function slopes
est.sc <- 
  sapply(split(plot.dat, plot.dat$loc.sown.div), function(x) {
    
    x1 <- x$realised.diversity
    y1 <- x$biomass
    
    lm1 <- lm(scale(y1)[,1] ~ scale(x1)[,1] )
    
    return(as.numeric(lm1$coefficients[2]))
    
  }
  )

# plot figure 3b
fig.3b <- 
  ggplot(mapping = aes(x = est.sc)) +
  geom_histogram(fill = viridis::viridis(n = 1, begin = 0.5, end = 0.5, option = "C")) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
  scale_colour_viridis_d(option = "C") +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  xlab(x3) +
  ylab(y3) +
  theme_meta()

fig.3b

# combine figure 3a and 3b
fig.3 <- 
  ggarrange(fig.3a, fig.3b, exp.l, nrow = 2, ncol = 2,
          labels = c("a", "b", ""),
          font.label = list(size = 9, color = "black", face = "plain", family = NULL),
          widths = c(1.3, 1),
          heights = c(1, 0.2))

ggsave(filename = here("figures/fig_3.pdf"), 
       plot = fig.3, width = 11, height = 7, units = "cm")


# fig. s3: plot the relationship between initial diversity and realised diversity
cor_bio <- 
  plot.dat %>%
  summarise(Pearson_r = cor(sown.diversity, realised.diversity, method = "pearson") %>%
              round(., 2)) %>%
  mutate(Pearson_r = paste("r = ", Pearson_r, sep = ""))

fig.s3 <- 
  ggplot() +
  geom_jitter(data = plot.dat,
             mapping = aes(x = sown.diversity, y = (realised.diversity),
                           colour = location), 
             shape = 16, size = 1.5, width = 0.1) +
  geom_smooth(data = plot.dat,
              mapping = aes(x = sown.diversity, y = (realised.diversity)),
              method = "lm", se = TRUE, size = 0.5, colour = "black") +
  xlab("Initial diversity") +
  ylab(x1) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() + 
  geom_text(
    data    = cor_bio,
    mapping = aes(x = -Inf, y = +Inf, label = Pearson_r),
    vjust = +1.7,
    hjust = -0.3,
    size = 4) +
  theme(legend.position = "bottom")

ggsave(filename = here("figures/fig_S3.pdf"), 
       plot = fig.s3, width = 6, height = 6, units = "cm",
       dpi = 450)


### END
