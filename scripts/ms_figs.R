
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


# simulation model

# load the data
mod.plot <- read_csv(file = "analysis_data/sim_mod_a_data.csv")

# plot interspecific competition legend
x <- 
  mod.plot %>% filter(disp == "disp.lim", env.con == "hom") %>%
  rename(`mean alpha     ` = a.m) %>%
  ggplot(data = .,
         mapping = aes(x = richness, y = functioning, group = id, colour = `mean alpha     `)) +
  geom_jitter(width = 0.5, alpha = 0.1) +
  scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
  guides(color = guide_colourbar(title.position = "left", 
                                 title.vjust = 1,
                                 frame.colour = "black", 
                                 ticks.colour = NA,
                                 barwidth = 4.5,
                                 barheight = 0.4)) +
  theme_meta() +
  theme(legend.direction="horizontal",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7))

ic.l <- gglegend(x)
plot(ic.l)


# function for plots related to scenario 1 and 3
p.sc.1.3.s <- 
  function(p.dat) {
    p1 <- 
      ggplot(data = p.dat ,
             mapping = aes(x = local.sp.pool, y = functioning, group = id, colour = a.m)) +
      geom_jitter(width = 0.5, alpha = 0.1, shape = 16) +
      geom_smooth(method = "lm", se = FALSE, size = 0.001, alpha = 0.01) +
      xlab(x2) +
      ylab(y1) +
      scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
      theme_meta() +
      theme(legend.position = "none")
    
    p2 <- 
      ggplot(data = p.dat,
             mapping = aes(x = richness, y = functioning, group = id, colour = a.m)) +
      geom_jitter(width = 0.5, alpha = 0.1, shape = 16) +
      geom_smooth(method = "lm", se = FALSE, size = 0.001, alpha = 0.01) +
      xlab(x1) +
      ylab(y1) +
      scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
      theme_meta() +
      theme(legend.position = "none")
    
    
    p.t <- 
      ggarrange(p1, p2, ic.l, nrow = 3, ncol = 1,
                labels = c("a", "b", ""),
                font.label = list(size = 9, color = "black", face = "plain", family = NULL),
                heights = c(1, 1, 0.2))
    
    return(p.t)
    
    
  }

# use the function to create the plots

# scenario 1
f.sc1.s <- 
  p.sc.1.3.s(p.dat = filter(mod.plot, disp == "disp.lim", env.con == "hom"))

# scenario 3
f.sc3.s <- 
  p.sc.1.3.s(p.dat = filter(mod.plot, disp == "disp.lim", env.con == "het"))


# function for plots related to scenario 2 and 4
p.sc.2.4.s <- 
  function(p.dat) {
    
    p.h <- 
      p.dat %>%
      group_by(id) %>%
      filter(patch == first(patch)) %>%
      ungroup() %>%
      ggplot(data = .,
             mapping = aes(x = slope)) +
      geom_histogram(fill = viridis::viridis(n = 1, begin = 0.5, end = 0.5, option = "C")) +
      geom_vline(xintercept = 0, colour = "red", linetype = "dashed", size = 1) +
      scale_colour_viridis_d(option = "C") +
      xlab(x3) +
      ylab(y3) +
      theme_meta()
    
    p2 <- 
      ggplot(data = p.dat,
             mapping = aes(x = richness, y = functioning, group = id, colour = a.m)) +
      geom_jitter(width = 0.5, alpha = 0.1, shape = 16) +
      geom_smooth(method = "lm", se = FALSE, size = 0.01, alpha = 0.01) +
      xlab(x1) +
      ylab(y1) +
      scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
      theme_meta() +
      theme(legend.position = "none") +
      theme(axis.title.y = element_text(colour = "black", size = 10, face = "plain", margin=margin(0,20,0,0,"pt")))
    
    
    p.t <- 
      ggarrange(p2, p.h, ic.l, nrow = 2, ncol = 2,
                labels = c("a", "b"),
                font.label = list(size = 9, color = "black", face = "plain", family = NULL),
                widths = c(1.3, 1),
                heights = c(1, 0.2))
    
    return(p.t)
    
  }

# use the function to create the plots

# scenario 2
f.sc2.s <- 
  p.sc.2.4.s(p.dat = filter(mod.plot, disp == "comp.equal", env.con == "hom"))

# scenario 4
f.sc4.s <- 
  p.sc.2.4.s(p.dat = filter(mod.plot, disp == "comp.equal", env.con == "het"))


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


# experiments: scenario 1
f.sc1.exp.1 <- 
  ggplot(data = exp.dat,
       mapping = aes(x = sown.diversity, y = biomass, colour = location)) +
  geom_jitter(width = 0.5, alpha = 0.5, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x2) +
  ylab(y2) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")

f.sc1.exp.2 <- 
  ggplot(data = exp.dat,
         mapping = aes(x = realised.diversity, y = biomass, colour = location)) +
  geom_jitter(width = 0.5, alpha = 0.5, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x1) +
  ylab(y2) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")

f.sc1.exp <- 
  ggarrange(f.sc1.exp.1, f.sc1.exp.2, nrow = 1, ncol = 2,
          labels = c("a", "b"),
          font.label = list(size = 9, color = "black", face = "plain", family = NULL),
          heights = c(1, 1, 0.2))


# experiments: scenario 1

# prepare the data
data.sc.12.exp <- 
  exp.dat %>%
  group_by(location, sown.diversity) %>%
  filter( (max(realised.diversity)-min(realised.diversity)) >= 1 ) %>%
  ungroup() %>%
  mutate(loc.sown.div = paste(location, sown.diversity, sep = "."))


f.sc2.exp.1 <- 
  ggplot(data = data.sc.12.exp,
       mapping = aes(x = realised.diversity, y = biomass, group = loc.sown.div,
                     colour = location, size = sown.diversity)) +
  geom_jitter(width = 0.5, alpha = 0.5, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x1) +
  ylab(y2) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
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
          widths = c(1, 1),
          heights = c(1, 0.2))

ggsave(filename = here("figures/ms_fig_3.pdf"), 
       plot = f.sc2.exp, width = 11, height = 6, units = "cm")




ggarrange(f.sc1.s, f.sc2.exp, nrow = 1, ncol = 2,
          labels = NULL, widths = c(0.9, 1))



### fig. 3: scenario 1
fig.3 <- 
  ggarrange(f.sc1.s, f.sc1.exp, nrow = 1, ncol = 2,
            labels = NULL, widths = c(0.9, 1))

ggsave(filename = here("figures/ms_fig_3.pdf"), 
       plot = fig.3, width = 11, height = 11, units = "cm")

### fig. 4: scenario 2
fig.4 <- 
  ggarrange(f.sc2.s, f.sc2.exp, nrow = 2, ncol = 1,
            labels = NULL, widths = c(1, 1))

ggsave(filename = here("figures/ms_fig_4.pdf"), 
       plot = fig.4, width = 11, height = 12, units = "cm")

### fig. 5/supplementary material figure





