
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Plot the figures for the manuscript

# load plotting libraries
library(ggplot2)
library(ggpubr)
library(viridis)
library(here)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))


# simulation model

# load the data
mod.plot <- read_csv(file = "analysis_data/sim_mod_a_data.csv")

# axis labels
x1 <- c("realised diversity")
x2 <- c("local species pool diversity")
x3 <- c("realised div. - function est.")
y1 <- c("ecosystem function")
y2 <- expression(paste("community biomass (g ",  " m"^"-2", ")") )

# interspecific competition legend
x <- 
  mod.plot %>% filter(disp == "disp.lim", env.con == "hom") %>%
  rename(`interspecific competition` = a.m) %>%
  ggplot(data = .,
         mapping = aes(x = richness, y = functioning, group = id, colour = `interspecific competition`)) +
  geom_jitter(width = 0.5, alpha = 0.1) +
  scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
  guides(color = guide_colourbar(title.position = "top", 
                                 title.vjust = 1,
                                 frame.colour = "black", 
                                 ticks.colour = NA,
                                 barwidth = 10,
                                 barheight = 0.5)) +
  theme_meta() +
  theme(legend.direction="horizontal",
        #legend.justification=c(1, 0), 
        legend.key.width=unit(1, "lines"), 
        legend.key.height=unit(1, "lines"),
        # plot.margin = unit(c(3, 1, 0.5, 0.5), "lines"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

ic.l <- gglegend(x)

# scenario 1 and 3
sim.plot.sc.1.3 <- 
  function(p.dat, x.min, y.max) {
    p1 <- 
      ggplot(data = p.dat ,
             mapping = aes(x = local.sp.pool, y = functioning, group = id, colour = a.m)) +
      geom_jitter(width = 0.5, alpha = 0.05) +
      geom_smooth(method = "lm", se = FALSE, size = 0.01, alpha = 0.1) +
      xlab(x2) +
      ylab(y1) +
      scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
      theme_meta() +
      theme(legend.position = "none")
    
    p.inset <- 
      p.dat %>%
      group_by(id) %>%
      filter(patch == first(patch)) %>%
      ungroup() %>%
      ggplot(data = .,
             mapping = aes(x = slope)) +
      geom_histogram(fill = viridis::viridis(n = 1, begin = 0.5, end = 0.5, option = "C")) +
      geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
      scale_colour_viridis_d(option = "C") +
      xlab(x3) +
      ylab(NULL) +
      theme_meta() +
      theme(axis.title.x = element_text(size = 8, face = "plain"),
            axis.title.y = element_text(size = 8, face = "plain"),
            axis.text.x = element_text(size = 8, face = "plain"),
            axis.text.y = element_text(size = 8, face = "plain"))
    
    p2 <- 
      ggplot(data = p.dat,
             mapping = aes(x = richness, y = functioning, group = id, colour = a.m)) +
      geom_jitter(width = 0.5, alpha = 0.05) +
      geom_smooth(method = "lm", se = FALSE, size = 0.01, alpha = 0.1) +
      xlab(x1) +
      ylab(y1) +
      scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
      theme_meta() +
      theme(legend.position = "none") +
      annotation_custom(grob = ggplotGrob(p.inset), 
                        xmin = x.min, xmax = Inf, ymin=-Inf, ymax=y.max)
    
    
    p.t <- 
      ggarrange(p1, p2, ic.l, nrow = 3, ncol = 1,
                labels = c("a", "b", ""),
                font.label = list(size = 12, color = "black", face = "plain", family = NULL),
                heights = c(1, 1, 0.2))
    
    return(p.t)
    
    
  }

# scenario 1
f.sc1 <- 
  sim.plot.sc.1.3(p.dat = filter(mod.plot, disp == "disp.lim", env.con == "hom"),
                  x.min = 5.5, y.max = 7)
f.sc1

# scenario 3
f.sc3 <- 
  sim.plot.sc.1.3(p.dat = filter(mod.plot, disp == "disp.lim", env.con == "het"),
                  x.min = 5.5, y.max = 7)
f.sc3


# scenario 2 and 4
sim.plot.sc.2.4 <- 
  function(p.dat) {
    
    p.h <- 
      p.dat %>%
      group_by(id) %>%
      filter(patch == first(patch)) %>%
      ungroup() %>%
      ggplot(data = .,
             mapping = aes(x = slope)) +
      geom_histogram(fill = viridis::viridis(n = 1, begin = 0.5, end = 0.5, option = "C")) +
      geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
      scale_colour_viridis_d(option = "C") +
      xlab(x3) +
      ylab(NULL) +
      theme_meta()
    
    p2 <- 
      ggplot(data = p.dat,
             mapping = aes(x = richness, y = functioning, group = id, colour = a.m)) +
      geom_jitter(width = 0.5, alpha = 0.1) +
      geom_smooth(method = "lm", se = FALSE, size = 0.1) +
      xlab(x1) +
      ylab(y1) +
      scale_colour_viridis_c(option = "C", begin = 0, end = 0.9) +
      theme_meta() +
      theme(legend.position = "none")
    
    
    p.t <- 
      ggarrange(p2, p.h, nrow = 1, ncol = 2,
                labels = c("a", "b"),
                font.label = list(size = 12, color = "black", face = "plain", family = NULL),
                widths = c(1.5, 1))
    
    return(p.t)
    
    
  }

# scenario 2
f.sc2 <- 
  sim.plot.sc.2.4(p.dat = filter(mod.plot, disp == "comp.equal", env.con == "hom"))
f.sc2

# scenario 4
f.sc4 <- 
  sim.plot.sc.2.4(p.dat = filter(mod.plot, disp == "comp.equal", env.con == "het"))
f.sc4


# Jena and BIODEPTH data

# load the data
exp.dat <- read_csv(file = "analysis_data/bio_exp_dat.csv")


# scenario 1
ggplot(data = exp.dat,
       mapping = aes(x = sown.diversity, y = biomass, colour = location)) +
  geom_jitter(width = 0.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(l2) +
  ylab(l1) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")


ggplot(data = exp.dat,
       mapping = aes(x = realised.diversity, y = biomass, colour = location)) +
  geom_jitter(width = 0.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x1) +
  ylab(l1) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")


# scenario 2
sc.2.exp.dat <- 
  exp.dat %>%
  group_by(location, sown.diversity) %>%
  filter( (max(realised.diversity)-min(realised.diversity)) >= 1 ) %>%
  ungroup() %>%
  mutate(loc.sown.div = paste(location, sown.diversity, sep = "."))


ggplot(data = sc.2.exp.dat,
       mapping = aes(x = realised.diversity, y = biomass, group = loc.sown.div,
                     colour = location, size = sown.diversity)) +
  geom_jitter(width = 0.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  xlab(x1) +
  ylab(l1) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.9) +
  theme_meta() +
  theme(legend.position = "none")


# get the realised diversity - function slopes
est.sc.2 <- 
  sapply(split(sc.2.exp.dat, sc.2.exp.dat$loc.sown.div), function(x) {
    
    x1 <- x$realised.diversity
    y1 <- x$biomass
    
    lm1 <- lm(scale(y1)[,1] ~ scale(x1)[,1] )
    
    return(as.numeric(lm1$coefficients[2]))
    
  }
  )

ggplot(mapping = aes(x = est.sc.2)) +
  geom_histogram(fill = viridis::viridis(n = 1, begin = 0.5, end = 0.5, option = "C")) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_colour_viridis_d(option = "C") +
  xlab(x3) +
  ylab(NULL) +
  theme_meta()






