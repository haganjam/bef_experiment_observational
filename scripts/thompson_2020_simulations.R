
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: run the Thompson model for a given set of parameters

library(here)
library(dplyr)

# set the source of the bef.sim function
source(here("scripts/thompson_2020_model.R"))
source(here("scripts/function_plotting_theme.R"))

# sim1: homogeneous environmental conditions
sim.pars <- 
  expand.grid(replicate = c(1:10),
              a.m = c(0.2, 0.4, 0.6, 0.8, 1),
              min.lsp = c(5),
              max.lsp = c(30),
              disp = c("comp.equal", "disp.lim"),
              env.con = c("hom", "het"))

# add how many replicate patches per species pool in the model
sim.pars <- 
   sim.pars %>%
   mutate(n.r = if_else(disp == "comp.equal", 50, 10))

# add an id column
sim.pars$id <- as.character(1:nrow(sim.pars))

# set a list to gather the output
sim.out <- vector("list", length = nrow(sim.pars))

for(i in 1:nrow(sim.pars)) {
  
   z <- 
     bef.sim(lsp = seq(sim.pars[i,]$max.lsp, sim.pars[i,]$min.lsp, by = -5),
            lsp.type = sim.pars[i,]$disp,
            reps = sim.pars[i, ]$n.r,
            rsp = 50,
            t_steps = 500,
            rmax = 5, sd.op = 0.25,
            n0 = 0.5,
            sim.comp = "sym",
            a_mean = sim.pars[i, ]$a.m, a_sd = 0.2, a_min = 0, a_max = 1.2, a_spp = 1,
            het.hom = sim.pars[i, ]$env.con,
            ext.thresh = 0.2)
   
   z$id <- sim.pars[i, ]$id 
   
   sim.out[[i]] <- z
  
}


# join the dataframes
mod.out <- full_join(bind_rows(sim.out, .id = "id"), sim.pars, by = "id")

# write this into a csv file


# get the realised diversity - function slopes
c.e <- split(mod.out, mod.out$id)
est <- 
   sapply(c.e, function(x) {
   
   x1 <- x$richness
   y1 <- x$functioning
   e1 <- x$env
   
   if ("hom" %in% x$env.con) {
      lm1 <- lm(scale(y1)[,1] ~ scale(x1)[,1] )
      return(as.numeric(lm1$coefficients[2]))
   } else if ("het" %in% x$env.con) {
      lm1 <- lm(scale(y1)[,1] ~ scale(x1)[,1] + scale(e1)[,1])
      return(as.numeric(lm1$coefficients[2]))
   } else {
      stop("error")
   }
   
}
)

df.est <- data.frame(id = names(c.e), slope = est)

# join these slope estimates
mod.plot <- full_join(mod.out, df.est, by = "id")



# load plotting libraries
library(ggplot2)
library(ggpubr)
library(viridis)

# axis labels
x1 <- c("realised diversity")
x2 <- c("local species pool diversity")
x3 <- c("realised div. - function est.")
y1 <- c("ecosystem function")

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
      geom_smooth(method = "lm", se = FALSE, size = 0.01, , alpha = 0.1) +
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




