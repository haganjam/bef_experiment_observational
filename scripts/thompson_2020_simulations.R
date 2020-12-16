
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: run the Thompson model for a given set of parameters

library(here)
library(dplyr)
library(ggplot2)

# set the source of the bef.sim function
source(here("scripts/thompson_2020_model.R"))


# sim1: homogeneous environmental conditions
sim.pars <- 
  expand.grid(replicate = c(1:10),
              a.m = c(0.2, 0.4, 0.6, 0.8, 1),
              min.lsp = c(5),
              max.lsp = c(30),
              disp = c("comp.equal", "disp.lim"))

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
            het.hom = "hom",
            ext.thresh = 0.2)
   
   z$id <- sim.pars[i, ]$id 
   
   sim.out[[i]] <- z
  
}

# join the dataframes
mod.out <- full_join(bind_rows(sim.out, .id = "id"), sim.pars, by = "id")

# plot out disp.lim
mod.out %>%
   filter(disp == "disp.lim") %>%
   ggplot(data = .,
          mapping = aes(x = local.sp.pool, y = functioning, group = id, colour = a.m)) +
   geom_jitter(width = 0.5, alpha = 0.1) +
   geom_smooth(method = "lm", se = FALSE, size = 0.5) +
   scale_colour_viridis_c() +
   theme_bw() +
   theme(legend.position = "bottom")

mod.out %>%
   filter(disp == "disp.lim") %>%
   ggplot(data = .,
          mapping = aes(x = richness, y = functioning, group = id, colour = a.m)) +
   geom_jitter(width = 0.2, alpha = 0.1) +
   geom_smooth(method = "lm", se = FALSE, size = 0.5) +
   scale_colour_viridis_c() +
   theme_bw() +
   theme(legend.position = "bottom")

# plot out comp.equal
mod.out %>%
   filter(disp == "comp.equal") %>%
   ggplot(data = .,
          mapping = aes(x = richness, y = functioning, group = id, colour = a.m)) +
   geom_jitter(width = 0.2, alpha = 0.01) +
   geom_smooth(method = "lm", se = FALSE, size = 0.5) +
   scale_colour_viridis_c() +
   theme_bw() +
   theme(legend.position = "bottom")

# subset out these data
c.e <- filter(mod.out, disp == "comp.equal")

est <- sapply(split(c.e, c.e$id), function(x) {
 
   x1 <- x$richness
   y1 <- x$functioning
   
   lm1 <- lm(scale(y1)[,1] ~ scale(x1)[,1] )
   
   return(as.numeric(lm1$coefficients[2]))
   
}
)
hist(est)




