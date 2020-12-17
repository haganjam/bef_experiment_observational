
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: run the Thompson model for a given set of parameters

# load the relevant libraries
library(here)
library(dplyr)

# make a folder to export the simulation data
if(! dir.exists(here("analysis_data"))){
   dir.create(here("analysis_data"))
}

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

# write this into a csv file
write_csv(x = mod.plot, file = here("analysis_data/sim_mod_a_data.csv"))






