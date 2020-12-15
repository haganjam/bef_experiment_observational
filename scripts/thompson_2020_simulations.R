
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: run the Thompson model for a given set of parameters

# deal with the replication issue!


# sim1: homogeneous environmental conditions
sim.pars <- 
  expand.grid(replicate = c(1:5),
              a.m = c(0.2, 0.4, 0.6, 0.8, 1),
              min.lsp = c(5),
              max.lsp = c(30),
              disp = c("no.disp.lim", "comp.equal", "disp.lim"))

# add an id column
sim.pars$id <- 1:nrow(sim.pars)

# set a list to gather the output
sim.out <- vector("list", length = nrow(sim.pars))

for(i in 1:nrow(sim.pars)) {
  
   z <- 
     bef.sim(lsp = seq(sim.pars[i,]$max.lsp, sim.pars[i,]$min.lsp, by = -5),
            lsp.type = sim.pars[i,]$disp,
            reps = 50,
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






