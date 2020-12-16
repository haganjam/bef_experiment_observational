
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Function to implement a simulation model that can handle heterogeneity and different species pool distributions

# model is a reformulation of the model from Thompson et al. (2020), Ecology Letters

# explanation of the model parameters and their default values

# set the different species pools to simulate
# lsp.type = "disp.lim"
# lsp = c(5, 10, 15, 20, 25)
# lsp.type = "no.disp.lim"
# lsp = c(15)

# set the number of replicates per local species pool richness
# reps = 10

# set the number of species in the regional species pool
# rsp = 50

# set the number of time-steps
# t_steps = 5

# species niche
# set the maximum growth rate (i.e. when species matches the environmental optimum)
# rmax = 5
# set the environmental optima standard deviation (i.e. niche breadth)
# sd.op = 0.25

# set the starting abundance for each species
# n0 = 0.5

# competition coefficients
# symmetric competition matrix or not
# sim.comp = "sym"
# parameters of the truncated normal distribution from which to draw competition coefficients
# a_mean = 0.8
# a_sd = 0.2
# a_min = 0.2
# a_max = 1.2
# strength of intraspecific competition
# a_spp = 1

# environmental heterogeneity
# choose whether there should be heterogeneity ("het") or not ("hom") among patches
# het.hom = "hom"

# set up a function to run this simulation
bef.sim <- function(lsp = c(5, 10, 15, 20, 25),
                    lsp.type = "disp.lim",
                    reps = 10,
                    rsp = 50,
                    t_steps = 200,
                    rmax = 5, sd.op = 0.25,
                    n0 = 0.5,
                    sim.comp = "sym",
                    a_mean = 0.8, a_sd = 0.2, a_min = 0.2, a_max = 1.2, a_spp = 1,
                    het.hom = "hom",
                    ext.thresh = 0.2) {
  
  if(! "dplyr" %in% installed.packages()[,1]) stop(
    "this function requires dplyr to be installed"
  )
  
  if(! "tidyr" %in% installed.packages()[,1]) stop(
    "this function requires tidyr to be installed"
  )
  
  if(! "truncnorm" %in% installed.packages()[,1]) stop(
    "this function requires truncnorm to be installed"
  )
  
  # calculate the number of patches
  if (lsp.type == "disp.lim") {
    patches <- reps*length(lsp)
  } else if(lsp.type == "comp.equal" | lsp.type == "no.disp.lim") {
    patches <- reps
  } else {
    stop("error: specify parameter for lsp.type" )
  }
  
  # generate matrix of competition coefficients
  alpha <- matrix(truncnorm::rtruncnorm(n = rsp*rsp, a = a_min, b = a_max, mean = a_mean, sd = a_sd), rsp, rsp)
  
  # make the matrix symmetric if chosen
  if (sim.comp == "sym") {
    alpha[lower.tri(alpha)] = t(alpha)[lower.tri(alpha)]
  }
  
  # make all intraspecific competition coefficients equal to 1
  diag(alpha) <- rep(a_spp, rsp)
  
  # get environmental optima for each species in the regional species pool
  z <- runif(n = rsp, min = 0, max = 1)
  
  # get environmental conditions in each patch
  if (het.hom == "het") { 
    env.p <- runif(n = patches, min = 0, max = 1)
  } else if (het.hom == "hom") {
    env.p <- rep(runif(n = 1, min = 0, max = 1), times = patches)
  } else {
    stop("error: specify either het or hom for the parameter: het.hom" )
  }
  
  # assign a local species spool size to each patch
  
  if (lsp.type == "disp.lim") {
    lsp.p <- rep(lsp, each = reps)[sample(x = (1:patches), size = patches, replace = FALSE)]
  } else if(lsp.type == "no.disp.lim" | lsp.type == "comp.equal") {
    lsp.p <- rep(lsp[1], times = patches)
  } else {
    stop("error: specify a type of dispersal limitation and local species pool size(s)" )
  }
  
  
  # put the environmental conditions and local species pools into a list
  df.p <- data.frame(env = env.p, loc.sp.p = lsp.p)
  df.p <- split(df.p, 1:nrow(df.p))
  
  # use apply to run the model for each patch
  
  l.out <- 
    lapply(df.p, function(x) {
      
      # get the environmental conditions in that patch
      p1 <- x$env
      
      # get the local species pool size
      lsp1 <- x$loc.sp.p
      
      # get a set of species from the regional species pool
      if ( lsp.type == "disp.lim" | lsp.type == "no.disp.lim" ) {
        lsp.id <- sample(x = (1:rsp), size = lsp1)
      } else if (lsp.type == "comp.equal") {
        set.seed(8957425)
        y <- sample(x = (1:rsp), size = (lsp1 + 3) )
        set.seed(NULL)
        lsp.id <- sample(x = y, size = (sample(x = c(-1, 0, 1), size = 1) + lsp1)  )
      }
      
      # code a nested for loop: for each time and for each species
      
      # create a vector of starting values for each species in the regional species pool
      n_vals <- rep(0, times = rsp)
      
      # add starting values for species in the lsp
      n_vals[lsp.id] <- n0
      
      # create an output list of species abundances for each time point
      n_t <- vector("list", length = t_steps)
      n_t
      
      # fill the first time point with starting abundances
      n_t[[1]] <- n_vals
      
      # for each time point m
      for(m in seq(from = 2, to = t_steps, by = 1)){
        
        # for each species g
        for (g in 1:rsp ) {
          
          # get the growth rate for the species
          r <- rmax*exp( -( ( (z[g] - p1)/(2*sd.op) )^2 ) )
          
          # define the first term
          t1 <- n_t[[m-1]][g] 
          
          # dsecond term of the equation
          t2 <- r/(1 + sum( alpha[, g]*n_t[[m-1]] ))
          
          # use three terms in the full equation
          if (t1*t2 > 0) {
            n_t[[m]][g] <- truncnorm::rtruncnorm(n = 1, a = 0, b = Inf, mean = t1*t2, sd = 0.1)
          } else {
            n_t[[m]][g] <- 0
          }
          
          # if a species abundance drops below 0.2 it is considered extinct
          if (n_t[[m]][g] < ext.thresh) { n_t[[m]][g] <- 0 }
          
        }
        
      }
      
      # collapse this into a dataframe
      df_n_t <- as.data.frame(do.call(rbind, n_t))
      names(df_n_t) <- paste0("sp_", 1:rsp)
      
      # add a column for the time-point
      df_n_t$time <- seq(from = 1, to = t_steps, by = 1)
      
      # pull this into two columns
      df_n_t <- 
        df_n_t %>%
        tidyr::pivot_longer(cols = starts_with(match = "sp_"),
                            names_to = "species",
                            values_to = "abundance") %>%
        dplyr::arrange(time, species)
      
      return(df_n_t)
      
    } 
    )
  
  # bind the rows together
  bef.res <- bind_rows(l.out, .id = "patch")
  
  # add a column for the local species pool
  lsp.patch <- 
    bef.res %>%
    filter(time == min(time)) %>%
    group_by(patch) %>%
    summarise(local.sp.pool = sum(if_else(abundance > 0, 1, 0)), .groups = "drop")
  
  bef.func <- 
    bef.res %>%
    filter(time == max(time)) %>%
    group_by(patch, time) %>%
    summarise(richness = sum(if_else(abundance > 0, 1, 0)),
              functioning = sum(abundance), .groups = "drop")
  
  # join these data.frames together
  sim.proc <- full_join(bef.func, lsp.patch, by = "patch")
  
  # add a column for the environment
  sim.proc <- full_join(sim.proc, data.frame(patch = as.character(1:patches), 
                                             env = env.p),
                        by = "patch")
  
  return(sim.proc)
  
}








