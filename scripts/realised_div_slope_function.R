
# data is a dataframe with the final time-point of the experiment/model with three variables (names must match):
# 1. community_biomass
# 2. species_pool
# 3. realised_richness

# reps is the the number of replicates per species pool combination (default = 30)

# plots is the number of plots to draw randomly (default = 12)

slope_est_func <- function(data, reps = 30, plots = 12) {
  
  if(! "broom" %in% installed.packages()[,1]) stop(
    "this function requires vegan to be installed"
  )
  
  if(! "dplyr" %in% installed.packages()[,1]) stop(
    "this function requires vegan to be installed"
  )
  
  g <- expand.grid(sort(unique(data$species_pool)), 
                   sort(unique(data$species_pool), decreasing = TRUE) )
  
  g <- 
    g %>%
    dplyr::filter(Var1 <= Var2)
  
  est_out <- vector("list", length = nrow(g))
  
  for (s in c(1:nrow(g)) ) {
    
    x <- 
      data %>%
      dplyr::filter(species_pool >= g$Var1[s],
                    species_pool <= g$Var2[s])
    
    
    rep_out <- vector("list", length = reps)
    
    for (i in c(1:reps) ) {
      
      y <- x[sample(1:nrow(x), size = plots), ]
      
      y <- 
        y %>%
        dplyr::mutate(community_biomass = as.numeric(scale(community_biomass, center = TRUE, scale = TRUE)),
                      realised_richness = as.numeric(scale(realised_richness, center = TRUE, scale = TRUE)))
      
      lmx <- lm(community_biomass ~ realised_richness, data = y)
      
      z <- broom::tidy(lmx)
      
      z <-
        z %>%
        dplyr::mutate(r2 = summary(lmx)$r.squared) %>%
        dplyr::mutate(realised_richness_min = min(x$realised_richness),
                      realised_richness_max = max(x$realised_richness)) %>%
        dplyr::mutate(realised_richness_range = (realised_richness_max - realised_richness_min) )
      
      rep_out[[i]] <- z
      
    }
    
    est_out[[s]] <- 
      dplyr::bind_rows(rep_out, .id = "rep") %>%
      dplyr::mutate(sp_low = g$Var1[s],
                    sp_upp = g$Var2[s]) %>%
      dplyr::mutate(sp_range = (sp_upp - sp_low) )
    
  }
  
  # bind this output into a dataframe
  ran_est_out <- 
    dplyr::bind_rows(est_out, .id = "sowndiv_comb")
  
  # remove duplicate rows
  ran_est_out <- 
    dplyr::distinct(ran_est_out)
  
  # get only the species richness slope and remove any where realised diversity range was below 1
  est_all <- 
    ran_est_out %>%
    dplyr::filter(term == "realised_richness", realised_richness_range > 1)
  
  return(est_all)
  
}





