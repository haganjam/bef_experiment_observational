
# data is a dataframe with the final time-point of the experiment/model with three variables (names must match):
# 1. community_biomass
# 2. species_pool
# 3. realised_richness

# reps is the the number of replicates to draw (default = 250)

# plots is proportion of time points to draw randomly (default = 0.3)

slope_est_func <- function(data, reps = 250, plots = 0.3) {
    
    if(! "broom" %in% installed.packages()[,1]) stop(
      "this function requires broom to be installed"
    )
    
    if(! "dplyr" %in% installed.packages()[,1]) stop(
      "this function requires dplyr to be installed"
    )
  
  sp <- sort(unique(data$species_pool), decreasing = FALSE)
  
  g <- matrix(c(min(sp), sp[median(1:length(sp))], min(sp), 
                sp[median(1:length(sp))], max(sp), max(sp)), 
              nrow = 3, ncol = 2)
    
    est_out <- vector("list", length = nrow(g))
    
    for (s in c(1:nrow(g)) ) {
      
      x <- 
        data %>%
        dplyr::filter(species_pool >= g[s, 1],
                      species_pool <= g[s, 2])
      
      
      rep_out <- vector("list", length = reps)
      
      for (i in c(1:reps) ) {
        
        y <- x[sample(1:nrow(x), size = (nrow(x)*plots), replace = TRUE ), ]
        
        y <- 
          y %>%
          dplyr::mutate(community_biomass = as.numeric(scale(community_biomass, center = TRUE, scale = TRUE)),
                        realised_richness = as.numeric(scale(realised_richness, center = TRUE, scale = TRUE)))
        
        if (any(is.na(y$realised_richness), na.rm = FALSE) == TRUE) {
          
          lmx <- lm(1 ~ 1)
          
        } else {
          
          lmx <- lm(community_biomass ~ realised_richness, data = y, singular.ok = TRUE)
          
          z <- broom::tidy(lmx)
          
          z <-
            z %>%
            dplyr::mutate(r2 = summary(lmx)$r.squared,
                          n = nrow(y)) %>%
            dplyr::mutate(realised_richness_min = min(x$realised_richness),
                          realised_richness_max = max(x$realised_richness)) %>%
            dplyr::mutate(realised_richness_range = (realised_richness_max - realised_richness_min) )
          
          rep_out[[i]] <- z
          
        }
        
      }
      
      est_out[[s]] <- 
        dplyr::bind_rows(rep_out, .id = "rep") %>%
        dplyr::mutate(sp_low = g[s, 1],
                      sp_upp = g[s, 2]) %>%
        dplyr::mutate(sp_range = (sp_upp - sp_low) )
      
    }
    
    # bind this output into a dataframe
    ran_est_out <- 
      dplyr::bind_rows(est_out, .id = "spp_pool")
    
    # remove duplicate rows
    ran_est_out <- 
      dplyr::distinct(ran_est_out)
    
    # remove NA models
    ran_est_out <- 
      filter(ran_est_out, !is.na(std.error))
    
    # remove slopes that were estimated from less than 15 data points
    ran_est_out <- 
      filter(ran_est_out, n >= 15)
    
    # get only the species richness slope and remove any where realised diversity range was below 1
    est_all <- 
      ran_est_out %>%
      dplyr::filter(term == "realised_richness", realised_richness_range > 1)
    
    return(est_all)
    
}


 





