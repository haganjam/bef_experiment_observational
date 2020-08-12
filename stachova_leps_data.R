
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Re-analysis of data extracted from Stachova and Leps (2010)

# load relevant libraries
library(readr)
library(gtools)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(viridis)
library(here)
library(vegan)
library(ggpubr)
library(truncnorm)

# create customised plotting theme
theme_meta <- function(base_size = 12, base_family = "") {
  theme(panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill="NA", color="black", size=0.75, linetype="solid"),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.ticks.length = unit(-0.16, "cm"),
        axis.title.x = element_text(colour ="black", size = 12, face = "plain", margin=margin(5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 12, face = "plain", margin=margin(0,5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=12, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=12, face = "plain", margin=margin(0,10,0,0,"pt")),
        axis.ticks.x = element_line(colour = "black", size = 0.4),
        axis.ticks.y = element_line(colour = "black", size = 0.4))
}


### code the simulation model

### code the Stachova and Leps (2010) simulation model (function = s_l_2010_mod)

# parameter description
# spp_n - number of species in the initial colonising pool
# runs - number of independent runs with different randomly drawn parameters (i.e. K, r and alphas)
# t - number of time-steps to run the model for
# n0 - starting abundance of all species
# m_alpha - mean alpha value when drawing from truncated normal
# sd_alpha - sd alpha value when drawing from truncated normal

# outputs a dataframe with:
# run (i.e. model run with randomly drawn parameter values)
# realised richness (i.e. number of species leftover)
# community biomass (i.e. sum of all species abundances)
# species pool (i.e. number of species initially allowed to colonise)

# these properties are only exported for the final time-step in the model

s_l_2010_mod <- 
  
  function(spp_n = 10, runs = 10, t = 20, n0 = 3, m_alpha = 1, sd_alpha = 0.2) {
    
    # output list for each model run
    run_out <- vector("list", length = runs)
    
    for (s in (1:runs) ) {
      
      # generate alpha values for each species pair
      # set up the permutations between species
      alpha <- 
        as.data.frame(gtools::permutations(n = spp_n, r = 2, v = c(1:spp_n), repeats.allowed = TRUE))
      
      names(alpha) <- c("j", "i")
      
      # add the alpha values to the alpha val dataframe
      alpha$alpha_vals <- 
        truncnorm::rtruncnorm(n = nrow(alpha), a = 0, b = 1.2, mean = m_alpha, sd = sd_alpha)
      
      # generate the carrying capacities for each species (K)
      k_vals <- runif(n = spp_n, min = 3, max = 150)
      
      # generate growth rates values (r)
      r_vals <- runif(n = spp_n, min = 0.01, max = 0.5)
      
      # code a nested for loop: for each time and for each species
      # create a vector of starting values for each species
      n_vals <- rep(n0, times = spp_n)
      
      # create an output list of species abundances for each time point
      n_t <- vector("list", length = t)
      n_t
      
      # fill the first time point with starting abundances
      n_t[[1]] <- n_vals
      
      # for each time point t
      for(m in seq(from = 2, to = t, by = 1)){
        
        # for each species k
        for (k in seq(from = 1, to = spp_n, by = 1)) {
          
          # first term in the equation
          t1 <- n_t[[m-1]][k] 
          
          # second term in the equation
          t2 <- (r_vals[k]*n_t[[m-1]][k])
          
          # code the influence of other species
          z <- alpha[(!(alpha$j %in% k) & alpha$i == k) | alpha$i == k, ]
          
          # third term in the equation
          t3 <- (1 - ( sum( (z$alpha_vals*n_t[[m-1]][z$j]) )/k_vals[k] ) )
          
          # complete equation
          n_t[[m]][k] <- t1 + (t2*t3)
          
          # if a species abundance drops below 0.2 it is considered extinct
          if (n_t[[m]][k] < 0.2) { n_t[[m]][k] <- 0 }
          
        }
        
      }
      
      # collapse this into a dataframe
      df_n_t <- as.data.frame(do.call(rbind, n_t))
      names(df_n_t) <- paste("sp_", 1:spp_n)
      
      # add a column for the time-point
      df_n_t$time <- seq(from = 1, to = t, by = 1)
      
      # pull this into two columns
      df_n_t <- 
        df_n_t %>%
        tidyr::pivot_longer(cols = starts_with(match = "sp_"),
                            names_to = "species",
                            values_to = "abundance") %>%
        dplyr::arrange(species, time)
      
      # summarise these data
      n_t_sum <- 
        df_n_t %>%
        dplyr::filter(time == last(time)) %>%
        dplyr::summarise(realised_richness = sum(if_else(abundance > 0, 1, 0)),
                         community_biomass = sum(abundance)) %>%
        dplyr::mutate(species_pool = spp_n)
      
      run_out[[s]] <- n_t_sum
      
    }
    
    dplyr::bind_rows(run_out, .id = "run")
    
  }

# test this function
s_l_2010_mod(spp_n = 10, runs = 10, t = 20, n0 = 3, m_alpha = 1, sd_alpha = 0.2)


# run this function for 10 species to check results from the paper
test_out <- 
  s_l_2010_mod(spp_n = 10, runs = 200, t = 4000, n0 = 3, m_alpha = 0.8, sd_alpha = 0.2)

test_out %>%
  ggplot(mapping = aes(x = realised_richness, y = community_biomass)) +
  geom_point() +
  geom_smooth(method = "lm")



df_n_t


### load the data directly (i.e. extracted from the paper)

# load the species pool data
spp_pool <- read_csv(here("data/stachova_leps_fig_1_data.csv")) 

# round off the species pool and realised diversity data to the nearest integer

spp_pool <- 
  spp_pool %>%
  mutate(unique_id = 1:n(),
         five_num = rep(LETTERS[1:5], times = 10),
         spp_pool_round = rep(seq(from = 10, to = 100, by = 10), each = 5)) %>%
  select(-spp_pool)

# biomass
bio_graph <- 
  spp_pool %>%
  select(-realised_diversity, -unique_id) %>%
  spread(key = "five_num", value = "biomass")

box_1_fig_1a <- 
  ggplot(data = bio_graph) +
  geom_boxplot(mapping = aes(x = spp_pool_round, group = spp_pool_round,
                             ymin = A, lower = B, middle = C, upper = D, ymax = E),
               stat = "identity",
               width = 4, fill = "grey88") +
  scale_x_continuous(breaks = c(seq(from = 0, to = 100, by = 10)) ) +
  ylab("community biomass") +
  xlab(expression(paste("inoculated ", alpha, " diversity", sep = ""))) +
  theme_meta()

box_1_fig_1a


# realised diversity
realised_graph <- 
  spp_pool %>%
  select(-biomass, -unique_id) %>%
  spread(key = "five_num", value = "realised_diversity")

box_1_fig_1a_inset <- 
  ggplot(data = realised_graph) +
  geom_boxplot(aes(x = spp_pool_round, group = spp_pool_round,
                   ymin = A, lower = B, middle = C, upper = D, ymax = E),
               stat = "identity",
               width = 4, fill = "grey88") +
  scale_y_continuous(limits = c(1, 7.1)) +
  scale_x_continuous(breaks = c(seq(from = 0, to = 100, by = 20)) ) +
  ylab(expression(paste("realised ", alpha, " diversity", sep = ""))) +
  xlab(expression(paste("inoculated ", alpha, " diversity", sep = ""))) +
  theme_meta() +
  theme(axis.title.x = element_text(colour ="black", size = 8, face = "plain", margin=margin(2.5,0,0,0,"pt")),
        axis.title.y = element_text(colour = "black", size = 8, face = "plain", margin=margin(0,2.5,0,0,"pt")),
        axis.text.x = element_text(colour = "black", size=8, face = "plain",  margin=margin(10,0,0,0,"pt")),
        axis.text.y = element_text(colour ="black", size=8, face = "plain", margin=margin(0,10,0,0,"pt")))

# add the inset to the fig 1a
box_1_fig_1a <- 
  box_1_fig_1a +
  annotation_custom(ggplotGrob(box_1_fig_1a_inset), 
                    xmin = 55, xmax = 105, 
                    ymin = 130, ymax = 170)

ggsave(filename = here("figures/box_1_fig1a.png"), plot = box_1_fig_1a,
       dpi = 500, units = "cm", width = 11, height = 10)


# load the constant species pool data
real_dat <- read_csv(here("data/stachova_leps_fig_2_data.csv"))

box_1_fig_1ab <- 
  real_dat %>%
  split(.$spp_pool) %>%
  map(~ ggplot(data = .,
               aes(x = realised_div, y = biomass)) +
        geom_jitter(width = 0.1, size = 1) +
        geom_smooth(method = "lm", alpha = 0.2, size = 0.1, colour = "black") +
        scale_x_continuous(limits = c(0, 12), breaks = seq(from = 2, to = 12, by = 2) ) +
        ylab("community biomass") +
        xlab(expression(paste("realised ", alpha, " diversity", sep = ""))) +
        theme_meta() +
        theme(axis.title.y = element_text(size = 9),
              axis.title.x = element_text(size = 9),
              axis.text.y = element_text(size = 9),
              axis.text.x = element_text(size = 9)))

box_1_fig_1ab_list <- list(box_1_fig_1ab[[1]], box_1_fig_1ab[[2]])
names(box_1_fig_1ab_list) <- c(1:length(box_1_fig_1ab_list))

for (i in seq_along(1:length(box_1_fig_1ab_list))) {
  
  ggsave(filename = paste0(here("figures"), "/box1_fig_1bc", names(box_1_fig_1ab_list)[i], ".png"), 
         plot = box_1_fig_1ab_list[[i]], dpi = 500, units = "cm", width = 5, height = 5)
}





