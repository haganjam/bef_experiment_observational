
# Project: Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

# Title: Preparation of the stachova and leps model output data

# load relevant libraries generally
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

# make a folder to export figures
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# where to access functions from
source(here("scripts/function_plotting_theme.R"))
source(here("scripts/stachova_leps_model.R"))


# run the model to generate the output data

# set the number of runs to do
n_exp <- 9

# set up the means for the model runs
set.seed(4897245)
a_mean_sim <- runif(n = n_exp, min = 0.3, max = 1.1)

sl_mod_out <- vector("list", length = n_exp)
for (i in (1:n_exp) ) {
  
  sl_mod_out[[i]] <- 
    s_l_2010_mod(reg_pool = 50,
                 t_steps = 4000, 
                 n0 = 3,
                 a_mean = a_mean_sim[i], 
                 a_sd = 0.2, a_min = 0.2, a_max = 1.2, a_spp = 1,
                 k_min = 3, k_max = 150,
                 r_min = 0.01, r_max = 0.5, 
                 lsp = c(1, 5, 10, 15, 20, 25, 30),
                 reps = 25)
  
}

sl_mod_an <- 
  bind_rows(sl_mod_out, .id = "run")

# output this into dataframe as a .csv file
write_csv(x = sl_mod_an,
          path = here("data/stachova_leps_model_data_full.csv"))









