# bef_experiment_observational
Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

The script 'thompson_2020_simulations.R' runs the simulations reported in the manuscript and calls the model function from the script 'thompson_2020_model.R'. The simulations are written to a .csv file in a folder called 'analysis_data'.

The data from the BIODEPTH and Jena experiments are prepared using the 'BIODEPTH_Jena_constant_spp_pool.R' script. These data are cleaned, merged and then written to a .csv file into a folder called 'analysis_data'.

The script 'ms_figs.R' loads the simulation data and the cleaned biodiversity experiment data. It then plots figs. X to X.