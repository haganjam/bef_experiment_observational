# bef_experiment_observational
Examining the relationship between biodiversity and ecosystem functioning in experimental and observational data

The script 'thompson_2020_simulations.R' runs the simulations reported in the manuscript and calls the model function from the script 'thompson_2020_model.R'. The simulations are written to a .csv file in a folder called 'analysis_data'.

The data from the BIODEPTH and Jena experiments are prepared using the 'BIODEPTH_Jena_data_preparation.R' script. These data are cleaned, merged and then written to a .csv file into a folder called 'analysis_data'.

The script 'ms_figs.R' loads the simulation data and the cleaned biodiversity experiment data. It then plots figs. X to X.

To reproduce the analysis the in Fig. 2, you need to run the script 'stachova_leps_model_data_preparation.R'. This runs nine simulation models and calls the model function from the script 'stachova_leps_model.R'. It then outputs the analysis data into a .csv file into a folder called 'analysis_data'. Then, you need to run the script 'Jena_data_reanalysis.R' script. This produces a cleaned version of the Jena data which is written to a .csv file and saved to a folder called 'analysis_data'. Once these scripts have been run, you can then run the 'ms_fig_2_analysis.R' script which produces Fig.2 reported in the manuscript.

To reproduce the analysis of van der Plas' (2019) systematic review for Fig. 5, you need to run the script 'van_der_Plas_review_analysis_data_preparation.R'. This outputs an excel file whereby information regarding the spatial grain and spatial extent were filled in based on the surveying the original papers. This data file is called: "van_der_Plas_2019_spatial_extent_complete.csv". Then, the script 'van_der_Plas_reanalysis.R' reproduces Fig. 5 and the associated analysis.

