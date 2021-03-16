

This repository contains the code for the simulations and analysis for the article by Hagan, Vanschoenwinkel and Gamfeldt

preliminary citation: 

> We should not necessarily expect positive relationships between biodiversity and ecosystem functioning in observational data (James G. Hagan, Bram Vanschoenwinkel & Lars Gamfeldt)

To reproduce the analysis reported in the paper, start by downloading all the scripts in the repository and running these scripts:

+ BIODEPTH_Jena_data_preparation.R
+ Jena_data_preparation.R
+ LTER_kelp_data_preparation

These scripts directly download the data and output cleaned versions of the data into a newly created folder called: analysis_data

Next, run the script:

+ stachova_leps_model_data_preparation.R

This script calls the function from stachova_leps_model.R and runs nine different simulated biodiversity experiments with varying levels of average interspecific competition. The resulting dataset is saved into the analysis_data folder. Note that this is computationally intensive and can take 10-20 minutes to run.

Once these four scripts have been run, you can run the following scripts to create fig. 2, fig. S1, fig. 3 and fig. 5:

+ ms_fig_2_fig_S1_analysis.R
+ ms_fig_3_analysis.R
+ ms_fig_5_analysis.R

To reproduce fig. 4 and fig. S2, you need to run the script:

+ ms_fig_4_fig_S2_analysis.R

This loads in data from van der Plas (2019)'s systematic review that we added spatial extent information to. The data is downloaded directly from the script via a figshare link (to be added soon...).

For reference, the script called: van_der_plas_review_create_data_template.R was used to take the raw file posted by van der Plas (2019, Biological Reviews) and create a template from which we could add the spatial extent information.


### Instruction to download a Github repo

#### with git

in a Unix Terminal (e.g. bash):

```cd path/to/local/folder``` 

(on your computer - the folder were you want the repository to live) command on Windows might differ. 


```git clone https://github.com/haganjam/bef_experiment_observational.git```

This should download the repository directory. 

#### without git
If you don't have git installed, you can download the repository as zip file and save it locally. 

+ Clone or download (green button top right)
+ Download Zip

then save and extract the zip where you want the directory to be saved on your computer. 
