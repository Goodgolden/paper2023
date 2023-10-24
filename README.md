# paper2023
This is the repository for Randy's Biometric paper in 2023

- The main codes of functions and objects are stored under `paper2023/R` folder

  - Please load this the `00_fucntions.R` first before running the .Rmd files
  
  - The working dataset is stored in `train_test.rda` file

- The analytic and simulated results are stored under `paper2023/results` folder

  - `result_alphas_2023-08-23` 
  - `result_anchors_2023-08-23`
  - `result_ss500_simulation`
  - `result_ss900_simulation`
  
- All the figures and latex tables are stored under `paper2023/figure` folder

  - all the figures and tables are automatically saved as `Rmd_file_name`_`results_name`_`System.date`
  - certain figures and tables are adjusted with the names or contents in the paper
  - there are several versions for each figure, including merged files
  - some of the intermediate result datasets are saved in this folder too
  
- The `.Rmd` files are listed below for all the tables and figure (there are several files reused for multiple figures are commented within)
  
  - "0- table- demographic.Rmd"      
  - "02_table2_plmlmm_code.R"
  - "02_table2_plmlmm.Rmd"           
  -  "03_table3_multiple-alpha_code.R"
  -  "03_table3_multiple-alpha.Rmd"   
  -  "04_figure- flowchart.Rmd"       
  -  "05_figure2_individual_plot.Rmd" 
  -  "06_figure3_small_simulation.Rmd"
  -  "07_figure4_large_simulation.Rmd"
  -  "08_supp_figure_chi_square.Rmd"  
  -  "09_supp_figure_merged.Rmd"      
  -  "10_supp_figure_lmm_aic.Rmd"     
  -  "11_myapp" 


- An under-development shiny app is saved in folder `- _myapp`
