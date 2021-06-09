# Otolith-derived field metabolic rates of myctophids (family Myctophidae) from the Scotia Sea (Southern Ocean)

Author: Sarah R. Alewijnse

* **NB:** **C<sub>resp</sub>** is often referred to as **M** or **M_oto** throught these analyses.

## Data

The main data sheets can be found in the Data folder:
* **Myctophids_Master.csv** contains the tidied otolith, muscle and water stable isotope data, and metadata for each individual myctophid.
* **Myctophids_M_Temp_Length_Maturity.csv** is as above, but includes estimates of otolith derived C<sub>resp</sub> and temperature, as well as standard length (mm) and maturity stage.
* **Myctophids_M_Temp_Bel.csv** is as above, but includes estimates of oxygen derived from allometric equations in Belcher et al. 2019.

The folder MixSIAR_Data includes tidied data on stable isotopes of the mixture (otolith) and source (DIC and muscle) and discrimination factors.

## Code

Code is divided into four sections plus miscillaneous.

### 01_Parameter_Calculations
1. **01_DIC_Diet** calculates parameters for dissolved inorganic carbon (DIC) and myctophid diet.
2. **02_C_resp** calculates otolith derived C<sub>resp</sub> using MixSIAR.
3. **03_Temperature** calculates otolith derived temperature using RJAGS.
4. **04_Belcher_O2_Consumptions** calculates oxygen consumptions from allometric equations in Belcher et al. 2019, using RJAGS.
5. **05_Combine_Parameter_Posteriors** combines posteriors from the analyses above into single .csv files.
6. **06_Parameter_Summaries** calculates means, standard errors and standard deviations from the above posteriors.
7. **07_Parameter_Conversions** converts oxygen consumption between C<sub>resp</sub>, ul/mg/h and mg/kg/h.
8. **08_Bel_2020_Conversions** converts ETS-dervied oxygen consumption (ul/mg/h) to mg/kg/h and C<sub>resp</sub>.
9. **09_Method_Comparison** summarises oxygen consumption estimates by all different methods. Used to create table 1. 

### 02_Linear_Models_Among_Species
1. **01_M_Body_Mass_Temperature** is a linear model of C<sub>resp</sub> with body mass, temperature and species in RStan (equation 5).
2. **02_M_Oxygen_Consumption** is a linear model of C<sub>resp</sub> with allometrically-derived estimates of oxygen consumption and species in RStan (equation 7).
* "a" indicates the linear models themselves, and "b" codes for the ouput (graphs and tables).
	
### 03_Linear_Models_Within_Species
1. **01_M_Body_Mass_Temperature** is linear models of C<sub>resp</sub> with body mass and temperature within species, in RStan (equation 6).
* "a" indicates the linear models themselves, and "b" codes for the ouput (graphs and tables).

### 04_Plots

* **Fig_2_Density_Cresp** is a kernel density plot of posterior predictions of C<sub>resp</sub> grouped by species (Figure 2).
* **Fig_3_4_Inter_Body_Mass_Temp** are scatterplots of C<sub>resp</sub> and body mass and temperature among species (Figures 3 & 4).
* **Fig_5_Intra_Posteriors** are posterior plots for intraspecific models of C<sub>resp</sub> with body mass and temperature (Figure 5).
* **Fig_6_Intra_Body_Mass** are scatterplots of C<sub>resp</sub> with body mass within species (Figure 6).
* **Fig_7_Intra_Temp** are scatterplots of C<sub>resp</sub> with temperature within species (Figure 7).
* **Fig_9_Inter_Bel** is a scatterplot of C<sub>resp</sub> with allometrically-derived oxygen consumption (Figure 9).
* The **LM_CI** folder contains the function used to add linear models with confidence intervals to plots from rethinking/Stan outputs.

### 05_Misc

1. **01_Methods** tests whether there are differences in resulting C<sub>resp</sub> due to otolith preparation method (Supplementary 2).
2. **02_Within_Species_Analyses** includes models of differences in C<sub>resp</sub> within species for different years (Supplementary 4.1). It also includes specific analyses of difference within *Protomyctophum bolini* by latitude (Supplementary 4.2) and *Gymnoscopelus nicholsi* by location (Supplementary 4.3).
3. **03_Among_Species_Analyses** is analyses of effect of length ration (SL/species max. SL) on C<sub>resp</sub> (Supplementary 3).
4. **04_Age_Estimation** estimates the age of each individual based on standard length (SL, Supplementary 5).
* "a" indicates the linear models themselves, and "b" codes for the ouput (graphs and tables).

## Outputs

**01_Parameter_Calculations** gives outputs for all RJAGS models used to estimate C<sub>resp</sub>, temperature, and allometrically-derived oxygen consumption.
Each folder includes:
* **Diagnostics** are the Gelman-Rubin and Geweke Diagnostics.
* **Posteriors** sampled posteriors for each parameter.
* **Summaries** are summary statistics (mean, standard deviation and posterior intervals) for each parameter.

**02_Linear_Models_Among_Species** and **03_Linear_Models_Within_Species** contain pairs plots, posterior plots and traceplots for each linear model.
Saved .rds files from model outputs are ommitted as they are too large to upload here.

**04_Misc**
* **01_Methods** contains the pairs plots, posteriors and traceplots of the supplementary analysis of crushed vs. milled otoliths. Saved .rds files are ommitted.
* **02_Among_Spp** contains pair plots, posteriors and traceplots of the effect of length ration on C<sub>resp</sub> (Supplementary 3). Saved .rds files are ommitted.
* **03_Within_Spp** contains pairs plots, posteriors and traceplots of the analysis of year on C<sub>resp</sub> (Supplementary 4.1), as well as analyses within *Protomyctophum bolini* and *Gymnoscopelus braueri* (Supplementary 4.2-4.3). Saved .rds files are ommitted.
* **04_JAGS_Model_Text_Files** contains the text files of models generated by JAGS.

## Plots

**01_Among_Species** contains:
1. **01_Cresp_Density** is a kernel density plot of posterior predictions of C<sub>resp</sub> grouped by species (Figure 2).
2. **02_Cresp_Body_Mass** is a scatterplot of C<sub>resp</sub> and body mass among species (Figure 3).
3. **03_Cresp_Temperature** is a scatterplot of C<sub>resp</sub> and temperature among species (Figure 4).
4. **04_Cresp_Oxygen_Consumption** is a scatterplot of C<sub>resp</sub> with allometrically-derived oxygen consumption (Figure 9).

**02_Within_Species** contains:
1. **01_Cresp_Body_Mass** are scatterplots of C<sub>resp</sub> with body mass within species (Figure 6).
2. **02_Cresp_Temperature** are scatterplots of C<sub>resp</sub> with temperature within species (Figure 7).
3. **03_Combined_Posterior** are posterior plots for intraspecific models of C<sub>resp</sub> with body mass and temperature (Figure 5).

## Session Information

R version 4.0.0 (2020-04-24)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252 
[2] LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] gridExtra_2.3      bayesplot_1.7.1    rethinking_2.01   
 [4] dagitty_0.2-2      rstan_2.19.3       StanHeaders_2.19.2
 [7] rjags_4-10         coda_0.19-3        MixSIAR_3.1.11    
[10] truncnorm_1.0-8    forcats_0.5.0      stringr_1.4.0     
[13] dplyr_0.8.5        purrr_0.3.4        readr_1.4.0       
[16] tidyr_1.0.3        tibble_3.0.1       ggplot2_3.3.0     
[19] tidyverse_1.3.0   

loaded via a namespace (and not attached):
 [1] httr_1.4.1         jsonlite_1.6.1     modelr_0.1.7      
 [4] assertthat_0.2.1   stats4_4.0.0       cellranger_1.1.0  
 [7] yaml_2.2.1         pillar_1.4.4       backports_1.1.6   
[10] lattice_0.20-41    glue_1.4.1         rvest_0.3.5       
[13] colorspace_1.4-1   plyr_1.8.6         pkgconfig_2.0.3   
[16] broom_0.5.6        haven_2.2.0        mvtnorm_1.1-0     
[19] scales_1.1.1       processx_3.4.2     generics_0.0.2    
[22] ellipsis_0.3.1     withr_2.2.0        cli_2.0.2         
[25] magrittr_1.5       crayon_1.3.4       readxl_1.3.1      
[28] ps_1.3.3           fs_1.4.1           fansi_0.4.1       
[31] nlme_3.1-147       MASS_7.3-53        xml2_1.3.2        
[34] pkgbuild_1.0.8     tools_4.0.0        loo_2.2.0         
[37] prettyunits_1.1.1  hms_0.5.3          lifecycle_0.2.0   
[40] matrixStats_0.56.0 V8_3.0.2           munsell_0.5.0     
[43] reprex_0.3.0       callr_3.4.3        compiler_4.0.0    
[46] rlang_0.4.6        grid_4.0.0         ggridges_0.5.2    
[49] rstudioapi_0.11    boot_1.3-24        gtable_0.3.0      
[52] inline_0.3.15      DBI_1.1.0          curl_4.3          
[55] R6_2.4.1           lubridate_1.7.9    shape_1.4.4       
[58] stringi_1.4.6      Rcpp_1.0.4.6       vctrs_0.3.0       
[61] dbplyr_1.4.3       tidyselect_1.1.0  