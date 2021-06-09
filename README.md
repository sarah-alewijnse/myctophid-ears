# Otolith-derived field metabolic rates of myctophids (family Myctophidae) from the Scotia Sea (Southern Ocean)

Author: Sarah R. Alewijnse

* **NB:** **C<sub>resp</sub>** is often referred to as **M** or **M_oto** throught these analyses.

## Data

The main data sheets can be found in the Data folder:
* **Myctophids_Master.csv** contains the tidied otolith, muscle and water stable isotope data, and metadata for each individual myctophid.
* **Myctophids_M_Temp_Length_Maturity.csv** is as above, but includes estimates of otolith derived C<sub>resp</sub> and temperature, as well as standard length (mm) and maturity stage.
* **Myctophids_M_Temp_Bel.csv** is as above, but includes estimates of oxygen derived from allometric equations in Belcher et al. 2019.
* **JCR_Station_Dat.csv** gives the station data for the cruises on which the specimens were collected.

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

R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] truncnorm_1.0-8      MixSIAR_3.1.12       gridExtra_2.3        rjags_4-10           coda_0.19-4          bayesplot_1.8.0      rethinking_2.01     
 [8] dagitty_0.3-1        rstan_2.21.2         StanHeaders_2.21.0-7 forcats_0.5.1        stringr_1.4.0        dplyr_1.0.5          purrr_0.3.4         
[15] readr_1.4.0          tidyr_1.1.3          tibble_3.1.0         ggplot2_3.3.3        tidyverse_1.3.1     

loaded via a namespace (and not attached):
 [1] httr_1.4.2         jsonlite_1.7.2     modelr_0.1.8       RcppParallel_5.1.2 assertthat_0.2.1   stats4_4.0.5       cellranger_1.1.0  
 [8] yaml_2.2.1         pillar_1.6.0       backports_1.2.1    lattice_0.20-41    glue_1.4.2         digest_0.6.27      rvest_1.0.0       
[15] colorspace_2.0-0   plyr_1.8.6         pkgconfig_2.0.3    broom_0.7.6        haven_2.4.0        mvtnorm_1.1-1      scales_1.1.1      
[22] processx_3.5.1     farver_2.1.0       generics_0.1.0     ellipsis_0.3.1     withr_2.4.2        cli_2.4.0          magrittr_2.0.1    
[29] crayon_1.4.1       readxl_1.3.1       ps_1.6.0           fs_1.5.0           fansi_0.4.2        MASS_7.3-53.1      xml2_1.3.2        
[36] pkgbuild_1.2.0     tools_4.0.5        loo_2.4.1          prettyunits_1.1.1  hms_1.0.0          lifecycle_1.0.0    matrixStats_0.58.0
[43] V8_3.4.0           munsell_0.5.0      reprex_2.0.0       callr_3.7.0        compiler_4.0.5     rlang_0.4.10       grid_4.0.5        
[50] ggridges_0.5.3     rstudioapi_0.13    labeling_0.4.2     boot_1.3-27        gtable_0.3.0       codetools_0.2-18   inline_0.3.17     
[57] DBI_1.1.1          curl_4.3           reshape2_1.4.4     R6_2.5.0           lubridate_1.7.10   utf8_1.2.1         shape_1.4.5       
[64] stringi_1.4.6      Rcpp_1.0.6         vctrs_0.3.7        dbplyr_2.1.1       tidyselect_1.1.0  