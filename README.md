# Otolith-derived field metabolic rates of mesopelagic fishes (Family Myctophidae) from the Scotia Sea, Southern Ocean

Author: Sarah Alewijnse

## Data

The main data sheets can be found in the Data folder:
* **Myctophids_Master.csv** contains the tidied otolith, muscle and water stable isotope data, and metadata for each individual myctophid.
* **Myctophids_M_Temp.csv** is as above, but includes estimates of otolith derived C<sub>resp</sub> and temperature.
* **Myctophids_M_Temp_Bel.csv** is as above, but includes estimates of oxygen derived from allometric equations in Belcher et al. 2019.

The folder MixSIAR_Data includes tidied data on stable isotopes of the mixture (otolith) and source (DIC and muscle) and discrimination factors.

## Analyses

Analyses are divided into four sections.

### 01_Parameter_Calculations
1. **01_DIC_Diet** calculates parameters for dissolved inorganic carbon (DIC) and myctophid diet.
2. **02_C_resp** calculates otolith derived C<sub>resp</sub> using MixSIAR.
3. **03_Temperature** calculates otolith derived temperature using RJAGS.
4. **04_Belcher_O2_Consumptions** calculates oxygen consumptions from allometric equations in Belcher et al. 2019, using RJAGS.
5. **05_Combine_Parameter_Posteriors** combines posteriors from the analyses above into single .csv files.
6. **06_Parameter_Summaries** calculates means, standard errors and standard deviations from the above posteriors.
7. **07_Parameter_Conversions** converts oxygen consumption between C<sub>resp</sub>, ul/mg/h and mg/kg/h.

### 02_Linear_Models_Among_Species
1. **01_M_Body_Mass_Temperature** is a linear model of C<sub>resp</sub> with body mass, temperature and species in RStan (equation 5).
2. **02_M_Oxygen_Consumption** is a linear model of C<sub>resp</sub> with allometrically-derived estimates of oxygen consumption and species in RStan (equation 7).

### 03_Linear_Models_Within_Species
1. **01_M_Body_Mass_Temperature** is linear models of C<sub>resp</sub> with body mass and temperature within species, in RStan (equation 6).

### 04_Plots

* **Fig_2_Density_Cresp** is a kernel density plot of posterior predictions of C<sub>resp</sub> grouped by species (Figure 2).
* **Fig_3_4_Inter_Body_Mass_Temp** are scatterplots of C<sub>resp</sub> and body mass and temperature among species (Figures 3 & 4).
* **Fig_5_Intra_Posteriors** are posterior plots for intraspecific models of C<sub>resp</sub> with body mass and temperature (Figure 5).
* **Fig_6_Intra_Body_Mass** are scatterplots of C<sub>resp</sub> with body mass within species (Figure 6).
* **Fig_7_Intra_Temp** are scatterplots of C<sub>resp</sub> with temperature within species (Figure 7).
* **Fig_9_Inter_Bel** is a scatterplot of C<sub>resp</sub> with allometrically-derived oxygen consumption (Figure 9).

## Outputs

**01_Parameter_Calculations** gives outputs for all RJAGS models used to estimate C<sub>resp</sub>, temperature and allometrically-derived oxygen consumption.
Each folder includes:
* **Diagnostics** are the Gelman-Rubin and Geweke Diagnostics.
* **Posteriors** sampled posteriors for each parameter.
* **Summaries** are summary statistics (mean, standard deviation and posterior intervals) for each parameter.

**02_Linear_Models_Among_Species** and **03_Linear_Models_Within_Species** contain pairs plots, posterior plots and traceplots for each linear model.
Saved .rds files from model outputs are ommitted as they are too large to upload here.

## Plots

**01_Among_Species** contains:
* **01_Cresp_Density** is a kernel density plot of posterior predictions of C<sub>resp</sub> grouped by species (Figure 2).
* **02_Cresp_Body_Mass** is a scatterplot of C<sub>resp</sub> and body mass among species (Figure 3).
* **03_Cresp_Temperature** is a scatterplot of C<sub>resp</sub> and temperature among species (Figure 4).
* **04_Cresp_Oxygen_Consumption** is a scatterplot of C<sub>resp</sub> with allometrically-derived oxygen consumption (Figure 9).

**02_Within_Species** contains:
* **01_Cresp_Body_Mass** are scatterplots of C<sub>resp</sub> with body mass within species (Figure 6).
* **02_Cresp_Temperature** are scatterplots of C<sub>resp</sub> with temperature within species (Figure 7).
* **03_Combined_Posterior** are posterior plots for intraspecific models of C<sub>resp</sub> with body mass and temperature (Figure 5).