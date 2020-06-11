# Otolith derived field metabolic rates of myctophids from the Scotia Sea

Author: Sarah Alewijnse

## Data

The main data sheets can be found in the Data folder:
* **Myctophids_Master.csv** contains the tidied otolith, muscle and water stable isotope data, and metadata for each individual myctophid.
* **Myctophids_M_Temp.csv** is as above, but includes estimates of otolith derived C<sub>resp</sub> and temperature.
* **Myctophids_M_Temp_Bel.csv** is as above, but includes estimates of metabolic rate derived from allometric equations in Belcher et al. 2019.

The folder MixSIAR_Data includes tidied data on stable isotopes of the mixture (otolith) and source (DIC and muscle) and discrimination factors.

## Analyses

Analyses are divided into four folders:

1. **01_Parameter_Calculations**
 1. **01_DIC_Diet** calculates parameters for dissolved inorganic carbon (DIC) and myctophid diet.
 2. **02_C_resp** calculates otolith derived C<sub>resp<\sub> using MixSIAR.
 3. **03_Temperature** calculates otolith derived temperature using RJAGS.
 4. **04_Belcher_O2_Consumptions** calculates oxygen consumptions from allometric equations in Belcher et al. 2019, using RJAGS.
 5. **05_Combine_Parameter_Posteriors** combines posteriors from the analyses above into single .csv files.
 6. **06_Parameter_Summaries** calculates means, standard errors and standard deviations from the above posteriors.
 7. **07_Parameter_Conversions** converts oxygen consumption between C<sub>resp<\sub>, ul/mg/h and mg/kg/h.