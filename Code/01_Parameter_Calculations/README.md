# Parameter Calculations

* **01_DIC_Diet** calculates diet and dissolved inorganic carbon (DIC) stable isotope values (d13C), based on random sampling of normal distributions. 
* **02_M** estimates C_resp (M_oto) using MixSIAR.
* **03_Temperature** estimates experienced temperature from oxygen isotopes (d18O) using RJAGS.
* **04_Belcher_O2_Consumption** estimates log oxygen consumption (ul/mg/h) based on equation 1 (Belcher et al. 2019)
* **05_Combine_Parameter_Posteriors** combines posteriors from all individuals into a single files
* **06_Parameter_Summaries** gives the summary statistics (mean, sd and se) for each parameter posterior.
	* With oxgyen consumption derived from equation 1, the parameter was back-transformed from log to get sd and se. Because this has the effect of drastically upweighting large samples in the posterior, means were taken from the log posterior and directly back-transformed (Dir_mean_Metabol).
* **07_Parameter_Conversions** converts between difference measures of oxygen consumption and metabolic rate