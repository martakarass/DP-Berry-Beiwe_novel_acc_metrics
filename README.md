This repository accompanies the manuscript preprint "Digital biomarkers from wearable devices quantify forearm movements in patients with ALS".

The repository contains the R code used to do statistical data analyses included in the manuscript. The repository does not contain data. 

## Repository content

/R/data_analysis/ dir files: 

- **define_study_sample_Y-m-d.R** - Define study analysis sample and analytical datasets. In particular, includes definition of "ALS_forearm_daily_proc_comb.csv" that was used to estimate: 
  (a) measures' change over time,
  (b) association between measures and ALSFRS-R score
for the measures proposed in the manuscript.                
- **estimate_alsfrsrse_vs_measure_Y-m-d.R** - Estimate measures change over time using linear mixed-effects models (LMMs). 
- **estimate_alsfrsrse_vs_measure_notstandardized_Y-m-d.R** - Estimate measures' change over time using LMMs, without outcome and covariate standardization. 
- **estimate_measure_vs_time_Y-m-d.R** - Estimate measures' change over time using LMMs.                           
- **plot_alsfrsrse_vs_measure_coefs_vary_angle_degrees_Y-m-d.R** -  Generate plot visualizing results of modeling associations between measures and ALSFRS-R, highlighting how the fixed effect of interest varies for different choices of degree parameter in measure definition.
- **plot_alsfrsrse_vs_measure_coefs_vary_model_adjustment_Y-m-d.R** - Generate plot visualizing results of modeling associations between measures and ALSFRS-R, highlighting how the fixed effect of interest varies depending on the model covariate adjustment and random effect choice.
- **plot_alsfrsrse_vs_measure_fitted_Y-m-d.R** - Generate plot visualizing results of modeling associations between measures and ALSFRS-R.                     
- **plot_measure_baseline_corr_Y-m-d.R** - Generate plot visualizing correlations between measures at baseline.
- **plot_measure_vs_time_fitted_Y-m-d.R** - Generate plot visualizing results of modeling measure change over time.
- **table_1_Y-m-d.R** - Generate Table 1.                                          
- **table_2_Y-m-d.R** - Generate Table 2.                                                  
- **table_format_lmm_estimates_results_Y-m-d.R** - Take the pre-computed results of LMM model estimation and format them into a nice table.                
- **table_format_lmm_estimates_results_notstandardized_Y-m-d.R** - Take the pre-computed results of LMM model estimation and format them into a nice table. 


## Analytical datasets

### (1) ALS_forearm_daily_proc.csv

This data set is used in the following R scripts: 

- /R/data_analysis/estimate_alsfrsrse_vs_measure_Y-m-d.R

This data set includes: 

- subject ID (column mame "subj_id"),
- subject Beiwe ID (column mame "beiwe_id"),
- start of the monitoring period ("date_start"),
- end of the monitoring period ("date_ended"),
- day date for day-level values ("wearbale_utc_time_date"),
- day-level values of the measures proposed in the manuscript (column names from "Cf45" to "Dsp135"),
- days elapsed from the start of the monitoring period ("day_since_start"),
- numer of minutes with device wear per day ("wear_time"),
- day-level Activity Counts ("ac"),
- day date for self-reported ALSFRS-R matched to the day-level values of the measures proposed in the manuscript; for each day date, one - the closest - self-reported ALSFRS-R score is matched ("survey_local_time_date"),
- self-reported ALSFRS-R scores: individual scores (column names from "frs1" to "frs12") and total score ("frs_total_score"). 

### (2) ALS_forearm_daily_proc_comb.csv

This data set is used in the following R scripts: 

- /R/data_analysis/estimate_alsfrsrse_vs_measure_Y-m-d.R
- /R/data_analysis/estimate_alsfrsrse_vs_measure_notstandardized_Y-m-d.R 

The dataset "ALS_forearm_daily_proc_comb.csv" contains the same columns as "ALS_forearm_daily_proc.csv" described above. However, it has undergone the following row filtering:

- Rows have been filtered to include only those where the time gap between the day-level measurement and the matched self-reported ALSFRS-R score is no more than 7 days.
- Only subjects with at least two observations remaining after applying the above have been retained.




