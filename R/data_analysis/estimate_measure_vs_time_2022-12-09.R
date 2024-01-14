
#' @description 
#' Estimate measures change over time using LMMs. 


rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(MuMIn)
library(lme4)
library(lmerTest)
options(digits.secs = 3)
options(scipen = 999)

source(file.path(here(), "R",  "config_figures.R"))
source(file.path(here(), "R",  "utils.R"))
source(file.path(here(), "R",  "config.R"))


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

measures_df_path <- file.path(here(), "data_actigraph_processed", "accelerometer", "ALS_forearm_daily_proc.csv")
measures_df <- 
  fread(measures_df_path) %>% 
  as.data.frame() %>%
  filter(utc_time_date >= alsfrsr_local_time_date_min)  %>%
  group_by(subj_id) %>%
  mutate(
    day_relative = as.numeric(difftime(utc_time_date, min(utc_time_date), units = c("days"))),
    month_relative = day_relative/30.5
  ) %>%
  ungroup()
dim(measures_df)
head(measures_df)
length(unique(measures_df$subj_id))


# ------------------------------------------------------------------------------
# LMM: measure ~ time 
# ------------------------------------------------------------------------------

# get the estimates (uses function from util.R)
mod_formula <- as.formula(y ~ month_relative + (1 + month_relative | subj_id_fct))
out_df <- get_lmm_measure_vs_time(measures_df, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula)
out_df
# save to file 
fwrite(out_df, file.path(here(), "results_tables", paste0("lmm_measure_vs_time_est_", "45angle", ".csv")))
rm(out_df)


# ------------------------------------------------------------------------------
# 90 angle 

# get the estimates (uses function from util.R)
mod_formula <- as.formula(y ~ month_relative + (1 + month_relative | subj_id_fct))
out_df <- get_lmm_measure_vs_time(measures_df, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula)
out_df
# save to file 
fwrite(out_df, file.path(here(), "results_tables", paste0("lmm_measure_vs_time_est_", "90angle", ".csv")))
rm(out_df)


# ------------------------------------------------------------------------------
# 135 angle

# get the estimates (uses function from util.R)
mod_formula <- as.formula(y ~ month_relative + (1 + month_relative | subj_id_fct))
out_df <- get_lmm_measure_vs_time(measures_df, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula)
out_df
# save to file 
fwrite(out_df, file.path(here(), "results_tables", paste0("lmm_measure_vs_time_est_", "135angle", ".csv")))
rm(out_df)



