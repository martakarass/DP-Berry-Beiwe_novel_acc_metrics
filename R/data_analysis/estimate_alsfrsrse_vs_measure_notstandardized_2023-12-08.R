
#' @description 
#' Estimate measures change over time using LMMs, without outcome and covariate 
#' standardization. 

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

# measures combined with ALSFRS-RSE
measures_df_path <- file.path(here(), "data_actigraph_processed", "accelerometer", "ALS_forearm_daily_proc_comb.csv")
measures_df <- 
  fread(measures_df_path) %>% 
  as.data.frame() %>%
  mutate(frs4567 = frs4 + frs5 + frs6 + frs7)
dim(measures_df)
head(measures_df)


# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept and slope
# ------------------------------------------------------------------------------

round_fct = 6

dat = measures_df
mod_formula = as.formula(y ~ x_mean + (1 + x_mean | subj_id_fct))
x_scale = FALSE
y_scale = FALSE
# var_name_vec = VAR_NAME_VEC; var_label_vec = VAR_LABEL_VEC

out_df_all <- data.frame()

y_name = "frs_total_score"
y_label = "ALSFRS-RSE total score"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs4567"
y_label = "ALSFRS-RSE Q4-7"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs4"
y_label = "ALSFRS-RSE Q4"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs5"
y_label = "ALSFRS-RSE Q5"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs6"
y_label = "ALSFRS-RSE Q6"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs7"
y_label = "ALSFRS-RSE Q7"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

dim(out_df_all)
table(out_df_all$conv)
fwrite(out_df_all, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_withrandomslope_45angle.csv"))

rm(out_df_all)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# To compare other angles
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept and slope, 90 angle 
# ------------------------------------------------------------------------------

# dat = measures_df
# mod_formula = as.formula(y ~ x_mean + (1 + x_mean | subj_id_fct))
# x_scale = FALSE
# y_scale = FALSE
# # var_name_vec = VAR_NAME_VEC; var_label_vec = VAR_LABEL_VEC
# 
# out_df_all <- data.frame()
# 
# y_name = "frs_total_score"
# y_label = "ALSFRS-RSE total score"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs4567"
# y_label = "ALSFRS-RSE Q4-7"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs4"
# y_label = "ALSFRS-RSE Q4"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs5"
# y_label = "ALSFRS-RSE Q5"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs6"
# y_label = "ALSFRS-RSE Q6"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs7"
# y_label = "ALSFRS-RSE Q7"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_90angle, VAR_LABEL_VEC_90angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# dim(out_df_all)
# table(out_df_all$conv)
# fwrite(out_df_all, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_withrandomslope_90angle.csv"))
# 
# rm(out_df_all)
# 
# 
# 
# # ------------------------------------------------------------------------------
# # LMM: ALSFRS-RSE ~ measure , with random intercept and slope, 135 angle 
# # ------------------------------------------------------------------------------
# 
# dat = measures_df
# mod_formula = as.formula(y ~ x_mean + (1 + x_mean | subj_id_fct))
# x_scale = FALSE
# y_scale = FALSE
# # var_name_vec = VAR_NAME_VEC; var_label_vec = VAR_LABEL_VEC
# 
# out_df_all <- data.frame()
# 
# y_name = "frs_total_score"
# y_label = "ALSFRS-RSE total score"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs4567"
# y_label = "ALSFRS-RSE Q4-7"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs4"
# y_label = "ALSFRS-RSE Q4"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs5"
# y_label = "ALSFRS-RSE Q5"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs6"
# y_label = "ALSFRS-RSE Q6"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# y_name = "frs7"
# y_label = "ALSFRS-RSE Q7"
# out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_135angle, VAR_LABEL_VEC_135angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
# out_df_all <- rbind(out_df_all, out_df)
# 
# dim(out_df_all)
# table(out_df_all$conv)
# fwrite(out_df_all, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_withrandomslope_135angle.csv"))
# 
# rm(out_df_all)




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# To compare adjustment for TAC
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept only
# ------------------------------------------------------------------------------

dat = measures_df
mod_formula = as.formula(y ~ x_mean + (1 | subj_id_fct))
x_scale = FALSE
y_scale = FALSE

out_df_all <- data.frame()

y_name = "frs_total_score"
y_label = "ALSFRS-RSE total score"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs4567"
y_label = "ALSFRS-RSE Q4-7"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs4"
y_label = "ALSFRS-RSE Q4"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs5"
y_label = "ALSFRS-RSE Q5"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs6"
y_label = "ALSFRS-RSE Q6"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs7"
y_label = "ALSFRS-RSE Q7"
out_df  <- get_lmm_survey_vs_measure(measures_df, y_name, y_label, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)


dim(out_df_all)
table(out_df_all$conv)
fwrite(out_df_all, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_norandomslope_45angle.csv"))



# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept only, adjusted for TAC
# ------------------------------------------------------------------------------

dat = measures_df
mod_formula = as.formula(y ~ x_mean + x_adj_mean + (1 | subj_id_fct))
x_scale = FALSE
y_scale = FALSE

var_name_adj = "ac"

out_df_all <- data.frame()

y_name = "frs_total_score"
y_label = "ALSFRS-RSE total score"
out_df  <- get_lmm_survey_vs_measure_adj(measures_df, y_name, y_label, var_name_adj, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs4567"
y_label = "ALSFRS-RSE Q4-7"
out_df  <- get_lmm_survey_vs_measure_adj(measures_df, y_name, y_label, var_name_adj, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs4"
y_label = "ALSFRS-RSE Q4"
out_df  <- get_lmm_survey_vs_measure_adj(measures_df, y_name, y_label, var_name_adj, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs5"
y_label = "ALSFRS-RSE Q5"
out_df  <- get_lmm_survey_vs_measure_adj(measures_df, y_name, y_label, var_name_adj, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs6"
y_label = "ALSFRS-RSE Q6"
out_df  <- get_lmm_survey_vs_measure_adj(measures_df, y_name, y_label, var_name_adj, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)

y_name = "frs7"
y_label = "ALSFRS-RSE Q7"
out_df  <- get_lmm_survey_vs_measure_adj(measures_df, y_name, y_label, var_name_adj, VAR_NAME_VEC_45angle, VAR_LABEL_VEC_45angle, mod_formula, y_scale, x_scale, round_fct = round_fct)
out_df_all <- rbind(out_df_all, out_df)


dim(out_df_all)
table(out_df_all$conv)
fwrite(out_df_all, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_norandomslope_adj_45angle.csv"))













