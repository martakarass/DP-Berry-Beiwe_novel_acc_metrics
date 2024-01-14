
#' @description 
#' This script takes the pre-computed results (LMM model estimation) and format them 
#' into a nice table.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(dplyr.summarise.inform = FALSE)
options(digits.secs = 0)
options(scipen = 999)

source(file.path(here(), "R", "config_figures.R"))
source(file.path(here(), "R", "utils.R"))
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# format utils
# ------------------------------------------------------------------------------

# function to format numeric value to a character
# rounds depending on the value to be formatted (large numbers rounded to 0 decimal
# places, abs()<10 numbers rounded to 3 decimal places)
format_num_to_char <-  function(val){
  val_abs <- abs(val)
  if (val_abs < 10){
    val_f = sprintf("%.5f", val)
  } else if (val_abs >= 10 & val_abs < 100) {
    val_f = sprintf("%.2f", val)
  } else if (val_abs >= 100 & val_abs < 1000){
    val_f = sprintf("%.1f", val)
  } else {
    val_f = sprintf("%.0f", val)
  }
  return(val_f)
}


# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept and slope
# ------------------------------------------------------------------------------

dat_all = fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_withrandomslope_45angle.csv")) %>% as.data.frame()

# format columns content
dat_all_F0 <- 
  dat_all %>%
  # arrange(model_group, wearable_group_fct, data_set_fct, desc(intcp_est)) %>%
  rowwise() %>%
  mutate_at(vars(intcp_est : R2c), format_num_to_char) %>%
  ungroup() %>%
  mutate(
    intcp_f = paste0(intcp_est, " [", intcp_ci_lo, ", ", intcp_ci_up, "]"),
    slope_f = paste0(slope_est, " [", slope_ci_lo, ", ", slope_ci_up, "] (", slope_pval, ")")
  ) 
dat_all_F0$model_no <- 1 : nrow(dat_all_F0)
dat_all_F <- 
  dat_all_F0 %>% 
  select(
    model_no, 
    y_label, 
    # x_label, 
    intcp_f,
    slope_f, 
    R2m, 
    R2c
  ) %>% 
  as.data.frame() 

names(dat_all_F) <- c(
  "No.",
  "Outcome", 
  # "Covariate", 
  "Interc. est. [95% CI]",
  "Slope est. [95% CI] (p-val.)",
  "R2m", "R2c")
dat_all_F
table(dat_all_F$Outcome)
# View(dat_all_F)

fwrite(dat_all_F, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_withrandomslope_45angle_FORMATTED.csv"))



# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept only
# ------------------------------------------------------------------------------

dat_all = fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_norandomslope_45angle.csv")) %>% as.data.frame()

# format columns content
dat_all_F0 <- 
  dat_all %>%
  # arrange(model_group, wearable_group_fct, data_set_fct, desc(intcp_est)) %>%
  rowwise() %>%
  mutate_at(vars(intcp_est : R2c), format_num_to_char) %>%
  ungroup() %>%
  mutate(
    intcp_f = paste0(intcp_est, " [", intcp_ci_lo, ", ", intcp_ci_up, "]"),
    slope_f = paste0(slope_est, " [", slope_ci_lo, ", ", slope_ci_up, "] (", slope_pval, ")")
  ) 
dat_all_F0$model_no <- 1 : nrow(dat_all_F0)
dat_all_F <- 
  dat_all_F0 %>% 
  select(
    model_no, 
    y_label, 
    # x_label, 
    intcp_f,
    slope_f, 
    R2m, 
    R2c
  ) %>% 
  as.data.frame() 

names(dat_all_F) <- c(
  "No.",
  "Outcome", 
  # "Covariate", 
  "Interc. est. [95% CI]",
  "Slope est. [95% CI] (p-val.)",
  "R2m", "R2c")
dat_all_F
table(dat_all_F$Outcome)
# View(dat_all_F)

fwrite(dat_all_F, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_norandomslope_45angle_FORMATTED.csv"))



# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept only, adjusted for TAC
# ------------------------------------------------------------------------------

dat_all = fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_norandomslope_adj_45angle.csv")) %>% as.data.frame()

# format columns content
dat_all_F0 <- 
  dat_all %>%
  # arrange(model_group, wearable_group_fct, data_set_fct, desc(intcp_est)) %>%
  rowwise() %>%
  mutate_at(vars(intcp_est : R2c), format_num_to_char) %>%
  ungroup() %>%
  mutate(
    intcp_f = paste0(intcp_est, " [", intcp_ci_lo, ", ", intcp_ci_up, "]"),
    slope_f = paste0(slope_est, " [", slope_ci_lo, ", ", slope_ci_up, "] (", slope_pval, ")")
  ) 
dat_all_F0$model_no <- 1 : nrow(dat_all_F0)
dat_all_F <- 
  dat_all_F0 %>% 
  select(
    model_no, 
    y_label, 
    # x_label, 
    intcp_f,
    slope_f, 
    R2m, 
    R2c
  ) %>% 
  as.data.frame() 

names(dat_all_F) <- c(
  "No.",
  "Outcome", 
  # "Covariate", 
  "Interc. est. [95% CI]",
  "Slope est. [95% CI] (p-val.)",
  "R2m", "R2c")
dat_all_F
table(dat_all_F$Outcome)
# View(dat_all_F)

fwrite(dat_all_F, file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_notstandardized_norandomslope_adj_45angle_FORMATTED.csv"))


