
#' @description 
#' This script takes the precomputed results (LMM model estimation) and format them 
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
    val_f = sprintf("%.3f", val)
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
# LMM: ALSFRS-RSE ~ time 
# ------------------------------------------------------------------------------

dat_45degree <- fread(file.path(here(), "results_tables", "lmm_measure_vs_time_est_45angle.csv")) %>% as.data.frame()
dat_90degree <- fread(file.path(here(), "results_tables", "lmm_measure_vs_time_est_90angle.csv")) %>% as.data.frame()
dat_135degree <- fread(file.path(here(), "results_tables", "lmm_measure_vs_time_est_135angle.csv")) %>% as.data.frame()
dat_all <- dat_45degree %>% rbind(dat_90degree) %>% rbind(dat_135degree)

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


# ------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept and slope
# ------------------------------------------------------------------------------

dat_all <- 
  fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_withrandomslope_45angle.csv")) %>%
  as.data.frame()

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
dat_all_F0$model_no <- rep(1 : 12, times = nrow(dat_all_F0) / 12)
dat_all_F <- 
  dat_all_F0 %>% 
  select(
    model_no, 
    y_label, 
    x_label, 
    # intcp_f,
    slope_f, 
    R2m, 
    R2c
  ) %>% 
  as.data.frame() 

names(dat_all_F) <- c(
  "No.",
  "Outcome", "Covariate", 
  # "Interc. est. [95% CI]", 
  "Slope est. [95% CI] (p-val.)",
  "R2m", "R2c")
dat_all_F
table(dat_all_F$Outcome)
# View(dat_all_F)

rm(dat_all_F)


# ------------------------------------------------------------------------------------------
# LMM: ALSFRS-RSE ~ measure , with random intercept only, adjusted for total activity counts
# ------------------------------------------------------------------------------------------

dat_all <- 
  fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_norandomslope_adj_45angle.csv")) %>%
  as.data.frame()

# format columns content
dat_all_F0 <- 
  dat_all %>%
  # arrange(model_group, wearable_group_fct, data_set_fct, desc(intcp_est)) %>%
  rowwise() %>%
  mutate_at(vars(intcp_est : R2c), format_num_to_char) %>%
  ungroup() %>%
  mutate(
    intcp_f = paste0(intcp_est, " [", intcp_ci_lo, ", ", intcp_ci_up, "]"),
    slope_f = paste0(slope_est, " [", slope_ci_lo, ", ", slope_ci_up, "] (", slope_pval, ")"),
    slope2_f = paste0(slope2_est, " [", slope2_ci_lo, ", ", slope2_ci_up, "] (", slope2_pval, ")")
  ) 
dat_all_F0$model_no <- rep(1 : 12, times = nrow(dat_all_F0) / 12)
dat_all_F <- 
  dat_all_F0 %>% 
  select(
    model_no, 
    y_label, 
    x_label, 
    # intcp_f,
    slope_f, 
    slope2_f,
    R2m, 
    R2c
  ) %>% 
  as.data.frame() 

names(dat_all_F) <- c(
  "No.",
  "Outcome", "Covariate 1", 
  # "Interc. est. [95% CI]", 
  "Slope 1 est. [95% CI] (p-val.)",
  "Slope 2 est. [95% CI] (p-val.)",
  "R2m", "R2c")
dat_all_F
table(dat_all_F$Outcome)
# View(dat_all_F)

# rm(dat_all_F)


