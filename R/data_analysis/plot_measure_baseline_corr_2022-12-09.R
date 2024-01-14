
#' @description 
#' Generate plot visualizing correlations between measures at baseline.


rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(MuMIn)
library(lme4)
library(lmerTest)
library(cowplot)
library(corrplot)
options(digits.secs = 3)
options(scipen = 999)

source(file.path(here(), "R",  "config_figures.R"))
source(file.path(here(), "R",  "utils.R"))
source(file.path(here(), "R",  "config.R"))


# ------------------------------------------------------------------------------
# predefined variables 
# ------------------------------------------------------------------------------

VAR_NAME_VEC_all <- list(
  VAR_NAME_VEC_45angle,
  VAR_NAME_VEC_90angle,
  VAR_NAME_VEC_135angle
)
OUTPUT_SUFFIX_all <- c(
  "45angle",
  "90angle",
  "135angle"
)

# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


# ------------------------------------------------------------------------------
# run
# ------------------------------------------------------------------------------

for (i in 1:3){ # i <- 1
  print(i)
  
  # ------------------------------------------------------------------------------
  # current iteration measures 
  
  VAR_NAME_VEC_tmp = VAR_NAME_VEC_all[[i]]
  OUTPUT_SUFFIX_tmp = OUTPUT_SUFFIX_all[[i]]
  
  vars_vec <- c(
    "subj_id",
    VAR_NAME_VEC_tmp,
    "ac",
    "frs_total_score",
    "frs4567",
    "frs4",
    "frs5",
    "frs6",
    "frs7"
  )
  
  vars_labels_vec <- c(
    "subj_id",
    VAR_NAME_VEC_tmp,
    "TAC",
    "Q1-12",
    "Q4-7",
    "Q4",
    "Q5",
    "Q6",
    "Q7"  
  )
  
  # ------------------------------------------------------------------------------
  # read data, calculate corr 
  
  # measures combined with ALSFRS-RSE
  measures_df_path <- file.path(here(), "data_actigraph_processed", "accelerometer", "ALS_forearm_daily_proc_comb.csv")
  corr_mat <- 
    fread(measures_df_path) %>% 
    as.data.frame() %>%
    group_by(subj_id) %>%
    # filter to keep only the day-level observations associated with the earliest survey
    filter(survey_local_time_date == min(survey_local_time_date)) %>%
    mutate(frs4567 = sum(frs4 + frs5 + frs6 + frs7)) %>%
    select(all_of(vars_vec)) %>% 
    rename_at(all_of(vars_vec), function(x) vars_labels_vec) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    select(-subj_id) %>%
    cor(use = "pairwise.complete.obs") %>%
    round(5)
  
  partial_tri <- get_lower_tri(corr_mat) 
  partial_tri_df <- 
    partial_tri %>%
    as.data.frame() %>%
    rownames_to_column(var = "Var1") %>%
    pivot_longer(cols = -Var1, names_to = "Var2") %>%
    mutate(Var1_fct = factor(Var1, levels = vars_labels_vec)) %>%
    mutate(Var2_fct = factor(Var2, levels = rev(vars_labels_vec))) %>%
    mutate(value_f = sprintf("%.2f", value)) %>%
    filter(complete.cases(.))
  
  # ------------------------------------------------------------------------------
  # generate plot
  
  plt <- 
    ggplot(partial_tri_df, aes(Var1_fct, Var2_fct, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                         limit = c(-1,1), space = "Lab", 
                         name = "Pearson\nCorrelation") +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          legend.position = c(0.15, 0.2)) +
    coord_fixed() + 
    geom_text(aes(Var1_fct, Var2_fct, label = value_f), color = "black", size = 3.3) + 
    labs(x = "", y = "") 
  
  # ------------------------------------------------------------------------------
  # save plot
  
  plt_fpath <- file.path(here(), "results_figures", paste0("baseline_corr_", OUTPUT_SUFFIX_tmp, ".jpeg"))
  save_plot(filename = plt_fpath, plot = plt, base_width = 8, base_height = 8, dpi = 300)
  
  
  # ------------------------------------------------------------------------------
  # save corr matrix
  
  tbl_fpath <- file.path(here(), "results_figures_exact_data_frames", paste0("baseline_corr_", OUTPUT_SUFFIX_tmp, ".csv"))
  fwrite(corr_mat, tbl_fpath)
}


