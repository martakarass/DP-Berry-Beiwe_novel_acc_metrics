
#' @description 
#' Generate plot visualizing results of modeling associations between 
#' measures and ALSFRS-R. 


rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(MuMIn)
library(lme4)
library(lmerTest)
library(cowplot)
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
# LMM: ALSFRS-RSE ~ measure, with random intercept and slope -- plot
# ------------------------------------------------------------------------------

dat = measures_df
y_name = "frs_total_score"
y_label = "ALSFRS-RSE Q1-12"
var_name_vec = VAR_NAME_VEC_45angle
var_label_vec = VAR_LABEL_VEC_45angle
mod_formula = as.formula(y ~ x_mean + (1 + x_mean | subj_id_fct))
x_scale = FALSE
y_scale = FALSE

pp <- length(var_name_vec)
subj_id_levels <- sort(unique(dat$subj_id))
dat$subj_id_fct <- factor(dat$subj_id, levels = subj_id_levels)

# objects to store results
plt_list <- list()
plt_df_subj_fitted_lines <- data.frame()
plt_df_subj_observed_points <- data.frame()
plt_df_popul_fitted_lines <- data.frame()

for (i in 1 : pp){ # i <- 1
  print(paste0("i = ", i))
  x_name <- var_name_vec[i]
  # prepare model data frame
  mod_df <- 
    dat %>%
    rename_with(~ c("y"), all_of(y_name))  %>%
    rename_with(~ c("x"), all_of(x_name)) %>% 
    filter(!is.na(x)) %>%
    # mutate(x = as.numeric(scale(x_orig, center = TRUE, scale = TRUE))) %>%
    group_by(subj_id_fct, subj_id, survey_local_time_date, y) %>%
    summarise(x_mean = mean(x, na.rm = TRUE)) %>%
    ungroup()
  if (x_scale){
    message("Scaling x...")
    mod_df <- mod_df %>% mutate(x_mean = scale(x_mean)[, 1])
  }
  if (y_scale){
    message("Scaling y...")
    mod_df <- mod_df %>% mutate(y = scale(y)[, 1])
  }
  # fit model
  mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa"))
  mod_df$mu <- getME(mod_out, "mu")
  plt_df_popul <- expand.grid(x_mean = range(mod_df$x_mean))
  plt_df_popul$mu <- predict(mod_out, newdata = plt_df_popul, re.form = NA)
  plt <- 
    ggplot(mod_df, aes(x = x_mean, y = y, color = subj_id_fct, group = subj_id_fct)) + 
    geom_line(aes(x = x_mean, y = mu), size = 0.7, linetype = 1) + 
    geom_point(alpha = 0.5, size = 1.5) + 
    geom_line(data = plt_df_popul, aes(x = x_mean, y = mu), inherit.aes = FALSE,
              size = 0.8, color = "black", alpha = 0.8) + 
    labs(x = x_name, y = y_label, title = "") + 
    theme(legend.position = "none", plot.margin = unit(c(0.2, 0.1, 0.2, 0.1), "cm")) 
  plt_list[[length(plt_list) + 1]] <- plt
  # save data frames
  plt_df_subj_observed_points_i <- mod_df %>% select(x = x_mean, y, subj_id_fct) %>% mutate(x_name = x_name, y_name = y_label)
  plt_df_subj_fitted_lines_i <- mod_df %>% select(x = x_mean, y = mu, subj_id_fct) %>% mutate(x_name = x_name, y_name = y_label)
  plt_df_popul_fitted_lines_i <- plt_df_popul %>% select(x = x_mean, y = mu) %>% mutate(x_name = x_name, y_name = y_label)
  plt_df_subj_fitted_lines <- rbind(plt_df_subj_fitted_lines, plt_df_subj_fitted_lines_i)
  plt_df_subj_observed_points <- rbind(plt_df_subj_observed_points, plt_df_subj_observed_points_i)
  plt_df_popul_fitted_lines <- rbind(plt_df_popul_fitted_lines, plt_df_popul_fitted_lines_i)
}


# ------------------------------------------------------------------------------
# save plot

# plt <- plot_grid(plotlist = plt_list, ncol = 4, align = "hv", byrow = FALSE)
plt <- plot_grid(plotlist = plt_list, ncol = 3, align = "hv", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "lmm_alsfrsrse_vs_measure_fitted.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10 * 4/3, dpi = 300)


# ------------------------------------------------------------------------------
# save data frames

fwrite(plt_df_subj_fitted_lines, file.path(here(), "results_figures_exact_data_frames", "lmm_alsfrsrse_vs_measure_fitted_SUBJECT_FITTED_LINES.csv"))
fwrite(plt_df_subj_observed_points, file.path(here(), "results_figures_exact_data_frames", "lmm_alsfrsrse_vs_measure_fitted_SUBJECT_OBSERVED_POINTS.csv"))
fwrite(plt_df_popul_fitted_lines, file.path(here(), "results_figures_exact_data_frames", "lmm_alsfrsrse_vs_measure_fitted_POPULATION_OBSERVED_POINTS.csv"))





