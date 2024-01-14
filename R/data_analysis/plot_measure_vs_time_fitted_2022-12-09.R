
#' @description 
#' Generate plot visualizing results of modeling measure change over time.

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
# check 
dim(measures_df)
head(measures_df)
length(unique(measures_df$subj_id))


# ------------------------------------------------------------------------------
# LMM: measure ~ time 
# ------------------------------------------------------------------------------

dat = measures_df
var_name_vec = VAR_NAME_VEC_45angle
var_label_vec = VAR_LABEL_VEC_45angle
mod_formula <- as.formula(y ~ month_relative + (1 + month_relative | subj_id_fct))

pp <- length(var_name_vec)
subj_id_levels <- sort(unique(dat$subj_id))
dat$subj_id_fct <- factor(dat$subj_id, levels = subj_id_levels)

# objects to store results
plt_list <- list()
plt_df_subj_fitted_lines <- data.frame()
plt_df_subj_observed_points <- data.frame()
plt_df_popul_fitted_lines <- data.frame()

# iterate over different wearable factors
for (i in 1 : pp){ # i <- 1
  print(paste0("i = ", i))
  # define model outcome y
  y_name <- var_name_vec[i]
  mod_df <- dat
  mod_df$y <- mod_df %>% pull(y_name) 
  mod_df <- mod_df %>% filter(!is.na(y)) 
  # fit model
  mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa"))
  mod_df$mu <- getME(mod_out, "mu")
  plt_df_popul <- expand.grid(month_relative = range(mod_df$month_relative))
  plt_df_popul$mu <- predict(mod_out, newdata = plt_df_popul, re.form = NA)
  plt <- 
    ggplot(mod_df, aes(x = month_relative, y = y, color = subj_id_fct, group = subj_id_fct)) + 
    geom_line(aes(x = month_relative, y = mu), size = 0.5, linetype = 1) + 
    geom_point(alpha = 0.3, size = 0.7) + 
    geom_line(data = plt_df_popul, aes(x = month_relative, y = mu), inherit.aes = FALSE,
              size = 0.8, color = "black", alpha = 0.8) + 
    labs(x = "Time [months]", y = y_name, title = "") + 
    theme(legend.position = "none", plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) 
  plt_list[[length(plt_list) + 1]] <- plt
  # save data frames
  plt_df_subj_observed_points_i <- mod_df %>% select(x = month_relative, y, subj_id_fct) %>% mutate(x_name = "Time [months]", y_name = y_name)
  plt_df_subj_fitted_lines_i <- mod_df %>% select(x = month_relative, y = mu, subj_id_fct) %>% mutate(x_name = "Time [months]", y_name = y_name)
  plt_df_popul_fitted_lines_i <- plt_df_popul %>% select(x = month_relative, y = mu) %>% mutate(x_name = "Time [months]", y_name = y_name)
  plt_df_subj_fitted_lines <- rbind(plt_df_subj_fitted_lines, plt_df_subj_fitted_lines_i)
  plt_df_subj_observed_points <- rbind(plt_df_subj_observed_points, plt_df_subj_observed_points_i)
  plt_df_popul_fitted_lines <- rbind(plt_df_popul_fitted_lines, plt_df_popul_fitted_lines_i)
}


# ------------------------------------------------------------------------------
# save plot

# plt <- plot_grid(plotlist = plt_list, ncol = 4, align = "hv", byrow = FALSE)
plt <- plot_grid(plotlist = plt_list, ncol = 3, align = "hv", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "lmm_measure_vs_time_fitted.jpeg")
# save_plot(filename = plt_fpath, plot = plt, base_width = 3 * 4, base_height = 3 * 3.2, dpi = 150)
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10 * 4/3, dpi = 300)


# ------------------------------------------------------------------------------
# save data frames

fwrite(plt_df_subj_fitted_lines, file.path(here(), "results_figures_exact_data_frames", "lmm_measure_vs_time_fitted_SUBJECT_FITTED_LINES.csv"))
fwrite(plt_df_subj_observed_points, file.path(here(), "results_figures_exact_data_frames", "lmm_measure_vs_time_fitted_SUBJECT_OBSERVED_POINTS.csv"))
fwrite(plt_df_popul_fitted_lines, file.path(here(), "results_figures_exact_data_frames", "lmm_measure_vs_time_fitted_POPULATION_OBSERVED_POINTS.csv"))






