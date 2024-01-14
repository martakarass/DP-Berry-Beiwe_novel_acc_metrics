
#' @description 
#' Generate plot visualizing results of modeling associations between 
#' measures and ALSFRS-R, highlighting how the fixed effect of interest varies
#' for different choices of degree parameter in measure definition.


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

VAR_LABEL_VEC_ALL <- c(VAR_LABEL_VEC, VAR_LABEL_VEC_90angle, VAR_LABEL_VEC_135angle)

dat1 <- 
  fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_withrandomslope_45angle.csv")) 
dat2 <- 
  fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_withrandomslope_90angle.csv")) 
dat3 <- 
  fread(file.path(here(), "results_tables", "lmm_alsfrsrse_vs_measure_est_withrandomslope_135angle.csv")) 

dat <- 
  as.data.frame(dat1) %>% 
  rbind(as.data.frame(dat2))%>% 
  rbind(as.data.frame(dat3)) %>%
  mutate(
    is_signf = ifelse(slope_ci_lo * slope_ci_up > 0, 1, 0),
    is_signf_fct = factor(is_signf, levels = c(0,1)),
    x_label_fct = factor(x_label, levels = VAR_LABEL_VEC_ALL),
    y_label_fct = factor(y_label, levels = rev(Y_LEVEL_VEC), labels = rev(Y_LABEL_VEC2))
  ) %>%
  separate(x_label, into = c("x_label_group", "x_label_val"), sep = "(?<=[A-Za-z])(?=[0-9])", remove = FALSE) %>%
  mutate(x_label_val_fct = factor(as.numeric(x_label_val), levels = c(45, 90, 135))) %>%
  filter(conv >= 0)

dim(dat)
head(dat)


# ------------------------------------------------------------------------------
# var_name_vec = VAR_LABEL_VEC
x_label_group_vec <- unique(dat$x_label_group)

pp <- length(x_label_group_vec)

val_max <- max(c(abs(dat$slope_ci_lo), abs(dat$slope_ci_up)))
val_max
y_lims <- c(-1, 1) * 1
  
# iterate over different wearable factors
# objects to store results
plt_list <- list()
plt_df_coefs <- data.frame()

for (i in 1 : pp){ # i <- 1
  print(paste0("i = ", i))
  # define model outcome y
  x_label_group_i <- x_label_group_vec[i]
  plt_df <- 
    dat %>% 
    filter(x_label_group == x_label_group_i)
  plt <- 
    ggplot(plt_df) + 
    geom_errorbar(aes(x = y_label_fct, ymin = slope_ci_lo, ymax = slope_ci_up), 
                  width = 0.1) +
    geom_point(aes(x = y_label_fct, y = slope_est, fill = is_signf_fct), 
               shape = 21, size = 2) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    facet_grid(. ~ x_label_fct) + 
    scale_fill_manual(values = c("black", "red"), drop = FALSE) + 
    labs(
      x = "",
      # y = ""
      y = x_label_group_i
    ) +
    scale_y_continuous(breaks = c(-1, 0, 1)) + 
    coord_flip(ylim = y_lims) + 
    theme_grey(base_size = 12) %+replace%    
    theme(legend.background = element_rect(fill=alpha('white', 0.6), color = NA),
          panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    # theme(plot.margin = unit(rep(0.3, 4), "cm")) 
  # plt
  plt_list[[length(plt_list) + 1]] <- plt
  plt_df_coefs_i <-  
    plt_df %>% 
    select(y_name = y_label_fct, x_name = x_label, x_name_group = x_label_group,
           slope_est, slope_ci_lo, slope_ci_up)
  plt_df_coefs <- rbind(plt_df_coefs, plt_df_coefs_i)

}


# ------------------------------------------------------------------------------
# save plot

# plt <- plot_grid(plotlist = plt_list, ncol = 4, align = "hv", byrow = FALSE)
plt <- plot_grid(plotlist = plt_list, ncol = 3, align = "hv", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "lmm_measure_vs_time_coefs_vary_angle_degrees.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 9, dpi = 300)


# ------------------------------------------------------------------------------
# save data frame

fwrite(plt_df_coefs, file.path(here(), "results_figures_exact_data_frames", "lmm_measure_vs_time_coefs_vary_angle_degrees.csv"))
