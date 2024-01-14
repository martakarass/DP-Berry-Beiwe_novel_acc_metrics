require(lme4)
options(dplyr.summarise.inform = FALSE)

# https://stackoverflow.com/questions/72090177/how-can-i-know-whether-the-model-is-converged-or-failed-to-converge-in-lme4-with
# Reutrns: 
# - 1 if the model converged normally ie not to a singular fit, 
# - 0 if it converges to a singular fit 
# - (-1) if it fails to converge
merMod_has_converged <- function (mm) {
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  retval <- NULL
  if(is.null(unlist(mm@optinfo$conv$lme4))) {
    retval = 1
  }
  else {
    if (isSingular(mm)) {
      retval = 0
    } else {
      retval = -1
    }
  }
  return(retval)
}


# ------------------------------------------------------------------------------

# get CI for fixed effects (default: with the use of lmertest)
get_mod_ci1 <- function(mod_out, mod_df, ci_confint = FALSE){
  mod_coef_names <- names(fixef(mod_out))
  # boot CI 
  if (ci_confint){
    out_vec <- tryCatch({
      c(
        as.numeric(confint(mod_out, parm = mod_coef_names[1])),
        as.numeric(confint(mod_out, parm = mod_coef_names[2]))
      )
    }, error = function(ee){
      message("Calculating Wald intervals...")
      c(
        as.numeric(confint(mod_out, parm = mod_coef_names[1], method = "Wald")),
        as.numeric(confint(mod_out, parm = mod_coef_names[2], method = "Wald"))
      )
    })
  } else {
    # lmertest CI
    out_vec <- c(
      as.numeric(contest(mod_out, L = c(1, 0), joint = FALSE)[c("lower", "upper")]),
      as.numeric(contest(mod_out, L = c(0, 1), joint = FALSE)[c("lower", "upper")])
    )
    if (max(abs(out_vec)) > 10e6){
      out_vec <- get_mod_ci1(mod_out, mod_df, ci_confint = TRUE)
    }
  }
  # out_vec <- round(out_vec, 3)
  return(out_vec)
}


get_mod_ci2 <- function(mod_out, mod_df, ci_confint = FALSE){
  mod_coef_names <- names(fixef(mod_out))
  # boot CI 
  if (ci_confint){
    out_vec <- tryCatch({
      c(
        as.numeric(confint(mod_out, parm = mod_coef_names[1])),
        as.numeric(confint(mod_out, parm = mod_coef_names[2])),
        as.numeric(confint(mod_out, parm = mod_coef_names[3]))
      )
    }, error = function(ee){
      message("Calculating Wald intervals...")
      c(
        as.numeric(confint(mod_out, parm = mod_coef_names[1], method = "Wald")),
        as.numeric(confint(mod_out, parm = mod_coef_names[2], method = "Wald")),
        as.numeric(confint(mod_out, parm = mod_coef_names[3], method = "Wald"))
      )
    })
  } else {
    # lmertest CI
    out_vec <- c(
      as.numeric(contest(mod_out, L = c(1, 0, 0), joint = FALSE)[c("lower", "upper")]),
      as.numeric(contest(mod_out, L = c(0, 1, 0), joint = FALSE)[c("lower", "upper")]),
      as.numeric(contest(mod_out, L = c(0, 0, 1), joint = FALSE)[c("lower", "upper")])
    )
    if (max(abs(out_vec)) > 10e6){
      out_vec <- get_mod_ci2(mod_out, mod_df, ci_confint = TRUE)
    }
  }
  # out_vec <- round(out_vec, 3)
  return(out_vec)
}



# ------------------------------------------------------------------------------
# measure ~ time

# function to fit models separately for each predictor listed in var_name_vec
get_lmm_measure_vs_time <- function(dat, var_name_vec, var_label_vec, mod_formula, round_fct = 3){
  pp <- length(var_name_vec)
  # objects to store model estimation results
  out_y_name <- rep(NA, pp)
  out_intcp_est <- rep(NA, pp)
  out_slope_est <- rep(NA, pp)
  out_intcp_pval <- rep(NA, pp)
  out_slope_pval <- rep(NA, pp)
  out_intcp_ci_lo <- rep(NA, pp)
  out_intcp_ci_up <- rep(NA, pp)
  out_slope_ci_lo <- rep(NA, pp)
  out_slope_ci_up <- rep(NA, pp)
  out_R2m <- rep(NA, pp)
  out_R2c <- rep(NA, pp)
  out_conv <- rep(NA, pp)
  # create beiwe factor
  subj_id_levels <- sort(unique(dat$subj_id))
  dat$subj_id_fct <- factor(dat$subj_id, levels = subj_id_levels)
  for (i in 1 : pp){ # i <- 1
    message(paste0("i = ", i))
    # define model outcome y
    y_name <- var_name_vec[i]
    mod_df <- dat
    mod_df$y <- mod_df %>% pull(y_name) 
    mod_df <- mod_df %>% filter(!is.na(y)) 
    # fit model
    mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa"))
    # generate additional model information / pull model parameters
    mod_out_ci     <- get_mod_ci1(mod_out, mod_df)
    mod_out_conv   <- merMod_has_converged(mod_out)
    mod_out_s      <- summary(mod_out)
    mod_out_sc     <- mod_out_s$coefficients %>% as.data.frame() %>% janitor::clean_names()
    mod_out_est    <- round(mod_out_sc$estimate, round_fct)
    mod_out_pvalue <- round(mod_out_sc$pr_t, round_fct)
    mod_out_r2     <- round(r.squaredGLMM(mod_out), round_fct)
    # mod_out_ranef_skew  <- round(sapply(ranef(mod_out)$subj_id_fct, skewness), 3)
    # append model parameters
    out_y_name[i]     <- y_name
    out_intcp_est[i]  <- mod_out_est[1]
    out_slope_est[i]  <- mod_out_est[2]
    out_intcp_pval[i] <- mod_out_pvalue[1]
    out_slope_pval[i] <- mod_out_pvalue[2]
    out_intcp_ci_lo[i] <- mod_out_ci[1]
    out_intcp_ci_up[i] <- mod_out_ci[2]
    out_slope_ci_lo[i] <- mod_out_ci[3]
    out_slope_ci_up[i] <- mod_out_ci[4]
    out_R2m[i]        <- mod_out_r2[1]
    out_R2c[i]        <- mod_out_r2[2]
    out_conv[i]       <- mod_out_conv
  }
  # combine into model df
  out_df <- data.frame(
    out_y_label = var_label_vec, 
    out_x_label = rep("Time [months]", pp),
    out_intcp_est,
    out_intcp_pval,
    out_slope_est,
    out_slope_pval,
    out_intcp_ci_lo,
    out_intcp_ci_up,
    out_slope_ci_lo,
    out_slope_ci_up,
    out_R2m,
    out_R2c,
    out_conv
  ) 
  names(out_df) <- gsub("out_", "", names(out_df))
  return(out_df)
}


# ------------------------------------------------------------------------------
# y ~ measure

get_lmm_survey_vs_measure <- function(dat, 
                                      y_name, 
                                      y_label, 
                                      var_name_vec, 
                                      var_label_vec,
                                      mod_formula, 
                                      y_scale = FALSE,
                                      x_scale = FALSE,
                                      round_fct = 3){
  
  pp <- length(var_name_vec)
  # objects to store model estimation results
  out_x_name <- rep(NA, pp)
  out_intcp_est <- rep(NA, pp)
  out_slope_est <- rep(NA, pp)
  out_intcp_pval <- rep(NA, pp)
  out_slope_pval <- rep(NA, pp)
  out_intcp_ci_lo <- rep(NA, pp)
  out_intcp_ci_up <- rep(NA, pp)
  out_slope_ci_lo <- rep(NA, pp)
  out_slope_ci_up <- rep(NA, pp)
  out_R2m <- rep(NA, pp)
  out_R2c <- rep(NA, pp)
  out_conv <- rep(NA, pp)
  # create beiwe factor
  subj_id_levels <- sort(unique(dat$subj_id))
  dat$subj_id_fct <- factor(dat$subj_id, levels = subj_id_levels)
  # iterate over different wearable factors
  for (i in 1 : pp){ # i <- 1
    print(paste0("i = ", i))
    x_name <- var_name_vec[i]
    # prepare model data frame
    mod_df <- 
      dat %>%
      rename_with(~ c("y"), all_of(y_name))  %>%
      rename_with(~ c("x"), all_of(x_name)) %>% 
      filter(!is.na(x)) %>%
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
    mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 20000)))
    # generate additional model information / pull model parameters
    mod_out_conv    <- merMod_has_converged(mod_out)
    mod_out_ci      <- get_mod_ci1(mod_out, mod_df)
    mod_out_s       <- summary(mod_out)
    mod_out_sc      <- mod_out_s$coefficients %>% as.data.frame() %>% janitor::clean_names()
    mod_out_est     <- round(mod_out_sc$estimate, round_fct)
    mod_out_pvalue  <- round(mod_out_sc$pr_t, round_fct)
    mod_out_r2      <- as.vector(round(r.squaredGLMM(mod_out), round_fct))
    # mod_out_ranef_skew  <- round(sapply(ranef(mod_out)$subj_id_fct, skewness), 3)
    # append model parameters
    out_x_name[i]     <- x_name
    out_intcp_est[i]  <- mod_out_est[1]
    out_slope_est[i]  <- mod_out_est[2]
    out_intcp_pval[i] <- mod_out_pvalue[1]
    out_slope_pval[i] <- mod_out_pvalue[2]
    out_intcp_ci_lo[i] <- mod_out_ci[1]
    out_intcp_ci_up[i] <- mod_out_ci[2]
    out_slope_ci_lo[i] <- mod_out_ci[3]
    out_slope_ci_up[i] <- mod_out_ci[4]
    out_R2m[i]        <- mod_out_r2[1]
    out_R2c[i]        <- mod_out_r2[2]
    out_conv[i]       <- mod_out_conv
  }
  # combine into model df
  out_df <- data.frame(
    # out_dataset_name = rep(dataset_name, pp),
    out_y_label = rep(y_label, pp),
    out_x_label = var_label_vec,
    out_intcp_est,
    out_intcp_pval,
    out_slope_est,
    out_slope_pval,
    out_intcp_ci_lo,
    out_intcp_ci_up,
    out_slope_ci_lo,
    out_slope_ci_up,
    out_R2m,
    out_R2c,
    out_conv
  ) 
  names(out_df) <- gsub("out_", "", names(out_df))
  return(out_df)
}


# ------------------------------------------------------------------------------
# y ~ measure + var2 

get_lmm_survey_vs_measure_adj <- function(dat, 
                                          y_name, 
                                          y_label, 
                                          var_name_adj,
                                          var_name_vec, 
                                          var_label_vec,
                                          mod_formula, 
                                          y_scale = FALSE,
                                          x_scale = FALSE,
                                          round_fct = 3){
      
  pp <- length(var_name_vec)
  # objects to store model estimation results
  out_x_name <- rep(NA, pp)
  out_intcp_est <- rep(NA, pp)
  out_slope_est <- rep(NA, pp)
  out_slope2_est <- rep(NA, pp)
  out_intcp_pval <- rep(NA, pp)
  out_slope_pval <- rep(NA, pp)
  out_slope2_pval <- rep(NA, pp)
  out_intcp_ci_lo <- rep(NA, pp)
  out_intcp_ci_up <- rep(NA, pp)
  out_slope_ci_lo <- rep(NA, pp)
  out_slope_ci_up <- rep(NA, pp)
  out_slope2_ci_lo <- rep(NA, pp)
  out_slope2_ci_up <- rep(NA, pp)
  out_R2m <- rep(NA, pp)
  out_R2c <- rep(NA, pp)
  out_conv <- rep(NA, pp)
  # create beiwe factor
  subj_id_levels <- sort(unique(dat$subj_id))
  dat$subj_id_fct <- factor(dat$subj_id, levels = subj_id_levels)
  # iterate over different wearable factors
  for (i in 1 : pp){ # i <- 1
    print(paste0("i = ", i))
    x_name <- var_name_vec[i]
    # prepare model data frame
    mod_df <- 
      dat %>%
      rename_with(~ c("y"), all_of(y_name))  %>%
      rename_with(~ c("x"), all_of(x_name)) %>% 
      rename_with(~ c("x_adj"), all_of(var_name_adj)) %>% 
      filter(!is.na(x)) %>%
      filter(!is.na(x_adj)) %>%
      group_by(subj_id_fct, subj_id, survey_local_time_date, y) %>%
      summarise(
        x_mean = mean(x, na.rm = TRUE),
        x_adj_mean = mean(x_adj, na.rm = TRUE)
      ) %>%
      ungroup()
    if (x_scale){
      message("Scaling x...")
      mod_df <- mod_df %>% mutate(x_mean = scale(x_mean)[, 1])
      mod_df <- mod_df %>% mutate(x_adj_mean = scale(x_adj_mean)[, 1])
    }
    if (y_scale){
      message("Scaling y...")
      mod_df <- mod_df %>% mutate(y = scale(y)[, 1])
    }
    # fit model
    mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 20000)))
    # generate additional model information / pull model parameters
    mod_out_conv    <- merMod_has_converged(mod_out)
    mod_out_ci      <- get_mod_ci2(mod_out, mod_df)
    mod_out_s       <- summary(mod_out)
    mod_out_sc      <- mod_out_s$coefficients %>% as.data.frame() %>% janitor::clean_names()
    mod_out_est     <- round(mod_out_sc$estimate, round_fct)
    mod_out_pvalue  <- round(mod_out_sc$pr_t, round_fct)
    mod_out_r2      <- as.vector(round(r.squaredGLMM(mod_out), round_fct))
    # mod_out_ranef_skew  <- round(sapply(ranef(mod_out)$subj_id_fct, skewness), 3)
    # append model parameters
    out_x_name[i]     <- x_name
    out_intcp_est[i]  <- mod_out_est[1]
    out_slope_est[i]  <- mod_out_est[2]
    out_slope2_est[i]  <- mod_out_est[3]
    out_intcp_pval[i] <- mod_out_pvalue[1]
    out_slope_pval[i] <- mod_out_pvalue[2]
    out_slope2_pval[i] <- mod_out_pvalue[3]
    out_intcp_ci_lo[i] <- mod_out_ci[1]
    out_intcp_ci_up[i] <- mod_out_ci[2]
    out_slope_ci_lo[i] <- mod_out_ci[3]
    out_slope_ci_up[i] <- mod_out_ci[4]
    out_slope2_ci_lo[i] <- mod_out_ci[5]
    out_slope2_ci_up[i] <- mod_out_ci[6]
    out_R2m[i]        <- mod_out_r2[1]
    out_R2c[i]        <- mod_out_r2[2]
    out_conv[i]       <- mod_out_conv
  }
  # combine into model df
  out_df <- data.frame(
    # out_dataset_name = rep(dataset_name, pp),
    out_y_label = rep(y_label, pp),
    out_x_label = var_label_vec,
    out_intcp_est,
    out_intcp_pval,
    out_slope_est,
    out_slope_pval,
    out_slope2_est,
    out_slope2_pval,
    out_intcp_ci_lo,
    out_intcp_ci_up,
    out_slope_ci_lo,
    out_slope_ci_up,
    out_slope2_ci_lo,
    out_slope2_ci_up,
    out_R2m,
    out_R2c,
    out_conv
  ) 
  names(out_df) <- gsub("out_", "", names(out_df))
  return(out_df)
}



message("The file utils.R (with functions and variable names/labels used across >1 script) was read.")


