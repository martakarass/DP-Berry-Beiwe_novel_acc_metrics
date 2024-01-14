#' @description 
#' Generate table 1. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read sample subset
# ------------------------------------------------------------------------------

study_sample_df_path <- file.path(here(), 'data_participants_other_processed', 'study_analysis_sample.csv')
study_sample_df <- fread(study_sample_df_path) %>% as.data.frame()
dim(study_sample_df)


# ------------------------------------------------------------------------------
# read participants characteristics
# ------------------------------------------------------------------------------

#' sex: 0 Female, 1 Male, 2 Prefer to not answer
#' ethnicity: 0 Hispanic or Latino, 1 NOT Hispanic or Latino, 2 Unknown / Not Reported
#' race: 
#' 0 American Indian/Alaska Native
#' 1 Asian
#' 2 Native Hawaiian or Other Pacifi c Islander
#' 3 Black or African American
#' 4 White
#' 5 More Than One Race
#' 6 Unknown / Not Reported
#' 7 Other
#' ios_android: 
#' 1 iOS
#' 2 Android

demog_df_path <- file.path(here(), 'data_participants_other_processed', 'wearables_table1_2022-05-15.csv')
demog_df <- 
  fread(demog_df_path) %>% 
  as.data.frame() %>%
  mutate(
    subj_id = as.numeric(str_sub(record_id, 5, 7))
  ) %>%
  inner_join(study_sample_df) %>%
  select(
    subj_id,
    age,
    sex,
    ethnicity,
    race,
    ios_android
  ) %>%
  mutate(
    sex = recode(sex, '0'='Female', '1'='Male', '2'='Prefer to not answer'),
    ethnicity = recode(ethnicity, '0'='Hispanic or Latino', '1'= 'Not Hispanic or Latino', '2'= 'Unknown / Not Reported'),
    race = recode(race, '0'= 'American Indian/Alaska Native', '1'= 'Asian', '2'= 'Native Hawaiian or Other Pacific Islander', '3'= 'Black or African American', '4'= 'White', '5'= 'More Than One Race', '6'= 'Unknown / Not Reported', '7'= 'Other'),
    ios_android = recode(ios_android, '1'= 'iOS', '2'= 'Android')
  )
demog_df
dim(demog_df)


#------------------------------------------------------------------------------
# read surveys responses
# ------------------------------------------------------------------------------

alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrs_ext.csv")
alsfrsrse <- 
  fread(alsfrsrse_path) %>%
  as.data.frame() %>%
  filter(subj_id %in% study_sample_df$subj_id)
head(alsfrsrse)
length(unique(alsfrsrse$subj_id))

# ALSFRS-R in-clinic, subset to keep first (baseline) data only
alsfrsrse_baseline <-
  alsfrsrse %>%
  group_by(subj_id, beiwe_id) %>%
  filter(local_time_date == min(local_time_date)) %>%
  ungroup() 

head(alsfrsrse_baseline)
length(unique(alsfrsrse_baseline$subj_id))



# ------------------------------------------------------------------------------
# Combine and make a table
# ------------------------------------------------------------------------------

comb_df <- 
  alsfrsrse_baseline %>% 
  select(subj_id, alsfrsrse_baseline = frs_total_score, alsfrsrse_baseline_date = local_time_date) %>%
  inner_join(demog_df) 
dim(comb_df)
# [1] 20  8

# ------------------------------------------------------------------------------
# count 
tbl_cnt <- 
  comb_df %>%
  summarise(value_f = n()) %>%
  mutate(var_name = "Count all (n)", .before = everything())


# ------------------------------------------------------------------------------
# age -- mean_sd
tbl_age_mean_sd <- 
  comb_df %>%
  summarise(
    value_mean = mean(age),
    value_sd = sd(age)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Age Mean (SD)", .before = everything())

# age -- median_min_max
tbl_age_median_min_max <- 
  comb_df %>%
  summarise(
    value_median = median(age),
    value_min = min(age),
    value_max = max(age)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Age Median [min, max]", .before = everything())


# ------------------------------------------------------------------------------
# sex 
sex_levels <- names(sort(table(comb_df$sex), decreasing = TRUE))
tbl_sex <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(sex, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, sex) %>%
  rename(var_name = sex) %>%
  mutate(var_name = factor(var_name, levels = sex_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Sex ", var_name, " (n (%))"))
tbl_sex[is.na(tbl_sex)] <- "0 (0.0%)"
tbl_sex


# ------------------------------------------------------------------------------
# ethnicity 
ethnicity_levels <- names(sort(table(comb_df$ethnicity), decreasing = TRUE))
tbl_ethnicity <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(ethnicity, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, ethnicity) %>%
  rename(var_name = ethnicity) %>%
  mutate(var_name = factor(var_name, levels = ethnicity_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Ethnicity ", var_name, " (n (%))"))
tbl_ethnicity[is.na(tbl_ethnicity)] <- "0 (0.0%)"
tbl_ethnicity


# ------------------------------------------------------------------------------
# race 
race_levels <- names(sort(table(comb_df$race), decreasing = TRUE))
tbl_race <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(race, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, race) %>%
  rename(var_name = race) %>%
  mutate(var_name = factor(var_name, levels = race_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Race ", var_name, " (n (%))"))
tbl_race[is.na(tbl_race)] <- "0 (0.0%)"
tbl_race


# ------------------------------------------------------------------------------
# ios_android 
ios_android_levels <- names(sort(table(comb_df$ios_android), decreasing = TRUE))
tbl_ios_android <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(ios_android, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, ios_android) %>%
  rename(var_name = ios_android) %>%
  mutate(var_name = factor(var_name, levels = ios_android_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("ios_android ", var_name, " (n (%))"))
tbl_ios_android[is.na(tbl_ios_android)] <- "0 (0.0%)"
tbl_ios_android


# ------------------------------------------------------------------------------
# alsfrsrse_baseline -- mean_sd
tbl_alsfrsrse_baseline_mean_sd <- 
  comb_df %>%
  summarise(
    value_mean = mean(alsfrsrse_baseline),
    value_sd = sd(alsfrsrse_baseline)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Baseline ALSFRS-RSE Mean (SD)", .before = everything())
tbl_alsfrsrse_baseline_mean_sd


# alsfrsrse_baseline -- median_min_max
tbl_alsfrsrse_baseline_median_min_max <- 
  comb_df %>%
  summarise(
    value_median = median(alsfrsrse_baseline),
    value_min = min(alsfrsrse_baseline),
    value_max = max(alsfrsrse_baseline)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Baseline ALSFRS-RSE Median [min, max]", .before = everything())
tbl_alsfrsrse_baseline_median_min_max



# ------------------------------------------------------------------------------
# combine altogether 

tbl_out <- 
  tbl_cnt %>%
  rbind(tbl_age_mean_sd) %>%
  rbind(tbl_age_median_min_max) %>%
  rbind(tbl_sex) %>%
  rbind(tbl_ethnicity) %>%
  rbind(tbl_race) %>%
  rbind(tbl_ios_android) %>%
  rbind(tbl_alsfrsrse_baseline_mean_sd) %>%
  rbind(tbl_alsfrsrse_baseline_median_min_max) 
tbl_out



# ------------------------------------------------------------------------------
# save to file: table 1
# ------------------------------------------------------------------------------

tbl_out_path <- file.path(here(), 'results_tables', "table_1.csv")
fwrite(tbl_out, tbl_out_path)


