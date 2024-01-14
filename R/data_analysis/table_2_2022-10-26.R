#' @description 
#' Generate table 2. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)
source(file.path(here(), "R",  "config_figures.R"))
source(file.path(here(), "R",  "utils.R"))
source(file.path(here(), "R",  "config.R"))

# ------------------------------------------------------------------------------
# read sample subset
# ------------------------------------------------------------------------------

study_sample_df_path <- file.path(here(), 'data_participants_other_processed', 'study_analysis_sample.csv')
study_sample_df <- fread(study_sample_df_path) %>% as.data.frame()
dim(study_sample_df)

smart_wearables_start_end_path <- file.path(here(), 'data_participants_other_processed', 'smart_wearables_start_end_dates_clean.csv')
smart_wearables_start_end <- 
  fread(smart_wearables_start_end_path) %>% 
  as.data.frame() %>%
  select(-c(beiwe_id, wearable_type))



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
# alsfrsrse number of submissions

alsfrsrse_submissions_cnt_median_min_max <- 
  alsfrsrse %>%
  group_by(beiwe_id) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "ALSFRS-RSE submissions number Mean (SD), Median [min, max]", .before = everything())


# ------------------------------------------------------------------------------
# alsfrsrse days between submissions

alsfrsrse_submissions_daydiff_median_min_max <- 
  alsfrsrse %>%
  group_by(beiwe_id) %>%
  mutate(days_diff = as.numeric(as.Date(local_time_date) - as.Date(lag(local_time_date))) ) %>%
  filter(!is.na(days_diff)) %>%
  summarise(value = mean(days_diff)) %>%
  ungroup() %>% 
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "ALSFRS-RSE avg. days difference between submissions Mean (SD), Median [min, max]", .before = everything())




# ------------------------------------------------------------------------------
# read wearable data 
# ------------------------------------------------------------------------------

# read data with start end of wearable wear
smart_wearables_start_end_path <- file.path(here(), 'data_participants_other_processed', 'smart_wearables_start_end_dates_clean.csv')
smart_wearables_start_end <- 
  fread(smart_wearables_start_end_path) %>% 
  as.data.frame() %>%
  select(-c(beiwe_id, wearable_type))

# read ALS_forearm_daily measures
# note: assumed these are calculated based on UTC time zone 
measures_df_path <- file.path(here(), "data_actigraph_processed", "accelerometer", "ALS_forearm_daily_2022-07-31.csv")
measures_df <- 
  fread(measures_df_path) %>%
  as.data.frame() %>%
  rename(
    utc_time_date = date,
    subj_id = subject
  ) %>%
  mutate(utc_time_date = as.Date(utc_time_date, format = "%d-%b-%y")) %>%
  ungroup() %>%
  arrange(subj_id, utc_time_date)
dim(measures_df)
head(measures_df)


# read wear_time from ActiGraph Choi
actigraph_t1min_path <- file.path(here(), "data_actigraph_processed", "epochsummarydata.csv")
actigraph_t1min0 <- 
  fread(actigraph_t1min_path) %>% 
  as.data.frame() 
actigraph_t1min <- 
  actigraph_t1min0 %>%
  select(subj_id = SUBJECT, wear_flag = WEAR, ac = VECTORMAGNITUDECOUNTS, utc_time = TIMESTAMPUTC) %>%
  mutate(
    wear_flag = as.numeric(wear_flag),
    utc_time_date = as.Date(utc_time)
  ) 
actigraph_t24h <- 
  actigraph_t1min %>% 
  group_by(
    subj_id, utc_time_date
  ) %>%
  summarise(
    ac = sum(ac),
    wear_time = sum(wear_flag)
  ) %>%
  ungroup()

# add actigraph information,
# filter to keep only days with minimum number of wear minutes
measures_df2 <-
  measures_df %>%
  inner_join(actigraph_t24h, by = c("subj_id", "utc_time_date")) %>%
  mutate(valid_day = as.numeric(wear_time >= WEAR_TIME_MINUTES_MIN))

# filter to keep only the days that fall within sensor wear
# (note: this is expected to change nothing as we already filtered for number of days)
measures_df3 <- 
  measures_df2 %>%
  inner_join(smart_wearables_start_end, by = "subj_id") %>%
  filter(utc_time_date >= date_start, utc_time_date <= date_ended) %>% 
  filter(subj_id %in% unique(study_sample_df$subj_id)) 

length(unique(measures_df3$subj_id))
dim(measures_df3)

wearables_compliance_df <- 
  measures_df3 %>%
  group_by(subj_id, obs_duration) %>%
  summarise(valid_day = sum(valid_day)) %>%
  ungroup()


wearables_compliance_daysinobsperiod <- 
  wearables_compliance_df %>%
  summarise(
    value_median = median(obs_duration),
    value_min = min(obs_duration),
    value_max = max(obs_duration),
    value_mean = mean(obs_duration),
    value_sd = sd(obs_duration)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Days in observation perid  Mean (SD), Median [min, max]", .before = everything())
wearables_compliance_daysinobsperiod



wearables_compliance_validdays <- 
  wearables_compliance_df %>%
  summarise(
    value_median = median(valid_day),
    value_min = min(valid_day),
    value_max = max(valid_day),
    value_mean = mean(valid_day),
    value_sd = sd(valid_day)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Valid days in observation period Mean (SD), Median [min, max]", .before = everything())
wearables_compliance_validdays



# ------------------------------------------------------------------------------
# combine altogether 

tbl_out <- 
  alsfrsrse_submissions_cnt_median_min_max %>%
  rbind(alsfrsrse_submissions_daydiff_median_min_max) %>%
  rbind(wearables_compliance_daysinobsperiod) %>%
  rbind(wearables_compliance_validdays) 
tbl_out

# View(tbl_out)

# ------------------------------------------------------------------------------
# save to file


tbl_out_path <- file.path(here(), 'results_tables', "table_2.csv")
fwrite(tbl_out, tbl_out_path)

