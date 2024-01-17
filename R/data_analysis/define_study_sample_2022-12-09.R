
#' @description 
#' Define study analysis sample and analytical datasets. In particular,
#' includes definition of "ALS_forearm_daily_proc_comb.csv" that 
#' was used to estimate: 
#' (a) measure's change over time,
#' (b) association between measure and ALSFRS-R score
#' for the measures proposed in the manuscript. 

rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# define beiwe_id sample
# ------------------------------------------------------------------------------

# read alsfrsr
alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrs_ext.csv")
alsfrsrse <- 
  fread(alsfrsrse_path) %>%
  as.data.frame()
head(alsfrsrse)

# clean @Beiwe ID master
master_path <- "/Users/martakaras/Dropbox/Beiwe Resources - for Accelerometry Analysis/data_participants_other/@Beiwe_IDs_Master.csv"
master <- 
  fread(master_path) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  rename(subj_id = subject_id) %>%
  filter(wearables != "no") %>%
  mutate(subj_id = as.numeric(sub("701-", "", subj_id))) %>%
  select(subj_id, als_hc_pls)
head(master)

# read the table with wearable type
smart_wearables_start_end_path <- file.path(here(), 'data_participants_other_processed', 'smart_wearables_start_end_dates_clean.csv')
beiwe_id_actigraph <- 
  fread(smart_wearables_start_end_path) %>% 
  as.data.frame() %>%
  select(beiwe_id, wearable_type) %>%
  filter(!is.na(beiwe_id), wearable_type == "actigraph")

# define "baseline date" as date of the first ALSFRS-R
beiwe_id_sample0 <- 
  alsfrsrse %>% 
  group_by(subj_id, beiwe_id) %>%
  summarise(
    alsfrsr_local_time_date_min = min(local_time_date),
    alsfrsr_local_time_date_max = max(local_time_date),
    cnt = n()
  ) %>%
  ungroup()

beiwe_id_sample1 <- 
  beiwe_id_sample0 %>%
  full_join(master, by = "subj_id") %>%
  as.data.frame() %>%
  filter(als_hc_pls == "ALS") %>%
  filter(cnt >= 2) %>%
  select(subj_id, beiwe_id, alsfrsr_local_time_date_min, alsfrsr_local_time_date_max)

# subset to keep actigraph folks only
beiwe_id_sample2 <- 
  beiwe_id_sample1 %>%
  filter(beiwe_id %in% beiwe_id_actigraph$beiwe_id)

beiwe_id_sample_F <- beiwe_id_sample2

length(unique(beiwe_id_sample_F$subj_id))
length(unique(beiwe_id_sample_F$beiwe_id))

tmp_path <- file.path(here(), "data_participants_other_processed", "study_analysis_sample.csv")
fwrite(beiwe_id_sample_F, tmp_path)


# ------------------------------------------------------------------------------
# define measures data subset 
# ------------------------------------------------------------------------------

# read data with start end of wearable wear
smart_wearables_start_end_path <- file.path(here(), 'data_participants_other_processed', 'smart_wearables_start_end_dates_clean.csv')
smart_wearables_start_end <- 
  fread(smart_wearables_start_end_path) %>% 
  as.data.frame() %>%
  select(-c(beiwe_id, wearable_type, obs_duration))

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
  filter(wear_time >= WEAR_TIME_MINUTES_MIN) 

# filter to keep only the days that fall within sensor wear
# (note: this is expected to change nothing as we already filtered for number of days)
measures_df3 <- 
  measures_df2 %>%
  inner_join(smart_wearables_start_end, by = "subj_id") %>%
  filter(utc_time_date >= date_start, utc_time_date <= date_ended) 

# filter to keep only those which are in the beiwe_id sample
measures_df4 <- 
  measures_df3 %>% 
  inner_join(beiwe_id_sample_F, by = "subj_id")

# filter to keep only days which are no earlier than around baseline 
# (here: 1st ALSFRS-RSE observation)
measures_df5 <- 
  measures_df4 %>% 
  mutate(
    alsfrsr_local_time_date_min = as.Date(alsfrsr_local_time_date_min),
    alsfrsr_local_time_date_max = as.Date(alsfrsr_local_time_date_max)
  ) %>%
  filter(utc_time_date >= (alsfrsr_local_time_date_min - DAYS_DIFF_MAX))

measures_df_F <- measures_df5

length(unique(measures_df_F$subj_id))
length(unique(measures_df_F$beiwe_id))

# save to file
measures_proc_df_path <- file.path(here(), "data_actigraph_processed", "accelerometer", "ALS_forearm_daily_proc.csv")
fwrite(measures_df_F, measures_proc_df_path)


# ------------------------------------------------------------------------------
# define measures data subset, combined with survey
# ------------------------------------------------------------------------------

# merge with measures
alsfrsrse_tojoin <- 
  alsfrsrse %>% 
  rename(survey_local_time_date = local_time_date) %>%
  filter(subj_id %in% beiwe_id_sample_F$subj_id) 

length(unique(alsfrsrse_tojoin$subj_id))
length(unique(alsfrsrse_tojoin$beiwe_id))


dat_comb <- 
  measures_df_F %>% 
  rename(wearbale_utc_time_date = utc_time_date) %>%
  # join with Beiwe surveys
  left_join(alsfrsrse_tojoin, by = c("subj_id", "beiwe_id")) %>%
  mutate(
    # wearbale_utc_time_date: later 
    # survey_local_time_date: earlier
    # diff: the positive 
    days_diff = as.numeric(difftime(wearbale_utc_time_date, survey_local_time_date, units = c("days"))),
    days_diff_abs = abs(days_diff)
  ) %>%
  # to each wearable observation, fit max one survey
  group_by(subj_id, beiwe_id, wearbale_utc_time_date) %>%
  arrange(subj_id, beiwe_id, wearbale_utc_time_date, days_diff_abs) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # filter(days_diff <= DAYS_DIFF_MAX, days_diff > 0) %>%
  filter(days_diff_abs <= DAYS_DIFF_MAX) %>%
  group_by(subj_id, beiwe_id) %>%
  filter(n_distinct(survey_local_time_date) >= 2) %>%
  ungroup() 
# mutate(
#   frs_sub_4_5_6 = frs4 + frs5 + frs6,
#   frs_sub_4_5_6_7 = frs4 + frs5 + frs6 + frs7
# )
dim(dat_comb)

length(unique(dat_comb$subj_id))
length(unique(dat_comb$beiwe_id))

dat_comb_F <- dat_comb

# save to file
dat_comb_F_path <- file.path(here(), "data_actigraph_processed", "accelerometer", "ALS_forearm_daily_proc_comb.csv")
fwrite(dat_comb_F, dat_comb_F_path)




