pacman::p_load(tidyverse, lubridate)

# read the newly provided date data
date_df <- read_dta("data/date_assessment.dta")
#----------------------------------------------------------------#
#### -----------------------predefined function--------------------------####
#------------------------------------------------------------------#
# find annual of measures in people participating in different waves
wave_filter <- function(df, wave_num) {
  essential_name <- c(
    "TBTV_adj_0", "GM_adj_0", "WMvolumes_adj_0", "CSF_adj_0", "Hippocampus_mean_adj_0",
    "Hippocampus_sum_adj_0", "Hippocampus_left_adj_0", "Hippocampus_right_adj_0",
    "Ventricles_adj_0", "WMHypertint_adj_0", "TBTV_adj_1", "GM_adj_1",
    "WMvolumes_adj_1", "CSF_adj_1", "Hippocampus_mean_adj_1", "Hippocampus_sum_adj_1",
    "Hippocampus_left_adj_1", "Hippocampus_right_adj_1", "Ventricles_adj_1",
    "WMHypertint_adj_1", "TBTV_adj_2", "GM_adj_2", "WMvolumes_adj_2",
    "CSF_adj_2", "Hippocampus_mean_adj_2", "Hippocampus_sum_adj_2",
    "Hippocampus_left_adj_2", "Hippocampus_right_adj_2", "Ventricles_adj_2",
    "WMHypertint_adj_2"
  )
  if (!all(essential_name %in% colnames(df))) {
    stop("The input data do not contain required ten adjusted variables at baseline, wave one and wave 2")
  }
  base_MRIname <- colnames(df %>% select(contains(paste0("adj_", 0))))
  wave1_MRIname <- colnames(df %>% select(contains(paste0("adj_", 1))))
  wave2_MRIname <- colnames(df %>% select(contains(paste0("adj_", 2))))
  filter_df <- NULL
  if (setequal(wave_num, c(0, 1, 2))) {
    filter_df <- df %>%
      filter_at(all_of(base_MRIname), any_vars(!is.na(.))) %>%
      filter_at(all_of(wave1_MRIname), any_vars(!is.na(.))) %>%
      filter_at(all_of(wave2_MRIname), any_vars(!is.na(.)))
  } else if (setequal(wave_num, c(0, 1))) {
    filter_df <- df %>%
      filter_at(all_of(base_MRIname), any_vars(!is.na(.))) %>%
      filter_at(all_of(wave1_MRIname), any_vars(!is.na(.))) %>%
      filter_at(all_of(wave2_MRIname), all_vars(is.na(.)))
  } else if (setequal(wave_num, c(0, 2))) {
    filter_df <- df %>%
      filter_at(all_of(base_MRIname), any_vars(!is.na(.))) %>%
      filter_at(all_of(wave1_MRIname), all_vars(is.na(.))) %>%
      filter_at(all_of(wave2_MRIname), any_vars(!is.na(.)))
  } else if (setequal(wave_num, c(0))) {
    filter_df <- df %>%
      filter_at(all_of(base_MRIname), any_vars(!is.na(.))) %>%
      filter_at(all_of(wave1_MRIname), all_vars(is.na(.))) %>%
      filter_at(all_of(wave2_MRIname), all_vars(is.na(.)))
  } else {
    print("incorrect wave number input, it should be 0, c(0,1), c(0,2) or c(0,1,2)")
  }
  return(filter_df)
}


cal_annual_mri <- function(df, interval_year) {
  annual_mri <- df
  mri_name <- names(df)[grepl("adj", names(df))]
  freq_tbl <- table(sub("_adj.*", "", mri_name))
  unique_mri_name <- unique(sub("_adj.*", "", mri_name))
  # iteration over 1 to 10 (number of measures)
  for (i in 1:length(unique_mri_name)) {
    # pick specific MRI variable name
    base_name <- unique_mri_name[i]
    # wave number, either 1 or 2
    wave_num <- freq_tbl[base_name] - 1
    # baseline variable name which is used in the subtraction
    baseline_name <- paste0(base_name, "_adj_0")
    # new column name
    new_var_name <- paste0(base_name, "_adj_annual")
    annual_mri[[new_var_name]] <- NA
    for (j in 1:wave_num) {
      # target wave's variable name
      wave_name <- paste0(base_name, "_adj_", j)
      # check if the participants have any data at wave 1 or wave 2
      # if participants come for both waves, then the second iteration will over-write the
      # one calculated at first iteration.
      if (sum(!is.na(annual_mri[[wave_name]])) != 0) {
        # calculate, annual = (adj_wave - base) / interval year
        annual_mri[[new_var_name]] <- (annual_mri[[wave_name]] - annual_mri[[baseline_name]]) / interval_year
      } else {
        (j <- j + 1)
      }
    }
  }
  return(annual_mri %>% select("lopnr", "Sex", "age_baseline", contains("annual")))
}



# calculate the accurate interval year with updated date_assessment.dta
# the year is rounded of a 6 month (0.5 years).
# i.e. If the interval year is 6.6 years then it is rounded to 7 years; if 6.4 years then rounded to 6 years
accur_year <- function(filtered_df, date_df, validated_wave) {
  #  date at validated wave
  date_wave <- paste0("DateC1", validated_wave)
  # date of interested observations
  date_obs <- left_join(x = filtered_df, y = date_df, by = "lopnr")
  # print(date_obs)
  # select columns for calculation
  date_obs <- date_obs %>% select("lopnr", "date_baseline", date_wave)
  accurate_year <- date_obs %>%
    # use lubridate to find the difference in month
    transmute(year_difference = round(interval(
      date_baseline,
      get(date_wave)
    ) %/% months(1) / 12))
  return(accurate_year$year_difference)
}

# input: validated_wave (0 or 1 or 2); filtered_mri from wave_filter function; date_df from new date data
# output: the participants with new interval year benefited from the new date dataset.
# filter_inproved_interval <- function(validated_wave, filtered_mri, date_df) {
#   date_wave <- paste0("DateC1", validated_wave)
#   accur_interval_year = accur_year(filtered_df = filtered_mri, date_df = date_df, validated_wave = validated_wave)
#   kable(left_join(x = filtered_mri, y = date_df, by = "lopnr") %>%
#           select("lopnr", "date_baseline", date_wave) %>%
#           slice(n = which(accur_interval_year != validated_wave * 3)) %>%
#           mutate(original_interval = validated_wave * 3) %>% 
#           # use lubridate to find the difference in month
#           mutate(real_year_diff = interval(
#             date_baseline,
#             get(date_wave)
#           ) %/% months(1) / 12) %>%
#           mutate(updated_interval = round(real_year_diff)) 
#           , "simple")
# }

filter_inproved_interval <- function(validated_wave, filtered_mri, date_df) {
  date_wave <- paste0("DateC1", validated_wave)
  accur_interval_year = accur_year(filtered_df = filtered_mri, date_df = date_df, validated_wave = validated_wave)
  updated_interval = left_join(x = filtered_mri, y = date_df, by = "lopnr") %>%
          select("lopnr", "date_baseline", date_wave) %>%
          slice(n = which(accur_interval_year != validated_wave * 3)) %>%
          mutate(original_interval = validated_wave * 3) %>% 
          # use lubridate to find the difference in month
          mutate(real_year_diff = interval(
            date_baseline,
            get(date_wave)
          ) %/% months(1) / 12) %>%
          mutate(updated_interval = round(real_year_diff))
  
  return(updated_interval)     
}



#### -------------------------------------------------------------------####

# combine the mri dataframe with interested variables with the date dataframe
# left_join(x = mri_iv, y = air_p, by = "lopnr")
#
# wave_filter(df = mri_iv, wave_num = c(0, 1, 2))

# nrow(wave_filter(df = mri_iv, wave_num = 0)) +
#   nrow(wave_filter(df = mri_iv, wave_num = c(0, 1, 2))) +
#   nrow(wave_filter(df = mri_iv, wave_num = c(1, 0))) +
#   nrow(wave_filter(df = mri_iv, wave_num = c(2, 0)))

# 1. only baseline
mri_0 <- wave_filter(df = mri_iv, wave_num = 0)
annual_mri_0 <- cal_annual_mri(df = mri_0, interval_year = 0)
# annual_mri_0$TBTV_adj_annual

# 2. only baseline and wave 1
mri_01 <- wave_filter(df = mri_iv, wave_num = c(0, 1))

accur_interval_year <- accur_year(mri_01, date_df, validated_wave = 1)
annual_mri_01 <- cal_annual_mri(df = mri_01, interval_year = accur_interval_year)

# 3. only baseline and wave 2
mri_02 <- wave_filter(df = mri_iv, wave_num = c(0, 2))
accur_interval_year <- accur_year(mri_02, date_df, validated_wave = 2)
annual_mri_02 <- cal_annual_mri(df = mri_02, interval_year = accur_interval_year)
# view(cal_annual_mri(df = mri_02, interval_year = 6))

# 4. baseline, wave 1 and wave 2
mri_012 <- wave_filter(df = mri_iv, wave_num = c(0, 1, 2))
# this part might be a bit problematic, because we need to check if the change of brain is linear
accur_interval_year <- accur_year(mri_012, date_df, validated_wave = 2)

annual_mri_012 <- cal_annual_mri(df = mri_012, interval_year = accur_interval_year)

# combine together all the above 4 situations
annual_mri_df <- rbind(annual_mri_012, annual_mri_01, annual_mri_02, annual_mri_0)


#------------------------------------------------------------------------------------------#
#### ----------------- check the validity of combining the interval =3 and 6 ------------####
#------------------------------------------------------------------------------------------#
# rbind(
#   annual_mri_02 %>% filter(age_baseline >= 78),
#   annual_mri_01 %>% filter(age_baseline >= 78),
#   annual_mri_012 %>% filter(age_baseline >= 78)
# )


#
# summary(annual_mri_02 %>% filter(age_baseline >= 78))
# summary(annual_mri_01 %>% filter(age_baseline >= 78))
# TBTV_adj_annual, WMvolumes, these two variables are very different due to interval selection

#------------------------------------------------------------------------------------------#
#### ----------------------------- check the interval time ------------------------####
#------------------------------------------------------------------------------------------#
#
# mri_b %>% select("lopnr", "DateofMRIBaseline")
# left_join(x = mri_b, y = date_df, by = "lopnr") %>% select("lopnr", date_baseline)
# the DateofMRIBaseline in the original data from Giulia is different from the
# date_baseline in the new date_assessment.dta
