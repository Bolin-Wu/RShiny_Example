pacman::p_load(haven, tidyverse, psych, mice, VIM, ggplot2,plyr, dplyr,psych, knitr)
#####------------------------- Read data ---------------#####
# read the date
# the data is before the baseline
air_p = read_dta('data/air_pollution_4bolin_20211015.dta')
mri_b = read_dta('data/adjusted_MRI.dta')

#----------------------------------------------------------------------#
#####------------------------- Pre-process data ---------------#####
#----------------------------------------------------------------------#

#  choose the interested variables from MRI dataset
mri_iv = mri_b %>% select("lopnr", "Sex", "Age",contains("adj"), contains("TIV_"))

# change the sex column to be factor
# this is important for future visualization and regression
mri_iv$Sex = as.factor(mri_iv$Sex)


# change Age name, indicating that it is actually the age at baseline
# age, age + 3, age + 6.
names(mri_iv)[names(mri_iv) == 'Age'] <- 'age_baseline'

# pick out MRI related names
mri_name = names(mri_iv)[grepl("adj", names(mri_iv))]
freq_tbl = table(sub("_adj.*", "",mri_name))
unique_mri_name = unique(sub("_adj.*", "",mri_name))
# start making the df with difference values through a loop
mri_diff_df = mri_iv[,1:3]
for (i in 1:length(unique_mri_name)) {
  # a specific MRI variable name
  base_name = unique_mri_name[i]
  # wave number, either 1 or 2
  wave_num = freq_tbl[base_name] - 1
  # baseline variable name which is used in the subtraction
  baseline_name = paste0(base_name,'_adj_0')
  for (j in 1:wave_num) {
    new_var_name = paste0(base_name,'_adj_diff',j)
    wave_name = paste0(base_name,'_adj_',j)
    mri_diff_df[[new_var_name]]= mri_iv[[wave_name]] - mri_iv[[baseline_name]]
  }
}



# join AP and MRI absolute change across the waves
df_join_diff = left_join(x = mri_diff_df, y = air_p, by = "lopnr")

# join AP and MRI measures at baseline, age, sex
df_join_base = left_join(x = mri_iv %>% 
                           select("lopnr","Sex","age_baseline",contains("adj_0")), y = air_p, by = "lopnr")
