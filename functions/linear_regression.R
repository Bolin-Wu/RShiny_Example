pacman::p_load(reshape2, tidyverse, ggplot2, ggpubr)
#--------------------------------------------------------#
####-------------------- prepare date --------------------####
#-------------------------------------------------------#
# source("code/pre_process_data.R")
# df_join_base
# filter out the independent variables
mri_base_names = mri_iv %>% select(contains("adj_0")) %>% colnames(.)
air_pollution_names = air_p %>% select(-"lopnr")%>% colnames(.)

#----------------------------------------------------------------------#
####-------------------- pre-defined function --------------------####
#----------------------------------------------------------------------#

# function that allows user to input a AP and MRI then output the LR summary result
LR_summary = function(df_join,AP_input, MRI_input){
  independent_part <- paste(c("Sex", "age_baseline",AP_input),collapse = "+")
  dependent_part <- MRI_input
  baseline_m <- lm(data = df_join, as.formula(paste(dependent_part,"~ ", independent_part)))
  return(summary(baseline_m))
}

# function to find if there is any significant AP in linear regression
LR_sig_pair = function(df_join){
  count_sig = 0
  note_sig = data.frame(AP_sig=character(),
                        MRI_sig=character(),
                        stringsAsFactors=FALSE)
  mri_names = df_join %>% select(contains("adj")) %>% colnames(.)
  air_pollution_names = df_join %>% select(matches("PM|NOX")) %>% colnames(.)
  
  for (i in 1:length(mri_names)) {
    dependent_part <- mri_names[i]
    for (j in 1:length(air_pollution_names)) {
      ap_name = air_pollution_names[j]
      independent_part <- paste(c("Sex", "age_baseline",ap_name),collapse = "+")
      baseline_m <- lm(data = df_join, as.formula(paste(dependent_part,"~ ", independent_part)))
      sig_logi = summary(baseline_m)$coefficients[ap_name,4] < 0.05
      if (sig_logi == TRUE) {
        count_sig = count_sig + 1
        note_sig[count_sig,"AP_sig"] = ap_name
        note_sig[count_sig,"MRI_sig"] = dependent_part
      }
    }
  }
  return(note_sig)
}


#----------------------------------------------------------------------#
####-------------------- cross-sectional analysis --------------------####
#----------------------------------------------------------------------#
# df_join_base


# these pairs of air pollution with MRI brain markers are significant in the linear regression
# LR_sig_pair(df_join_base)

# get specific linear regression summary result
# air_pollution_names
# LR_summary(df_join_base,AP_input = "TBTV_adj_0", MRI_input = "PM25_3y")



#----------------------------------------------------------------------#
####-------------------- longitudinal analysis --------------------####
#----------------------------------------------------------------------#
# get the annual change data
# source("code/MRIannual_difference.R")
# annual_mri_df

df_join_annual = left_join(x = annual_mri_df , y = air_p, by = "lopnr")


LR_sig_pair(df_join_annual)
# no air pollutant is significant in any pair.

# try the pre-defined LR_summary function at annual data
mri_annual_names =  annual_mri_df %>% select(contains("adj")) %>% colnames(.)
# mri_annual_names
# LR_summary(df_join_annual,AP_input = "NOX_3y", MRI_input = "TBTV_adj_annual")














