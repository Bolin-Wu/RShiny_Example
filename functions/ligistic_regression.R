# source("code/pre_process_data.R")
# source("code/MRIannual_difference.R")


# Plz note: in this analysis, when find the quantile, I do not exclude the increasing values of brain markers.



#------------------------------------------------------------------------------#
#### --------------------------- pre-defined function --------------------------####
#------------------------------------------------------------------------------#

# input: a vector of number
# output: annotate 1/0 according to the quantile threshold
mri_quantile_func <- function(input_mri) {
  mri_quantile <- quantile(na.omit(input_mri))
  # if the median number is above zero, then find the steepest increase (like CSF)
  if (mri_quantile[3] < 0) {
    thres = mri_quantile[2]
    annote_by_threshold <- ifelse(input_mri < thres, 1, 0)
  } else {
    # if the median nnumber is below zero, then find the steepest decrease (like TBTV)
    thres = mri_quantile[4]
    annote_by_threshold <- ifelse(input_mri > thres, 1, 0)
  }
  # join the threshold with ap and mri data
  return(list(annote_by_threshold = annote_by_threshold, median = mri_quantile[3], thres = thres))
  # return(annote_by_threshold)
}


# input: MRI data with sex and age; air pollution data
# output: significant air pullutants paired with certain brain markers
LL_sig_pair_test <- function(mri_df, ap_df) {
  air_pollution_names <- colnames(ap_df %>% select(-"lopnr"))
  mri_names <- mri_df %>%
    select(contains("adj")) %>%
    colnames(.)
  count_sig <- 0
  note_sig <- data.frame(
    AP_sig = character(),
    MRI_sig = character(),
    threshold_above_zero = logical(),
    stringsAsFactors = FALSE
  )
  # loop through every MRI
  for (i in 1:length(mri_names)) {
    dependent_part <- "annotation"
    # add annotation 1 or 0 by pre-defined function
    func_threshold <- mri_quantile_func(mri_df[[mri_names[i]]])
    annotation = func_threshold$annote_by_threshold
    # join the threshold with ap and mri data
    logistic_join_df <- mri_df %>%
      select("lopnr", "Sex", "age_baseline") %>%
      mutate(annotation = as.factor(annotation)) %>%
      left_join(y = ap_df, by = "lopnr")
    for (j in 1:length(air_pollution_names)) {
      ap_name <- air_pollution_names[j]
      independent_part <- paste(c("Sex", "age_baseline", ap_name), collapse = "+")
      baseline_logi_m <- glm(data = logistic_join_df, as.formula(paste(dependent_part, "~ ", independent_part)), family = binomial)
      sig_logi <- summary(baseline_logi_m)$coefficients[ap_name, 4] < 0.05
      if (sig_logi == TRUE) {
        count_sig <- count_sig + 1
        note_sig[count_sig, "AP_sig"] <- ap_name
        note_sig[count_sig, "MRI_sig"] <- dependent_part
        note_sig[count_sig, "threshold_above_zero"] <- mri_quantile[3] > 0
      }
    }
  }
  return(note_sig)
}

# input: MRI data with sex and age; air pollution data; MRI name;AP name
# output: logistic regression result summary
LL_pair_summary <- function(mri_df, ap_df, mri_name, ap_name) {
  record_df <- data.frame(
    MRI_name = character(),
    MRI_threshold = double(),
    MRI_median_above_zero = logical(),
    logistic_significant = logical(),
    stringsAsFactors = FALSE
  )
  dependent_part <- "annotation"
  # add annotation 1 or 0 by pre-defined function
  func_threshold <- mri_quantile_func(mri_df[[mri_name]])
  annotation = func_threshold$annote_by_threshold
  # join the threshold with ap and mri data
  logistic_join_df <- mri_df %>%
    select("lopnr", "Sex", "age_baseline") %>%
    mutate(annotation = as.factor(annotation)) %>%
    left_join(y = ap_df, by = "lopnr")
  independent_part <- paste(c("Sex", "age_baseline", ap_name), collapse = "+")
  baseline_logi_m <- glm(data = logistic_join_df, as.formula(paste(dependent_part, "~ ", independent_part)), family = binomial)
  sig_logi <- summary(baseline_logi_m)$coefficients[ap_name, 4] < 0.05
  print(summary(baseline_logi_m))
  # fill in the information
  record_df[1, "MRI_name"] <- mri_name
  record_df[1, "MRI_threshold"] <- func_threshold$thres
  record_df[1, "MRI_median_above_zero"] <- func_threshold$median > 0
  record_df[1, "logistic_significant"] <- sig_logi
  return(record_df)
}

#--------------------------------------------------------------------#
#### ---------------- logistic regression result -----------------####
#--------------------------------------------------------------------#

# is there any significant AP?

# LL_sig_pair_test(annual_mri_df, air_p)
# no.....

# 
# LL_pair_summary(annual_mri_df, air_p,"CSF_adj_annual", "PM25_3y")
# LL_pair_summary(annual_mri_df, air_p,"TBTV_adj_annual", "PM10_3y")


#--------------------------------------------------------------------#
#### --------------------------- sand box --------------------------####
#--------------------------------------------------------------------#



# 
# 
# quantile(na.omit(annual_mri_df[["TBTV_adj_annual"]]))
