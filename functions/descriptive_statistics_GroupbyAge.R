check_descriptive_stat = function(df, AgeGroup_below78 = T){
  # by default AgeGroup_below78 is T
  dat_descr = NULL
  # check if the input data have the age column we need
  if (!c("age_baseline") %in% colnames(df) ) {
    stop("The input data frame does not contain column 'age_baseline'")
    }
  if (AgeGroup_below78 == T) {
    dat_descr = describe(df %>% filter(age_baseline < 78),na.rm = T)
  } else{
    dat_descr = describe(df %>% filter(age_baseline >= 78),na.rm = T)
  }
  # for age_baseline < 78, people do not have data at wave 1 so that diff1 = NA; 
  # get rid off the NA rows in the descriptive statistics
  dat_descr = na.omit(dat_descr[-(1:2),c("n", "mean", "se", "min", "max")])
  return(dat_descr)
}

# check_descriptive_stat(df =mri_diff_df, AgeGroup_below78 = TRUE )
# check_descriptive_stat(df =mri_diff_df, AgeGroup_below78 = FALSE )
