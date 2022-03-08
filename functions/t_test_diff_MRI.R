# test the difference of MRI in older group and younger group
# source("code/MRIannual_difference.R")

# get the MRi
younger_MRI <- annual_mri_df %>%
  filter(age_baseline < 78) %>%
  select(contains("adj"))
older_MRI <- annual_mri_df %>%
  filter(age_baseline >= 78) %>%
  select(contains("adj"))

# first check the descriptive statistics manually
# check_descriptive_stat(annual_mri_df,AgeGroup_below78 = T)
# check_descriptive_stat(annual_mri_df,AgeGroup_below78 = F)

# run the loop to make the t-test for every MRI measure
# here x = younger group, y = older group
# null hypothesis is |x| = |y|
# alternative assumption is |x| < |y|, which means the annul change of younger group is smaller than the older group
MRI_t_test <- tibble(.rows = 1)
for (i in 1:length(colnames(younger_MRI))) {
  mri_base_name <- unique(sub("_adj.*", "", names(younger_MRI)))
  MRI_t_test[, mri_base_name[i]] <- (t.test(x = abs(younger_MRI[, i]), y = abs(older_MRI[, i]), alternative = "less"))$p.value
}

MRI_t_test <- t(MRI_t_test)
colnames(MRI_t_test) <- "p-value_of_t-test"
# rownames(MRI_t_test)[MRI_t_test[, "p-value_of_t-test"] < 0.05]
# all the 10 measures's annual change is smaller in the younger group
