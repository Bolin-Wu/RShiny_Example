pacman::p_load(tidyverse, psych)
# annual change data
annual_mri_df

# baseline data
base_mri_df <- mri_iv %>% select("lopnr", "age_baseline", contains("adj_0"))
# exclude the row with only NA data, lopnr = 2513
base_mri_df <- base_mri_df %>% filter(lopnr != 2513)
base_mri_df

# AP data
air_p


#------------------------------------------------------------------------------------#
#### ------------------------------ Cross-sectional analysis ----------------------####
#----------------------------------- without considering age ------------------------#

# Combine the MRI measures at baseline and all the AP measures
base_MRI_AP_df <- left_join(x = base_mri_df, y = air_p, by = "lopnr") %>%
  select(-c("lopnr", "age_baseline"))
base_join_air <- select(base_MRI_AP_df, matches("PM|NOX"))
base_join_brain <- select(base_MRI_AP_df, contains("adj"))

# check scatter plots
ggplot(base_MRI_AP_df, aes(x = GM_adj_0, y = PM25_3y)) +
  geom_point(size = 2, shape = 23)
# its too much combinations to do it. Maybe its better to put it in the APP
# need to find other strategy to find the linear correlation.


# pearson correlation test
pearson_test <- cor.test(base_MRI_AP_df$GM_adj_0, base_MRI_AP_df$NOX_5y,
  method = "pearson"
)

pearson_test$p.value
pearson_test$estimate


corr_m(base_join_air,base_join_brain)
# The maximum positive correlation is 0.1512242 between CSF_adj_0 and PM25_1y


pearson_test_p <- corr.test(x = base_join_air, y = base_join_brain)$p
pearson_test_r <- corr.test(x = base_join_air, y = base_join_brain)$r


pearson_test_p[pearson_test_p < 0.05]

sig_pos <- which(pearson_test_p < 0.05, arr.ind = TRUE)
pearson_test_p[sig_pos]
pearson_test_r[sig_pos]

sig_pos[1, ]
# run LM model
# first check the correlation among the predictors by scatter plot matrices
pairs(air_p[2:13])
# jpeg("plots/check_association/corr_among_predictors.jpg", width = 1030, height = 578, pointsize = 20)
# pairs(air_p[2:13])
# dev.off()
# the linearity is so strong, lets recheck by pearson test
cor.test(air_p$PM10_3y, air_p$NOX_5y,
  method = "pearson"
)
# okay.... linearity testified

# since there is multicollinearity, it is actually not plausible to run LM with all the predictors;
# the se of estimated coefficients explode;
# therefore the  P values can be misleading (a P value can be high, even though the variable is important)
# website: http://fasihkhatib.com/2019/03/26/The-Machine-Learning-Notebook-Precision-of-OLS-Estimates/
independent_part <- paste(colnames(air_p %>% select(-"lopnr")), collapse = "+")
baseline_m <- lm(data = base_MRI_AP_df, as.formula(paste("GM_adj_0 ~ ", independent_part)))
baseline_m
summary(baseline_m)
plot(baseline_m)


regclass::VIF(baseline_m)
# the VIF explodes... much larger than 10

lm(data = base_MRI_AP_df, GM_adj_0 ~ NOX_10y)


# extract the significant variables
significant_x <- summary(baseline_m)$coeff[-1, 4] < 0.05


# basically we can just use the previous pearson correlation test

# Or, just check correlation heatmap.
corr_air_brain <- corr_m(x_brain = base_join_brain, y_air = base_join_air)

melted_cormat <- melt(corr_air_brain$cor_df, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1,
    size = 10, hjust = 1
  )) +
  coord_fixed() +
  labs(
    title = "Correlation Heatmap of Air Pollution and Baseline MRI Measures",
    x = "MRI measures",
    y = "Air pollution"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# CONCLUSION: No linear association at baseline, or very weak association between several variables...

#------------------------------------------------------------------------------------#
#### ------------------------------ longitudinal analysis ----------------------####
#----------------------------------- without considering age ------------------------#



head(annual_mri_df)
# hmmmm there is no lopnr, so it can not be joined together with AP data
# need to fix it

# after adding select("lopnr") in the cal_annual_mri function and rerun theMRIannual_difference.R file:
annual_mri_df
# great!

annual_MRI_AP_df <- left_join(x = annual_mri_df, y = air_p, by = "lopnr") %>% select(-"lopnr")
annual_join_air <- select(annual_MRI_AP_df, matches("PM|NOX"))
annual_join_brain <- select(annual_MRI_AP_df, contains("adj"))

# check the correlation
corr.test(x = base_join_air, y = base_join_brain)$r
corr.test(x = base_join_air, y = base_join_brain)$p

corr_air_brain <- corr_m(annual_join_air, annual_join_brain)
# # make a correlation heat map
melted_cormat <- melt(corr.test(x = annual_join_air, y = annual_join_brain)$r,
  na.rm = TRUE
)
# # Heatmap
ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1,
    size = 10, hjust = 1
  )) +
  coord_fixed() +
  labs(
    title = "Correlation Heatmap of Air Pollution and Annual Change MRI Measures",
    x = "MRI measures",
    y = "Air pollution"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


## Conclusion, can not find any association.




#------------------------------------------------------------------------------------#
#### ------------------------------ Cross-sectional analysis ----------------------####
#----------------------------------- Considering Age -------------------------------#


# ## older age group
# # Combine the MRI measures at baseline and all the AP measures
# base_MRI_AP_df <- left_join(x = base_mri_df, y = air_p, by = "lopnr") %>%
#   filter(age_baseline >= 78) %>%
#   select(-c("lopnr", "age_baseline"))
# base_join_air <- select(base_MRI_AP_df, matches("PM|NOX"))
# base_join_brain <- select(base_MRI_AP_df, contains("adj"))

# # The maximum positive correlation is 0.1512242 between CSF_adj_0 and PM25_1y
#
corr_m(base_join_air, base_join_brain)

pearson_test_p <- corr.test(x = base_join_air, y = base_join_brain)$p
pearson_test_r <- corr.test(x = base_join_air, y = base_join_brain)$r
pearson_test_r

# make a correlation heat map
melted_cormat <- melt(pearson_test_r, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1,
    size = 10, hjust = 1
  )) +
  coord_fixed() +
  labs(
    title = "Correlation Heatmap of Air Pollution and Annual Change MRI Measures",
    subtitle = "Older group",
    x = "MRI measures",
    y = "Air pollution"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

## younger age group
#
# base_MRI_AP_df <- left_join(x = base_mri_df, y = air_p, by = "lopnr") %>%
#   filter(age_baseline < 78) %>%
#   select(-c("lopnr", "age_baseline"))
# base_join_air <- select(base_MRI_AP_df, matches("PM|NOX"))
# base_join_brain <- select(base_MRI_AP_df, contains("adj"))
#
# # The maximum positive correlation is 0.1512242 between CSF_adj_0 and PM25_1y
#
# corr_m(base_join_air, base_join_brain)
#
# pearson_test_p <- corr.test(x = base_join_air, y = base_join_brain)$p
# pearson_test_r <- corr.test(x = base_join_air, y = base_join_brain)$r
# pearson_test_r
#
# # make a correlation heat map
# melted_cormat <- melt(pearson_test_r, na.rm = TRUE)
# # Heatmap
# ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "blue", high = "red", mid = "white",
#     midpoint = 0, limit = c(-1, 1), space = "Lab",
#     name = "Pearson\nCorrelation"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(
#     angle = 45, vjust = 1,
#     size = 10, hjust = 1
#   )) +
#   coord_fixed() +
#   labs(
#     title = "Correlation Heatmap of Air Pollution and Annual Change MRI Measures",
#     subtitle = "Younger group",
#     x = "MRI measures",
#     y = "Air pollution"
#   ) +
#   theme(plot.title = element_text(hjust = 0.5))
#


#------------------------------------------------------------------------------------#
#### ------------------------------ longitudinal analysis ----------------------####
#----------------------------------- Considering Age -------------------------------#

# older group
# annual_MRI_AP_df <- left_join(x = annual_mri_df, y = air_p, by = "lopnr") %>% 
#   filter(age_baseline >= 78) %>%
#   select(-c("lopnr", "age_baseline"))
# 
# annual_join_air <- select(annual_MRI_AP_df, matches("PM|NOX"))
# annual_join_brain <- select(annual_MRI_AP_df, contains("adj"))
# 
# # check the correlation
# corr.test(x = annual_join_air, y = annual_join_brain)$r
# corr.test(x = annual_join_air, y = annual_join_brain)$p
# 
# corr_air_brain <- corr_m(annual_join_air, annual_join_brain)
# # make a correlation heat map
# melted_cormat <- melt(corr.test(x = annual_join_air, y = annual_join_brain)$r,
#                       na.rm = TRUE
# )
# # Heatmap
# ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "blue", high = "red", mid = "white",
#     midpoint = 0, limit = c(-1, 1), space = "Lab",
#     name = "Pearson\nCorrelation"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(
#     angle = 45, vjust = 1,
#     size = 10, hjust = 1
#   )) +
#   coord_fixed() +
#   labs(
#     title = "Correlation Heatmap of Air Pollution and Annual Change MRI Measures",
#     subtitle = "Older group",
#     x = "MRI measures",
#     y = "Air pollution"
#   ) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# 
# # younger group
# annual_MRI_AP_df <- left_join(x = annual_mri_df, y = air_p, by = "lopnr") %>% 
#   filter(age_baseline < 78) %>%
#   select(-c("lopnr", "age_baseline"))
# 
# annual_join_air <- select(annual_MRI_AP_df, matches("PM|NOX"))
# annual_join_brain <- select(annual_MRI_AP_df, contains("adj"))
# 
# # check the correlation
# corr.test(x = annual_join_air, y = annual_join_brain)$r
# corr.test(x = annual_join_air, y = annual_join_brain)$p
# 
# corr_air_brain <- corr_m(annual_join_air, annual_join_brain)
# # make a correlation heat map
# melted_cormat <- melt(corr.test(x = annual_join_air, y = annual_join_brain)$r,
#                       na.rm = TRUE
# )
# Heatmap
# ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "blue", high = "red", mid = "white",
#     midpoint = 0, limit = c(-1, 1), space = "Lab",
#     name = "Pearson\nCorrelation"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(
#     angle = 45, vjust = 1,
#     size = 10, hjust = 1
#   )) +
#   coord_fixed() +
#   labs(
#     title = "Correlation Heatmap of Air Pollution and Annual Change MRI Measures",
#     subtitle = "Older group",
#     x = "MRI measures",
#     y = "Air pollution"
#   ) +
#   theme(plot.title = element_text(hjust = 0.5))


# not so strong either....



# However, we do can find some relatively large correlation variables...

