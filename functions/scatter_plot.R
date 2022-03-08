pacman::p_load(reshape2, tidyverse, ggplot2, ggpubr)
source("functions/pre_process_data.R")
source("functions/MRIannual_difference.R")

#------------------------------------------------------------------------#
#### --------------------- predefined function ------------------------####
#------------------------------------------------------------------------#

# this function makes the scatter plot of one MRI measure VS three APs at 5-year average
scatter_3sub_AP_MRI <- function(joined_df, MRI_name) {
  clean_MRI_name <- sub("_adj.*", "", MRI_name)
  AP_vis <- joined_df %>% select(contains(c("5y")))
  PM25_vis <- joined_df %>%
    gather(colnames(AP_vis)[1], key = AP_name, value = pollutant_value) %>%
    drop_na() %>%
    ggplot(aes(pollutant_value, get(MRI_name))) +
    geom_point() +
    facet_grid(AP_name ~ ., scales = "free") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "pollutant value",
      y = clean_MRI_name
    ) +
    theme(
      panel.grid.major = element_line(
        colour = NA,
        linetype = "blank"
      ), panel.grid.minor = element_line(linetype = "dashed"),
      panel.background = element_rect(
        fill = "antiquewhite2",
        linetype = "longdash"
      ), plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    ) +
    theme(panel.background = element_rect(fill = "gray90"))

  PM10_vis <- joined_df %>%
    gather(colnames(AP_vis)[2], key = AP_name, value = pollutant_value) %>%
    drop_na() %>%
    ggplot(aes(pollutant_value, get(MRI_name))) +
    geom_point() +
    facet_grid(AP_name ~ ., scales = "free") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "pollutant value",
      y = clean_MRI_name
    ) +
    theme(
      panel.grid.major = element_line(
        colour = NA,
        linetype = "blank"
      ), panel.grid.minor = element_line(linetype = "dashed"),
      panel.background = element_rect(
        fill = "antiquewhite2",
        linetype = "longdash"
      ), plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    ) +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    theme(panel.background = element_rect(fill = "gray90"))

  NOX_vis <- joined_df %>%
    gather(colnames(AP_vis)[3], key = AP_name, value = pollutant_value) %>%
    drop_na() %>%
    ggplot(aes(pollutant_value, get(MRI_name))) +
    geom_point() +
    facet_grid(AP_name ~ ., scales = "free") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "pollutant value",
      y = clean_MRI_name
    ) +
    theme(
      panel.grid.major = element_line(
        colour = NA,
        linetype = "blank"
      ), panel.grid.minor = element_line(linetype = "dashed"),
      panel.background = element_rect(
        fill = "antiquewhite2",
        linetype = "longdash"
      ), plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    ) +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    theme(panel.background = element_rect(fill = "gray90"))

  merge_plot <- ggpubr::ggarrange(PM25_vis, PM10_vis, NOX_vis,
    # labels = c("A", "B", "C"),
    ncol = 1, nrow = 3,
    legend = "right"
  )
  ggpubr::annotate_figure(merge_plot, 
                          top = ggpubr::text_grob(paste("air pullutants and", clean_MRI_name, "at baseline"),
    size = 14
  ))
}

# this function makes the scatter plot of one MRI measure VS one AP at 4 different years' average
scatter_4yr_AP_MRI <- function(joined_df, MRI_name, AP_name) {
  clean_MRI_name <- sub("_adj.*", "", MRI_name)
  clean_AP_name <- sub("_1y.*", "", AP_name)
  # complete_MRI_name = paste0(clean_MRI_name, "_adj_0")
  AP_vis <- joined_df %>% select(contains(clean_AP_name))
  four_year_vis <- joined_df %>%
    gather(colnames(AP_vis), key = year_name, value = pollutant_value) %>%
    drop_na() %>%
    ggplot(aes(pollutant_value, get(MRI_name))) +
    geom_point() +
    facet_grid(year_name ~ ., scales = "free") +
    geom_smooth(method = "lm", se = FALSE) +
    # scale_x_continuous(limits=c(5,55))+
    labs(
      x = "pollutant value",
      y = clean_MRI_name,
      title = paste("Scatter plot of", clean_MRI_name, "and averaged", clean_AP_name, "at four different years' duration")
    ) +
    theme(
      panel.grid.major = element_line(
        colour = NA,
        linetype = "blank"
      ), panel.grid.minor = element_line(linetype = "dashed"),
      panel.background = element_rect(
        fill = "antiquewhite2",
        linetype = "longdash"
      ), plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    ) +
    theme(panel.background = element_rect(fill = "gray90"), plot.title = element_text(hjust = 0.5))
  # print the plot
  four_year_vis
}

#------------------------------------------------------------------------#
#### ------------------ for cross sectional analysis ---------------------####
#------------------------------------------------------------------------#
base_mri_df <- mri_iv %>%
  select("lopnr", "age_baseline", contains("adj_0")) %>%
  filter(lopnr != 2513)

base_MRI_AP_df <- left_join(x = base_mri_df, y = air_p, by = "lopnr") %>%
  select(-c("lopnr", "age_baseline"))

# visualization at different time average for the same AP
# ggplot(base_MRI_AP_df, aes(x = GM_adj_0, y = PM25_3y)) +
#   geom_point(size = 2, shape = 23)
# ggplot(base_MRI_AP_df, aes(x = GM_adj_0, y = PM25_5y)) +
#   geom_point(size = 2, shape = 23)
# ggplot(base_MRI_AP_df, aes(x = GM_adj_0, y = PM25_10y)) +
#   geom_point(size = 2, shape = 23)
# it seems there is not so much difference amoong 3y, 5y and 10y.
# so just choose the 5y for each of the pollutants for further visualization.
# there are 10 * 3 combinations
# base_MRI_AP_df <- base_MRI_AP_df %>% select(contains(c("5y", "adj")))
#
# MRI_name <- colnames(base_MRI_AP_df %>% select(contains(c("adj"))))
# AP_name <- colnames(base_MRI_AP_df %>% select(contains(c("PM", "NOX"))))
# 
# # plot the data, faceting by AP name
# MRI_name
# scatter_3sub_AP_MRI(base_MRI_AP_df, MRI_name = "Hippocampus_sum_adj_0")
# scatter_3sub_AP_MRI(base_MRI_AP_df, MRI_name = "GM_adj_0")
# 
# 
# # however, if one wants to see each pollutant at 4 different averaged years
# scatter_4yr_AP_MRI(joined_df = base_MRI_AP_df, MRI_name = "TBTV_adj_0", AP_name = "PM25")

#------------------------------------------------------------------------#
#### ------------------ for longitudinal analysis ---------------------####
#------------------------------------------------------------------------#
# source("code/MRIannual_difference.R")
annual_MRI_AP_df <- left_join(x = annual_mri_df, y = air_p, by = "lopnr") %>%
  select(-c("lopnr", "age_baseline"))

# scatter_3sub_AP_MRI(annual_MRI_AP_df, MRI_name = "WMvolumes_adj_annual")
# 
# scatter_4yr_AP_MRI(joined_df = annual_MRI_AP_df, MRI_name = "TBTV_adj_annual", AP_name = "NOX")

# for scatter_3sub_AP_MRI(), there are 10 plots in total
# for scatter_4yr_AP_MRI(), there are 10 x 3 plots in total
# it can be done by running a loop and print them all,
# but maybe it is more elegant to make an app to show the visualization?

#----------------------------------------------------------------#
#### ------------------------- sandbox ------------------####
#----------------------------------------------------------------#
