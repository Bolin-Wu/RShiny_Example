pacman::p_load(reshape2, tidyverse, ggplot2, ggpubr)
source("functions/pre_process_data.R")
source("functions/MRIannual_difference.R")

#------------------------------------------------------------------------#
#### --------------------- predefined function ------------------------####
#------------------------------------------------------------------------#

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

