pacman::p_load(reshape2, tidyverse, ggplot2, ggpubr)
cbPalette <- c(
  "#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
  "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"
)
#-----------------------------------------------------------------------------------#
### --------------------------- predefined function plot ---------------------------####
#-----------------------------------------------------------------------------------#

# get the merged legend
get_merge_legend <- function(df_plot, plot_type) {
  # set the order of variable in x-axis by range
  r <- apply(df_plot, MARGIN = 2, FUN = range, na.rm = TRUE)
  diff_r <- r[2, ] - r[1, ]
  o <- order(diff_r, decreasing = T)
  # get the succint version of varaible names so that it looks cleaner on the plot
  names(df_plot) <- sub("_adj.*", "", names(df_plot))
  df_plot <- df_plot[, o]
  merge_plot <- df_plot %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(df_plot)))
  if (plot_type == "boxplot") {
    merge_plot <- merge_plot %>%
      ggplot(aes(x = wave, y = value, fill = wave)) +
      geom_boxplot(na.rm = T) +
      scale_fill_manual(
        name = "MRI Measures",
        values = cbPalette
      )
  } else if (plot_type == "violin") {
    merge_plot <- merge_plot %>%
      ggplot(aes(x = wave, y = value, fill = wave)) +
      # this part should be the same as the individual plots
      geom_violin(alpha = .5, scale = "width", trim = FALSE, position = position_dodge(1), na.rm = T) +
      scale_fill_manual(
        name = "MRI Measures",
        values = cbPalette
      )
  } else {
    stop("Wrong input. The df_plot should only contain the measures and the plot_type should be 'boxplot' or 'violin")
  }

  merge_leg <- get_legend(merge_plot)
  return(merge_leg)
}


### DIFF visualization
MRIchange_boxplot <- function(df, wave_num) {
  wave_name <- colnames(df %>% select(contains(paste0("diff", wave_num))))
  # get the specific wave data
  boxplot_wave <- df[, wave_name]
  r <- apply(boxplot_wave, MARGIN = 2, FUN = range, na.rm = TRUE)
  diff_r <- r[2, ] - r[1, ]
  # set the order of variable in x-axis
  o <- order(diff_r, decreasing = T)
  # get the succint version of varaible names so that it looks cleaner on the plot
  names(boxplot_wave) <- sub("_adj.*", "", names(boxplot_wave))
  
  # get merged legend
  leg <- get_merge_legend(boxplot_wave, "boxplot")

  ## part A, variable 1:4
  subs <- c(1:4)
  data_boxplot_A <- subset(boxplot_wave, select = o[subs])
  boxplot_diff_A <- data_boxplot_A %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_boxplot_A))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_boxplot(na.rm = T) +
    stat_boxplot(geom = "errorbar", na.rm = T) +
    labs(
      # title = paste("Change of MRI variables in Follow-up", wave_num),
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_boxplot_A, na.rm = T), 0), round(max(data_boxplot_A, na.rm = T), 0), 10)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  ## part B, variable 5:6
  subs <- c(5:6)
  data_boxplot_B <- subset(boxplot_wave, select = o[subs])
  boxplot_diff_B <- data_boxplot_B %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_boxplot_B))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_boxplot(na.rm = T) +
    stat_boxplot(geom = "errorbar", na.rm = T) +
    labs(
      # title = paste("Change of MRI variables in Follow-up", wave_num),
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_boxplot_B, na.rm = T), 0), round(max(data_boxplot_B, na.rm = T), 0), 5)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  ## part C, variable 7:10
  subs <- c(7:10)
  data_boxplot_C <- subset(boxplot_wave, select = o[subs])
  boxplot_diff_C <- data_boxplot_C %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_boxplot_C))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_boxplot(na.rm = T) +
    stat_boxplot(geom = "errorbar", na.rm = T) +
    labs(
      # title = paste("Change of MRI variables in Follow-up", wave_num),
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_boxplot_C, na.rm = T), 0), round(max(data_boxplot_C, na.rm = T), 0), 0.25)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  merge_plot <- ggarrange(boxplot_diff_A, boxplot_diff_B, boxplot_diff_C,
    labels = c("A", "B", "C"),
    ncol = 3, nrow = 1,
    legend.grob = leg, legend = "right"
  )
  annotate_figure(merge_plot, top = text_grob(paste("Change of MRI variables in Wave", wave_num),
    color = "red", face = "bold", size = 14
  ))
}


MRIchange_violinplot <- function(df, wave_num) {
  wave_name <- colnames(df %>% select(contains(paste0("diff", wave_num))))
  # get the specific wave data
  violin_plot_wave <- df[, wave_name]
  r <- apply(violin_plot_wave, MARGIN = 2, FUN = range, na.rm = TRUE)
  diff_r <- r[2, ] - r[1, ]
  # set the order of variable in x-axis
  o <- order(diff_r, decreasing = T)
  # get the succint version of varaible names so that it looks cleaner on the plot
  names(violin_plot_wave) <- sub("_adj.*", "", names(violin_plot_wave))

  # get merged legend
  leg <- get_merge_legend(violin_plot_wave, "violin")


  ## part A, variable 1:6
  data_violin_A <- subset(violin_plot_wave, select = o[1:6])
  violin_plot_diff1_1 <- data_violin_A %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_violin_A))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_violin(alpha = .5, scale = "width", trim = FALSE, position = position_dodge(1), na.rm = T) +
    labs(
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_violin_A, na.rm = T), 0), round(max(data_violin_A, na.rm = T), 0), 10)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[1:6]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  # part B, 7:10
  data_violin_B <- subset(violin_plot_wave, select = o[7:10])
  violin_plot_diff1_2 <- data_violin_B %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_violin_B))) %>%
    ggplot(aes(x = factor(wave, levels = names(data_violin_B)), y = value, fill = wave)) +
    geom_violin(alpha = .5, scale = "width", trim = FALSE, position = position_dodge(1), na.rm = T) +
    labs(
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_violin_B, na.rm = T), 0), round(max(data_violin_B, na.rm = T), 0), 0.5)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[7:10]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  # merge the two plots together
  merge_plot <- ggarrange(violin_plot_diff1_1, violin_plot_diff1_2,
    labels = c("A", "B"),
    ncol = 2, nrow = 1,
    legend.grob = leg, legend = "right"
  )
  annotate_figure(merge_plot, top = text_grob(paste("Change of MRI variables in Wave", wave_num),
    color = "red", face = "bold", size = 14
  ))
}

### annual visualization
# ordering on x-axis
MRIAAchange_boxplot <- function(df) {
  wave_name <- colnames(df %>% select(contains("annual")))
  # get the specific wave data
  boxplot_wave <- df[, wave_name]
  # set the order of variable in x-axis by range
  r <- apply(boxplot_wave, MARGIN = 2, FUN = range, na.rm = TRUE)
  diff_r <- r[2, ] - r[1, ]
  o <- order(diff_r, decreasing = T)
  # get the succint version of varaible names so that it looks cleaner on the plot
  names(boxplot_wave) <- sub("_adj.*", "", names(boxplot_wave))

  # get the merged_legend
  leg <- get_merge_legend(boxplot_wave, "boxplot")

  ## part A, variable 1:4
  subs <- c(1:4)
  data_boxplot_A <- subset(boxplot_wave, select = o[subs])
  boxplot_diff_A <- data_boxplot_A %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_boxplot_A))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_boxplot(na.rm = T) +
    stat_boxplot(geom = "errorbar", na.rm = T) +
    labs(
      # title = paste("Change of MRI variables in Follow-up", wave_num),
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_boxplot_A, na.rm = T), 0), round(max(data_boxplot_A, na.rm = T), 0), 5)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  ## part B, variable 5:6
  subs <- c(5:6)
  data_boxplot_B <- subset(boxplot_wave, select = o[subs])
  boxplot_diff_B <- data_boxplot_B %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_boxplot_B))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_boxplot(na.rm = T) +
    stat_boxplot(geom = "errorbar", na.rm = T) +
    labs(
      # title = paste("Change of MRI variables in Follow-up", wave_num),
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_boxplot_B, na.rm = T), 0), round(max(data_boxplot_B, na.rm = T), 0), 2)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  ## part C, variable 7:10
  subs <- c(7:10)
  data_boxplot <- subset(boxplot_wave, select = o[subs])
  boxplot_diff_C <- data_boxplot %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_boxplot))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_boxplot(na.rm = T) +
    stat_boxplot(geom = "errorbar", na.rm = T) +
    labs(
      # title = paste("Change of MRI variables in Follow-up", wave_num),
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_boxplot, na.rm = T), 2), round(max(data_boxplot, na.rm = T), 2), 0.15)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  merge_plot <- ggarrange(boxplot_diff_A, boxplot_diff_B, boxplot_diff_C,
    labels = c("A", "B", "C"),
    ncol = 3, nrow = 1,
    legend.grob = leg, legend = "right"
  )
  annotate_figure(merge_plot,
    top = text_grob(paste("Annual Average Change of MRI Measures"),
      color = "black", face = "bold", size = 14
    )
  )
}



MRIAAchange_violinplot <- function(df) {
  wave_name <- colnames(df %>% select(contains("annual")))
  # get the specific wave data
  violin_plot_wave <- df[, wave_name]
  # set the order of variable in x-axis by range
  r <- apply(violin_plot_wave, MARGIN = 2, FUN = range, na.rm = TRUE)
  diff_r <- r[2, ] - r[1, ]
  o <- order(diff_r, decreasing = T)
  # get the succint version of varaible names so that it looks cleaner on the plot
  names(violin_plot_wave) <- sub("_adj.*", "", names(violin_plot_wave))

  # get merged legend
  leg <- get_merge_legend(violin_plot_wave, "violin")

  ## part A, variable 1:6
  subs <- c(1:6)
  data_violin_A <- subset(violin_plot_wave, select = o[subs])
  violin_plot_diff_A <- data_violin_A %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_violin_A))) %>%
    ggplot(aes(x = wave, y = value, fill = wave)) +
    geom_violin(alpha = .5, scale = "width", trim = FALSE, position = position_dodge(1), na.rm = T) +
    labs(
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_violin_A, na.rm = T), 2), round(max(data_violin_A, na.rm = T), 2), 2)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  # part B, 7:10
  subs <- c(7:10)
  data_violin_B <- subset(violin_plot_wave, select = o[subs])
  violin_plot_diff_B <- data_violin_B %>%
    tidyr::gather(key = "wave", value = "value") %>%
    mutate(wave = factor(wave, levels = colnames(data_violin_B))) %>%
    ggplot(aes(x = factor(wave, levels = names(data_violin_B)), y = value, fill = wave)) +
    geom_violin(alpha = .5, scale = "width", trim = FALSE, position = position_dodge(1), na.rm = T) +
    labs(
      x = "variables"
    ) +
    scale_y_continuous(breaks = seq(round(min(data_violin_B, na.rm = T), 2), round(max(data_violin_B, na.rm = T), 2), 0.05)) +
    # scale_fill_discrete(name="MRI Measures",
    #                     labels=names(data_boxplot)) +
    scale_fill_manual(
      name = "MRI Measures",
      values = cbPalette[subs]
    ) +
    # change title position
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

  # merge the two plots together
  merge_plot <- ggarrange(violin_plot_diff_A, violin_plot_diff_B,
    labels = c("A", "B"),
    ncol = 2, nrow = 1,
    legend.grob = leg, legend = "right"
  )
  annotate_figure(merge_plot, top = text_grob(paste("Annual Average Change of MRI Measures"),
    color = "black", face = "bold", size = 14
  ))
}

#---------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------#
### --------------------------- Analysis of DIFF measures ---------------------------####
#--------------------------------------------------------------------------------------#

### --------------------------- check TBTV outlier ---------------------------####

# extreme TBTV
# df_join %>% filter(TBTV_adj_diff1 == min(TBTV_adj_diff1, na.rm = T))
# # lopnr = 1518, this woman might have diseases that we are not considering.
# 
# # increase TBTV
# df_join %>% filter(TBTV_adj_diff1 > 0)
# # lopnr = 1404 and 1528 have increase for the first wave
# df_join %>% filter(TBTV_adj_diff2 > 0)
# # lopnr = 646, 900 and 1553 have increase for the second wave
# 
# 
# ### --------------------------- box plot ---------------------------####
# 
# MRIchange_boxplot(df = df_join, wave_num = 1)
# MRIchange_boxplot(df = df_join, wave_num = 2)
# 
# 
# ### --------------------------- violin plot ---------------------------####
# 
# MRIchange_violinplot(df_join, wave_num = 1)
# MRIchange_violinplot(df_join, wave_num = 2)

#--------------------------------------------------------------------------------------#
### --------------------------- Analysis of annual measures ---------------------------####
#--------------------------------------------------------------------------------------#

# ### --------------------------- box plot ---------------------------####
# MRIAAchange_boxplot(annual_mri_df)
# 
# ### --------------------------- violin plot ---------------------------####
# MRIAAchange_violinplot(annual_mri_df)




