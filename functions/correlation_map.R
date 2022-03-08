
pacman::p_load(reshape2, tidyverse,ggplot2, ggThemeAssist)


# the whole correlation matrix of diff data
corr_air_brain$cor_df
# Melt the correlation matrix
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
    size = 12, hjust = 1
  )) +
  coord_fixed() +
  labs(
    title = "Correlation Heatmap of air pollution and change of MRI measures",
    x = "MRI measures",
    y = "Air pollution")+
  theme(plot.title = element_text(hjust = 0.5))


