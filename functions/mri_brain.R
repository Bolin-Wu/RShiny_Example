# Read R package
start_time <- Sys.time()

pacman::p_load(haven, tidyverse, psych, mice, VIM,ggplot2,plyr, dplyr)
#####------------------------- Read data ---------------#####
# read the date
# the data is before the baseline
air_p = read_dta('data/air_pollution_4bolin_20211015.dta')
mri_b = read_dta('data/adjusted_MRI.dta')

#----------------------------------------------------------------------#
#####------------------------- pre-defined function ---------------#####
#----------------------------------------------------------------------#
# checking missing value pattern
check_MissingValue = function(df){
  print(md.pattern(df, plot = FALSE))
  mice_plot <- aggr(df, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(df), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"),
                    oma = c(10,5,5,3))
}

# used in observatory analysis
# change of one of the MRI measures (var_name) between n_wave and baseline
# grouped by Sex
# The color-blind friendly palette with grey:
cbPalette <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", 
               "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")
compare_MRI_change_BySex = function(df, var_name, n_wave, save_img, mainDir){
  # fill the whole name
  whole_var_name = paste0(var_name,'_adj_diff',n_wave)
  # find the average of change
  mu <- ddply(df, "Sex", here(summarize), grp.mean=mean(get(whole_var_name), na.rm=TRUE))
  # make plot
  plot = ggplot(df, aes(x = get(whole_var_name), color = Sex)) + 
    geom_density() +
    # draw vertical line representing the average value 
    geom_vline(data = mu, aes(xintercept=grp.mean, color= Sex), linetype = "dashed") +
    # add text
    # geom_text(aes(x=(mu[,2]), label=paste0("",round(mu[1,2],3)), y=0), color = cbPalette[1], size = 4,check_overlap = TRUE)+
    # geom_text(aes(x=(mu[1,2]), label=paste0("",round(mu[1,2],3)), y=0.01), color = cbPalette[1], size = 3)+
    # geom_text(aes(x=(mu[2,2]), label=paste0("",round(mu[2,2],3)), y=0), color = cbPalette[2], size = 3)+
    labs(title = paste("Density Distribution of", var_name ,"Change in Two Sex Groups"),
         subtitle = paste0("dashed line = the mean value \n n = " , length(na.omit(df[[whole_var_name]])), "(exclude NA)"),
         x = paste("change of",var_name,"between baseline and follow-up", n_wave))+
    # change title position
    theme(plot.title = element_text(hjust = 0.5)) +
    # set the scale of x axis, so that its more informative when reading the mean dashed line
    scale_x_continuous(breaks=round(seq(min(df[[whole_var_name]],na.rm = T),max(df[[whole_var_name]],na.rm = T), length.out = 18),2)) +
    scale_color_manual(values=c(cbPalette[1], cbPalette[2]))
  print(plot)
  # decide if plot should be saved
  if (save_img ==1) {
    setwd(mainDir)
    dComp_plotDir = "plots/density_comparison"
    if (file.exists(dComp_plotDir)){
      setwd(file.path(mainDir,dComp_plotDir))
    } else {
      dir.create(file.path(mainDir, dComp_plotDir))
      setwd(file.path(mainDir,dComp_plotDir))
    }
    ggsave(paste0("change_of_",var_name,"_between_baseline_and_follow_up_", n_wave,".png"), width = 30, height = 20, units = "cm")
  }
  setwd(mainDir)
}

# calculate the correlaiton matrix of brain and air pollution
corr_m = function(x_brain,y_air){
  # define a matrix to store the correlation
  cor_df = matrix(NA, nrow =  ncol(x_brain), ncol = ncol(y_air))
  for (i in 1:ncol(x_brain)) {
    for (j in 1:ncol(y_air)) {
      cor_df[i,j] = cor(x_brain[,i], y_air[,j],use =  "na.or.complete")
    }
  }
  rownames(cor_df) = colnames(x_brain)
  colnames(cor_df) = colnames(y_air)
  MaxCorr = max(cor_df)
  MinCorr = min(cor_df)
  least_corr = min(abs(cor_df))
  MaxCorr_index = which(cor_df == MaxCorr, arr.ind = T)
  MinCorr_index = which(cor_df == MinCorr, arr.ind = T)
  # find the min absolute value
  least_corr_index = arrayInd(which.min(cor_df), dim(cor_df))
  cat("The maximum positive correlation is",MaxCorr,"between", rownames(cor_df)[MaxCorr_index[1]], "and", colnames(cor_df)[MaxCorr_index[2]],"\n")
  cat("The maximum negative correlation is",MinCorr,"between", rownames(cor_df)[MinCorr_index[1]], "and", colnames(cor_df)[MinCorr_index[2]],"\n")
  cat("The least correlation is",least_corr,"between", rownames(cor_df)[least_corr_index[1]], "and", colnames(cor_df)[least_corr_index[2]])
  list("cor_df" = cor_df,
       "max_correlation_value_pos" = MaxCorr,
       "max_correlation_value_neg" = MinCorr,
       "least_correlation" = least_corr
  )
}

# generate boxplot for specific wave
MRIchange_boxplot = function(df, wave_num){
  wave_name = colnames(df %>% select(contains(paste0("diff",wave_num))))
  # get the specific wave data
  boxplot_wave = df[,wave_name]
  r = apply(boxplot_wave, MARGIN = 2, FUN = range,na.rm = TRUE)
  diff_r = r[2,] - r[1,]
  # set the order of variable in x-axis
  o = order(diff_r, decreasing = T)
  # get the succint version of varaible names so that it looks cleaner on the plot
  names(boxplot_wave) = sub("_adj.*", "",names(boxplot_wave))
  boxplot_wave %>% 
    tidyr::gather( key = "wave", value = "value") %>% 
    ggplot(aes(x = factor(wave,levels = names(boxplot_wave)[o] ), y = value))+
    geom_boxplot(na.rm = T)+
    stat_boxplot(geom = 'errorbar',na.rm = T) +
    labs(title = paste("Change of MRI variables in Follow-up",wave_num),
         subtitle = "Ordering by range",
         x = "variables")+
    scale_y_continuous(breaks=round(seq( min(boxplot_wave,na.rm = T),max(boxplot_wave,na.rm = T), length.out = 18),2)) +
    # change title position
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90))
}


# give the mean, sd and n of different follow-ups
summary_wave_measure = function(conditioned_df){
  conditioned_df %>%
    select(starts_with(c("PM","NOX"))) %>% 
    gather( key = "pollution_name", value = "measure") %>% 
    group_by(pollution_name) %>% 
    dplyr::summarize(mean = mean(measure, na.rm = TRUE), sd = sd(measure, na.rm = TRUE), n = n()) 
  
}
#----------------------------------------------------------------------#
#####------------------------- Pre-process data ---------------#####
#----------------------------------------------------------------------#

#  choose the interested variables from MRI dataset
mri_iv = mri_b %>% select("lopnr", "Sex", "Age",contains("adj"), contains("TIV_"))

# take a look at the data
head(air_p)
head(mri_iv)

summary(air_p)
summary(mri_iv)
describe(air_p)
describe(mri_iv)
sort(colnames(mri_iv))

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

# change the sex column to be factor
# this is important for future visualization and regression
mri_diff_df$Sex = as.factor(mri_diff_df$Sex)

# join the mri_diff_df and air_p datasets
df_join = left_join(x = mri_diff_df, y = air_p, by = "lopnr")

#------------------------------------------------------------------------------#
#####------------------------- observatory analysis -------------------------#####
#-----------------------------------------------------------------------------#
mri_diff_df
describe(mri_diff_df)
## Check missing value
check_MissingValue(df = mri_diff_df)
jpeg("plots/MissingPattern_DiffMRI.jpg", width = 1030, height = 578, pointsize = 20)
check_MissingValue(df = mri_diff_df)
dev.off()


# check missing value of joint df
check_MissingValue(df = df_join)
jpeg("plots/MissingPattern_DiffMRI_air.jpg", width = 1030, height = 578, pointsize = 20)
check_MissingValue(df = df_join)
dev.off()

mri_diff_df

# compare by density distribution
#"C:/work_tasks/NEAR_projects/MRI_Brain"
mainDir = getwd()
unique_mri_name
compare_MRI_change_BySex(df = mri_diff_df, var_name = 'CSF', n_wave = 2,save_img = 1,mainDir)


# get the correlation matrix
df_join_air = select(df_join, matches("PM|NOX"))
df_join_brain = select(df_join,contains("adj"), contains("TIV_"))
# check the correlation matrix by pre-defined functions
corr_air_brain = corr_m(x_brain = df_join_brain, y_air = df_join_air)
# the whole correlation matrix
corr_air_brain$cor_df

# visualize the increment of each MRI measures by box plot
# for wave 1
MRIchange_boxplot(df = df_join,wave_num = 1)
# for wave 2
MRIchange_boxplot(df = df_join,wave_num = 2)



# find difference of measures in people participating in different waves
measure_DiffWave_df = left_join(x = mri_iv, y = air_p, by = "lopnr")
# 1. baseline
check_df = wave_filter(df = measure_DiffWave_df, wave_num = 0)

summary_wave_measure(conditioned_df = check_df)
check_df$lopnr[]

# baseline & wave 2
source("MRIannual_difference.R")
source("descriptive_statistics_GroupbyAge.R")
check_descriptive_stat(df =annual_mri_df, AgeGroup_below78 = F )
annual_mri_df

check_df =wave_filter(df = measure_DiffWave_df, wave_num = c(0,1))

summary_wave_measure(conditioned_df = check_df)

# baseline & wave 2
wave3_MRIname = colnames(mri_iv %>% select(contains(paste0("adj_",2))))
check_df = wave_filter(df = measure_DiffWave_df, wave_num = c(0,2))


summary_wave_measure(conditioned_df = check_df)

# baseline & wave1 & wave2
check_df =  wave_filter(df = measure_DiffWave_df, wave_num = c(0,1,2))


summary_wave_measure(conditioned_df = check_df)


end_time <- Sys.time()
end_time - start_time
# the time difference is around 4.45 on KI desktop 

# # listwise deletion
# df_join_clean = na.omit(df_join)
# check the covariance matrix
# brain_df = df_join_clean %>% select(c("WMHVolumemL", "GMvolumemL","WMvolumemL","CSFvolumemL", 
#                                  "TotalTissueVolumeGMWMmL","TotalIntracranialVolumeGMWM",
#                                  "LeftHippocampusVolumeFreesurf","RightHippocampusVolumeFreesur","VolumeLateralVentriclesmL"))
# 
# 
# air_df = select(df_join_clean, matches("PM|NOX"))
# 
# cor_df = matrix(NA, nrow =  ncol(brain_df), ncol = ncol(air_df))
# 
# for (i in 1:ncol(brain_df)) {
#   for (j in 1:ncol(air_df)) {
#     cor_df[i,j] = cor(brain_df[,i], air_df[,j])
#   }
#   
# }
# # row names are brain variables, col names are air variables
# rownames(cor_df) = colnames(brain_df)
# colnames(cor_df) = colnames(air_df)
# cor_df
