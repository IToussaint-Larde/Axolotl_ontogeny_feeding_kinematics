#############################################################################################################################
#                                                                                                                           #
# Adulthood is not an illusion! Testing the impact of ontogeny on suction feeding kinematics in the axolotl (A. mexicanum)  #
#                                                                                                                           #
#############################################################################################################################


####################################
#                                  #
# PARTI : preparation of the data  #
#                                  #
####################################

#open the data file ("data_Ambystoma_mexicanum.csv")
all_data <- read.csv(file.choose(), header = TRUE, sep = ";", dec = ".", row.names = 1)

#convert in absolute value
all_data_positive <-all_data #we create a new data_frame with all the column of all_data
col_MSGC <- which(colnames(all_data) == 'MSGC')#to find the position of column 'MSGC' in 'all_data'
col_MShd1up<-which(colnames(all_data) == 'MShd1up')
col_MShd2up<-which(colnames(all_data) == 'MShd2up')
columns_to_put_in_av<-c(col_MSGC,col_MShd1up,col_MShd2up) #we want to convert in positive value the data concerning speeds of mouth closing and of hyoid retraction
all_data_positive[, columns_to_put_in_av] <- abs(all_data_positive[, columns_to_put_in_av])#we convert the columns in absolute values

#search negative values in the data_frame to convert them in positive ones (Thd1, Thd2)

col_Thd1<-all_data_positive$Thd1#column to be treated
col_Thd1[col_Thd1<0]<-0 #to replace negative value by 0 because depression of the hyoid began after mouth opening and negative value are due to limits in tracking
all_data_positive$Thd1<-col_Thd1#to replace the column by the treated column

col_Thd2<-all_data_positive$Thd2
col_Thd2[col_Thd2<0]<-0 
all_data_positive$Thd2<-col_Thd2

#add +2 to all the time value to avoid the problem of 0 when transforming in log10

col_TMG<-all_data_positive$TMG
col_TMG<-col_TMG + 2
all_data_positive$TMG<-col_TMG

col_TMGA<-all_data_positive$TMGA
col_TMGA<-col_TMGA + 2
all_data_positive$TMGA<-col_TMGA

col_TMHA<-all_data_positive$TMHA
col_TMHA<-col_TMHA + 2
all_data_positive$TMHA<-col_TMHA

col_Thd1<-all_data_positive$Thd1
col_Thd1<-col_Thd1 + 2
all_data_positive$Thd1<-col_Thd1

col_TMhd1<-all_data_positive$TMhd1
col_TMhd1<-col_TMhd1 + 2
all_data_positive$TMhd1<-col_TMhd1

col_Thd2<-all_data_positive$Thd2
col_Thd2<-col_Thd2 + 2
all_data_positive$Thd2<-col_Thd2

col_TMhd2<-all_data_positive$TMhd2
col_TMhd2<-col_TMhd2 + 2
all_data_positive$TMhd2<-col_TMhd2

#normalize the data
all_data_normalize<-all_data_positive

col_MG <- which(colnames(all_data_positive) == 'MG')#to find the position of column 'MSGC' in 'all_data'
col_TMG<-which(colnames(all_data_positive) == 'TMG')
col_MGA<-which(colnames(all_data_positive) == 'MGA')
col_TMGA <- which(colnames(all_data_positive) == 'TMGA')
col_DG <- which(colnames(all_data_positive) == 'DG')
col_MSGO <- which(colnames(all_data_positive) == 'MSGO')
col_MSGC <- which(colnames(all_data_positive) == 'MSGC')
col_MAGO <- which(colnames(all_data_positive) == 'MAGO')
col_MAGC <- which(colnames(all_data_positive) == 'MAGC')
col_MHA <- which(colnames(all_data_positive) == 'MHA')
col_TMHA <- which(colnames(all_data_positive) == 'TMHA')


col_Thd1 <- which(colnames(all_data_positive) == 'Thd1')
col_Mhd1 <- which(colnames(all_data_positive) == 'Mhd1')
col_TMhd1 <- which(colnames(all_data_positive) == 'TMhd1')
col_Dhd1 <- which(colnames(all_data_positive) == 'Dhd1')
col_MShd1down <- which(colnames(all_data_positive) == 'MShd1down')
col_MShd1up <- which(colnames(all_data_positive) == 'MShd1up')
col_MAhd1down <- which(colnames(all_data_positive) == 'MAhd1down')

col_Thd2 <- which(colnames(all_data_positive) == 'Thd2')
col_Mhd2 <- which(colnames(all_data_positive) == 'Mhd2')
col_TMhd2 <- which(colnames(all_data_positive) == 'TMhd2')
col_Dhd2 <- which(colnames(all_data_positive) == 'Dhd2')
col_MShd2down <- which(colnames(all_data_positive) == 'MShd2down')
col_MShd2up <- which(colnames(all_data_positive) == 'MShd2up')
col_MAhd2down <- which(colnames(all_data_positive) == 'MAhd2down')

col_PCD <- which(colnames(all_data_positive) == 'PCD')

col_SVL <- which(colnames(all_data_positive) == 'SVL')


columns_to_normalize <- c(col_MG, col_TMG, col_MGA, col_TMGA, col_DG, col_MSGO, col_MAGO, col_MSGC, col_MAGC, col_MHA, col_TMHA,
                          col_Thd1, col_Mhd1, col_TMhd1, col_Dhd1, col_MShd1down, col_MShd1up, col_MAhd1down,
                          col_Thd2, col_Mhd2, col_TMhd2, col_Dhd2, col_MShd2down, col_MShd2up, col_MAhd2down,
                          col_PCD,col_SVL)# Colonnes à convertir en log10 
all_data_normalize[, columns_to_normalize]<-log10(all_data_normalize[, columns_to_normalize])


# Export table in CSV format
write.csv(all_data_normalize, file = "all_data_normalize.csv")


#Packages to be installed
install.packages("Matrix")
install.packages("lmerTest")
install.packages("emmeans")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("stats")
install.packages("writexl")


#libraries needed 
library(Matrix)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(broom)
library(stats)
library(writexl)



#####################################
#                                   #
# PARTII : Testing size differences #
# depending  on developmental stage #
#                                   #
#####################################

#Create a data frame Keeping only the column stage2, SVL, and ind (the table contains a lot of duplicates as there are several video sequences for each individuals)
df_size_stage<-data.frame(stage2=all_data$stage2, SVL=all_data$SVL, ind=all_data$ind )

#Remove duplicates
New_size_stage <- df_size_stage %>%
  distinct(ind, SVL, .keep_all = TRUE)

New_size_stage$stage2 <- factor(New_size_stage$stage2, levels = c("larvae", "juveniles", "adults"))

# Create a boxplot
ggplot(New_size_stage, aes(x = stage2, y = SVL, fill=stage2)) +
  geom_boxplot(width = 0.5) +# Adjust width
  labs(x = "developmental stage", y = "SVL in cm", title = "SVL repartition among the different developmental stages of the dataset") +
  scale_fill_manual(values = c("larvae" = "pink", "juveniles" = "lavender", "adults" = "steelblue")) +
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "white"),  # white background
    panel.grid.major = element_blank(),  # No grid lines
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5), # Bold and centered title
    #legend.position = "inside"
  )

# Calculate group values
summary_groups <- New_size_stage %>%
  group_by(stage2) %>%
  summarise(
    Valeur_min = min(SVL),
    Valeur_max = max(SVL),
    Moyenne = mean(SVL),
    Mediane= median(SVL),
    etendue = diff(range(SVL))
  )

# Show results
print(summary_groups)

#this part of the code was used to create Supplementary Table S2



#####################################
#                                   #
# PART III : Scaling                #
#                                   #
#                                   #
#####################################

#for each kinematic variables the code records, for each individual,
#-the name of the video sequence with the maximum value, 
#-the maximum value itself, 
#-and the individual's SVL.

#METRICS (MG, Mhd1, Mhd2)

result_MG <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MG =  rownames(all_data_normalize)[which.max(MG)],
    max_MG = max(MG, na.rm = TRUE),
    SVL=SVL[which.max(MG)]
  )
print(result_MG)

result_Mhd1 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_Mhd1 = rownames(all_data_normalize)[which.max(Mhd1)],
    max_Mhd1 = max(Mhd1, na.rm = TRUE),
    SVL=SVL[which.max(Mhd1)]
  )
print(result_Mhd1)

result_Mhd2 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_Mhd2 = rownames(all_data_normalize)[which.max(Mhd2)],
    max_Mhd2 = max(Mhd2, na.rm = TRUE),
    SVL=SVL[which.max(Mhd2)]
  )
print(result_Mhd2)


#ANGLES (MGA, MHA)

result_MGA <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MGA = rownames(all_data_normalize)[which.max(MGA)],
    max_MGA = max(MGA, na.rm = TRUE),
    SVL=SVL[which.max(MGA)]
  )
print(result_MGA)

result_MHA <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MHA = rownames(all_data_normalize)[which.max(MHA)],
    max_MHA = max(MHA, na.rm = TRUE),
    SVL=SVL[which.max(MHA)]
  )
print(result_MHA)


#SPEEDS (MSGO, MSGC, MShd1down, MShd2down, MShd1up, MShd2up)

result_MSGO <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MSGO = rownames(all_data_normalize)[which.max(MSGO)],
    max_MSGO = max(MSGO, na.rm = TRUE),
    SVL=SVL[which.max(MSGO)]
  )
print(result_MSGO)

result_MSGC <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MSGC = rownames(all_data_normalize)[which.max(MSGC)],
    max_MSGC = max(MSGC, na.rm = TRUE),
    SVL=SVL[which.max(MSGC)]
  )
print(result_MSGC)

result_MShd1down <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MShd1down = rownames(all_data_normalize)[which.max(MShd1down)],
    max_MShd1down = max(MShd1down, na.rm = TRUE),
    SVL=SVL[which.max(MShd1down)]
  )
print(result_MShd1down)

result_MShd2down <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MShd2down = rownames(all_data_normalize)[which.max(MShd2down)],
    max_MShd2down = max(MShd2down, na.rm = TRUE),
    SVL=SVL[which.max(MShd2down)]
  )
print(result_MShd2down)

result_MShd1up <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MShd1up = rownames(all_data_normalize)[which.max(MShd1up)],
    max_MShd1up = max(MShd1up, na.rm = TRUE),
    SVL=SVL[which.max(MShd1up)]
  )
print(result_MShd1up)

result_MShd2up <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MShd2up = rownames(all_data_normalize)[which.max(MShd2up)],
    max_MShd2up = max(MShd2up, na.rm = TRUE),
    SVL=SVL[which.max(MShd2up)]
  )
print(result_MShd2up)


#ACCELERATIONS (MAGO, MAGC, MAhd1down, MAhd2down, MAhd1up, MAhd2up)

result_MAGO <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MAGO = rownames(all_data_normalize)[which.max(MAGO)],
    max_MAGO = max(MAGO, na.rm = TRUE),
    SVL=SVL[which.max(MAGO)]
  )
print(result_MAGO)

result_MAGC <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MAGC = rownames(all_data_normalize)[which.max(MAGC)],
    max_MAGC = max(MAGC, na.rm = TRUE),
    SVL=SVL[which.max(MAGC)]
  )
print(result_MAGC)

result_MAhd1down <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MAhd1down = rownames(all_data_normalize)[which.max(MAhd1down)],
    max_MAhd1down = max(MAhd1down, na.rm = TRUE),
    SVL=SVL[which.max(MAhd1down)]
  )
print(result_MAhd1down)

result_MAhd2down <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_MAhd2down = rownames(all_data_normalize)[which.max(MAhd2down)],
    max_MAhd2down = max(MAhd2down, na.rm = TRUE),
    SVL=SVL[which.max(MAhd2down)]
  )
print(result_MAhd2down)


#TIMINGS (TMG, Thd1, Thd2, TMhd1, TMhd2,TMHA)

result_TMG <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_TMG = rownames(all_data_normalize)[which.max(TMG)],
    max_TMG = max(TMG, na.rm = TRUE),
    SVL=SVL[which.max(TMG)]
  )
print(result_TMG)

result_Thd1 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_Thd1 = rownames(all_data_normalize)[which.max(Thd1)],
    max_Thd1 = max(Thd1, na.rm = TRUE),
    SVL=SVL[which.max(Thd1)]
  )
print(result_Thd1)

result_Thd2 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_Thd2 = rownames(all_data_normalize)[which.max(Thd2)],
    max_Thd2 = max(Thd2, na.rm = TRUE),
    SVL=SVL[which.max(Thd2)]
  )
print(result_Thd2)

result_TMhd1 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_TMhd1 = rownames(all_data_normalize)[which.max(TMhd1)],
    max_TMhd1 = max(TMhd1, na.rm = TRUE),
    SVL=SVL[which.max(TMhd1)]
  )
print(result_TMhd1)

result_TMhd2 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_TMhd2 = rownames(all_data_normalize)[which.max(TMhd2)],
    max_TMhd2 = max(TMhd2, na.rm = TRUE),
    SVL=SVL[which.max(TMhd2)]
  )
print(result_TMhd2)

result_TMHA <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_TMHA = rownames(all_data_normalize)[which.max(TMHA)],
    max_TMHA = max(TMHA, na.rm = TRUE),
    SVL=SVL[which.max(TMHA)]
  )
print(result_TMHA)


#DURATIONS (DG, Dhd1, Dhd2, PCD)

result_DG <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_DG = rownames(all_data_normalize)[which.max(DG)],
    max_DG = max(DG, na.rm = TRUE),
    SVL=SVL[which.max(DG)]
  )
print(result_DG)

result_Dhd1 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_Dhd1 = rownames(all_data_normalize)[which.max(Dhd1)],
    max_Dhd1 = max(Dhd1, na.rm = TRUE),
    SVL=SVL[which.max(Dhd1)]
  )
print(result_Dhd1)


result_Dhd2 <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_Dhd2 = rownames(all_data_normalize)[which.max(Dhd2)],
    max_Dhd2 = max(Dhd2, na.rm = TRUE),
    SVL=SVL[which.max(Dhd2)]
  )
print(result_Dhd2)


result_PCD <- all_data_normalize %>%
  group_by(ind) %>%
  summarise(
    sequence_video_PCD = rownames(all_data_normalize)[which.max(PCD)],
    max_PCD = max(PCD, na.rm = TRUE),
    SVL=SVL[which.max(PCD)]
  )
print(result_PCD)


#Data frame containing the name of the video sequence with the greatest value for each individuals and each kinematic variables
performance_sequence<-data.frame(result_MG$ind,
                                 result_MG$sequence_video_MG, result_Mhd1$sequence_video_Mhd1, result_Mhd2$sequence_video_Mhd2,
                                 result_MGA$sequence_video_MGA, result_MHA$sequence_video_MHA,
                                 result_MSGO$sequence_video_MSGO,result_MSGC$sequence_video_MSGC, 
                                 result_MShd1down$sequence_video_MShd1down,result_MShd2down$sequence_video_MShd2down, 
                                 result_MShd1up$sequence_video_MShd1up,result_MShd2up$sequence_video_MShd2up,        
                                 result_MAGO$sequence_video_MAGO,result_MAGC$sequence_video_MAGC,
                                 result_MAhd1down$sequence_video_MAhd1down,result_MAhd2down$sequence_video_MAhd2down,
                                 result_TMG$sequence_video_TMG,result_Thd1$sequence_video_Thd1,result_Thd2$sequence_video_Thd2,
                                 result_TMhd1$sequence_video_TMhd1,result_TMhd2$sequence_video_TMhd2,result_TMHA$sequence_video_TMHA,
                                 result_DG$sequence_video_DG,result_Dhd1$sequence_video_Dhd1,result_Dhd2$sequence_video_Dhd2,result_PCD$sequence_video_PCD
)

#Data frame containing the maximum value per individuals and per kinematic variables + the SVL information
performance_value<-data.frame(ind=result_MG$ind,SVL=result_MG$SVL,
                              max_MG=result_MG$max_MG, max_Mhd1=result_Mhd1$max_Mhd1, max_Mhd2=result_Mhd2$max_Mhd2,
                              max_MGA=result_MGA$max_MGA, max_MHA=result_MHA$max_MHA,
                              max_MSGO=result_MSGO$max_MSGO,max_MSGC=result_MSGC$max_MSGC, 
                              max_MShd1down=result_MShd1down$max_MShd1down,max_MShd2down=result_MShd2down$max_MShd2down, 
                              max_MShd1up=result_MShd1up$max_MShd1up,max_MShd2up=result_MShd2up$max_MShd2up,        
                              max_MAGO=result_MAGO$max_MAGO,max_MAGC=result_MAGC$max_MAGC,
                              max_MAhd1down=result_MAhd1down$max_MAhd1down,max_MAhd2down=result_MAhd2down$max_MAhd2down,
                              max_TMG=result_TMG$max_TMG,max_Thd1=result_Thd1$max_Thd1,max_Thd2=result_Thd2$max_Thd2,
                              max_TMhd1=result_TMhd1$max_TMhd1,max_TMhd2=result_TMhd2$max_TMhd2,max_TMHA=result_TMHA$max_TMHA,
                              max_DG=result_DG$max_DG,max_Dhd1=result_Dhd1$max_Dhd1,max_Dhd2=result_Dhd2$max_Dhd2,max_PCD=result_PCD$max_PCD
)

#code to obtain the equation of the scaling regression lines

# Initialize an empty array to store results
regr_properties_2 <- data.frame(
  Variable = character(),
  Slope = character(),       # Text format to include slope ± standard error
  Intercept = character(),   # Text format to include intercept ± standard error
  CI_95_Lower = numeric(),
  CI_95_Upper = numeric(),
  R2 = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# Identify the columns for which the regression will be calculated
cols_to_analyze <- setdiff(names(performance_value), c("ind", "SVL"))

# Loop on each column
for (col in cols_to_analyze) {
  # Build the linear model
  model <- lm(performance_value[[col]] ~ performance_value$SVL)
  
  # Get statistical summaries
  summary_model <- summary(model)
  conf_int <- confint(model, level = 0.95)
  
  #  Extract the desired metrics
  slope_value <- summary_model$coefficients[2, 1]
  intercept_value <- summary_model$coefficients[1, 1]
  std_error_slope <- summary_model$coefficients[2, 2]
  std_error_intercept <- summary_model$coefficients[1, 2]
  ci_95_lower <- round(conf_int[2, 1], 2)
  ci_95_upper <- round(conf_int[2, 2], 2)
  r_squared <- round(summary_model$r.squared, 2)
  p_value <- round(summary_model$coefficients[2, 4], 2)
  
  # Replace p-value < 0.001 with “<0.001”.
  if (p_value < 0.001) {
    p_value <- "<0.001"
  } else {
    p_value <- round(p_value, 2)
  }  
  
  # Build Slope and Intercept columns in “value ± standard error” format
  slope <- paste0(round(slope_value, 2), " ± ", round(std_error_slope, 2))
  intercept <- paste0(round(intercept_value, 2), " ± ", round(std_error_intercept, 2))
  
  # Add results to table
  regr_properties_2 <- rbind(regr_properties_2, data.frame(
    Variable = col,
    Slope = slope,
    Intercept = intercept,
    CI_95_Lower = ci_95_lower,
    CI_95_Upper = ci_95_upper,
    R2 = r_squared,
    P_value = p_value
  ))
}

# show the results
print(regr_properties_2)

# Export results in Excel format. 
write_xlsx(regr_properties_2, "regr_properties_new2.xlsx")

#This part of the code was used to create Table 2 in the manuscript



#####################################
#                                   #
# PART IV : Kinematic analyses      #
# variable by variable              #
#                                   #
#####################################


####################################
#MG: MAXIMUM GAPE
####################################

#fitting the data to a linear mixed model
fit.modelMG <- lmer(MG ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMG)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMG, type=2)

#new fitting of the data without interaction to improve the statistical power
newfit.modelMG <- lmer(MG ~ stage2 + SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMG)

#new ancova. See Table 4 in the manuscript
anova(newfit.modelMG , type=2)

#Post-hoc test. See Table S3
#the post hoc test adjust for the differences of size
emmeans(newfit.modelMG,pairwise~stage2)

#Post-hoc test without SVL correction. See Table S3
#the post-hoc test do not adjust for size differences 
fit.2 <- lmer(MG ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE )
summary(fit.2)
emmeans(fit.2,pairwise~stage2)


####################################
#TMG: TIME TO MAXIMUM GAPE
####################################

#fitting the data to a linear mixed model
fit.modelTMG <- lmer(TMG ~ stage2*SVL + (1 | ind), data = all_data_normalize,REML=FALSE)
summary(fit.modelTMG)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelTMG, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelTMG <- lmer(TMG ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)
anova(newfit.modelTMG, type=2)#See Table 4 in the manuscript


####################################
#MGA: MAXIMUM GAPE ANGLE
####################################

#fitting the data
fit.modelMGA <- lmer(MGA ~ stage2*SVL + (1 | ind), data = all_data_normalize,REML=FALSE)
summary(fit.modelMGA)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMGA, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMGA <- lmer(MGA ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE) 
anova(newfit.modelMGA, type=2)#See Table 4 in the manuscript


####################################
#DG: DURATION OF THE GAPE
####################################

#fitting the data to a linear mixed model
fit.modelDG <- lmer(DG ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelDG)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelDG, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelDG <- lmer(DG ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)  
anova(newfit.modelDG, type=2)#See Table 4 in the manuscript



####################################
#MSGO: MAXIMUM SPEED OF MOUTH OPENING
####################################

#fitting the data to a linear mixed model
fit.modelMSGO <- lmer(MSGO ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMSGO)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript
anova(fit.modelMSGO, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMSGO <- lmer(MSGO ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)   
anova(newfit.modelMSGO, type=2)#See Table 4 in the manuscript

#Post hoc test. See table S3
emmeans(newfit.modelMSGO,pairwise~stage2)

#Post hoc test without correction for size. See table S3
fit2.MSGO <- lmer(MSGO ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.MSGO)
emmeans(fit2.MSGO,pairwise~stage2)


####################################
#MAGO : MAXIMUM ACCELERATION DURING MOUTH OPENING
####################################

#fitting the data to a linear mixed model
fit.modelMAGO <- lmer(MAGO ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMAGO)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMAGO, type=2)

#ANCOVA larvae adult. See Table 5 in the manuscript.
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelMAGO.larvae.adults <- lmer(MAGO ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelMAGO.larvae.adults )

anova(fit.modelMAGO.larvae.adults, type=2)

#ANCOVA Juveniles- adults. See Table 5 in the manuscript.
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelMAGO.juveniles.adults <- lmer(MAGO ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults, REML=FALSE)
summary(fit.modelMAGO.juveniles.adults )

anova(fit.modelMAGO.juveniles.adults, type=2)

#ANCOVA Larvae-Juveniles. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae','juveniles'))

fit.modelMAGO.larvae.juveniles <- lmer(MAGO ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelMAGO.larvae.juveniles )

anova(fit.modelMAGO.larvae.juveniles, type=2)



####################################
#MSGC : MAXIMUM SPEED DURING MOUTH CLOSING
####################################

#fitting the data to a linear mixed model
fit.modelMSGC <- lmer(MSGC ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMSGC)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMSGC, type=2)

#ANCOVA larvae-adult. See Table 5 in the manuscript.
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelMSGC.larvae.adults <- lmer(MSGC ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelMSGC.larvae.adults )

anova(fit.modelMSGC.larvae.adults, type=2)

#ANCOVA Juvenile vs adults.  See Table 5 in the manuscript
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelMSGC.juveniles.adults <- lmer(MSGC ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults)
summary(fit.modelMSGC.juveniles.adults)

anova(fit.modelMSGC.juveniles.adults, type=2)

#ANCOVA larvae vs juvenile. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'juveniles'))

fit.modelMSGC.larvae.juveniles <- lmer(MSGC ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelMSGC.larvae.juveniles)

anova(fit.modelMSGC.larvae.juveniles, type=2)


####################################
#MAGC : MAXIMUM ACCELERATION DURING MOUTH CLOSING
####################################

#fitting the data to a linear mixed model
fit.modelMAGC <- lmer(MAGC ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMAGC)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMAGC, type=2)

#ANCOVA larvae vs adults. See Table 5 in the manuscript
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelMAGC.larvae.adults <- lmer(MAGC ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelMAGC.larvae.adults )

anova(fit.modelMAGC.larvae.adults, type=2)

#ANCOVA juvenile vs adults. See Table 5 in the manuscript
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelMAGC.juveniles.adults <- lmer(MAGC ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults, REML=FALSE)
summary(fit.modelMAGC.juveniles.adults)

anova(fit.modelMAGC.juveniles.adults, type=2)

#ANCOVA larvae vs juveniles. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'juveniles'))

fit.modelMAGC.larvae.juveniles <- lmer(MAGC ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelMAGC.larvae.juveniles)

anova(fit.modelMAGC.larvae.juveniles, type=2)


####################################
#MHA: MAXIMUM HEAD ANGLE
####################################

#fitting the data to a linear mixed model
fit.modelMHA <- lmer(MHA ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMHA)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMHA, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMHA <- lmer(MHA ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)   
anova(newfit.modelMHA, type=2)#See Table 4 in the manuscript


####################################
#TMHA: TIME TO MAXIMUM HEAD ANGLE
####################################

#fitting the data to a linear mixed model
fit.modelTMHA <- lmer(TMHA ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelTMHA)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelTMHA, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelTMHA <- lmer(TMHA ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)   
anova(newfit.modelTMHA, type=2)#See Table 4 in the manuscript

#Post-hoc test. See Table S3 in the manuscript
emmeans(newfit.modelTMHA,pairwise~stage2)

#Post-hoc test without SVL correction. See Table S3 in the manuscript
fit2.TMHA <- lmer(TMHA ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE ) 
summary(fit2.TMHA) 
emmeans(fit2.TMHA,pairwise~stage2)


####################################
#Thd1 : TIME WHEN HD1 BEGIN TO DEPRESS
####################################

#fitting the data to a linear mixed model
fit.modelThd1<- lmer(Thd1 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelThd1)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelThd1, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelThd1 <- lmer(Thd1 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)   
anova(newfit.modelThd1, type=2)#See Table 4 in the manuscript


####################################
#Mhd1: MAXIMUM DEPRESSION OF THE ANTERIOR PART OF THE HYOID
####################################

#fitting the data to a linear mixed model
fit.modelMhd1<- lmer(Mhd1 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMhd1)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMhd1, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMhd1 <- lmer(Mhd1 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)    
anova(newfit.modelMhd1, type=2)#See Table 4 in the manuscript

#post-hoc test. See Table S3 in the manuscript
emmeans(newfit.modelMhd1,pairwise~stage2)

#post-hoc test without correction for size. See Table S3 in the manuscript
fit2.Mhd1 <- lmer(Mhd1 ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.Mhd1)
emmeans(fit2.Mhd1,pairwise~stage2)


####################################
#TMhd1: TIME TO MAXIMUM DEPRESSION OF THE ANTERIOR PART OF THE HYOID
####################################

#fitting the data to a linear mixed model
fit.modelTMhd1<- lmer(TMhd1 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelTMhd1)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelTMhd1, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelTMhd1 <- lmer(TMhd1 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)     
anova(newfit.modelTMhd1, type=2)#See Table 4 in the manuscript

#Post-hoc. See Table S3 in the manuscript
emmeans(newfit.modelTMhd1,pairwise~stage2)

#Post-hoc without correction for size. See Table S3 in the manuscript
fit2.TMhd1 <- lmer(TMhd1 ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.TMhd1)
emmeans(fit2.TMhd1,pairwise~stage2)


####################################
#Dhd1: DURATION OF HYOID1 CYCLE
####################################

#fitting the data to a linear mixed model
fit.modelDhd1<- lmer(Dhd1 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelDhd1)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelDhd1, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelDhd1 <- lmer(Dhd1 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)      
anova(newfit.modelDhd1, type=2)#See Table 4 in the manuscript

#Post-hoc. See Table S3 in the manuscript
emmeans(newfit.modelDhd1,pairwise~stage2)

#Post-hoc without correction for size. See Table S3 in the manuscript
fit2.Dhd1 <- lmer(Dhd1 ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE) 
summary(fit2.Dhd1) 
emmeans(fit2.Dhd1,pairwise~stage2)


####################################
#MShd1down: MAXIMUM SPEED DURING HD1 DEPRESSION
####################################

#fitting the data to a linear mixed model
fit.modelMShd1down<- lmer(MShd1down ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMShd1down)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMShd1down, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMShd1down <- lmer(MShd1down ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)       
anova(newfit.modelMShd1down, type=2)#See Table 4 in the manuscript

#post-hoc. See Table S3 in the manuscript
emmeans(newfit.modelMShd1down,pairwise~stage2)

#post-hoc without correction for size. See Table S3 in the manuscript
fit2.MShd1down <- lmer(MShd1down ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.MShd1down)
emmeans(fit2.MShd1down,pairwise~stage2)


####################################
#MAhd1down: MAXIMUM ACCELERATION DURING HD1 DEPRESSION
####################################

#fitting the data to a linear mixed model
fit.modelMAhd1down<- lmer(MAhd1down ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMAhd1down)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMAhd1down, type=2)

#ANCOVA larvae - adult. See Table 5 in the manuscript
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelMAhd1down.larvae.adults <- lmer(MAhd1down ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelMAhd1down.larvae.adults )

anova(fit.modelMAhd1down.larvae.adults, type=2)

#ANCOVA juveniles-adults. See Table 5 in the manuscript
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelMAhd1down.juveniles.adults <- lmer(MAhd1down ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults, REML=FALSE)
summary(fit.modelMAhd1down.juveniles.adults)

anova(fit.modelMAhd1down.juveniles.adults, type=2)

#ANCOVA larvae-juveniles. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'juveniles'))

fit.modelMAhd1down.larvae.juveniles <- lmer(MAhd1down ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelMAhd1down.larvae.juveniles)

anova(fit.modelMAhd1down.larvae.juveniles, type=2)


####################################
#MShd1up: MAXIMUM SPEED DURING HD1 GOING BACK TO ITS RESTING POSITION
####################################

#fitting the data to a linear mixed model
fit.modelMShd1up<- lmer(MShd1up ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMShd1up)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMShd1up, type=2)

#ANCOVA larvae-adult. See Table 5 in the manuscript
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelMShd1up.larvae.adults <- lmer(MShd1up ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelMShd1up.larvae.adults )

anova(fit.modelMShd1up.larvae.adults, type=2)

#ANCOVA juveniles-adult. See Table 5 in the manuscript
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelMShd1up.juveniles.adults <- lmer(MShd1up ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults, REML=FALSE)
summary(fit.modelMShd1up.juveniles.adults)

anova(fit.modelMShd1up.juveniles.adults, type=2)

#ANCOVA larvae-juveniles. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'juveniles'))

fit.modelMShd1up.larvae.juveniles <- lmer(MShd1up ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelMShd1up.larvae.juveniles)

anova(fit.modelMShd1up.larvae.juveniles, type=2)


####################################
#Thd2 : TIME WHEN HD2 BEGIN TO DEPRESS
####################################

#fitting the data to a linear mixed model
fit.modelThd2<- lmer(Thd2 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelThd2)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelThd2, type=2)

#ANCOVA larvae - adults. See Table 5 in the manuscript
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelThd2.larvae.adults <- lmer(Thd2 ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelThd2.larvae.adults )

anova(fit.modelThd2.larvae.adults, type=2)

#ANCOVA juveniles -adults. See Table 5 in the manuscript
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelThd2.juveniles.adults <- lmer(Thd2 ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults, REML=FALSE)
summary(fit.modelThd2.juveniles.adults)

anova(fit.modelThd2.juveniles.adults, type=2)

#ANCOVA larvae - juvenile. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'juveniles'))

fit.modelThd2.larvae.juveniles <- lmer(Thd2 ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelThd2.larvae.juveniles)

anova(fit.modelThd2.larvae.juveniles, type=2)


####################################
#Mhd2: MAXIMUM DEPRESSION OF THE POSTERIOR PART OF THE HYOID
####################################

#fitting the data to a linear mixed model
fit.modelMhd2<- lmer(Mhd2 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMhd2)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMhd2, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMhd2 <- lmer(Mhd2 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)        
anova(newfit.modelMhd2, type=2)#See Table 4 in the manuscript

#Post-hoc. See Table S3 in the manuscript
emmeans(newfit.modelMhd2,pairwise~stage2)

#post-hoc test without correction for svl. See Table S3 in the manuscript
fit2.Mhd2 <- lmer(Mhd2 ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.Mhd2)
emmeans(fit2.Mhd2,pairwise~stage2)


####################################
#TMhd2: TIME TO MAXIMUM DEPRESSION OF THE POSTERIOR PART OF THE HYOID
####################################

#fitting the data to a linear mixed model
fit.modelTMhd2<- lmer(TMhd2 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelTMhd2)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelTMhd2, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelTMhd2 <- lmer(TMhd2 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)        
anova(newfit.modelTMhd2, type=2)#See Table 4 in the manuscript

#Post hoc. See Table S3 in the manuscript
emmeans(newfit.modelTMhd2,pairwise~stage2)

#Post-hoc test without correction for size. See Table S3 in the manuscript
fit2.TMhd2 <- lmer(TMhd2 ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.TMhd2)
emmeans(fit2.TMhd2,pairwise~stage2)


####################################
#Dhd2: DURATION OF HYOID2 CYCLE
####################################

#fitting the data to a linear mixed model
fit.modelDhd2<- lmer(Dhd2 ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelDhd2)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelDhd2, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelDhd2 <- lmer(Dhd2 ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)        
anova(newfit.modelDhd2, type=2)#See Table 4 in the manuscript

#post-hoc. See Table S3 in the manuscript
emmeans(newfit.modelDhd2,pairwise~stage2)

#post-hoc without correction for size. See Table S3 in the manuscript
fit2.Dhd2 <- lmer(Dhd2 ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE) 
summary(fit2.Dhd2) 
emmeans(fit2.Dhd2,pairwise~stage2)


####################################
#MShd2down: MAXIMUM SPEED DURING HD2 DEPRESSION
####################################

#fitting the data to a linear mixed model
fit.modelMShd2down<- lmer(MShd2down ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMShd2down)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMShd2down, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMShd2down <- lmer(MShd2down ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)        
anova(newfit.modelMShd2down, type=2)#See Table 4 in the manuscript

#post hoc test. See Table S3 in the manuscript
emmeans(newfit.modelMShd2down,pairwise~stage2)

#post-hoc test without corretion for size. See Table S3 in the manuscript
fit2.MShd2down <- lmer(MShd2down ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.MShd2down)
emmeans(fit2.MShd2down,pairwise~stage2)


####################################
#MAhd2down: MAXIMUM ACCELERATION DURING HD2 DEPRESSION
####################################

#fitting the data to a linear mixed model
fit.modelMAhd2down<- lmer(MAhd2down ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMAhd2down)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMAhd2down, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelMAhd2down <- lmer(MAhd2down ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)        
anova(newfit.modelMAhd2down, type=2)#See Table 4 in the manuscript

#post-hoc. See Table S3 in the manuscript
emmeans(newfit.modelMAhd2down,pairwise~stage2)

#post-hoc test without correction for size. See Table S3 in the manuscript
fit2.MAhd2down <- lmer(MAhd2down ~ stage2 + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit2.MAhd2down)
emmeans(fit2.MAhd2down,pairwise~stage2)


####################################
#MShd2up: MAXIMUM SPEED DURING HD2 GOING BACK TO ITS RESTING POSITION
####################################

#fitting the data to a linear mixed model
fit.modelMShd2up<- lmer(MShd2up ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelMShd2up)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelMShd2up, type=2)

#ANCOVA larvae -adults. See Table 5 in the manuscript
filtered_data_larvae_adults <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'adults'))

fit.modelMShd2up.larvae.adults <- lmer(MShd2up ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_adults, REML=FALSE)
summary(fit.modelMShd2up.larvae.adults )

anova(fit.modelMShd2up.larvae.adults, type=2)

#ANCOVAs juveniles-adults. See Table 5 in the manuscript
filtered_data_juveniles_adults <- all_data_normalize %>%
  filter(stage2 %in% c('juveniles', 'adults'))

fit.modelMShd2up.juveniles.adults <- lmer(MShd2up ~ stage2*SVL + (1 | ind), data = filtered_data_juveniles_adults, REML=FALSE)
summary(fit.modelMShd2up.juveniles.adults)

anova(fit.modelMShd2up.juveniles.adults, type=2)

#ANCOVA larvae-juveniles. See Table 5 in the manuscript
filtered_data_larvae_juveniles <- all_data_normalize %>%
  filter(stage2 %in% c('larvae', 'juveniles'))

fit.modelMShd2up.larvae.juveniles <- lmer(MShd2up ~ stage2*SVL + (1 | ind), data = filtered_data_larvae_juveniles, REML=FALSE)
summary(fit.modelMShd2up.larvae.juveniles)

anova(fit.modelMShd2up.larvae.juveniles, type=2)



####################################
#PCD: PREY CAPTURE DURATION
####################################

#fitting the data to a linear mixed model
fit.modelPCD<- lmer(PCD ~ stage2*SVL + (1 | ind), data = all_data_normalize, REML=FALSE)
summary(fit.modelPCD)

#type II ANCOVA to test if there is a significant interaction. See Table 3 in the manuscript.
anova(fit.modelPCD, type=2)

#new fitting of the data without interaction term to improve statistical power + new ancova
newfit.modelPCD <- lmer(PCD ~ stage2+SVL + (1 | ind), data = all_data_normalize,REML=FALSE)        
anova(newfit.modelPCD, type=2)#See Table 4 in the manuscript




#####################################
#                                   #
# PART V : Regression lines         #
# Equation                          #
#                                   #
#                                   #
#####################################

#this script was used to create Supplementary Table S4

####################################
#Regression performed across all individuals (metrics MG, Mhd1, Mhd2, timings TMG, 
#TMhd1 and TMhd2, duration DG, speeds MSGO,MShd1down, MShd2down, and acceleration MAhd2down)
####################################

#remove superfluous columns
df_variables <- all_data_normalize[, c("MG", "Mhd1", "Mhd2", "TMG", "TMhd1", "TMhd2", "DG", "MSGO", "MShd1down", "MShd2down", "MAhd2down", "SVL")]


# Results storage
results <- data.frame(matrix(nrow = length(df_variables) - 1, ncol = 6))
colnames(results) <- c("Kinematic variable", "Slope", "Intercept", "p", "R2", "95% CI")

# Loop for each kinematic variable
for (i in 1:(ncol(df_variables)-1)) {
  # Linear regression
  model <- lm(df_variables[, i] ~ SVL, data = df_variables)
  
  # Extraction of coefficients and standard errors
  coefficients <- coef(model)
  std_errors <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculation of 95% confidence intervals for slope
  conf_int <- confint(model)[2,]
  
  # Calculation of R² and p-value
  r_squared <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
  
  
  # Replace p-value < 0.001 with “<0.001”.
  if (p_value < 0.001) {
    p_value <- "<0.001"
  } else {
    p_value <- round(p_value, 3)
  }
  
  
  # Storing results in the table
  results[i, 1] <- colnames(df_variables)[i]
  results[i, 2] <- paste(round(coefficients[2], 2), "±", round(std_errors[2], 2))
  results[i, 3] <- paste(round(coefficients[1], 2), "±", round(std_errors[1], 2))
  results[i, 4] <- p_value
  results[i, 5] <- round(r_squared, 2)
  results[i, 6] <- paste("(", round(conf_int[1], 2), "-", round(conf_int[2], 2), ")")
}

# Results display
print(results)

#export in excel file of the equation parameters
#excel_file <- "output_table.xlsx"
#write_xlsx(results, excel_file)



####################################
#Regression performed for larvae (MShd2up, MAGO)
####################################

# Filter DataFrame for each 'stage2' level
larvae_data <- filter(all_data_normalize, stage2 == 'larvae')

#remove superfluous columns
df_variables_larvae<-select(larvae_data, c("MShd2up", "MAGO", "SVL"))

# Initialiser un data frame pour stocker les résultats
results <- data.frame(variable = character(), slope = character(), intercept = character(), p_value = character(), R_squared = character(), CI = character(), stringsAsFactors = FALSE)


# Loop for each kinematic variable
for (i in 1:(ncol(df_variables_larvae)-1)) {
  # Linear regressions
  model_larvae <- lm(df_variables_larvae[, i] ~ SVL, data = df_variables_larvae)
  
  # Extraction of coefficients and standard errors
  coefficients_larvae <- coef(model_larvae)
  std_errors_larvae <- summary(model_larvae)$coefficients[, "Std. Error"]
  
  # Calculation of 95% confidence intervals for slope
  conf_int_larvae <- confint(model_larvae)[2,]
  
  # Calculation of R² and p-value
  r_squared_larvae <- summary(model_larvae)$r.squared
  p_value_larvae <- summary(model_larvae)$coefficients[2, "Pr(>|t|)"]
  
  
  # Replace p-value < 0.001 with “<0.001”.
  if (p_value_larvae < 0.001) {
    p_value_larvae <- "<0.001"
  } else {
    p_value_larvae <- round(p_value_larvae, 3)
  }
  
  
  # Storing results in the table
  results[i, 1] <- colnames(df_variables_larvae)[i]
  results[i, 2] <- paste(round(coefficients_larvae[2], 2), "±", round(std_errors_larvae[2], 2))
  results[i, 3] <- paste(round(coefficients_larvae[1], 2), "±", round(std_errors_larvae[1], 2))
  results[i, 4] <- p_value_larvae
  results[i, 5] <- round(r_squared_larvae, 2)
  results[i, 6] <- paste("(", round(conf_int_larvae[1], 2), "-", round(conf_int_larvae[2], 2), ")")
}
print(results)

# Export table in Excel format
#excel_file <- "output_table_larvae.xlsx"
#write_xlsx(results, excel_file)



####################################
#Regression performed for juveniles (MShd2up, MAGO)
####################################


# Filter DataFrame for each 'stage2' level
juveniles_data <- filter(all_data_normalize, stage2 == 'juveniles')

# Remove superfluous columns and select variables of interest
df_variables_juveniles <- select(juveniles_data, c("MShd2up", "MAGO", "SVL"))

# Initialize a data frame to store results
results <- data.frame(variable = character(), slope = character(), intercept = character(), p_value = character(), R_squared = character(), CI = character(), stringsAsFactors = FALSE)

# Loop for each kinematic variable
for (i in 1:(ncol(df_variables_juveniles)-1)) {
  # Linear regression
  model_juveniles <- lm(df_variables_juveniles[, i] ~ SVL, data = df_variables_juveniles)
  
  # Extraction of coefficients and standard errors
  coefficients_juveniles <- coef(model_juveniles)
  std_errors_juveniles <- summary(model_juveniles)$coefficients[, "Std. Error"]
  
  # Calculation of 95% confidence intervals for slope
  conf_int_juveniles <- confint(model_juveniles)[2,]
  
  # Calculation of R² and p-value
  r_squared_juveniles <- summary(model_juveniles)$r.squared
  p_value_juveniles <- summary(model_juveniles)$coefficients[2, "Pr(>|t|)"]
  
  # Replace p-value < 0.001 with “<0.001”.
  if (p_value_juveniles < 0.001) {
    p_value_juveniles <- "<0.001"
  } else {
    p_value_juveniles <- round(p_value_juveniles, 3)
  }
  
  # Storing results in the table
  results[i, 1] <- colnames(df_variables_juveniles)[i]
  results[i, 2] <- paste(round(coefficients_juveniles[2], 2), "±", round(std_errors_juveniles[2], 2))
  results[i, 3] <- paste(round(coefficients_juveniles[1], 2), "±", round(std_errors_juveniles[1], 2))
  results[i, 4] <- p_value_juveniles
  results[i, 5] <- round(r_squared_juveniles, 2)
  results[i, 6] <- paste("(", round(conf_int_juveniles[1], 2), "-", round(conf_int_juveniles[2], 2), ")")
}

# showing the results
print(results)

# Export table in Excel format
#excel_file <- "output_table_juveniles.xlsx"
#write_xlsx(results, excel_file)


####################################
#Regression performed for immatures ("MAGO","MAhd1down","MSGC","MAGC", "MShd1up","MShd2up")
####################################

# Create a new categorical variable by combining 'larvae' and 'juveniles'
all_data_normalize$stage2_combined <- ifelse(all_data_normalize$stage2 %in% c('larvae', 'juveniles'), 'larvae + juveniles', 'adults') 

# Filter DataFrame for each 'stage2' level
immature_data <- filter(all_data_normalize, stage2_combined == 'larvae + juveniles')


#remove superfluous columns
df_variables_immature<-select(immature_data, c("MAGO","MAhd1down","MSGC","MAGC", "MShd1up","MShd2up", "SVL"))


# Initialize a data frame to store results
results <- data.frame(variable = character(), slope = character(), intercept = character(), p_value = character(), R_squared = character(), CI = character(), stringsAsFactors = FALSE)


# Loop for each kinematic variable
for (i in 1:(ncol(df_variables_immature)-1)) {
  # Linear regression
  model_immature <- lm(df_variables_immature[, i] ~ SVL, data = df_variables_immature)
  
  # Extraction of coefficients and standard errors
  coefficients_immature <- coef(model_immature)
  std_errors_immature <- summary(model_immature)$coefficients[, "Std. Error"]
  
  # Calculation of 95% confidence intervals for slope
  conf_int_immature <- confint(model_immature)[2,]
  
  # Calculation of R² and p-value
  r_squared_immature <- summary(model_immature)$r.squared
  p_value_immature <- summary(model_immature)$coefficients[2, "Pr(>|t|)"]
  
  
  # Replace p-value < 0.001 with “<0.001”
  if (p_value_immature < 0.001) {
    p_value_immature <- "<0.001"
  } else {
    p_value_immature <- round(p_value_immature, 3)
  }
  
  
  #Storing results in the table
  results[i, 1] <- colnames(df_variables_immature)[i]
  results[i, 2] <- paste(round(coefficients_immature[2], 2), "±", round(std_errors_immature[2], 2))
  results[i, 3] <- paste(round(coefficients_immature[1], 2), "±", round(std_errors_immature[1], 2))
  results[i, 4] <- p_value_immature
  results[i, 5] <- round(r_squared_immature, 2)
  results[i, 6] <- paste("(", round(conf_int_immature[1], 2), "-", round(conf_int_immature[2], 2), ")")
}

# Shoing the results
print(results)

# Export table in Excel format
#excel_file <- "output_table_immature.xlsx"
#write_xlsx(results, excel_file)



####################################
#Regression performed for adults ("MAGO","MAhd1down","MSGC","MAGC", "MShd1up","MShd2up")
####################################


# Filter DataFrame for each 'stage2' level
adults_data <- filter(all_data_normalize, stage2 == 'adults')

# Remove superfluous columns and select variables of interest
df_variables_adults <- select(adults_data, c("MAGC","MShd2up", "MAGO", "MSGC", "MShd1up", "MAhd1down", "SVL"))

# Initialize a data frame to store results
results <- data.frame(variable = character(), slope = character(), intercept = character(), p_value = character(), R_squared = character(), CI = character(), stringsAsFactors = FALSE)

# Loop for each kinematic variable
for (i in 1:(ncol(df_variables_adults)-1)) {
  # Linear regression
  model_adults <- lm(df_variables_adults[, i] ~ SVL, data = df_variables_adults)
  
  # Extraction of coefficients and standard errors
  coefficients_adults <- coef(model_adults)
  std_errors_adults <- summary(model_adults)$coefficients[, "Std. Error"]
  
  # Calculation of 95% confidence intervals for slope
  conf_int_adults <- confint(model_adults)[2,]
  
  # Calculation of R² and p-value
  r_squared_adults <- summary(model_adults)$r.squared
  p_value_adults <- summary(model_adults)$coefficients[2, "Pr(>|t|)"]
  
  # Replace p-value < 0.001 with “<0.001”
  if (p_value_adults < 0.001) {
    p_value_adults <- "<0.001"
  } else {
    p_value_adults <- round(p_value_adults, 3)
  }
  
  # Storing results in the table
  results[i, 1] <- colnames(df_variables_adults)[i]
  results[i, 2] <- paste(round(coefficients_adults[2], 2), "±", round(std_errors_adults[2], 2))
  results[i, 3] <- paste(round(coefficients_adults[1], 2), "±", round(std_errors_adults[1], 2))
  results[i, 4] <- p_value_adults
  results[i, 5] <- round(r_squared_adults, 2)
  results[i, 6] <- paste("(", round(conf_int_adults[1], 2), "-", round(conf_int_adults[2], 2), ")")
}

# Showing the results
print(results)


# Export table in Excel format
#excel_file <- "output_table_adults.xlsx"
#write_xlsx(results, excel_file)



#####################################
#                                   #
# PART VI : Regression lines        #
# Plots                             #
#                                   #
#                                   #
#####################################

#this script was used to create Figure 2 and Figure 3

####################################
#Plots for the regression performed across all individuals (metrics MG, Mhd1, Mhd2, timings TMG, 
#TMhd1 and TMhd2, duration DG, speeds MSGO,MShd1down, MShd2down, and acceleration MAhd2down)
####################################

#Plots of the regression lines 

#Regression MG=f(SVL)
ggplot(all_data_normalize, aes(y=MG, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL) log10", y = "Maximum gape opening of the mouth log10", title = "Maximum gape opening in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression Mhd1=f(SVL)
ggplot(all_data_normalize, aes(y=Mhd1, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL)", y = "Maximum depression of hyoid1", title = "Maximum depression of hyoid1 in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression Mhd2=f(SVL)
ggplot(all_data_normalize, aes(y=Mhd2, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL)", y = "Maximum depression of hyoid2", title = "Maximum depression of hyoid2 in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression TMG=f(SVL)
ggplot(all_data_normalize, aes(y=TMG, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL)", y = "Time to maximum gape", title = "Time to maximum gape in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression TMhd1=f(SVL)
ggplot(all_data_normalize, aes(y=TMhd1, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL)", y = "TMhd1", title = "TMhd1 in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression TMhd2=f(SVL)
ggplot(all_data_normalize, aes(y=TMhd2, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL)", y = "TMhd2", title = "TMhd2 in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression DG=f(SVL)
ggplot(all_data_normalize, aes(y=DG, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL)", y = "DG", title = "DG in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression MSGO=f(SVL)
ggplot(all_data_normalize, aes(y=MSGO, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL) log10 values", y = "MSGO log10 values", title = "MSGO in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression MShd1down=f(SVL)
ggplot(all_data_normalize, aes(y=MShd1down, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL) log10 values", y = "MShd1down log10 values", title = "MShd1down in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression MShd2down=f(SVL)
ggplot(all_data_normalize, aes(y=MShd2down, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL) log10 values", y = "MShd2down log10 values", title = "MShd2down in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))

#Regression MAhd2down=f(SVL)
ggplot(all_data_normalize, aes(y=MAhd2down, x=SVL))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL) log10 values", y = "MAhd2down log10 values", title = "MAhd2down in regard to size")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))


####################################
#Plots for the Variables with shift between immature versus adult individuals
####################################

# Create a new categorical variable "stage2_combined" by grouping 'larvae' and 'juveniles'.
all_data_normalize$stage2_combined <- ifelse(all_data_normalize$stage2 %in% c('larvae', 'juveniles'), 'larvae + juveniles', 'adults')

# Order for levels of the factor stage2_combined
all_data_normalize$stage2_combined <- factor(all_data_normalize$stage2_combined, levels = c('larvae + juveniles', 'adults'))


#Regression MSGC=f(SVL)
ggplot(all_data_normalize, aes(y = MSGC, x = SVL, colour = stage2_combined)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "size (SVL) log10 values", y = "MSGC log10 values", title = "Maximum speed during mouth closing in regard to size") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = stage2_combined))


#Regression MShd1up=f(SVL)
ggplot(all_data_normalize, aes(y = MShd1up, x = SVL, colour = stage2_combined)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "size (SVL) log10 values", y = "MShd1up log10 values", title = "Maximum speed when the anterior part of the hyoid returns to its resting position") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = stage2_combined))


#Regression MAGO=f(SVL)
ggplot(all_data_normalize, aes(y = MAGO, x = SVL, colour = stage2_combined)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "size (SVL) log10 values", y = "MAGO log10 values", title = "Maximum acceleration during mouth opening in regard to size") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = stage2_combined))


#Regression MAhd1down=f(SVL)
ggplot(all_data_normalize, aes(y = MAhd1down, x = SVL, colour = stage2_combined)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "size (SVL) log10 values", y = "MAhd1down log10 values", title = "Maximum acceleration of the anterior part of the hyoid depression in regard to size") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = stage2_combined))


#Regression MAGC=f(SVL)
ggplot(all_data_normalize, aes(y = MAGC, x = SVL, colour = stage2_combined)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "size (SVL) log10 values", y = "MAGC log10 values", title = "MAGC in regard to size") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = stage2_combined))


####################################
#Plots the Variable with differences between all developmental stages
####################################

#order for levels of the factor stage2
all_data_normalize$stage2 <- factor(all_data_normalize$stage2, levels = c("larvae", "juveniles", "adults"))

#Regression MShd2up=f(SVL)
ggplot(all_data_normalize, aes(y=MShd2up, x=SVL, colour=stage2))+
  geom_point()+
  geom_smooth(method="lm")+ 
  labs(x = "size (SVL) log10 values", y = "MShd2up log10 value", title = "maximum speed of the posterior part of the hyoid when returning to its resting position")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = stage2))

