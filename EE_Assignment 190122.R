# Set working directory
setwd("G:\\My Drive\\Modules\\HAR6260 Economic evaluation\\Assignment 2")
wd <- "G:\\My Drive\\Modules\\HAR6260 Economic evaluation\\Assignment 2"

# Read in supporting R packages
library(tidyr)
library(readr)
library(vroom)
library(haven)
library(stringr)
library(dplyr)
library(tidyverse)
library(mice)
library(boot)
library(ggplot2)

# Read in data file
Raw_data <- read.csv(paste(wd,"\\HAR6260_Assignment2_2022.csv",sep=""), header=TRUE)

# Generate QALY data column using EQ5D utilities
Raw_data$QALY_EQ5D <- ((0.25*(Raw_data$EQ5DM3+Raw_data$EQ5DM0))+(0.75*(Raw_data$EQ5DM12+Raw_data$EQ5DM3))+(1*(Raw_data$EQ5DM24+Raw_data$EQ5DM12)))/2

# Generate QALY data column using SF6D utilities
Raw_data$QALY_SF6D <- ((0.25*(Raw_data$SF6DM3+Raw_data$SF6DM0))+(0.75*(Raw_data$SF6DM12+Raw_data$SF6DM3))+(1*(Raw_data$SF6DM24+Raw_data$SF6DM12)))/2

# Add Simvastatin cost
Raw_data$Simvastatin_unit_cost <- ifelse(Raw_data$SimvastatinDose==10,"0.92",ifelse(Raw_data$SimvastatinDose==20,"0.96",ifelse(Raw_data$SimvastatinDose==40,"1.08",ifelse(Raw_data$SimvastatinDose==80,"1.50","0"))))
Raw_data$Simvastatin_unit_cost <- as.numeric(Raw_data$Simvastatin_unit_cost)
Raw_data$Total_cost_Simvastatin <- Raw_data$Simvastatin_unit_cost*24

# Add GP visit cost
Raw_data$GP_visit_unit_cost <- 39.23
Raw_data$Total_cost_GP_visits <- Raw_data$GP_visit_unit_cost*Raw_data$GPvisits

# Generate Total costs column
Raw_data$Total_Costs <- Raw_data$Costs24 + Raw_data$Total_cost_Simvastatin + Raw_data$Total_cost_GP_visits

# Create a separate dataset for each treatment group
Data_T1 <- Raw_data[Raw_data$treatment=="1",]
Data_T2 <- Raw_data[Raw_data$treatment=="2",]

# Summarise baseline demographics for each treatment group
summary(Data_T1$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 20.00   34.00   42.00   41.89   50.00   63.00 
summary(Data_T2$age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#26.00   34.00   42.00   43.12   51.00   64.00 
summary(Data_T1$EQ5DM0)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.2390  0.5160  0.6900  0.5440  0.7518  1.0000      42 
summary(Data_T2$EQ5DM0)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -0.1810  0.5515  0.6200  0.5383  0.7435  0.7960      20 
summary(Data_T1$SF6DM0)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.3110  0.5000  0.5780  0.5935  0.6680  0.8920       3 
summary(Data_T2$SF6DM0)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.3110  0.5150  0.6020  0.6084  0.7125  0.8920 
Data_T1 %>% group_by(gender) %>% count
# male      1    40
# female      2    60
Data_T2 %>% group_by(gender) %>% count
# male      1    25
# female    2    26
Data_T2 %>% group_by(ethnict) %>% count
# White      51
Data_T1 %>% group_by(ethnict) %>% count
# White      100

# Review missing data in raw data set, as a % of total data (overall)
sum(is.na(Raw_data$treatment)) / length(Raw_data$ï..studyid) #0%
sum(is.na(Raw_data$age))/ length(Raw_data$ï..studyid) #0%
sum(is.na(Raw_data$ethnict))/ length(Raw_data$ï..studyid) #0%
sum(is.na(Raw_data$Costs24))/ length(Raw_data$ï..studyid)# 28%
sum(is.na(Raw_data$EQ5DM0))/ length(Raw_data$ï..studyid) # 41%
sum(is.na(Raw_data$EQ5DM3))/ length(Raw_data$ï..studyid) # 41% 
sum(is.na(Raw_data$EQ5DM12))/ length(Raw_data$ï..studyid) # 13%
sum(is.na(Raw_data$EQ5DM24))/ length(Raw_data$ï..studyid) # 33%
sum(is.na(Raw_data$SF6DM0))/ length(Raw_data$ï..studyid) # 2%
sum(is.na(Raw_data$SF6DM3))/ length(Raw_data$ï..studyid) # 11%
sum(is.na(Raw_data$SF6DM12))/ length(Raw_data$ï..studyid) # 24% 
sum(is.na(Raw_data$SF6DM24))/ length(Raw_data$ï..studyid) # 34%
sum(is.na(Raw_data$gender))/ length(Raw_data$ï..studyid) # 0%
sum(is.na(Raw_data$SimvastatinDose))/ length(Raw_data$ï..studyid) # 0%
sum(is.na(Raw_data$GPvisits))/ length(Raw_data$ï..studyid) # 0%
sum(is.na(Raw_data$QALY_EQ5D))/ length(Raw_data$ï..studyid) # 64%
sum(is.na(Raw_data$QALY_SF6D))/ length(Raw_data$ï..studyid) # 41%
sum(is.na(Raw_data$Total_Costs))/ length(Raw_data$ï..studyid) # 28%

par(mar=c(1,1,1,1))
md.pattern(Raw_data)

# Review missing data in each sub-data set (i.e. disaggregated by treatment type), as a % of total data
# Treatment 1:
sum(is.na(Data_T1$treatment)) / length(Data_T1$treatment) # 0%
sum(is.na(Data_T1$age)) / length(Data_T1$treatment) # 0%
sum(is.na(Data_T1$ethnict)) / length(Data_T1$treatment) # 0%
sum(is.na(Data_T1$Costs24)) / length(Data_T1$treatment) # 28%
sum(is.na(Data_T1$EQ5DM0)) / length(Data_T1$treatment) # 42%
sum(is.na(Data_T1$EQ5DM3)) / length(Data_T1$treatment) # 39%
sum(is.na(Data_T1$EQ5DM12)) / length(Data_T1$treatment) # 10%
sum(is.na(Data_T1$EQ5DM24)) / length(Data_T1$treatment) # 31%
sum(is.na(Data_T1$SF6DM0)) / length(Data_T1$treatment) # 3%
sum(is.na(Data_T1$SF6DM3)) / length(Data_T1$treatment) # 9%
sum(is.na(Data_T1$SF6DM12)) / length(Data_T1$treatment) # 20%
sum(is.na(Data_T1$SF6DM24)) / length(Data_T1$treatment) # 31%
sum(is.na(Data_T1$gender)) / length(Data_T1$treatment) # 0%
sum(is.na(Data_T1$SimvastatinDose)) / length(Data_T1$treatment) # 0%
sum(is.na(Data_T1$GPvisits)) / length(Data_T1$treatment) # 0%
sum(is.na(Data_T1$QALY_EQ5D)) / length(Data_T1$treatment) # 63%
sum(is.na(Data_T1$QALY_SF6D)) / length(Data_T1$treatment) # 38%
sum(is.na(Data_T1$Total_Costs)) / length(Data_T1$treatment) # 28%
# Treatment 2:
sum(is.na(Data_T2$treatment)) / length(Data_T2$treatment) # 0%
sum(is.na(Data_T2$age)) / length(Data_T2$treatment) # 0%
sum(is.na(Data_T2$ethnict)) / length(Data_T2$treatment) # 0% 
sum(is.na(Data_T2$Costs24)) / length(Data_T2$treatment) # 29%
sum(is.na(Data_T2$EQ5DM0)) / length(Data_T2$treatment) # 39%
sum(is.na(Data_T2$EQ5DM3)) / length(Data_T2$treatment) # 45%
sum(is.na(Data_T2$EQ5DM12)) / length(Data_T2$treatment) # 18%
sum(is.na(Data_T2$EQ5DM24)) / length(Data_T2$treatment) # 37%
sum(is.na(Data_T2$SF6DM0)) / length(Data_T2$treatment) # 0%
sum(is.na(Data_T2$SF6DM3)) / length(Data_T2$treatment) # 16%
sum(is.na(Data_T2$SF6DM12)) / length(Data_T2$treatment) # 31%
sum(is.na(Data_T2$SF6DM24)) / length(Data_T2$treatment) # 39%
sum(is.na(Data_T2$gender)) / length(Data_T2$treatment) # 0%
sum(is.na(Data_T2$SimvastatinDose)) / length(Data_T2$treatment) # 0%
sum(is.na(Data_T2$GPvisits)) / length(Data_T2$treatment) # 0%
sum(is.na(Data_T2$QALY_EQ5D)) / length(Data_T2$treatment) # 65%
sum(is.na(Data_T2$QALY_SF6D)) / length(Data_T2$treatment) # 47%
sum(is.na(Data_T2$Total_Costs)) / length(Data_T2$treatment) # 29%

# Interpretation: 
# 28% of total cost data, 63% of EQ5D-derived QALY data and 38% of SF6D derived QALY data is missing for Treatment 1.
# 29% of total cost data, 65% of EQ5D-derived QALY data and 47% of SF6D derived QALY data is missing for Treatment 2.
# There is more missing EQ5D data than there is SF36 data in both groups.

################################################# CUA using EQ5D utilities

# Calculate mean costs and CIs for each treatment & ignoring missing data:
# Treatment 1:
summary(Data_T1$Total_Costs)
se_DataT1_Cost <- sd(Data_T1$Total_Costs, na.rm=TRUE)/sqrt(length(Data_T1$Total_Costs))
CI_Costs_raw_Tx1 <- c(mean(Data_T1$Total_Costs, na.rm=TRUE) - 1.96*se_DataT1_Cost, mean(Data_T1$Total_Costs, na.rm=TRUE) + 1.96*se_DataT1_Cost)
CI_Costs_raw_Tx1
#     Output:
#     Min.   1st Qu. Median  Mean    3rd Qu.  Max.     NA's 
#     62.27  329.16  470.65  560.37  608.56 2861.47     28 
#     CI: 471.8-648.9
# Treatment 2:
summary(Data_T2$Total_Costs)
se_DataT2_Cost <- sd(Data_T2$Total_Costs, na.rm=TRUE)/sqrt(length(Data_T2$Total_Costs))
CI_Costs_raw_Tx2 <- c(mean(Data_T2$Total_Costs, na.rm=TRUE) - 1.96*se_DataT2_Cost, mean(Data_T2$Total_Costs, na.rm=TRUE) + 1.96*se_DataT2_Cost)
CI_Costs_raw_Tx2 
#     Output:
#     Min.    1st Qu. Median  Mean    3rd Qu.  Max.     NA's 
#     85.92   188.90  287.10  580.96  570.38   6724.84  15 
#     CI 278.6-883.3

# Calculate mean and CIs for EQ5D QALYs, ignoring missing data:
# Treatment 1:
summary(Data_T1$QALY_EQ5D)
se_DataT1_QALY_EQ5D <- sd(Data_T1$QALY_EQ5D, na.rm=TRUE)/sqrt(length(Data_T1$QALY_EQ5D))
CI_QALY_EQ5D_raw_Tx1 <- c(mean(Data_T1$QALY_EQ5D, na.rm=TRUE) - 1.96*se_DataT1_QALY_EQ5D, mean(Data_T1$QALY_EQ5D, na.rm=TRUE) + 1.96*se_DataT1_QALY_EQ5D)
CI_QALY_EQ5D_raw_Tx1
#     Output:
#     Min.    1st Qu. Median  Mean    3rd Qu. Max.    NA's 
#     0.5064  1.3699  1.5920  1.5208  1.7915  1.9745  63 
#     Mean 1.52, CI 1.45-1.59
# Treatment 2:
summary(Data_T2$QALY_EQ5D)
se_DataT2_QALY_EQ5D <- sd(Data_T2$QALY_EQ5D, na.rm=TRUE)/sqrt(length(Data_T2$QALY_EQ5D))
CI_QALY_EQ5D_raw_Tx2 <- c(mean(Data_T2$QALY_EQ5D, na.rm=TRUE) - 1.96*se_DataT2_QALY_EQ5D, mean(Data_T2$QALY_EQ5D, na.rm=TRUE) + 1.96*se_DataT2_QALY_EQ5D)
CI_QALY_EQ5D_raw_Tx2
#     Output:
#     Min.    1st Qu. Median  Mean    3rd Qu. Max.    NA's 
#     0.3135  1.2705  1.3984  1.3558  1.6685  1.9659  33 
#     Mean 1.36, CI 1.24-1.48

# Create data matrix using only the variables to be included in the imputation.  
Raw_data2 <- Raw_data[,c("treatment","age", "ethnict", "Costs24","gender", "SimvastatinDose", "GPvisits","QALY_EQ5D","QALY_SF6D","Total_cost_Simvastatin","Total_Costs")]

# Impute the missing values, specifying 100 imputed data sets. 
impute <- mice(Raw_data2, m=100, seed = 1234)
summary(impute)

# Check one of the imputed datasets to see if imputed data looks reasonable
ImputationExample <- complete(impute, 50)
View(ImputationExample)

# Plot of the imputation trace to check convergence
plot(impute)

# create a long matrix with stacked complete data, not including the original data
impdat <- complete(impute,action="long",include = FALSE)

# Create a separate imputed dataset for each treatment group
Data_T1_impute <- impdat[impdat$treatment=="1",]
Data_T2_impute <- impdat[impdat$treatment=="2",]

# Estimate the pooled mean across the 100 imputations, and CIs (using 95% CI formula), for costs:
#Treatment 1:
pool_mean <- with(Data_T1_impute, by(Data_T1_impute, .imp, function(x) c(mean(x$Total_Costs),sd(x$Total_Costs))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean)  
#       Output: Mean 548.08
se_DataT1_impute_cost <- sd(Data_T1_impute$Total_Costs)/sqrt(length(Data_T1_impute$Total_Costs))
CICostimputeTx1 <- c(mean(Data_T1_impute$Total_Costs) - 1.96*se_DataT1_impute_cost, mean(Data_T1_impute$Total_Costs) + 1.96*se_DataT1_impute_cost)
CICostimputeTx1
# Output: CI 538.32-557.84

# Treatment 2:
pool_mean <- with(Data_T2_impute, by(Data_T2_impute, .imp, function(x) c(mean(x$Total_Costs),sd(x$Total_Costs))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean) 
#       Output: Mean 582.60 
se_DataT2_impute_cost <- sd(Data_T2_impute$Total_Costs)/sqrt(length(Data_T2_impute$Total_Costs))
CICostImputeTx2 <- c(mean(Data_T2_impute$Total_Costs) - 1.96*se_DataT2_impute_cost, mean(Data_T2_impute$Total_Costs) + 1.96*se_DataT2_impute_cost)
CICostImputeTx2
#       Output: CI 554.94-610.25

# Estimate the pooled mean across the 100 imputations, and CIs (using 95% CI formula), for QALYs:
#Treatment 1:
pool_mean <- with(Data_T1_impute, by(Data_T1_impute, .imp, function(x) c(mean(x$QALY_EQ5D),sd(x$QALY_EQ5D))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean)  
#       Output: Mean 1.53  
se_DataT1_impute_QALY_EQ5D <- sd(Data_T1_impute$QALY_EQ5D)/sqrt(length(Data_T1_impute$QALY_EQ5D))
CIQALY_EQ5D_imputeTx1 <- c(mean(Data_T1_impute$QALY_EQ5D) - 1.96*se_DataT1_impute_QALY_EQ5D, mean(Data_T1_impute$QALY_EQ5D) + 1.96*se_DataT1_impute_QALY_EQ5D)
CIQALY_EQ5D_imputeTx1
#       Output: CI 1.52-1.54

# Treatment 2:
pool_mean <- with(Data_T2_impute, by(Data_T2_impute, .imp, function(x) c(mean(x$QALY_EQ5D),sd(x$QALY_EQ5D))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean) 
#       Output: Mean 1.44
se_DataT2_impute_QALY_EQ5D <- sd(Data_T2_impute$QALY_EQ5D)/sqrt(length(Data_T2_impute$QALY_EQ5D))
CIQALY_EQ5D_imputeTx2 <- c(mean(Data_T2_impute$QALY_EQ5D) - 1.96*se_DataT2_impute_QALY_EQ5D, mean(Data_T2_impute$QALY_EQ5D) + 1.96*se_DataT2_impute_QALY_EQ5D)
CIQALY_EQ5D_imputeTx2
#       Output: CI 1.43-1.45

# Plot histogram for imputed datasets to see if normally distributed
hist(Data_T1_impute$Total_Costs)
hist(Data_T1_impute$QALY_EQ5D)
hist(Data_T2_impute$Total_Costs)
hist(Data_T2_impute$QALY_EQ5D)
# None of the resulting histograms are normally distributed, so need to do bootstrapping

# Define function to be used for bootstrapping (taking the mean of Total_Costs and QALYs)
boot_function1 <- function(dat, d) {
  E <- dat[d,] # allows boot to select sample 
  return(c(mean(E$Total_Costs), mean(E$QALY_EQ5D)))
} 
# Use the function to get CIs for Treatment 1:
set.seed(123)
BootstrapReps_T1_impute <- boot(Data_T1_impute, boot_function1, 10000)
BootstrapReps_T1_impute
#       Output: Bootstrap Statistics:
# Bootstrap Statistics :
#   original       bias    std. error
# costs* 548.079664 1.972655e-02 5.000858188
# QALYs*   1.531215 5.204173e-05 0.003683282
boot.ci(BootstrapReps_T1_impute, type=c("norm", "perc", "bca"), index=1)
#       Output: 
# Level      Normal             Percentile            BCa          
# 95%   (538.3, 557.9 )      (538.5, 558.0 )    (538.6, 558.2 )   
# QALYs:
boot.ci(BootstrapReps_T1_impute, type=c("norm", "perc", "bca"), index=2)
#       Output: 
#       Level      Normal             Percentile            BCa          
#       95%   ( 1.524,  1.538 )   ( 1.524,  1.538 )   ( 1.524,  1.538 )    

# Use the function to get CIs for Treatment 2:
set.seed(123)
BootstrapReps_T2_impute <- boot(Data_T2_impute, boot_function1, 10000)
BootstrapReps_T2_impute
#       Output: Bootstrap Statistics:
#          original    bias          std. error
# Costs: 582.597392  1.971374e-02   14.292633871
# QALYs:  1.439354   -4.535701e-05  0.005657476

# Costs:
boot.ci(BootstrapReps_T2_impute, type=c("norm", "perc", "bca"), index=1)
#       Output:
#       Level      Normal         Percentile        BCa          
#       95%   (554.6, 610.6 )   (555.4, 611.7 )   (556.6, 612.7 )  

# QALYs:
boot.ci(BootstrapReps_T2_impute, type=c("norm", "perc", "bca"), index=2)
#       Output:
#       Level      Normal             Percentile            BCa          
# 95%   ( 1.428,  1.450 )   ( 1.429,  1.450 )   ( 1.429,  1.451 )    

# Save the results of bootstraps
write.csv(BootstrapReps_T1_impute$t, "BootstrapReps_T1_EQ5D.csv")
write.csv(BootstrapReps_T2_impute$t, "BootstrapReps_T2_EQ5D.csv")

# Code to read files back in to save re-runnning bootstraps 
BootstrapReps_T1_impute <- read.csv("BootstrapReps_T1_EQ5D.csv")
BootstrapReps_T2_impute <- read.csv("BootstrapReps_T2_EQ5D.csv")

# Calculate difference in costs and QALYs
DiffMeanCost <- BootstrapReps_T1_impute$V1 - BootstrapReps_T2_impute$V1
DiffMeanQALY <- BootstrapReps_T1_impute$V2 - BootstrapReps_T2_impute$V2
summary(DiffMeanCost)
#     Output:
#      Min.    1st Qu.   Median  Mean    3rd Qu.  Max. 
#      -90.61  -44.41    -34.23  -34.52  -24.34   19.47 
summary(DiffMeanQALY)
#     Output:
# Min.    1st Qu. Median   Mean     3rd Qu.  Max. 
# 0.06385 0.08733 0.09202  0.09196  0.09658  0.11852 

quantile(DiffMeanCost, c(.025, .5, .975)) 
#     Output:
# 2.5%        50%         97.5% 
# -64.488609  -34.229134  -5.624326 
quantile(DiffMeanQALY, c(.025, .5, .975)) 
# 2.5%        50%        97.5% 
# 0.07867105  0.09201930 0.10518536 

# Calculate ICER
mean(DiffMeanCost)/mean(DiffMeanQALY)
# -£375.36    Interpretation: A negative ICER on its own is meaningless, meaning a cost-effectiveness plane is needed to interpret the findings

# Plot bootstrapped samples on the cost-effectiveness plane and save plot
Results_table <- data.frame(DiffMeanCost, DiffMeanQALY)
CE_Plane_EQ5D <- ggplot(Results_table, aes(DiffMeanQALY, DiffMeanCost))+ geom_point(alpha=0.2) + xlab("Difference in Mean QALYs")+ ylab("Difference in Mean Costs") + xlim(-0.15,0.15)+ ylim(-140,140)+ geom_vline(aes(xintercept=0)) + geom_hline(aes(yintercept=0)) + geom_abline(aes(intercept = 0, slope = 20000), col="red")+ geom_text(aes(0.03, 100, label = "threshold @ £20,000"), size=5) +  theme(axis.title=element_text(size=25)) + theme(axis.text=element_text(size=15))
ggsave("CEP_EQ5D_190122.png", CE_Plane_EQ5D, dpi=300, width=33, height=19, units="cm")
# Plot shows majority of values in the North-West quadrant (Tx1 dominates)

# Plot CEAC
CEAC <- as.data.frame(matrix(data=NA, nrow=51, ncol=3))
colnames(CEAC) <- c("CeilingRatio","ProbCE1","ProbCE2")
CEAC$CeilingRatio <- seq(from = 0, to = 50000, by = 1000)
# calculate prob at different thresholds
for (i in 1:51) {       
  INB <- (DiffMeanQALY)*CEAC$CeilingRatio[i] - DiffMeanCost 
  CEAC$ProbCE1[i] <- sum(INB >=0)/length(DiffMeanQALY)
  CEAC$ProbCE2[i] <- 1 - CEAC$ProbCE1[i]
}

CEAC_EQ5D <- ggplot(data=CEAC, aes(x=CeilingRatio, y=ProbCE1)) + geom_line(colour="black") + xlab("threshold, £") + ylab("probability Tx1 is cost-effective") +ylim(0,1)+  theme(axis.title=element_text(size=25))+ theme(axis.text=element_text(size=15))
ggsave("CEAC_EQ5D_190122.png", CEAC_EQ5D, dpi=300, width=33, height=19, units="cm")

### CUA using SF6D utilities

# Calculate mean and CIs for QALYs derived from SF6D, ignoring missing data:
# Treatment 1:
summary(Data_T1$QALY_SF6D)
se_DataT1_QALY_SF6D <- sd(Data_T1$QALY_SF6D, na.rm=TRUE)/sqrt(length(Data_T1$QALY_SF6D))
CI_QALY_SF6D_raw_Tx1 <- c(mean(Data_T1$QALY_SF6D, na.rm=TRUE) - 1.96*se_DataT1_QALY_SF6D, mean(Data_T1$QALY_SF6D, na.rm=TRUE) + 1.96*se_DataT1_QALY_SF6D)
#       Output:
#       Min.    1st Qu.  Median   Mean    3rd Qu.  Max.    NA's 
#       0.9451  1.2841   1.5058   1.4666  1.6341   1.9154  38 
#       Mean 1.4666, CI 1.418978-1.514204

# Treatment 2:
summary(Data_T2$QALY_SF6D)
se_DataT2_QALY_SF6D <- sd(Data_T2$QALY_SF6D, na.rm=TRUE)/sqrt(length(Data_T2$QALY_SF6D))
CI_QALY_SF6D_raw_Tx2 <- c(mean(Data_T2$QALY_SF6D, na.rm=TRUE) - 1.96*se_DataT2_QALY_SF6D, mean(Data_T2$QALY_SF6D, na.rm=TRUE) + 1.96*se_DataT2_QALY_SF6D)
#       Output
#       Min.    1st Qu.  Median  Mean   3rd Qu. Max.    NA's 
#       0.846   1.258    1.449   1.416  1.558   1.844   24
#       Mean 1.416, CI 1.3543-1.47766

# Estimate the pooled mean across the 100 imputations, and CIs (using 95% CI formula), for SF6D-derived QALYs:
#Treatment 1:
pool_mean <- with(Data_T1_impute, by(Data_T1_impute, .imp, function(x) c(mean(x$QALY_SF6D),sd(x$QALY_SF6D))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean)  
#       Output: Mean 1.48
se_DataT1_impute_QALY_SF6D <- sd(Data_T1_impute$QALY_SF6D)/sqrt(length(Data_T1_impute$QALY_SF6D))
CIQALY_SF6D_imputeTx1 <- c(mean(Data_T1_impute$QALY_SF6D) - 1.96*se_DataT1_impute_QALY_SF6D, mean(Data_T1_impute$QALY_SF6D) + 1.96*se_DataT1_impute_QALY_SF6D)
CIQALY_SF6D_imputeTx1 
#       Output: CI 1.48- 1.49

# Treatment 2:
pool_mean <- with(Data_T2_impute, by(Data_T2_impute, .imp, function(x) c(mean(x$QALY_SF6D),sd(x$QALY_SF6D))))
pool_mean
Reduce("+",pool_mean)/length(pool_mean) 
#       Output: Mean 1.45  
se_DataT2_impute_QALY_SF6D <- sd(Data_T2_impute$QALY_SF6D)/sqrt(length(Data_T2_impute$QALY_SF6D))
CIQALY_SF6D_imputeTx2 <- c(mean(Data_T2_impute$QALY_SF6D) - 1.96*se_DataT2_impute_QALY_SF6D, mean(Data_T2_impute$QALY_SF6D) + 1.96*se_DataT2_impute_QALY_SF6D)
CIQALY_SF6D_imputeTx2
#       Output: CI 1.44-1.45

# Define function to be used for bootstrapping (taking the mean of Total_Costs and QALYs)
boot_function2 <- function(dat, d) {
  E <- dat[d,] # allows boot to select sample 
  return(c(mean(E$Total_Costs), mean(E$QALY_SF6D)))
} 

# Use the function to get SF6D-derived QALY CIs for Treatment 1:
set.seed(123)
BootstrapReps_T1_impute_SF6D <- boot(Data_T1_impute, boot_function2, 10000)
BootstrapReps_T1_impute_SF6D
#       Bootstrap Statistics :
#       original    bias          std. error
#Costs  548.079664  1.972655e-02  5.000858188
#QALYs  1.481524    4.008529e-05  0.002416599
boot.ci(BootstrapReps_T1_impute_SF6D, type=c("norm", "perc", "bca"), index=2)
#      Level      Normal             Percentile            BCa          
#       95%   ( 1.477,  1.486 )   ( 1.477,  1.486 )   ( 1.477,  1.486 )  

# Use the function to get CIs for Treatment 2:
set.seed(123)
BootstrapReps_T2_impute_SF6D <- boot(Data_T2_impute, boot_function2, 10000)
BootstrapReps_T2_impute_SF6D
#      Bootstrap Statistics :
#                   original     bias          std. error
# Bootstrap Statistics:
#         original     bias           std. error
#Costs:   582.597392   1.971374e-02   14.292633871
#QALYs:   1.448831     -2.736496e-05  0.003360677
boot.ci(BootstrapReps_T2_impute_SF6D, type=c("norm", "perc", "bca"), index=2)
#      Level      Normal             Percentile            BCa          
#       95%   ( 1.442,  1.455 )   ( 1.442,  1.456 )   ( 1.442,  1.456 )  

# Save the results of bootstraps
write.csv(BootstrapReps_T1_impute_SF6D$t, "BootstrapReps_T1_SF6D.csv")
write.csv(BootstrapReps_T2_impute_SF6D$t, "BootstrapReps_T2_SF6D.csv")

# Code to read back in to save re-running bootstraps
BootstrapReps_T1_impute_SF6D <- read.csv("BootstrapReps_T1_SF6D.csv")
BootstrapReps_T2_impute_SF6D <- read.csv("BootstrapReps_T2_SF6D.csv")

# Calculate the difference in SF6D_derived QALYs:
DiffMeanQALY_SF6D <- BootstrapReps_T1_impute_SF6D$V2 - BootstrapReps_T2_impute_SF6D$V2
summary(DiffMeanQALY_SF6D)
#     Output:
#     Min.    1st Qu.    Median      Mean     3rd Qu.     Max. 
#     0.01494 0.02988    0.03276    0.03276  0.03559    0.04856 

quantile(DiffMeanQALY_SF6D, c(.025, .5, .975)) 
#     2.5%           50%         97.5% 
#     0.02476550  0.03276029   0.04078073 

# Calculate ICER
mean(DiffMeanCost)/mean(DiffMeanQALY_SF6D)
# -1053.64      Interpretation: A negative ICER on its own is meaningless, meaning a cost-effectiveness plane is needed to interpret the findings

# Plot bootstrapped samples on the cost-effectiveness plane and save plot
Results_table_SF6D <- data.frame(DiffMeanCost, DiffMeanQALY_SF6D)
CE_Plane_SF6D <- ggplot(Results_table_SF6D, aes(DiffMeanQALY_SF6D, DiffMeanCost))+ geom_point(alpha=0.2) + xlab("Difference in Mean QALY (SF6D derived)") + ylab("Difference in Mean Costs") + xlim(-0.15,0.15)+ ylim(-140,140)+ geom_vline(aes(xintercept=0)) + geom_hline(aes(yintercept=0)) + geom_abline(aes(intercept = 0, slope = 20000), col="red") + geom_text(aes(0.03, 100, label = "threshold @ £20,000"), size=5) + theme(axis.title=element_text(size=25)) +theme(axis.text=element_text(size=15)) 
ggsave("CEP_SF6D_190122.png", CE_Plane_SF6D, dpi=300, width=33, height=19, units="cm")
# Plot shows majority of values remain in North-West quadrant.  However, the points have moved closer toward the North East Quadrant than when EQ5D-derived QALYs were used.

# Plot CEAC
CEAC_SF6D <- as.data.frame(matrix(data=NA, nrow=51, ncol=3))
colnames(CEAC_SF6D) <- c("CeilingRatio","ProbCE1","ProbCE2")
CEAC_SF6D$CeilingRatio <- seq(from = 0, to = 50000, by = 1000)
# calculate prob at different thresholds
for (i in 1:51) {       
  INB <- (DiffMeanQALY_SF6D)*CEAC_SF6D$CeilingRatio[i] - DiffMeanCost 
  CEAC_SF6D$ProbCE1[i] <- sum(INB >=0)/length(DiffMeanQALY)
  CEAC_SF6D$ProbCE2[i] <- 1 - CEAC_SF6D$ProbCE1[i]
}

CEAC_SF6D <- ggplot(data=CEAC_SF6D, aes(x=CeilingRatio, y=ProbCE1)) + geom_line(colour="black") + xlab("threshold, £") + ylab("probability Tx1 is cost-effective") + ylim(0,1) + theme(axis.text=element_text(size=15)) + theme(axis.title=element_text(size=25))
ggsave("CEAC_SF6D_190122.png", CEAC_SF6D, dpi=300, width=33, height=19, units="cm")
