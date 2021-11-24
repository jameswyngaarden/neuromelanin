
# Script for Social Doors SBU correlations, striatal activation with total use
# Jimmy Wyngaarden, November 2021

# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/")

# load packages
library("readxl")
library("ggpubr")
library("plot")
library("Hmisc")
library("olsrr")
library("performance")
library("ggplot2")
library("sjPlot")
library("reshape2")
library("interactions")

# import data
df_maineffects <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")



# # # # # # # Plot: NM-MRI signal and Substance Abuse (i.e., Total_Use) # # # # # # # 

plot(df_maineffects$NM_full, df_maineffects$Total_Use, pch=19,
     main = "Correlation between NM-MRI Signal and Substance Abuse",
     xlab = "NM-MRI Signal",
     ylab = "Substance Abuse",
     col = "blue")
abline(lm(df_maineffects$Total_Use ~ df_maineffects$NM_full))

cor.test(df_maineffects$NM_full, df_maineffects$Total_Use, method = "pearson")



# # # # # # # # # # # # # # # # Correlations # # # # # # # # # # # # # # # # # # 

# Total use and Monetary Ventral Striatum Left
cor.test(df_maineffects$Total_Use, df_maineffects$MVSL_Contrast, method = "pearson")
# t = -0.20819, df = 33, p-value = 0.8364, -0.03621676 

# Total use and Monetary Ventral Striatum Right
cor.test(df_maineffects$Total_Use, df_maineffects$MVSR_Contrast, method = "pearson")
# t = 0.93615, df = 33, p-value = 0.356, r = 0.1608405

# Total use and Monetary Ventral Striatum (combined)
cor.test(df_maineffects$Total_Use, df_maineffects$MVS_Contrast, method = "pearson")
# t = 0.27972, df = 33, p-value = 0.7814, r = 0.04863482 



# Total use and Social Ventral Striatum Left
cor.test(df_maineffects$Total_Use, df_maineffects$SVSL_Contrast, method = "pearson")
# t = 0.57417, df = 33, p-value = 0.5697, r = 0.09945531 

# Total use and Social Ventral Striatum Right
cor.test(df_maineffects$Total_Use, df_maineffects$SVSR_Contrast, method = "pearson")
# t = 0.23077, df = 33, p-value = 0.8189, r = 0.04013894 

# Total use and Social Ventral Striatum (combined)
cor.test(df_maineffects$Total_Use, df_maineffects$SVS_Contrast, method = "pearson")
# t = 0.40081, df = 33, p-value = 0.6911, r = 0.06960346



# Total use and Monetary Dorsal Striatum Left
cor.test(df_maineffects$Total_Use, df_maineffects$MDSL_Contrast, method = "pearson")
# t = -0.42534, df = 33, p-value = 0.6734, r = -0.07383959 

# Total use and Monetary Dorsal Striatum Right
cor.test(df_maineffects$Total_Use, df_maineffects$MDSR_Contrast, method = "pearson")
# t = -0.093148, df = 33, p-value = 0.9263, r = -0.01621285 

# Total use and Monetary Dorsal Striatum (combined)
cor.test(df_maineffects$Total_Use, df_maineffects$MDS_Contrast, method = "pearson")
# t = -0.29014, df = 33, p-value = 0.7735, r = -0.05044322



# Total use and Social Dorsal Striatum Left
cor.test(df_maineffects$Total_Use, df_maineffects$SDSL_Contrast, method = "pearson")
# t = 0.57463, df = 33, p-value = 0.5694, r = 0.09953384

# Total use and Social Dorsal Striatum Right
cor.test(df_maineffects$Total_Use, df_maineffects$SDSR_Contrast, method = "pearson")
# t = 0.41432, df = 33, p-value = 0.6813, r = 0.0719372

# Total use and Social Dorsal Striatum (combined)
cor.test(df_maineffects$Total_Use, df_maineffects$SDS_Contrast, method = "pearson")
#t = 0.50777, df = 33, p-value = 0.615, r = 0.08804879 



