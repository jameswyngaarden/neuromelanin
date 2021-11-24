
# Script for NM paper figures
# Jimmy Wyngaarden, November 2021

# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/")

# load packages
library("readxl")

# import data
df_maineffects <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
df_moderation <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_moderation.xlsx")


# # # # # # Plot: NM-MRI signal and Substance Abuse (i.e., Total_Use) # # # # # 

plot(df_maineffects$NM_full, df_maineffects$Total_Use, pch=19,
     main = "Correlation between NM-MRI Signal and Substance Abuse",
     xlab = "NM-MRI Signal",
     ylab = "Substance Abuse",
     col = "blue")
abline(lm(df_maineffects$Total_Use ~ df_maineffects$NM_full))

cor.test(df_maineffects$NM_full, df_maineffects$Total_Use, method = "pearson")



# # Plot: Moderating effect of NM on striatal activation and substance abuse # #

plot(df_moderation$SVSR_contrast, df_moderation$SubAbuse, pch=19,
     main = "Moderating effect of NM for social task in right ventral striatum",
     xlab = "Right VS activation in the social task (correct > incorrect)",
     ylab = "Substance Abuse",
     col = df_moderation$NM_full)

#abline(lm(df_maineffects$Total_Use ~ df_maineffects$NM_full))
