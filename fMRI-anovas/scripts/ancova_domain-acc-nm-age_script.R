
# jimmy wyngaarden, sept. 2021
# script for running anovas & ancovas on social doors college sbu data:

# we have four rois: left & right ventral & dorsal striatum
# at each roi, we'll run the following analyses:
# 3. ancova: domain x accuracy x nm x age

# set working directory
setwd("~/Documents/GitHub/neuromelanin/fMRI-anovas")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/")

# load packages
library("readxl")
library("ggpubr")
library("Hmisc")
library("olsrr")
library("performance")
library("ggplot2")
library("sjPlot")
library("reshape2")

# load data
df1 <- read_excel("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/NMxPositiveAccuracy_anova3way.xlsx")
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
marijuana_use <- read_excel("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/ESI-single-items_marijuana-use.xlsx")
drug_use <- read_excel("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/ESI-single-items_drug-use.xlsx")
alcohol_use <- read_excel("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/ESI-single-items_alcohol-use.xlsx")


# quick calculation: cronbach's alpha
library("ltm")

cronbach.alpha(marijuana_use)
cronbach.alpha(drug_use)
cronbach.alpha(alcohol_use)



# ancova: domain x accuracy x neuromelanin, left ventral striatum
y1 <- df1$VSL
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full + df1$Age)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full * df1$Age)

# print results
print("Left Ventral Striatum Domain x Acc x NM x Age ANCOVA Results")
summary(ancova)
summary(interaction)



# ancova: domain x accuracy x neuromelanin, right ventral striatum
y1 <- df1$VSR
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full + df1$Age)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full * df1$Age)

# print results
print("Right Ventral Striatum Domain x Acc x NM xAge ANCOVA Results")
summary(ancova)
summary(interaction)



# ancova: domain x accuracy x neuromelanin, left dorsal striatum
y1 <- df1$DSL
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full + df1$Age)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full * df1$Age)

# print results
print("Left Dorsal Striatum Domain x Acc x NM x Age ANCOVA Results")
summary(ancova)
summary(interaction)



# ancova: domain x accuracy x neuromelanin, right dorsal striatum
y1 <- df1$DSR
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full + df1$Age)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full * df1$Age)

# print results
print("Right Dorsal Striatum Domain x Acc x NM x Age ANCOVA Results")
summary(ancova)
summary(interaction)


