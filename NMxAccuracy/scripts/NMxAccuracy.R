
# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/")

# load packages
library("readxl")
#library("ggpubr")

# import data
df <- read_excel("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/NMxPositiveAccuracy_anova.xlsx")
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
head(df)
summary(df)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Left Ventral Striatum

# 2-way anova model and model with interaction:
two.way.MVSL <- aov(MVSL ~ NM + Acc, data = df)
interaction.MVSL <- aov(MVSL ~ NM * Acc, data = df)

# print statistics for 2-way anova & interaction
summary(two.way.MVSL)
summary(interaction.MVSL)
#plot(two.way.MVSL)

# main effect of NM, so we do a simple linear correlation of NM with averaged accuracy:
MVSL_cor <- cor.test(df2$NM, df2$MVSL_avg, method = "pearson")
MVSL_cor

# scatterplot
plot(df2$NM, df2$MVSL_avg,
     main = "Monetary: NM & Left Ventral Striatal Activation",
     xlab = "Neuromelanin Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     col = "blue")
abline(lm(df2$MVSL_avg ~ df2$NM))

# these steps repeated for right VS without comment below; no main effects for NM in dorsal striatum:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Right Ventral Striatum
two.way.MVSR <- aov(MVSR ~ NM + Acc, data = df)
interaction.MVSR <- aov(MVSR ~ NM * Acc, data = df)

summary(two.way.MVSR)
summary(interaction.MVSR)
#plot(two.way.MVSR)

MVSR_cor <- cor.test(df2$NM, df2$MVSR_avg, method = "pearson")
MVSR_cor

plot(df2$NM, df2$MVSR_avg,
     main = "Monetary: NM & Right Ventral Striatal Activation",
     xlab = "Neuromelanin Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     col = "blue")
abline(lm(df2$MVSR_avg ~ df2$NM))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Left Dorsal Striatum
two.way.SDSL <- aov(SDSL ~ NM + Acc, data = df)
interaction.SDSL <- aov(SVSR ~ NM * Acc, data = df)

summary(two.way.SDSL)
summary(interaction.SDSL)
#plot(two.way.SDSL)

#SDSL_cor <- cor.test(df2$NM, df2$SDSL_avg, method = "pearson")
#SDSL_cor

#plot(df2$NM, df2$SDSL_avg,
#     main = "Social: NM & Left Dorsal Striatal Activation",
#     xlab = "Neuromelanin Signal",
#     ylab = "Avg. Left Dorsal Striatal Activation",
#     col = "blue")
#abline(lm(df2$SDSL_avg ~ df2$NM))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Right Dorsal Striatum
two.way.SDSR <- aov(SDSR ~ NM + Acc, data = df)
interaction.SDSR <- aov(SVSR ~ NM * Acc, data = df)

summary(two.way.SDSR)
summary(interaction.SDSR)
#plot(two.way.SDSR)

#SDSR_cor <- cor.test(df2$NM, df2$SDSR_avg, method = "pearson")
#SDSR_cor

#plot(df2$NM, df2$SDSR_avg,
#     main = "Social: NM & Left Dorsal Striatal Activation",
#     xlab = "Neuromelanin Signal",
#     ylab = "Avg. Left Dorsal Striatal Activation",
#     col = "blue")
#abline(lm(df2$SDSR_avg ~ df2$NM))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


