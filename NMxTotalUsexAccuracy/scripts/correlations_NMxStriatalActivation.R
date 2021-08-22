# Correlations

# Script for analyzing social doors NM data: specifically, correlations
# Jimmy Wyngaarden, August 2021



# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/")

# load packages
library("readxl")
library("ggpubr")
library("ggplot2")
library("reshape2")

# import data
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")



######### MONETARY #########
######### MONETARY #########
######### MONETARY #########
######### MONETARY #########
######### MONETARY #########

# Set Input Variables
monetary_contrast <- df2$MDSL_Contrast
monetary_incorrect <- df2$MDSL_Incorrect
monetary_correct <- df2$MDSL_Correct

# stats for contrast
cor <- cor.test(df2$NM_full, monetary_contrast, method = "pearson")
cor

# scatter plot for contrast
plot(df2$NM_full, monetary_contrast, pch=19,
     main = "Monetary: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Striatal Activation (Correct > Incorrect)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(monetary_contrast ~ df2$NM_full))

# stats for incorrect
cor1 <- cor.test(df2$NM_full, monetary_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, monetary_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, monetary_incorrect, pch=19,
     main = "Monetary: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkgreen")
abline(lm(monetary_incorrect ~ df2$NM_full),col = "darkgreen")
points(df2$NM_full, monetary_correct, pch=19, col = "green")
abline(lm(monetary_correct ~ df2$NM_full), col = "green")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("green", "darkgreen"), lty=1:1, cex=0.8)



######### SOCIAL #########
######### SOCIAL #########
######### SOCIAL #########
######### SOCIAL #########
######### SOCIAL #########

# Input Variables:
social_contrast <- df2$SVSR_Contrast
social_incorrect <- df2$SVSR_Incorrect
social_correct <- df2$SVSR_Correct

# stats for contrast
cor <- cor.test(df2$NM_full, social_contrast, method = "pearson")
cor

# plot for contrast
plot(df2$NM_full, social_contrast, pch=19,
     main = "Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Striatal Activation (Correct > Incorrect)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(social_contrast ~ df2$NM_full))


# stats for incorrect
cor1 <- cor.test(df2$NM_full, social_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, social_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, x1, pch=19,
     main = "Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkblue")
abline(lm(social_incorrect ~ df2$NM_full),col = "darkblue")
points(df2$NM_full, social_correct, pch=19, col = "blue")
abline(lm(social_correct ~ df2$NM_full), col = "blue")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("blue", "darkblue"), lty=1:1, cex=0.8)



# END



# stats
cor <- cor.test(df2$NM_full, df2$SDSR_Correct, method = "pearson")
cor
