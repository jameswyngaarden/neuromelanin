
# jimmy wyngaarden, sept. 2021
# script for running anovas & ancovas on social doors college sbu data:

# we have four rois: left & right ventral & dorsal striatum
# at each roi, we'll run the following analyses:
# 2. ancova: domain x accuracy x nm

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



# ancova: domain x accuracy x neuromelanin, left ventral striatum
y1 <- df1$Ventral
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full)

# print results
print("Ventral Striatum Domain x Acc x NM ANCOVA Results")
summary(ancova)
summary(interaction)

# break down ancova: monetary
monetary_incorrect <- df2$MVSL_Incorrect
monetary_correct <- df2$MVSL_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, monetary_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, monetary_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, monetary_incorrect, pch=19,
     main = "Left Ventral Striatum, Monetary: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkgreen")
abline(lm(monetary_incorrect ~ df2$NM_full),col = "darkgreen")
points(df2$NM_full, monetary_correct, pch=19, col = "green")
abline(lm(monetary_correct ~ df2$NM_full), col = "green")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("green", "darkgreen"), lty=1:1, cex=0.8)

# break down ancova: social
social_incorrect <- df2$SVSL_Incorrect
social_correct <- df2$SVSL_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, social_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, social_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, social_incorrect, pch=19,
     main = "Left Ventral Striatum, Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkblue")
abline(lm(social_incorrect ~ df2$NM_full),col = "darkblue")
points(df2$NM_full, social_correct, pch=19, col = "blue")
abline(lm(monetary_correct ~ df2$NM_full), col = "blue")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("blue", "darkblue"), lty=1:1, cex=0.8)



# ancova: domain x accuracy x neuromelanin, right ventral striatum
y1 <- df1$VSR
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full)

# print results
print("Right Ventral Striatum Domain x Acc x NM ANCOVA Results")
summary(ancova)
summary(interaction)

# break down ancova: monetary
monetary_incorrect <- df2$MVSR_Incorrect
monetary_correct <- df2$MVSR_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, monetary_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, monetary_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, monetary_incorrect, pch=19,
     main = "Right Ventral Striatum, Monetary: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkgreen")
abline(lm(monetary_incorrect ~ df2$NM_full),col = "darkgreen")
points(df2$NM_full, monetary_correct, pch=19, col = "green")
abline(lm(monetary_correct ~ df2$NM_full), col = "green")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("green", "darkgreen"), lty=1:1, cex=0.8)

# break down ancova: social
social_incorrect <- df2$SVSR_Incorrect
social_correct <- df2$SVSR_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, social_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, social_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, social_incorrect, pch=19,
     main = "Right Ventral Striatum, Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkblue")
abline(lm(social_incorrect ~ df2$NM_full),col = "darkblue")
points(df2$NM_full, social_correct, pch=19, col = "blue")
abline(lm(monetary_correct ~ df2$NM_full), col = "blue")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("blue", "darkblue"), lty=1:1, cex=0.8)



# ancova: domain x accuracy x neuromelanin, left dorsal striatum
y1 <- df1$DSL
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full)

# print results
print("Left Dorsal Striatum Domain x Acc x NM ANCOVA Results")
summary(ancova)
summary(interaction)

# break down ancova: monetary
monetary_incorrect <- df2$MDSL_Incorrect
monetary_correct <- df2$MDSL_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, monetary_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, monetary_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, monetary_incorrect, pch=19,
     main = "Left Dorsal Striatum, Monetary: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkgreen")
abline(lm(monetary_incorrect ~ df2$NM_full),col = "darkgreen")
points(df2$NM_full, monetary_correct, pch=19, col = "green")
abline(lm(monetary_correct ~ df2$NM_full), col = "green")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("green", "darkgreen"), lty=1:1, cex=0.8)

# break down ancova: social
social_incorrect <- df2$SDSL_Incorrect
social_correct <- df2$SDSL_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, social_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, social_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, social_incorrect, pch=19,
     main = "Left Dorsal Striatum, Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkblue")
abline(lm(social_incorrect ~ df2$NM_full),col = "darkblue")
points(df2$NM_full, social_correct, pch=19, col = "blue")
abline(lm(monetary_correct ~ df2$NM_full), col = "blue")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("blue", "darkblue"), lty=1:1, cex=0.8)



# ancova: domain x accuracy x neuromelanin, right dorsal striatum
y1 <- df1$DSR
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full)

# print results
print("Right Dorsal Striatum Domain x Acc x NM ANCOVA Results")
summary(ancova)
summary(interaction)

# break down ancova: monetary
monetary_incorrect <- df2$MDSR_Incorrect
monetary_correct <- df2$MDSR_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, monetary_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, monetary_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, monetary_incorrect, pch=19,
     main = "Right Dorsal Striatum, Monetary: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkgreen")
abline(lm(monetary_incorrect ~ df2$NM_full),col = "darkgreen")
points(df2$NM_full, monetary_correct, pch=19, col = "green")
abline(lm(monetary_correct ~ df2$NM_full), col = "green")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("green", "darkgreen"), lty=1:1, cex=0.8)

# break down ancova: social
social_incorrect <- df2$SDSR_Incorrect
social_correct <- df2$SDSR_Correct

# stats for incorrect
cor1 <- cor.test(df2$NM_full, social_incorrect, method = "pearson")
cor1

# stats for correct
cor2 <- cor.test(df2$NM_full, social_correct, method = "pearson")
cor2

# scatter plot for correct & incorrect
plot(df2$NM_full, social_incorrect, pch=19,
     main = "Right Dorsal Striatum, Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkblue")
abline(lm(social_incorrect ~ df2$NM_full),col = "darkblue")
points(df2$NM_full, social_correct, pch=19, col = "blue")
abline(lm(monetary_correct ~ df2$NM_full), col = "blue")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("blue", "darkblue"), lty=1:1, cex=0.8)



