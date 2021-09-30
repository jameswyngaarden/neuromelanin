
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

# load data
#df1 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_anova.xlsx")
#df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
#df3 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_anova3way.xlsx")
#df4 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects2.xlsx")

# ancova: domain x accuracy x neuromelanin, left ventral striatum
y1 <- df1$VSL
ancova <- aov(y1 ~ df1$Domain + df1$Acc + df1$NM_full)
interaction <- aov(y1 ~ df1$Domain * df1$Acc * df1$NM_full)

# print results
print("Left Ventral Striatum ANCOVA Results")
summary(ancova)
summary(interaction)
#plot(three.way.SVSR)

# Breaking down by accuracy
lmSVSRincorrect <- lm(SVSR_Correct ~ NM_full + Total_Use, data = df2)
summary(lmSVSRincorrect)

lmSVSRcorrect <- lm(SVSR_Correct ~ NM_full + Total_Use, data = df2)
summary(lmSVSRcorrect)

# SVSR_Contrast & total use
ggscatter(df2, x = "Total_Use", y = "SVSR_Contrast",
          main="SVSR_Contrast & Total Use",
          xlab = "Total Use", ylab = "SVSR_Contrast",
          col = "blue",
          add = "reg.line",
          conf.int = TRUE,
          add.params = list(color = "darkgray",
                            fill = "lightgray")
)+
  stat_cor(method = "pearson")

# SDSR_Correct & Total_Use
ggscatter(df2, x = "Total_Use", y = "SDSR_Correct",
          main="SDSR_Correct & Total Use",
          xlab = "Total Use", ylab = "SDSR_Correct",
          col = "blue",
          add = "reg.line",
          conf.int = TRUE,
          add.params = list(color = "darkgray",
                            fill = "lightgray")
)+
  stat_cor(method = "pearson")

# SDSR_Incorrect & NM full
ggscatter(df2, x = "NM_full", y = "SDSR_Incorrect",
          main="SDSR_Incorrect & NM full",
          xlab = "NM full", ylab = "SDSR_Incorrect",
          col = "red",
          add = "reg.line",
          conf.int = TRUE,
          add.params = list(color = "darkgray",
                            fill = "lightgray")
)+
  stat_cor(method = "pearson")
                      
########### Right Dorsal Striatum ###############
# 3-way anova model and model with interaction:
three.way.SDSR <- aov(SDSR ~ NM_full + Acc + Total_Use, data = df1)
interaction.SDSR <- aov(SDSR ~ NM_full * Acc * Total_Use, data = df1)

# print statistics for 3-way anova & interaction
summary(three.way.SDSR)
summary(interaction.SDSR)
plot(three.way.SDSR)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Mediation analyses
# NM_full as mediator for Total Use & Striatal (MVSL) activation?

# Mediation with PROCESS (model 4)
#process(data = df2, y = "SDSR_Contrast", x = "Substance_Abuse", m = "NM_full", model = 4)

# Moderation with PROCESS (model 1)
process(data = df2, y = "MDSR_Contrast", x = "Total_Use", w = "NM_full", model = 1)

# Moderation using lm
model = lm(SVSR_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df2))
summary(model)

# Plotting simple slopes 10 Aug 2021
interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE)
print(interaction)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Normality checks:

# OLS normality test
ols_test_normality(model)

# shapiro test
shapiro.test(model$residuals)

# ks test
ks.test(model$residuals, "pnorm")

check_normality(model)
check_zeroinflation(model)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANOVAs: Accuracy(correct vs. Incorrect) x Neuromelanin Signal -> % Signal Change BOLD Activation?
# - ran this analysis for monetary left ventral striatum (mvsl), mvsr, sdsl, sdsr
# - for significant main effect of NM: combined accuracy (averaged together) and looked at linear relationship between NM & Accuracy
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Left Ventral Striatum

# 2-way anova model and model with interaction:
two.way.MVSL <- aov(MVSL ~ NM + Acc, data = df1)
interaction.MVSL <- aov(MVSL ~ NM * Acc, data = df1)

# print statistics for 2-way anova & interaction
summary(two.way.MVSL)
summary(interaction.MVSL)
#plot(two.way.MVSL)

# main effect of NM, so we do a simple linear correlation of NM with averaged accuracy:
MVSL_cor <- cor.test(df2$NM, df2$MVSL_avg, method = "pearson")
MVSL_cor

## MONETARY ##

# scatterplot
x <- df2$MVSL_Contrast
x1 <- df2$MVSL_Incorrect
x2 <- df2$MVSL_Correct

MVSL_cor <- cor.test(df2$NM_full, df2$MVSL_Contrast, method = "pearson")
MVSL_cor

plot(df2$NM_full, x, pch=19,
     main = "MVSL: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Striatal Activation (Correct > Incorrect)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$MVSL_Contrast ~ df2$NM_full))

# linear correlations for correct & incorrect (separate lines)

MVSL_cor <- cor.test(df2$NM_full, x1, method = "pearson")
MVSL_cor

MVSL_cor <- cor.test(df2$NM_full, x2, method = "pearson")
MVSL_cor

# scatterplot
plot(df2$NM_full, x1, pch=19,
     main = "MVSL: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkgreen")
abline(lm(x1 ~ df2$NM_full),col = "darkgreen")
points(df2$NM_full, x2, pch=19, col = "green")
abline(lm(x2 ~ df2$NM_full), col = "green")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("green", "darkgreen"), lty=1:1, cex=0.8)


# scatterplot
x <- df2$MVSL_Contrast
x1 <- df2$MVSL_Incorrect
x2 <- df2$MVSL_Correct

MVSL_cor <- cor.test(df2$NM_full, df2$MVSL_Contrast, method = "pearson")
MVSL_cor

plot(df2$NM_full, x, pch=19,
     main = "MVSL: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Striatal Activation (Correct > Incorrect)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$MVSL_Contrast ~ df2$NM_full))


## SOCIAL ##

# scatterplot
x <- df2$SVSL_Contrast
x1 <- df2$SVSL_Incorrect
x2 <- df2$SVSL_Correct

cor <- cor.test(df2$NM_full, x, method = "pearson")
cor

plot(df2$NM_full, x, pch=19,
     main = "Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Striatal Activation (Correct > Incorrect)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(x ~ df2$NM_full))

# linear correlations for correct & incorrect (separate lines)

cor1 <- cor.test(df2$NM_full, x1, method = "pearson")
cor1

cor2 <- cor.test(df2$NM_full, x2, method = "pearson")
cor2

# scatterplot
plot(df2$NM_full, x1, pch=19,
     main = "Social: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Avg. Activation",
     ylim = c(-.4,.3),
     col = "darkblue")
abline(lm(x1 ~ df2$NM_full),col = "darkblue")
points(df2$NM_full, x2, pch=19, col = "blue")
abline(lm(x2 ~ df2$NM_full), col = "blue")
legend("bottomright", legend=c("Correct", "Incorrect"), col=c("blue", "darkblue"), lty=1:1, cex=0.8)


# scatterplot
x <- df2$MVSL_Contrast
x1 <- df2$MVSL_Incorrect
x2 <- df2$MVSL_Correct

MVSL_cor <- cor.test(df2$NM_full, df2$MVSL_Contrast, method = "pearson")
MVSL_cor

plot(df2$NM_full, x, pch=19,
     main = "MVSL: NM Full & Striatal Activation",
     xlab = "NM Full Signal",
     ylab = "Striatal Activation (Correct > Incorrect)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$MVSL_Contrast ~ df2$NM_full))


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

MVSR_cor <- cor.test(df2$NM, df2$MVSR_Contrast, method = "pearson")
MVSR_cor

plot(df2$NM, df2$MVSR_avg,
     main = "MVSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$MVSR_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
MVSR_cor <- cor.test(df2$NM, df2$MVSR_Incorrect, method = "pearson")
MVSR_cor

MVSR_cor <- cor.test(df2$NM, df2$MVSR_Correct, method = "pearson")
MVSR_cor

# scatterplot
plot(df2$NM, df2$MVSR_Incorrect, pch=19,
     main = "MVSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$MVSR_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$MVSR_Correct, pch=19, col = "green")
abline(lm(df2$MVSR_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Left Ventral Striatum
two.way.SVSL <- aov(SVSL ~ NM + Acc, data = df)
interaction.SVSL <- aov(SVSL ~ NM * Acc, data = df)

summary(two.way.SVSL)
summary(interaction.SVSL)
#plot(two.way.SVSL)

SVSL_cor <- cor.test(df2$NM, df2$SVSL_avg, method = "pearson")
SVSL_cor

SVSL_cor <- cor.test(df2$NM, df2$SVSL_Contrast, method = "pearson")
SVSL_cor

plot(df2$NM, df2$SVSL_avg,
     main = "SVSL: NMvstri & Left Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$SVSL_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
SVSL_cor <- cor.test(df2$NM, df2$SVSL_Incorrect, method = "pearson")
SVSL_cor

SVSL_cor <- cor.test(df2$NM, df2$SVSL_Correct, method = "pearson")
SVSL_cor

# scatterplot
plot(df2$NM, df2$SVSL_Incorrect, pch=19,
     main = "SVSL: NMvstri & Left Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$SVSL_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$SVSL_Correct, pch=19, col = "green")
abline(lm(df2$SVSL_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Right Ventral Striatum
two.way.SVSR <- aov(SVSR ~ NM + Acc, data = df)
interaction.SVSR <- aov(SVSR ~ NM * Acc, data = df)

summary(two.way.SVSR)
summary(interaction.SVSR)
#plot(two.way.SVSR)

SVSR_cor <- cor.test(df2$NM, df2$SVSR_avg, method = "pearson")
SVSR_cor

SVSR_cor <- cor.test(df2$NM, df2$SVSR_Contrast, method = "pearson")
SVSR_cor

plot(df2$NM, df2$SVSR_avg,
     main = "SVSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$SVSR_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
SVSR_cor <- cor.test(df2$NM, df2$SVSR_Incorrect, method = "pearson")
SVSR_cor

SVSR_cor <- cor.test(df2$NM, df2$SVSR_Correct, method = "pearson")
SVSR_cor

# scatterplot
plot(df2$NM, df2$SVSR_Incorrect, pch=19,
     main = "SVSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$SVSR_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$SVSR_Correct, pch=19, col = "green")
abline(lm(df2$SVSR_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Left Dorsal Striatum
two.way.MDSL <- aov(MDSL ~ NM + Acc, data = df)
interaction.MDSL <- aov(MVSL ~ NM * Acc, data = df)

summary(two.way.MDSL)
summary(interaction.MDSL)
#plot(two.way.MDSL)

MDSL_cor <- cor.test(df2$NM, df2$MDSL_avg, method = "pearson")
MDSL_cor

MDSL_cor <- cor.test(df2$NM, df2$MDSL_Contrast, method = "pearson")
MDSL_cor

plot(df2$NM, df2$MDSL_avg,
     main = "MDSL: NMvstri & Left Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$MDSL_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
MDSL_cor <- cor.test(df2$NM, df2$MDSL_Incorrect, method = "pearson")
MDSL_cor

MDSL_cor <- cor.test(df2$NM, df2$MDSL_Correct, method = "pearson")
MDSL_cor

# scatterplot
plot(df2$NM, df2$MDSL_Incorrect, pch=19,
     main = "MDSL: NMvstri & Left Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$MDSL_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$MDSL_Correct, pch=19, col = "green")
abline(lm(df2$MDSL_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Right Dorsal Striatum
two.way.MDSR <- aov(MDSR ~ NM + Acc, data = df)
interaction.MDSR <- aov(MVSR ~ NM * Acc, data = df)

summary(two.way.MDSR)
summary(interaction.MDSR)
#plot(two.way.MDSR)

MDSR_cor <- cor.test(df2$NM, df2$MDSR_avg, method = "pearson")
MDSR_cor

MDSR_cor <- cor.test(df2$NM, df2$MDSR_Contrast, method = "pearson")
MDSR_cor

plot(df2$NM, df2$MDSR_avg,
     main = "MDSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$MDSR_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
MDSR_cor <- cor.test(df2$NM, df2$MDSR_Incorrect, method = "pearson")
MDSR_cor

MDSR_cor <- cor.test(df2$NM, df2$MDSR_Correct, method = "pearson")
MDSR_cor

# scatterplot
plot(df2$NM, df2$MDSR_Incorrect, pch=19,
     main = "MDSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$MDSR_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$MDSR_Correct, pch=19, col = "green")
abline(lm(df2$MDSR_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Left Dorsal Striatum
two.way.SDSL <- aov(SDSL ~ NM + Acc, data = df)
interaction.SDSL <- aov(SVSL ~ NM * Acc, data = df)

summary(two.way.SDSL)
summary(interaction.SDSL)
#plot(two.way.SDSL)

SDSL_cor <- cor.test(df2$NM, df2$SDSL_avg, method = "pearson")
SDSL_cor

SDSL_cor <- cor.test(df2$NM, df2$SDSL_Contrast, method = "pearson")
SDSL_cor

plot(df2$NM, df2$SDSL_avg,
     main = "SDSL: NMvstri & Left Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$SDSL_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
SDSL_cor <- cor.test(df2$NM, df2$SDSL_Incorrect, method = "pearson")
SDSL_cor

SDSL_cor <- cor.test(df2$NM, df2$SDSL_Correct, method = "pearson")
SDSL_cor

# scatterplot
plot(df2$NM, df2$SDSL_Incorrect, pch=19,
     main = "SDSL: NMvstri & Left Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$SDSL_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$SDSL_Correct, pch=19, col = "green")
abline(lm(df2$SDSL_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Right Dorsal Striatum
two.way.SDSR <- aov(SDSR ~ NM + Acc, data = df)
interaction.SDSR <- aov(SVSR ~ NM * Acc, data = df)

summary(two.way.SDSR)
summary(interaction.SDSR)
#plot(two.way.SDSR)

SDSR_cor <- cor.test(df2$NM, df2$SDSR_avg, method = "pearson")
SDSR_cor

SDSR_cor <- cor.test(df2$NM, df2$SDSR_Contrast, method = "pearson")
SDSR_cor

plot(df2$NM, df2$SDSR_avg,
     main = "SDSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "green")
abline(lm(df2$SDSR_avg ~ df2$NM))

# linear correlations for correct & incorrect (separate lines):
SDSR_cor <- cor.test(df2$NM, df2$SDSR_Incorrect, method = "pearson")
SDSR_cor

SDSR_cor <- cor.test(df2$NM, df2$SDSR_Correct, method = "pearson")
SDSR_cor

# scatterplot
plot(df2$NM, df2$SDSR_Incorrect, pch=19,
     main = "SDSR: NMvstri & Right Ventral Striatal Activation",
     xlab = "NMvstri Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df2$SDSR_Incorrect ~ df2$NM),col = "blue")
points(df2$NM, df2$SDSR_Correct, pch=19, col = "green")
abline(lm(df2$SDSR_Correct ~ df2$NM), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

################################################################################
################################################################################
################################################################################

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Left Ventral Striatum

# 2-way anova model and model with interaction:
two.way.MVSL <- aov(MVSL ~ NM_full + Acc, data = df4)
interaction.MVSL <- aov(MVSL ~ NM_full * Acc, data = df4)

# print statistics for 2-way anova & interaction
summary(two.way.MVSL)
summary(interaction.MVSL)
#plot(two.way.MVSL)

# main effect of NM, so we do a simple linear correlation of NM with averaged accuracy:
MVSL_cor <- cor.test(df5$NM_full, df5$MVSL_avg, method = "pearson")
MVSL_cor

MVSL_cor <- cor.test(df5$NM_full, df5$MVSL_Contrast, method = "pearson")
MVSL_cor

# scatterplot
plot(df5$NM_full, df5$MVSL_avg,
     main = "MVSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MVSL_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
MVSL_cor <- cor.test(df5$NM_full, df5$MVSL_Incorrect, method = "pearson")
MVSL_cor

MVSL_cor <- cor.test(df5$NM_full, df5$MVSL_Correct, method = "pearson")
MVSL_cor

# scatterplot
plot(df5$NM_full, df5$MVSL_Incorrect, pch=19,
     main = "MVSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MVSL_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$MVSL_Correct, pch=19, col = "green")
abline(lm(df5$MVSL_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Right Ventral Striatum
two.way.MVSR <- aov(MVSR ~ NM_full + Acc, data = df4)
interaction.MVSR <- aov(MVSR ~ NM_full * Acc, data = df4)

summary(two.way.MVSR)
summary(interaction.MVSR)
#plot(two.way.MVSR)

MVSR_cor <- cor.test(df5$NM_full, df5$MVSR_avg, method = "pearson")
MVSR_cor

MVSR_cor <- cor.test(df5$NM_full, df5$MVSR_Contrast, method = "pearson")
MVSR_cor

plot(df5$NM_full, df5$MVSR_avg,
     main = "MVSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MVSR_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
MVSR_cor <- cor.test(df5$NM_full, df5$MVSR_Incorrect, method = "pearson")
MVSR_cor

MVSR_cor <- cor.test(df5$NM_full, df5$MVSR_Correct, method = "pearson")
MVSR_cor

# scatterplot
plot(df5$NM_full, df5$MVSR_Incorrect, pch=19,
     main = "MVSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MVSR_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$MVSR_Correct, pch=19, col = "green")
abline(lm(df5$MVSR_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Left Ventral Striatum
two.way.SVSL <- aov(SVSL ~ NM_full + Acc, data = df4)
interaction.SVSL <- aov(SVSL ~ NM_full * Acc, data = df4)

summary(two.way.SVSL)
summary(interaction.SVSL)
#plot(two.way.SVSL)

SVSL_cor <- cor.test(df5$NM_full, df5$SVSL_avg, method = "pearson")
SVSL_cor

SVSL_cor <- cor.test(df5$NM_full, df5$SVSL_Contrast, method = "pearson")
SVSL_cor

plot(df5$NM_full, df5$SVSL_avg,
     main = "SVSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SVSL_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
SVSL_cor <- cor.test(df5$NM_full, df5$SVSL_Incorrect, method = "pearson")
SVSL_cor

SVSL_cor <- cor.test(df5$NM_full, df5$SVSL_Correct, method = "pearson")
SVSL_cor

# scatterplot
plot(df5$NM_full, df5$SVSL_Incorrect, pch=19,
     main = "SVSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SVSL_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$SVSL_Correct, pch=19, col = "green")
abline(lm(df5$SVSL_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Right Ventral Striatum
two.way.SVSR <- aov(SVSR ~ NM_full + Acc, data = df4)
interaction.SVSR <- aov(SVSR ~ NM_full * Acc, data = df4)

summary(two.way.SVSR)
summary(interaction.SVSR)
#plot(two.way.SVSR)

SVSR_cor <- cor.test(df5$NM_full, df5$SVSR_avg, method = "pearson")
SVSR_cor

SVSR_cor <- cor.test(df5$NM_full, df5$SVSR_Contrast, method = "pearson")
SVSR_cor

plot(df5$NM_full, df5$SVSR_avg,
     main = "SVSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SVSR_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
SVSR_cor <- cor.test(df5$NM_full, df5$SVSR_Incorrect, method = "pearson")
SVSR_cor

SVSR_cor <- cor.test(df5$NM_full, df5$SVSR_Correct, method = "pearson")
SVSR_cor

# scatterplot
plot(df5$NM_full, df5$SVSR_Incorrect, pch=19,
     main = "SVSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SVSR_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$SVSR_Correct, pch=19, col = "green")
abline(lm(df5$SVSR_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Left Dorsal Striatum
two.way.MDSL <- aov(MDSL ~ NM_full + Acc, data = df4)
interaction.MDSL <- aov(MVSL ~ NM_full * Acc, data = df4)

summary(two.way.MDSL)
summary(interaction.MDSL)
#plot(two.way.MDSL)

MDSL_cor <- cor.test(df5$NM_full, df5$MDSL_avg, method = "pearson")
MDSL_cor

MDSL_cor <- cor.test(df5$NM_full, df5$MDSL_Contrast, method = "pearson")
MDSL_cor

plot(df5$NM_full, df5$MDSL_avg,
     main = "MDSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MDSL_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
MDSL_cor <- cor.test(df5$NM_full, df5$MDSL_Incorrect, method = "pearson")
MDSL_cor

MDSL_cor <- cor.test(df5$NM_full, df5$MDSL_Correct, method = "pearson")
MDSL_cor

# scatterplot
plot(df5$NM_full, df5$MDSL_Incorrect, pch=19,
     main = "MDSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MDSL_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$MDSL_Correct, pch=19, col = "green")
abline(lm(df5$MDSL_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Monetary: Right Dorsal Striatum
two.way.MDSR <- aov(MDSR ~ NM_full + Acc, data = df4)
interaction.MDSR <- aov(MVSR ~ NM_full * Acc, data = df4)

summary(two.way.MDSR)
summary(interaction.MDSR)
#plot(two.way.MDSR)

MDSR_cor <- cor.test(df5$NM_full, df5$MDSR_avg, method = "pearson")
MDSR_cor

MDSR_cor <- cor.test(df5$NM_full, df5$MDSR_Contrast, method = "pearson")
MDSR_cor

plot(df5$NM_full, df5$MDSR_avg,
     main = "MDSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MDSR_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
MDSR_cor <- cor.test(df5$NM_full, df5$MDSR_Incorrect, method = "pearson")
MDSR_cor

MDSR_cor <- cor.test(df5$NM_full, df5$MDSR_Correct, method = "pearson")
MDSR_cor

# scatterplot
plot(df5$NM_full, df5$MDSR_Incorrect, pch=19,
     main = "MDSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$MDSR_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$MDSR_Correct, pch=19, col = "green")
abline(lm(df5$MDSR_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Left Dorsal Striatum
two.way.SDSL <- aov(SDSL ~ NM_full + Acc, data = df4)
interaction.SDSL <- aov(SVSL ~ NM_full * Acc, data = df4)

summary(two.way.SDSL)
summary(interaction.SDSL)
#plot(two.way.SDSL)

SDSL_cor <- cor.test(df5$NM_full, df5$SDSL_avg, method = "pearson")
SDSL_cor

SDSL_cor <- cor.test(df5$NM_full, df5$SDSL_Contrast, method = "pearson")
SDSL_cor

plot(df5$NM_full, df5$SDSL_avg,
     main = "SDSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SDSL_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
SDSL_cor <- cor.test(df5$NM_full, df5$SDSL_Incorrect, method = "pearson")
SDSL_cor

SDSL_cor <- cor.test(df5$NM_full, df5$SDSL_Correct, method = "pearson")
SDSL_cor

# scatterplot
plot(df5$NM_full, df5$SDSL_Incorrect, pch=19,
     main = "SDSL: Full NM & Left Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Left Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SDSL_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$SDSL_Correct, pch=19, col = "green")
abline(lm(df5$SDSL_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Social: Right Dorsal Striatum
two.way.SDSR <- aov(SDSR ~ NM_full + Acc, data = df4)
interaction.SDSR <- aov(SVSR ~ NM_full * Acc, data = df4)

summary(two.way.SDSR)
summary(interaction.SDSR)
#plot(two.way.SDSR)

SDSR_cor <- cor.test(df5$NM_full, df5$SDSR_avg, method = "pearson")
SDSR_cor

SDSR_cor <- cor.test(df5$NM_full, df5$SDSR_Contrast, method = "pearson")
SDSR_cor

plot(df5$NM_full, df5$SDSR_avg,
     main = "SDSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation (Avg.)",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SDSR_avg ~ df5$NM_full))

# linear correlations for correct & incorrect (separate lines):
SDSR_cor <- cor.test(df5$NM_full, df5$SDSR_Incorrect, method = "pearson")
SDSR_cor

SDSR_cor <- cor.test(df5$NM_full, df5$SDSR_Correct, method = "pearson")
SDSR_cor

# scatterplot
plot(df5$NM_full, df5$SDSR_Incorrect, pch=19,
     main = "SDSR: Full NM & Right Ventral Striatal Activation",
     xlab = "Full NM Signal",
     ylab = "Avg. Right Ventral Striatal Activation",
     ylim = c(-.4,.3),
     col = "blue")
abline(lm(df5$SDSR_Incorrect ~ df5$NM_full),col = "blue")
points(df5$NM_full, df5$SDSR_Correct, pch=19, col = "green")
abline(lm(df5$SDSR_Correct ~ df5$NM_full), col = "green")
legend("bottomright", legend=c("Incorrect", "Correct"), col=c("blue", "green"), lty=1:1, cex=0.8)

################################################################################
################################################################################
################################################################################

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANOVAs: Domain (Monetary vs. Social) x Accuracy (correct vs. Incorrect) x Neuromelanin Signal -> % Signal Change BOLD Activation?
# - ran this analysis for left ventral striatum (vsl), vsr, dsl, dsr
# - found main effect of accuracy for each region; main effect of domain and & NM also for RVS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Left Ventral Striatum
three.way.VSL <- aov(VSL ~ NM + Acc + Domain, data = df3)
interaction.VSL <- aov(VSL ~ NM * Acc * Domain * Age, data = df3)
summary(three.way.VSL)
summary(interaction.VSL)

# Right Ventral Striatum
three.way.VSR <- aov(VSR ~ NM + Acc + Domain, data = df3)
interaction.VSR <- aov(VSR ~ NM * Acc * Domain * Age, data = df3)
summary(three.way.VSR)
summary(interaction.VSR)

# RVS EMMs
library("emmeans")
library(ggplot2)

(frg <- ref_grid(interaction.VSR))
emmeans(frg, "NM")
emmeans(frg, "Acc")
emmeans(frg, "Domain")
emmeans(frg, "Age")

# scatterplot for NM and average RVS
plot(df2$NM, df2$VSR,
     main = "Neuromelanin and RVS reward activation",
     xlab = "Neuromelanin Signal",
     ylab = "Percent signal change, RVS activation",
     col = "blue")
abline(lm(df2$VSR ~ df2$NM))

VSR_cor <- cor.test(df2$NM, df2$VSR, method = "pearson")
VSR_cor

# plot of estimated marginal means for Accuracy (avg across domain) & average RVS
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(-0.0354, 0.0505)
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

VSR_Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("RVS activation by Accuracy")+
  xlab("Accuracy")+ylab("Percent signal change, RVS activation")+
  geom_col(fill = "lightgreen")+
  geom_errorbar(aes(ymin=Acc.emmeans-0.0145, ymax=Acc.emmeans+0.0145), width=.2)
VSR_Acc_plot

# plot of estimated marginal means for Domain (avg across accuracy) & average RVS
Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(-0.0189, 0.0340)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

VSR_Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("RVS activation by Domain")+
  xlab("Domain")+ylab("Percent signal change, RVS activation")+
  geom_col(fill = "lightgreen")+
  geom_errorbar(aes(ymin=Dom.emmeans-0.0145, ymax=Dom.emmeans+0.0145), width=.2)
VSR_Dom_plot


# Left Dorsal Striatum
three.way.DSL <- aov(DSL ~ NM + Acc + Domain, data = df3)
interaction.DSL <- aov(DSL ~ NM * Acc * Domain * Age, data = df3)
summary(three.way.DSL)
summary(interaction.DSL)

# Right Dorsal Striatum
three.way.DSR <- aov(DSR ~ NM + Acc + Domain, data = df3)
interaction.DSR <- aov(DSR ~ NM * Acc * Domain * Age, data = df3)
summary(three.way.DSR)


