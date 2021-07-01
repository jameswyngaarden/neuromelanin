
# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/")

# load packages
library("readxl")
#library("ggpubr")

install.packages("readxl")

# import data
df <- read_excel("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/NMxPositiveAccuracy_anova.xlsx")
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
df3 <- read_excel("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/NMxPositiveAccuracy_anova3way.xlsx")
head(df)
summary(df)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANOVAs: Accuracy(correct vs. Incorrect) x Neuromelanin Signal -> % Signal Change BOLD Activation?
# - ran this analysis for monetary left ventral striatum (mvsl), mvsr, sdsl, sdsr
# - for significant main effect of NM: combined accuracy (averaged together) and looked at linear relationship between NM & Accuracy
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


