
# Script for analyzing social doors NM data: NM x Accuracy x Domain
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
library("emmeans")

# import data

# use this for 3-way anova:
df1 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_anova3way.xlsx")

# use this for 2-way analyses (e.g., scatter plots, bar graphs)
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Analyses: Does striatal activation vary by NM, Accuracy, or Domain?

# In this script, we're running 3-way anovas for NM x Accuracy x Domain in each of 
# the four ROIs. Run the 3-way anova first to determine if the overall model is significant.
# Then plot main effects for each var, followed by interaction (NM x Correct & Incorrect
# in each domain)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# anova models
y1 <- df1$VSL

three.way <- aov(y1 ~ df1$NM_full + df1$Acc + df1$Domain)
interaction <- aov(y1 ~ df1$NM_full * df1$Acc * df1$Domain)
summary(three.way)
summary(interaction)

# estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
#emmeans(frg, "NM_full")
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# plot of estimated marginal means for Accuracy (avg across domain) & average striatal activation
y2 <- df2$DSR
emcorr <- 0.0462
emincorr <- -0.0159
emmon <- 0.0120
emsoc <- 0.0183
se <- 0.0111

t.test(emcorr, emincorr)

Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# plot of estimated marginal means for Domain (avg across accuracy) & average RVS
Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# scatter plot for NM and average striatal activation
plot(df2$NM_full, y2, pch=19,
     main = "Neuromelanin and Striatal reward activation",
     xlab = "Neuromelanin Signal",
     ylab = "Percent signal change, striatal activation",
     col = "pink")
abline(lm(y2 ~ df2$NM_full))

y2 <- df2$VSL
cor <- cor.test(df2$NM_full, y2, method = "pearson")
cor


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# plot interactions: NM x activation for correct & incorrect trials, one plot per domain
# NOTE: these are the same plots as from the correlations_NMxStriatalActivation.R script

# scatter plot for correct & incorrect: monetary
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

# scatter plot for correct & incorrect: social
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






