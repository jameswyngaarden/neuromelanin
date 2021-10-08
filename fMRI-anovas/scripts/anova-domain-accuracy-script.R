
# jimmy wyngaarden, sept. 2021
# script for running anovas & ancovas on social doors college sbu data:
 
# we have four rois: left & right ventral & dorsal striatum
# at each roi, we'll run the following analyses:
# 1. anova: domain x accuracy
# 2. ancova: domain x accuracy x nm
# 3. ancova: domain x accuracy x nm x age

# set working directory
setwd("~/Documents/GitHub/neuromelanin/fMRI-anovas")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/")

# load packages
library("readxl")
library("ggpubr")
library("ggplot2")
library("reshape2")
library("emmeans")

# load data
df1 <- read_excel("~/Documents/GitHub/neuromelanin/fMRI-anovas/data/NMxPositiveAccuracy_anova3way.xlsx")

# anova: domain x accuracy, left ventral striatum
y1 <- df1$VSL
anova <- aov(y1 ~ df1$Domain + df1$Acc)
interaction <- aov(y1 ~ df1$Domain * df1$Acc)

# print results
print("Left Ventral Striatum Domain x Accuracy ANOVA Results")
summary(anova)
summary(interaction)

# plot the interaction
ggboxplot(df1, x = "Acc", y = "VSL", color = "Domain",
          palette = c("darkgreen", "blue"),
          ylab = "Percent Signal Change, Left Ventral",
          xlab = "Accuracy",
          main = "Domain x Accuracy, Left Ventral")

# calculate estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# plot estimated marginal means for accuracy (avg across domain) & average striatal activation
emcorr <- 0.283
emincorr <- -0.0606
emmon <- -0.032771
emsoc <- 0.000447
se <- 0.0146

# test for main effects; doesn't work, doesn't like that it's a single var (i.e., needs to see the values that created this avg)
t.test(emcorr, emincorr)
t.test(emmon, emsoc)

# plot main effects of accuracy and domain:
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy, Left Ventral")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain, Left Ventral")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

# anova: domain x accuracy, right ventral striatum
y1 <- df1$VSR
anova <- aov(y1 ~ df1$Domain + df1$Acc)
interaction <- aov(y1 ~ df1$Domain * df1$Acc)

# print results
print("Right Ventral Striatum Domain x Accuracy ANOVA Results")
summary(anova)
summary(interaction)

# plot the interaction
ggboxplot(df1, x = "Acc", y = "VSR", color = "Domain",
          palette = c("darkgreen", "blue"),
          ylab = "Percent Signal Change, Right Ventral",
          xlab = "Accuracy",
          main = "Domain x Accuracy, Right Ventral")

# calculate estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# plot estimated marginal means for accuracy (avg across domain) & average striatal activation
emcorr <- 0.0497
emincorr <- -0.0361
emmon <- -0.02
emsoc <- 0.0335
se <- 0.0149

# test for main effects; doesn't work, doesn't like that it's a single var (i.e., needs to see the values that created this avg)
t.test(emcorr, emincorr)
t.test(emmon, emsoc)

# plot main effects of accuracy and domain:
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy, Right Ventral")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain, Right Ventral")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

# anova: domain x accuracy, left dorsal striatum
y1 <- df1$DSL
anova <- aov(y1 ~ df1$Domain + df1$Acc)
interaction <- aov(y1 ~ df1$Domain * df1$Acc)

# print results
print("Left Dorsal Striatum Domain x Accuracy ANOVA Results")
summary(anova)
summary(interaction)

# plot the interaction
ggboxplot(df1, x = "Acc", y = "DSL", color = "Domain",
          palette = c("darkgreen", "blue"),
          ylab = "Percent Signal Change, Left Dorsal",
          xlab = "Accuracy",
          main = "Domain x Accuracy, Left Dorsal")

# calculate estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# plot estimated marginal means for accuracy (avg across domain) & average striatal activation
emcorr <- 0.0273
emincorr <- -0.0352
emmon <- -0.0132
emsoc <- 0.00531
se <- 0.0129

# test for main effects; doesn't work, doesn't like that it's a single var (i.e., needs to see the values that created this avg)
t.test(emcorr, emincorr)
t.test(emmon, emsoc)

# plot main effects of accuracy and domain:
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy, Left Dorsal")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain, Left Dorsal")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

# anova: domain x accuracy, right dorsal striatum
y1 <- df1$DSR
anova <- aov(y1 ~ df1$Domain + df1$Acc)
interaction <- aov(y1 ~ df1$Domain * df1$Acc)

# print results
print("Right Dorsal Striatum Domain x Accuracy ANOVA Results")
summary(anova)
summary(interaction)

# plot the interaction
ggboxplot(df1, x = "Acc", y = "DSL", color = "Domain",
          palette = c("darkgreen", "blue"),
          ylab = "Percent Signal Change, Right Dorsal",
          xlab = "Accuracy",
          main = "Domain x Accuracy, Right Dorsal")

# calculate estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# plot estimated marginal means for accuracy (avg across domain) & average striatal activation
emcorr <- 0.0462
emincorr <- -0.0159
emmon <- 0.012
emsoc <- 0.0183
se <- 0.011

# test for main effects; doesn't work, doesn't like that it's a single var (i.e., needs to see the values that created this avg)
t.test(emcorr, emincorr)
t.test(emmon, emsoc)

# plot main effects of accuracy and domain:
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy, Right Dorsal")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain, Right Dorsal")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# anova: domain x accuracy, ventral striatum
y1 <- df1$Ventral
anova <- aov(y1 ~ df1$Domain + df1$Acc)
interaction <- aov(y1 ~ df1$Domain * df1$Acc)

# print results
print("Ventral Striatum Domain x Accuracy ANOVA Results")
summary(anova)
summary(interaction)

# plot the interaction
ggboxplot(df1, x = "Acc", y = "Ventral", color = "Domain",
          palette = c("darkgreen", "blue"),
          ylab = "Percent Signal Change, Ventral Striatum",
          xlab = "Accuracy",
          main = "Domain x Accuracy, Ventral Striatum")

# calculate estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# plot estimated marginal means for accuracy (avg across domain) & average striatal activation
emcorr <- 0.0390
emincorr <- -0.0484
emmon <- -0.0264
emsoc <- 0.0170
se <- 0.0134

# test for main effects; doesn't work, doesn't like that it's a single var (i.e., needs to see the values that created this avg)
t.test(emcorr, emincorr)
t.test(emmon, emsoc)

# plot main effects of accuracy and domain:
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy, Ventral")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain, Ventral")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

# anova: domain x accuracy, dorsal striatum
y1 <- df1$Dorsal
anova <- aov(y1 ~ df1$Domain + df1$Acc)
interaction <- aov(y1 ~ df1$Domain * df1$Acc)

# print results
print("Dorsal Striatum Domain x Accuracy ANOVA Results")
summary(anova)
summary(interaction)

# plot the interaction
ggboxplot(df1, x = "Acc", y = "Dorsal", color = "Domain",
          palette = c("darkgreen", "blue"),
          ylab = "Percent Signal Change, Dorsal Striatum",
          xlab = "Accuracy",
          main = "Domain x Accuracy, Dorsal Striatum")

# calculate estimated marginal means (EMMs)
(frg <- ref_grid(interaction))
emmeans(frg, "Acc")
emmeans(frg, "Domain")

# plot estimated marginal means for accuracy (avg across domain) & average striatal activation
emcorr <- 0.0368
emincorr <- -0.0256
emmon <- -0.000586
emsoc <- 0.011787
se <- 0.0114

# test for main effects; doesn't work, doesn't like that it's a single var (i.e., needs to see the values that created this avg)
t.test(emcorr, emincorr)
t.test(emmon, emsoc)

# plot main effects of accuracy and domain:
Acc.levels <- c("Incorrect", "Correct")
Acc.emmeans <- c(emincorr, emcorr) #input accuracy emmeans
Acc.emmeans.df <- data.frame(Acc.levels, Acc.emmeans)

Acc_plot <- ggplot(Acc.emmeans.df, aes(x=Acc.levels, y=Acc.emmeans)) + 
  ggtitle("Striatal activation by Accuracy, Dorsal")+
  xlab("Accuracy")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Acc.emmeans-se, ymax=Acc.emmeans+se), width=.2) #input accuracy emmeans SE
Acc_plot

Dom.levels <- c("Monetary", "Social")
Dom.emmeans <- c(emmon, emsoc)
Dom.emmeans.df <- data.frame(Dom.levels, Dom.emmeans)

Dom_plot <- ggplot(Dom.emmeans.df, aes(x=Dom.levels, y=Dom.emmeans)) + 
  ggtitle("Striatal activation by Domain, Dorsal")+
  xlab("Domain")+ylab("Percent signal change, striatal activation")+
  geom_col(fill = "pink")+
  geom_errorbar(aes(ymin=Dom.emmeans-se, ymax=Dom.emmeans+se), width=.2)
Dom_plot

