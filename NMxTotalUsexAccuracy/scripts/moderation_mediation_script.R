# Script for analyzing social doors NM data
# Jimmy Wyngaarden, July 2021

# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/")

# install.packages("readxl")
#install.packages("olsrr")
#install.packages("performance")
#install.packages("ggplot2")
#install.packages("sjPlot")
#install.packages("reshape2")
install.packages("interactions")

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
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
head(df2)

df3 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
head(df3)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Mediation analyses

# NM_full as mediator for Total Use & Striatal (MVSL) activation?

# Mediation with PROCESS (model 4)
#process(data = df2, y = "SDSR_Contrast", x = "Substance_Abuse", m = "NM_full", model = 4)

# Moderation with PROCESS (model 1)
process(data = df3, y = "SDSR_Contrast", x = "Total_Use", w = "NM_full", model = 1)

# Moderation using lm
model = lm(SDSR_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df3))
summary(model)

# Plotting simple slopes 10 Aug 2021
interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE)
print(interaction)

# Plot Johnson-Neyman
sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = "J-N Plot for SDSR_Contrast Model")

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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Set Input Variables
use <- df3$Total_Use
nm <- df3$NM_full

# stats for contrast
cor <- cor.test(use, nm, method = "pearson")
cor

# scatter plot for contrast
plot(nm, use, pch=19,
     main = "NM Full & Total Substance Use",
     xlab = "NM Full Signal",
     ylab = "Total Substance Use",
     col = "blue")
abline(lm(use ~ nm))






