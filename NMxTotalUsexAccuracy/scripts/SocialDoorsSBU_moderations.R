
# Script for Social Doors SBU moderations, NM_full as a moderator of substance use predicting striatal activation
# Jimmy Wyngaarden, November 2021

# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/")

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
df_maineffects <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
head(df_maineffects)

# # # # # # # # # # # # # # # # Moderations # # # # # # # # # # # # # # # # # # 

# Does NM Full moderate Total Use as a predictor of Monetary Ventral Striatal Left activation (correct-incorrect)
model = lm(MVSL_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.28304 -0.08381 -0.00568  0.08608  0.39512 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.1949511  0.2111411   0.923    0.363
# Total_Use          0.0030040  0.0072971   0.412    0.683
# NM_full           -0.0068927  0.0230068  -0.300    0.766
# Total_Use:NM_full -0.0004941  0.0008838  -0.559    0.580

# Residual standard error: 0.1633 on 31 degrees of freedom
# Multiple R-squared:  0.04837,	Adjusted R-squared:  -0.04372 
# F-statistic: 0.5253 on 3 and 31 DF,  p-value: 0.6682

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for MVSL_Contrast"))



# Does NM Full moderate Total Use as a predictor of Monetary Ventral Striatal Right activation (correct-incorrect)
model = lm(MVSR_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.24127 -0.06766  0.01435  0.06022  0.27824 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.0162715  0.1478489   0.110    0.913
# Total_Use          0.0061260  0.0051097   1.199    0.240
# NM_full            0.0077610  0.0161103   0.482    0.633
# Total_Use:NM_full -0.0006667  0.0006189  -1.077    0.290

# Residual standard error: 0.1143 on 31 degrees of freedom
# Multiple R-squared:  0.06854,	Adjusted R-squared:  -0.0216 
# F-statistic: 0.7604 on 3 and 31 DF,  p-value: 0.5249

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for MVSR_Contrast"))



# Does NM Full moderate Total Use as a predictor of Monetary Ventral Striatal (combined) activation (correct-incorrect)
model = lm(MVS_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.20508 -0.10138  0.01825  0.06790  0.33668 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.1056113  0.1684475   0.627    0.535
# Total_Use          0.0045650  0.0058216   0.784    0.439
# NM_full            0.0004341  0.0183548   0.024    0.981
# Total_Use:NM_full -0.0005804  0.0007051  -0.823    0.417

# Residual standard error: 0.1303 on 31 degrees of freedom
# Multiple R-squared:  0.0489,	Adjusted R-squared:  -0.04314 
# F-statistic: 0.5313 on 3 and 31 DF,  p-value: 0.6642

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for MVS_Contrast"))



# Does NM Full moderate Total Use as a predictor of Social Ventral Striatal Left activation (correct-incorrect)
model = lm(SVSL_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
#  Min       1Q   Median       3Q      Max 
# -0.37312 -0.05675 -0.01919  0.07775  0.24795 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.2712572  0.1694138   1.601    0.119
# Total_Use         -0.0070837  0.0058550  -1.210    0.235
# NM_full           -0.0253042  0.0184601  -1.371    0.180
# Total_Use:NM_full  0.0009307  0.0007091   1.312    0.199

# Residual standard error: 0.131 on 31 degrees of freedom
# Multiple R-squared:  0.07148,	Adjusted R-squared:  -0.01837 
# F-statistic: 0.7955 on 3 and 31 DF,  p-value: 0.5057

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for SVSL_Contrast"))



# Does NM Full moderate Total Use as a predictor of Social Ventral Striatal Right activation (correct-incorrect)
model = lm(SVSR_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.36963 -0.08623 -0.00914  0.10107  0.36924 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)        0.4540361  0.1989233   2.282  0.02948 * 
#   Total_Use         -0.0190242  0.0068749  -2.767  0.00945 **
#   NM_full           -0.0447408  0.0216755  -2.064  0.04746 * 
#   Total_Use:NM_full  0.0024214  0.0008326   2.908  0.00667 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.1538 on 31 degrees of freedom
# Multiple R-squared:  0.2165,	Adjusted R-squared:  0.1406 
# F-statistic: 2.855 on 3 and 31 DF,  p-value: 0.05312

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for SVSR_Contrast"))



# Does NM Full moderate Total Use as a predictor of Social Ventral Striatal (combined) activation (correct-incorrect)
model = lm(SVS_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.37138 -0.06766 -0.01781  0.07702  0.30759 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)        0.3626467  0.1757307   2.064   0.0475 *
#   Total_Use         -0.0130539  0.0060733  -2.149   0.0395 *
#   NM_full           -0.0350225  0.0191484  -1.829   0.0770 .
# Total_Use:NM_full  0.0016760  0.0007356   2.279   0.0297 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.1359 on 31 degrees of freedom
# Multiple R-squared:  0.1482,	Adjusted R-squared:  0.06578 
# F-statistic: 1.798 on 3 and 31 DF,  p-value: 0.1681

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for SVS_Contrast"))



# Does NM Full moderate Total Use as a predictor of Monetary Dorsal Striatal Left activation (correct-incorrect)
model = lm(MDSL_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)

# Residuals:
# Min       1Q   Median       3Q      Max 
# -0.16726 -0.04499 -0.01860  0.03437  0.31444 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.0925075  0.1307860   0.707    0.485
# Total_Use          0.0023780  0.0045200   0.526    0.603
# NM_full           -0.0021761  0.0142510  -0.153    0.880
# Total_Use:NM_full -0.0003933  0.0005474  -0.718    0.478

# Residual standard error: 0.1011 on 31 degrees of freedom
# Multiple R-squared:  0.0557,	Adjusted R-squared:  -0.03569 
# F-statistic: 0.6095 on 3 and 31 DF,  p-value: 0.6139

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for MDSL_Contrast"))



# Does NM Full moderate Total Use as a predictor of Monetary Dorsal Striatal Right activation (correct-incorrect)
model = lm(MDSR_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
# Min       1Q   Median       3Q      Max 
# -0.14988 -0.05374 -0.01137  0.04776  0.22149 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.0820368  0.1024849   0.800    0.430
# Total_Use          0.0033152  0.0035419   0.936    0.357
# NM_full           -0.0008041  0.0111672  -0.072    0.943
# Total_Use:NM_full -0.0004703  0.0004290  -1.096    0.281

# Residual standard error: 0.07925 on 31 degrees of freedom
# Multiple R-squared:  0.09118,	Adjusted R-squared:  0.003226 
# F-statistic: 1.037 on 3 and 31 DF,  p-value: 0.39

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for MDSR_Contrast"))



# Does NM Full moderate Total Use as a predictor of Monetary Dorsal Striatal (combined) activation (correct-incorrect)
model = lm(MDS_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
# Min       1Q   Median       3Q      Max 
# -0.15857 -0.04814 -0.01334  0.04549  0.26797 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.0872721  0.1113270   0.784    0.439
# Total_Use          0.0028466  0.0038475   0.740    0.465
# NM_full           -0.0014901  0.0121307  -0.123    0.903
# Total_Use:NM_full -0.0004318  0.0004660  -0.927    0.361

# Residual standard error: 0.08609 on 31 degrees of freedom
# Multiple R-squared:  0.07547,	Adjusted R-squared:  -0.014 
# F-statistic: 0.8436 on 3 and 31 DF,  p-value: 0.4805

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for MDS_Contrast"))



# Does NM Full moderate Total Use as a predictor of Social Dorsal Striatal Left activation (correct-incorrect)
model = lm(SDSL_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
# Min        1Q    Median        3Q       Max 
# -0.151716 -0.073432 -0.008228  0.074565  0.296486 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.2210053  0.1384561   1.596    0.121
# Total_Use         -0.0058662  0.0047851  -1.226    0.229
# NM_full           -0.0188393  0.0150868  -1.249    0.221
# Total_Use:NM_full  0.0007813  0.0005795   1.348    0.187

# Residual standard error: 0.1071 on 31 degrees of freedom
# Multiple R-squared:  0.06822,	Adjusted R-squared:  -0.02196 
# F-statistic: 0.7565 on 3 and 31 DF,  p-value: 0.5271

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for SDSL_Contrast"))



# Does NM Full moderate Total Use as a predictor of Social Dorsal Striatal Right activation (correct-incorrect)
model = lm(SDSR_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
# Min        1Q    Median        3Q       Max 
# -0.178065 -0.061655 -0.001039  0.028576  0.232304 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)        0.2716637  0.1313711   2.068   0.0471 *
#   Total_Use         -0.0097572  0.0045402  -2.149   0.0395 *
#   NM_full           -0.0254468  0.0143148  -1.778   0.0853 .
# Total_Use:NM_full  0.0012586  0.0005499   2.289   0.0291 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.1016 on 31 degrees of freedom
# Multiple R-squared:  0.1491,	Adjusted R-squared:  0.06677 
# F-statistic: 1.811 on 3 and 31 DF,  p-value: 0.1657

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for SDSR_Contrast"))



# Does NM Full moderate Total Use as a predictor of Social Dorsal Striatal (combined) activation (correct-incorrect)
model = lm(SDS_Contrast ~ Total_Use * NM_full, 
           data=na.omit(df_maineffects))
summary(model)
# Residuals:
# Min       1Q   Median       3Q      Max 
# -0.16489 -0.06990 -0.01598  0.04962  0.26439 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)        0.2463345  0.1315226   1.873   0.0705 .
# Total_Use         -0.0078117  0.0045455  -1.719   0.0957 .
# NM_full           -0.0221431  0.0143313  -1.545   0.1325  
# Total_Use:NM_full  0.0010200  0.0005505   1.853   0.0735 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.1017 on 31 degrees of freedom
# Multiple R-squared:  0.1079,	Adjusted R-squared:  0.02159 
# F-statistic:  1.25 on 3 and 31 DF,  p-value: 0.3085

interaction <- plot_model(model, type = "int",
                          mdrt.values = "meansd",
                          show.data = TRUE,)
print(interaction)

sim_slopes(model, pred = Total_Use , modx = NM_full, jnplot = TRUE, title = paste("J-N Plot for SDS_Contrast"))

