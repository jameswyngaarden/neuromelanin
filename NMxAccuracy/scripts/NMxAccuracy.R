
# set working directory
setwd("~/Desktop/")

# load packages
library("readxl")
library("ggpubr")

# import data
df <- read_excel("NMxPositiveAccuracy_anova.xlsx")
head(df)
summary(df)

# # # # # # # # # # # # # # # # # 
# Monetary: Left Ventral Striatum
two.way.VSL <- aov(VSL ~ NM + Acc, data = df)
interaction.VSL <- aov(VSL ~ NM*Acc, data = df)

summary(two.way.VSL)
summary(interaction.VSL)
#plot(two.way.VSL)

# # # # # # # # # # # # # # # # # 
# Monetary: Right Ventral Striatum
two.way.VSR <- aov(VSR ~ NM + Acc, data = df)
interaction.VSR <- aov(VSR ~ NM*Acc, data = df)

summary(two.way.VSR)
summary(interaction.VSR)
#plot(two.way.VSR)

# # # # # # # # # # # # # # # # # 
# Social: Left Dorsal Striatum
two.way.DSL <- aov(DSL ~ NM + Acc, data = df)
interaction.DSL <- aov(VSR ~ NM*Acc, data = df)

summary(two.way.DSL)
summary(interaction.DSL)
#plot(two.way.DSL)

# # # # # # # # # # # # # # # # # 
# Social: Right Dorsal Striatum
two.way.DSR <- aov(DSR ~ NM + Acc, data = df)
interaction.DSR <- aov(VSR ~ NM*Acc, data = df)

summary(two.way.DSR)
summary(interaction.DSR)
#plot(two.way.DSR)

