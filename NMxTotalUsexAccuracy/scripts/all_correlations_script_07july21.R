## Script for correlations between Social Doors stats, NM values,  substance use
## Jimmy Wyngaarden, 07 July 2021

###############################################################################
###############################################################################

setwd("~/Documents/GitHub/neuromelanin/NMxAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/")

# load packages
library("readxl")
library("ggpubr")
library("plot")
library("Hmisc")

#install.packages("apaTables")
library("apaTables")

# import data
df <- read_excel("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/all_correlations_07july21.xlsx")
head(df)

setwd("~/Documents/GitHub/neuromelanin/NMxAccuracy/data/")

apa.cor.table(df, filename = "correlations.doc", table.number = NA, show.conf.interval = TRUE, landscape = TRUE)

mcor <- round(cor(df),4)

mcor
write.csv(mcor, 'correlationmatrix.csv')

# from displayr website:
mydata = df
mydata.cor <- cor(df)
mydata.cor

mydata.rcorr = rcorr(as.matrix(df))
mydata.rcorr

mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
write.csv(mydata.coeff, 'correlationmatrix_coeff.csv')
write.csv(mydata.p, 'correlationmatrix_p.csv')

###############################################################################

