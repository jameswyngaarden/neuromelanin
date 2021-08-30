# Correlations

# Script for analyzing social doors NM data: specifically, correlations
# Jimmy Wyngaarden, August 2021



# set working directory
setwd("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy")
maindir <- getwd()
datadir <- file.path("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/")

# load packages
library("tidyverse")
library("readxl")
library("ggpubr")
library("ggplot2")
library("reshape2")
library("ppcor")


# import data
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
head(df2)
model14 <- df2 %>% select(NM_full, Total_Use, SVSR_Contrast, SVSR_Incorrect, SVSR_Correct)


# heat map of all correlations:
cormat <- round(cor(model14),2)
head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)
head(melted_cormat)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
  


# Partial Correlations

# calculate the correlations between each pair with all other variables are adjusted 
partial.corr <- pcor(x=model14, method="pearson")
#Results interpretation: ?pcor()
df <- model14

# calculate the correlations between each pair with specified variables are adjusted 
partial_correlation <- function(df) {
  n <- ncol(df)
  results <- list() # define an empty list
  results[["estimate"]] <- sapply(1:n, function(x) {
    sapply(1:n, function(y) {
      ifelse(x == y, 1, pcor.test(df[,x], df[,y], df[,c((n-2):n)], method="pearson")$estimate)
    })
  })
  results[["p.value"]] <- sapply(1:n, function(x) {
    sapply(1:n, function(y) {
      ifelse(x == y, 0, pcor.test(df[,x], df[,y], df[,c((n-2):n)], method="pearson")$p.value)
    })
  })
  colnames(results[["estimate"]]) <- rownames(results[["estimate"]]) <- colnames(df[, c(1:n)])
  colnames(results[["p.value"]]) <- rownames(results[["p.value"]]) <- colnames(df[, c(1:(n))])
  return(results)
}

pcor <- partial_correlation(df)
cormat2 <- pcor$estimate

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat2){
  cormat[upper.tri(cormat2)] <- NA
  return(cormat2)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat2){
  cormat[lower.tri(cormat2)]<- NA
  return(cormat2)
}

upper_tri <- get_upper_tri(cormat2)
upper_tri

melted_cormat2 <- melt(upper_tri, na.rm = TRUE)
head(melted_cormat2)

ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()










library(ggcorrplot)
ggcorrplot(pcor$estimate,
           method = "square",  #"square" (default), "circle"
           type = "full", # "full" (default), "lower" or "upper" display.
           hc.order = TRUE, #logical value. If TRUE, correlation matrix will be hc.ordered using hclust function.
           outline.col = "black", #the outline color of square or circle. Default "gray"
           ggtheme = ggplot2::theme_classic, #Change theme
           colors = c("blue", "white", "red"), #Change colors
           lab = TRUE, #Add correlation coefficients
           sig.level=0.05, #set significant level
           p.mat = pcor$p.value, #Add correlation significance level
           #insig = "blank", #Leave blank on no significant coefficient
           title = "Partial correlations for model 14"
)
