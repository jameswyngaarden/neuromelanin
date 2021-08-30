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
library("dplyr")

# import data
df2 <- read_excel("~/Documents/GitHub/neuromelanin/NMxTotalUsexAccuracy/data/NMxPositiveAccuracy_maineffects.xlsx")
head(df2)
model14 <- subset(df2, select = c("NM_full", "Total_Use", "SVSR_Contrast", "SVSR_Incorrect", "SVSR_Correct"))
model30 <- subset(df2, select = c("NM_full", "Total_Use", "SDSR_Contrast", "SDSR_Incorrect", "SDSR_Correct"))
head(model14)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Heat map of all correlations:
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

ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
  
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    #legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
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

# Plot
library(ggcorrplot)
ggcorrplot(pcor$estimate,
           method = "square",  #"square" (default), "circle"
           type = "lower", # "full" (default), "lower" or "upper" display.
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
