#This script creates figure 2: retention basins and figure S1: monthly water budgets of retention basins.

#Required packages:
library(ggplot2)
library(ggtern)
library(dplyr)

#Set project directory
setwd("E:/TrianglR/WaterBudgetTriangle")

#load dataset
R <- read.csv("data/Retention.csv")

#calculate ratios from raw values
R$sum <- R$Q + R$I + R$ET
R$Qr <- R$Q/R$sum * 100
R$Ir <- R$I/R$sum * 100
R$ETr <- R$ET/R$sum * 100

#text label adjustments
R$vjust <- c(0.3,0.3,0,0.3,0,0,0,0.3,0.3)
R$hjust <- c(-0.2,-0.2,1.2,-0.2,1.2,1.2,1.2,-0.2,-0.2)
R$Site <- c("Club II 2009", "Club II 2010", "Elder Creek", "Poppleton   ","Navy Canal", "Palm Bay", "Tampa  ", "Austin 1953", "Austin 1956")

#amended dataset
str(R)
R

#plot on Ternary diagram
pdf("results/Fig2Retention_HRT.pdf", height = 4, width = 6)
ggtern(data= R, aes(Qr, Ir, ETr)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + geom_mask() +
  geom_point(aes(color = log(HRTime)), size = 2) +
  geom_point(aes(color = log(HRTime)), size = 5, alpha = 0.4) +
  scale_colour_gradient("Hydraulic\nRetention\nTime",low = "darkblue", high= "red", 
                        breaks=c(2.996,3.689,4.605,5.298,5.991),
                        labels=c("20","40","100","200","400 days")) +
  geom_text(aes(label= Site,vjust = vjust, hjust = hjust), size = 2.5)
dev.off()

ggsave("results/Fig2Retention_HRT.png", height = 4, width = 6, dpi = 900, device = "png")
ggtern(data= R, aes(Qr, Ir, ETr)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + geom_mask() +
  geom_point(aes(color = log(HRTime)), size = 2) +
  geom_point(aes(color = log(HRTime)), size = 5, alpha = 0.4) +
  scale_colour_gradient("Hydraulic\nRetention\nTime",low = "darkblue", high= "red", 
                        breaks=c(2.996,3.689,4.605,5.298,5.991),
                        labels=c("20","40","100","200","400 days")) +
  geom_text(aes(label= Site,vjust = vjust, hjust = hjust), size = 2.5)
dev.off()

#Calculate summary statistics: mean, median and count
R %>% summarize(avgQ = mean(Qr), avgI = mean(Ir), avgET = mean(ETr),
                medQ = median(Qr), medI = median(Ir), medET = median(ETr),
                n = n())

#Get quantile ranges (rounded to nearest 5%)
round(quantile(R$Qr/5,c(0.05,0.95)))*5 #Q
round(quantile(R$Ir/5,c(0.05,0.95)))*5 #I
round(quantile(R$ETr/5,c(0.05,0.95)))*5 #ET

#Get confidence intervals around mean values
#How many times do you want to sample?
S <- 1000
m <- numeric(S) #initialize vector of means

#resample Q
set.seed(1)
for(i in 1:S) {
  s <- sample(R$Qr, size = length(R$Qr), replace = T)
  m[i] <- mean(s)
}
hist(m) #histogram of resampled means
mean(m) #mean of means
#5th and 95th percentiles of Q distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample I
set.seed(1)
for(i in 1:S) {
  s <- sample(R$Ir, size = length(R$Ir), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of I distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

#resample ET
set.seed(1)
for(i in 1:S) {
  s <- sample(R$ETr, size = length(R$ETr), replace = T)
  m[i] <- mean(s)
}
hist(m)
mean(m)
#5th and 95th percentiles of ET distribution of means (rounded to nearest 1%)
round(quantile(m, c(0.05,0.95)))

SR <- read.csv("data/SeasonalRetention.csv")
SR$Date <- as.Date(SR$Date, format = "%m/%d/%Y")
SR$Month <- month(SR$Date)

#load color blind palette
cbPalette <- c("#999999", 
               "#E69F00", 
               "#56B4E9", 
               "#009E73", 
               "#F0E442", 
               "#0072B2", 
               "#D55E00", 
               "#CC79A7",
               "#000000")

#plot seasonal retention
#Q,I and ET are all in mm
pdf("results/FigS1SeasonalRetention.pdf", height = 5, width = 6)
ggtern(data= SR, aes(Q, I, ET, color = Site)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + geom_mask() +
  geom_point(aes(size = P.cm), alpha = 0.65) +
  scale_colour_manual(values=cbPalette) +
  scale_size_area("Precipitation\n(cm)")
dev.off()

ggsave("results/FigS1SeasonalRetention.png", height = 5, width = 6, dpi = 900, device = "png")
ggtern(data= SR, aes(Q, I, ET, color = Site)) + 
  theme_bw() + theme_clockwise() +
  theme_rotate(60) + theme_showarrows() +
  Llab("Q", labelarrow = "Q - Runoff") +
  Tlab("I", labelarrow = "I - Infiltration") +
  Rlab("ET", labelarrow = "ET - Evapotranspiration") + geom_mask() +
  geom_point(aes(size = P.cm), alpha = 0.65) +
  scale_colour_manual(values=cbPalette) +
  scale_size_area("Precipitation\n(cm)")
dev.off()
